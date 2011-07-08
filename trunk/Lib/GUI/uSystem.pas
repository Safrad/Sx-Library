unit uSystem;

interface

uses uTypes, SysUtils, Forms, ShlObj, Controls, Classes, uFile;

procedure StringArrayToStrings(const StringArray: array of string; const Strings: TStrings; const StartIndex: SG = 0);

function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: U1): Integer;
function ThreadPriority(const Prior: U1): Integer;

procedure BeginLongOperation(const Background: BG = False);
procedure EndLongOperation(const Sound: BG = True);

function ReadLinesFromFile(const FileName: TFileName; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG; overload;
function ReadLinesFromFile(const F: TFile; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG; overload;
function WriteLinesToFile(const FileName: TFileName; const Lines: TStrings; const Append: BG): BG;
function ReadStreamFromFile(const FileName: TFileName; Stream: TMemoryStream): BG;
function WriteStreamToFile(const FileName: TFileName; Stream: TMemoryStream): BG;

type
	TDriveLetter = 'A'..'Z';
	TDriveInfo = packed record // 32
		FreeSpace: U8;
		DriveSize: U8;
		ClusterSize: U4;
		DriveType: U1;
		DriveLetter: TDriveLetter; // 1
		Reserved: array[0..9] of U8; // 10
	end;
function GetDriveInfo(const Drive: TDriveLetter): TDriveInfo;

function SelectFolder(var Path: string; const browseTitle: string = ''): BG;
function SelectFile(var FileName: TFileName; const browseTitle: string = ''; const Filter: string = ''; const Save: BG = False): BG;

implementation

uses
	Windows, Math, Dialogs,
	uStrings, uFiles, uDParser, uWave, uMath, uDictionary;

procedure StringArrayToStrings(const StringArray: array of string; const Strings: TStrings; const StartIndex: SG = 0);
var
	i: SG;
begin
	Strings.BeginUpdate;
	try
		for i := StartIndex to Length(StringArray) - 1 do
		begin
			Strings.Add(StringArray[i]);
		end;
	finally
		Strings.EndUpdate;
	end;
end;

function DriveTypeToStr(const DriveType: Integer): string;
begin
	Result := '';
	case DriveType of
	DRIVE_UNKNOWN:  Result := 'Unknown'; // The drive type cannot be determined.
	DRIVE_NO_ROOT_DIR: Result := 'No root dir'; // The root directory does not exist.
	DRIVE_REMOVABLE: Result := 'Removable'; // The drive can be removed from the drive.
	DRIVE_FIXED: Result := 'Fixed'; // The disk cannot be removed from the drive.
	DRIVE_REMOTE: Result := 'Remote'; // The drive is a remote (network) drive.
	DRIVE_CDROM: Result := 'CD/DVD'; // The drive is a CD-ROM drive.
	DRIVE_RAMDISK: Result := 'Ramdisk'; // The drive is a RAM disk.
	end;
end;

function ProcessPriority(const Prior: U1): Integer;
begin
	case Prior of
	0: Result := IDLE_PRIORITY_CLASS;
	1: Result := $00004000; //BELOW_NORMAL_PRIORITY_CLASS
	2: Result := NORMAL_PRIORITY_CLASS;
	3: Result := $00008000; //ABOVE_NORMAL_PRIORITY_CLASS
	4: Result := HIGH_PRIORITY_CLASS;
	5: Result := REALTIME_PRIORITY_CLASS;
	else
		Result := NORMAL_PRIORITY_CLASS;
	end;
end;

function ThreadPriority(const Prior: U1): Integer;
begin
	case Prior of
	0: Result := THREAD_PRIORITY_IDLE;
	1: Result := THREAD_PRIORITY_LOWEST;
	2: Result := THREAD_PRIORITY_BELOW_NORMAL;
	3: Result := THREAD_PRIORITY_NORMAL;
	4: Result := THREAD_PRIORITY_ABOVE_NORMAL;
	5: Result := THREAD_PRIORITY_HIGHEST;
	6: Result := THREAD_PRIORITY_TIME_CRITICAL;
	else
		Result := THREAD_PRIORITY_NORMAL;
	end;
end;

procedure BeginLongOperation(const Background: BG = False);
begin
	if Background then
		Screen.Cursor := crAppStart
	else
		Screen.Cursor := crHourGlass;
end;

procedure EndLongOperation(const Sound: BG);
begin
	if Sound then
		PlayWinSound(wsAsterisk);
	Screen.Cursor := crDefault;
end;

function GetDriveInfo(const Drive: TDriveLetter): TDriveInfo;
var
	P: array[0..3] of Char;
	SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
	TotalNumberOfClusters: U4;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.DriveLetter := Drive;
	P[0] := Drive;
	P[1] := DriveDelim;
	P[2] := PathDelim;
	P[3] := CharNul;
	Result.DriveType := GetDriveType(P);
	Result.FreeSpace := -1;
	Result.DriveSize := -1;
	case Result.DriveType of
//  DRIVE_UNKNOWN:  Result := 4096;
	DRIVE_NO_ROOT_DIR: Result.ClusterSize := 0;
	DRIVE_REMOVABLE, DRIVE_CDROM:
	begin
		// Skip media
	end
	else
	begin
		SectorsPerCluster := 0;
		BytesPerSector := 0;
		if GetDiskFreeSpace(P, SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
			TotalNumberOfClusters) then
			Result.ClusterSize := SectorsPerCluster * BytesPerSector;
		Result.FreeSpace := Result.ClusterSize * U8(NumberOfFreeClusters);
		Result.DriveSize := Result.ClusterSize * U8(TotalNumberOfClusters);
	end;
	end;
	if Result.ClusterSize = 0 then
		case Result.DriveType of
		DRIVE_UNKNOWN:  Result.ClusterSize := 4096;
		DRIVE_FIXED: Result.ClusterSize := 4096;
		DRIVE_REMOTE: Result.ClusterSize := 4096;
		DRIVE_CDROM: Result.ClusterSize := 2048;
		DRIVE_REMOVABLE: Result.ClusterSize := 512;
		end;
end;

var
	lg_StartFolder: String;

///////////////////////////////////////////////////////////////////
// Call back function used to set the initial browse directory.
///////////////////////////////////////////////////////////////////
function BrowseForFolderCallBack(Wnd: HWND; uMsg: UINT;
				lParam, lpData: LPARAM): Integer stdcall;
begin
	if uMsg = BFFM_INITIALIZED then
		SendMessage(Wnd,BFFM_SETSELECTION,1,Integer(@lg_StartFolder[1]));
	result := 0;
end;

function SelectFolder(var Path: string; const browseTitle: string = ''): BG;
var
	browse_info: TBrowseInfo;
	folder: array[0..MAX_PATH] of char;
	find_context: PItemIDList;
begin
	FillChar(browse_info, SizeOf(browse_info), #0);
	lg_StartFolder := RepairDirectory(ExpandDir(Path));
	browse_info.pszDisplayName := @folder[0];
	if browseTitle <> '' then
		browse_info.lpszTitle := PChar(Translate('Select' + CharSpace + browseTitle) + '.')
	else
		browse_info.lpszTitle := '';
	browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI or BIF_VALIDATE;
	browse_info.hwndOwner := Application.Handle;
	if Path <> '' then
		browse_info.lpfn := BrowseForFolderCallBack;
	find_context := SHBrowseForFolder(browse_info);
	if Assigned(find_context) then
	begin
		Result := SHGetPathFromIDList(find_context, folder);
		Path := folder;
		CorrectDir(Path);
		GlobalFreePtr(find_context);
	end
	else
		result := False;
end;

var
	OpenDialog: TOpenDialog;
	SaveDialog: TSaveDialog;

function SelectFile(var FileName: TFileName; const browseTitle: string = ''; const Filter: string = ''; const Save: BG = False): BG;
begin
	if Save then
	begin
		if SaveDialog = nil then
			SaveDialog := TSaveDialog.Create(nil);
		if Filter = '' then
			SaveDialog.Filter := AllFiles
		else
			SaveDialog.Filter := Filter;
		SaveDialog.Options := SaveDialog.Options + [ofPathMustExist];
		SaveDialog.Options := SaveDialog.Options - [ofFileMustExist] + [ofOverwritePrompt];
		SaveDialog.Title := browseTitle;
		Result := ExecuteDialog(SaveDialog, FileName);
	end
	else
	begin
		if OpenDialog = nil then
			OpenDialog := TOpenDialog.Create(nil);
		if Filter = '' then
			OpenDialog.Filter := AllFiles
		else
			OpenDialog.Filter := Filter;
		OpenDialog.Options := OpenDialog.Options + [ofPathMustExist];
		OpenDialog.Options := OpenDialog.Options + [ofFileMustExist];
		OpenDialog.Title := browseTitle;
		Result := ExecuteDialog(OpenDialog, FileName);
	end;
end;

function ReadLinesFromFile(const F: TFile; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG;
var
	Line: string;
begin
	Assert(Lines <> nil);
	Lines.Clear;
	while not F.Eof do
	begin
		F.Readln(Line);
		Lines.Add(Line);
	end;
	Result := True;
end;

function ReadLinesFromFile(const FileName: TFileName; Lines: TStrings; const DefaultCharset: TFileCharset = fcAnsi): BG;
var
	F: TFile;
//	Line: string;
begin
	Assert(Lines <> nil);
	Result := False;
//	Lines.Clear;
	F := TFile.Create;
	try
		F.DefaultCharset := DefaultCharset;
		if F.Open(FileName, fmReadOnly) then
		begin
			ReadLinesFromFile(F, Lines, DefaultCharset);
{			while not F.Eof do
			begin
				F.Readln(Line);
				Lines.Add(Line);
			end;}
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function WriteLinesToFile(const FileName: TFileName; const Lines: TStrings; const Append: BG): BG;
var
	F: TFile;
	i: SG;
	FileMode: TFileMode;
begin
	Result := False;
	if Append then
		FileMode := fmAppend
	else
		FileMode := fmRewrite;
	F := TFile.Create;
	try
		if F.Open(FileName, FileMode) then
		begin
			i := 0;
			while i < Lines.Count do
			begin
				F.Write(Lines[i] + FileSep);
				Inc(i);
			end;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadStreamFromFile(const FileName: TFileName; Stream: TMemoryStream): BG;
var
	Buf: Pointer;
	Count: SG;
begin
	try
		Result := ReadBufferFromFile(FileName, Buf, Count);
	{	Stream.SetSize(Count);
		Stream.Seek(0, 0);}
		if Buf <> nil then
		begin
			Stream.WriteBuffer(Buf^, Count);
			Stream.Seek(0, 0);
		end;
	finally
		FreeMem(Buf);
	end;
end;

function WriteStreamToFile(const FileName: TFileName; Stream: TMemoryStream): BG;
var
	Buf: Pointer;
begin
	GetMem(Buf, Stream.Size);
	Stream.Seek(0, 0);
	Stream.ReadBuffer(Buf^, Stream.Size);
	Result := WriteBufferToFile(FileName, Buf, Stream.Size);
	FreeMem(Buf);
end;

initialization

finalization
	FreeAndNil(OpenDialog);
end.
