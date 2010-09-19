//* File:     Lib\uSystem.pas
//* Created:  1998-01-01
//* Modified: 2005-08-29
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSystem;

interface

uses uTypes, SysUtils, Forms, ShlObj, ActiveX, ComObj, ComCtrls, Controls, Classes;

function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: U1): Integer;
function ThreadPriority(const Prior: U1): Integer;

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const ReadOnly: BG; const Index, Count: Integer): string;

procedure BeginLongOperation(const Background: BG = False);
procedure EndLongOperation(const Sound: BG = True);

procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: U2;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
function WriteLinesToFile(var FileName: TFileName; Lines: TStrings; Append: BG): BG;
function ReadStreamFromFile(var FileName: TFileName; Stream: TMemoryStream): BG;
function WriteStreamToFile(var FileName: TFileName; Stream: TMemoryStream): BG;

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
function GetDriveInfo(const Drive: U1): TDriveInfo;

function SelectFolder(var Path: string; browseTitle: string = ''): BG;

implementation

uses
	Windows, Math, Dialogs, ShellAPI,
	uStrings, uInput, uFiles, uParser, uWave, uMath, uFormat, uError;

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
	1: Result := NORMAL_PRIORITY_CLASS;
	2: Result := HIGH_PRIORITY_CLASS;
	3: Result := REALTIME_PRIORITY_CLASS;
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

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const ReadOnly: BG; const Index, Count: Integer): string;
begin
	Result := Application.Title;
	if Count > 0 then
	begin
		Result := Result + ' - ';
		if Count > 1 then
			Result := Result + '(' + NToS(Index + 1) + '/' + NToS(Count) + ') ';
		Result := Result + ShortDir(FName);
		if Changed then Result := Result + ' *';
		if New <> 0 then Result := Result + ' (New)';
		if ReadOnly then Result := Result + ' (Read Only)';
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

procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: U2;
	const Description: string;
	const IconFileName: TFileName;
	const IconIdex: Integer);
var
	MyObject : IUnknown;
	MySLink : IShellLink;
	MyPFile : IPersistFile;
begin
	MyObject := CreateComObject(CLSID_ShellLink);
	MySLink := MyObject as IShellLink;
	MyPFile := MyObject as IPersistFile;

	MySLink.SetArguments(@Arguments[1]);
	MySLink.SetPath(@Target[1]);
	MySLink.SetWorkingDirectory(@StartIn[1]);
	MySLink.SetDescription(@Description[1]);
	MySLink.SetIconLocation(@IconFileName[1], IconIdex);
	MySLink.SetHotkey(HotKey);

	if not DirectoryExists(ExtractFileDir(LinkFileName)) then
		CreateDir(ExtractFileDir(LinkFileName));
	MyPFile.Save(PWChar(LinkFileName), False);
	MySLink := nil;
	MyPFile := nil;
	MyObject := nil;
end;

function ReadStreamFromFile(var FileName: TFileName; Stream: TMemoryStream): BG;
label LRetry;
var
	Buf: Pointer;
	Count: SG;
begin
	Result := ReadBufferFromFile(FileName, Buf, Count);
{	Stream.SetSize(Count);
	Stream.Seek(0, 0);}
	Stream.WriteBuffer(Buf^, Count);
	Stream.Seek(0, 0);
	FreeMem(Buf);
end;

function WriteStreamToFile(var FileName: TFileName; Stream: TMemoryStream): BG;
label LRetry;
var
	Buf: Pointer;
begin
	GetMem(Buf, Stream.Size);
	Stream.Seek(0, 0);
	Stream.ReadBuffer(Buf^, Stream.Size);
	Result := WriteBufferToFile(FileName, Buf, Stream.Size);
	FreeMem(Buf);
end;

function GetDriveInfo(const Drive: U1): TDriveInfo;
var
	P: array[0..3] of Char;
	SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
	TotalNumberOfClusters: U4;
begin
	FillChar(Result, SizeOf(Result), 0);
	Result.DriveLetter := Char(Drive + Ord('A'));
	P[0] := Chr(Drive + Ord('A'));
	P[1] := ':';
	P[2] := '\';
	P[3] := CharNul;
	Result.DriveType := GetDriveType(P);
	case Result.DriveType of
//  DRIVE_UNKNOWN:  Result := 4096;
	DRIVE_NO_ROOT_DIR: Result.ClusterSize := 0;
	DRIVE_REMOVABLE:
	begin
		Result.ClusterSize := 512;
		Result.FreeSpace := -1;
		Result.DriveSize := -1;
	end;
{ DRIVE_FIXED: Result := 4096;
	DRIVE_REMOTE: Result := 4096;
	DRIVE_CDROM: Result := 2048;
	DRIVE_RAMDISK: Result := 4096;}
	else
	begin
		SectorsPerCluster := 0;
		BytesPerSector := 0;
		if GetDiskFreeSpace(P, SectorsPerCluster, BytesPerSector, NumberOfFreeClusters,
			TotalNumberOfClusters) then
			Result.ClusterSize := SectorsPerCluster * BytesPerSector;
		Result.FreeSpace := Result.ClusterSize * U8(NumberOfFreeClusters);
		Result.DriveSize := Result.ClusterSize * U8(TotalNumberOfClusters);
		if Result.ClusterSize = 0 then
			case Result.DriveType of
			DRIVE_UNKNOWN:  Result.ClusterSize := 4096;
			DRIVE_FIXED: Result.ClusterSize := 4096;
			DRIVE_REMOTE: Result.ClusterSize := 4096;
			DRIVE_CDROM: Result.ClusterSize := 2048;
			end;
	end;
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

function SelectFolder(var Path: string; browseTitle: string = ''): BG;
var
	browse_info: TBrowseInfo;
	folder: array[0..MAX_PATH] of char;
	find_context: PItemIDList;
begin
	FillChar(browse_info,SizeOf(browse_info),#0);
	lg_StartFolder := RepairDirectory(Path);
	browse_info.pszDisplayName := @folder[0];
	if browseTitle <> '' then
		browse_info.lpszTitle := PChar('Select the folder ' + browseTitle + '.')
	else
		browse_info.lpszTitle := '';
	browse_info.ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI;
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

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
label LRetry;
var
	F: TFile;
	Line: string;
begin
	Result := False;
	{$ifopt d+}
	if not Assigned(Lines) then
	begin
		Exit;
	end;
	{$endif}
	Lines.Clear;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			if not F.Readln(Line) then goto LRetry;
			Lines.Add(Line);
		end;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function WriteLinesToFile(var FileName: TFileName; Lines: TStrings; Append: BG): BG;
label LRetry;
var
	F: TFile;
	i: SG;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if Append then F.SeekEnd;
		i := 0;
		while i < Lines.Count do
		begin
			if not F.Write(Lines[i] + FileSep) then goto LRetry;
			Inc(i);
		end;
		if Append = False then F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

end.
