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


// System

function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: Byte): Integer;
function ThreadPriority(const Prior: Byte): Integer;

function GetCaption(const FName: TFileName; const Changed: Boolean;
	const New: Integer; const ReadOnly: BG; const Index, Count: Integer): string;

procedure BeginLongOperation(const Background: BG = False);
procedure EndLongOperation(const Sound: BG = True);

function RemoveEscape(s: string): string;
function AddEscape(s: string): string; // 2.8x larger for random data

function RandomString(Size: SG): string;


//procedure CorrectFormPos(Form: TForm);
//procedure SetListViewItems(ListView: TListView; NewSize: SG);
procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: Word;
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

function GetDriveInfo(const Drive: Byte): TDriveInfo;
function SelectFolder(var Path: string; browseTitle: string = ''): BG;
function DeleteFileDialog(const FileName: TFileName): Boolean;

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
	DRIVE_CDROM: Result := 'CD-ROM'; // The drive is a CD-ROM drive.
	DRIVE_RAMDISK: Result := 'Ramdisk'; // The drive is a RAM disk.
	end;
end;

function ProcessPriority(const Prior: Byte): Integer;
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

function ThreadPriority(const Prior: Byte): Integer;
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

procedure EndLongOperation(const Sound: BG = True);
begin
	if Sound then
		PlayWinSound(wsAsterisk);
	Screen.Cursor := crDefault;
end;
{
procedure CorrectFormPos(Form: TForm);
begin
	if not Assigned(Form) then Exit;
	if Form.Left + Form.Width > Screen.Width then Form.Left := Screen.Width - Form.Width;
	if Form.Top + Form.Height > Screen.Height then Form.Top := Screen.Height - Form.Height;
	if Form.Left < 0 then Form.Left := 0;
	if Form.Top < 0 then Form.Top := 0;
end;}
{
procedure SetListViewItems(ListView: TListView; NewSize: SG);
var j: SG;
begin
	if NewSize > ListView.Items.Count then
	begin
		for j := 0 to NewSize - ListView.Items.Count - 1 do
		begin
			ListView.Items.Add;
		end;
	end
	else
	begin
		for j := ListView.Items.Count - 1 downto NewSize do
		begin
			ListView.Items[j].Delete;
		end;
	end;
end;
}

{
// Standard Escape Sequences:
\b       backspace
\f       formfeed
\n       new line
\r       carriage return
\t       horizontal tab
\'       single quote
\0       null


Sequence	Value	Char	What it does
\a	0x07	BEL	Audible bell
\b	0x08	BS	Backspace
\f	0x0C	FF	Formfeed
\n	0x0A	LF	Newline (linefeed)
\r	0x0D	CR	Carriage return
\t	0x09	HT	Tab (horizontal)
\v	0x0B	VT	Vertical tab
\\	0x5c	\	Backslash
\'	0x27	'	Single quote (apostrophe)
\"	0x22	"	Double quote
\?	0x3F	?	Question mark
\O		any	O=a string of up to three octal digits
\xH		any	H=a string of hex digits
\XH		any	H=a string of hex digits
}

function RemoveEscape(s: string): string;
var
	i, j: SG;
	x, v: U1;
	Special: BG;
begin
	Result := '';
	i := 1;
	Special := False;
	while i <= Length(s) do
	begin
		if s[i] = '\' then
		begin
			if Special then
			begin
				Result := Result + s[i];
				Special := False;
			end
			else
				Special := True;
		end
		else
			if Special then
			begin
				case s[i] of
				'a': Result := Result + CharBell;
				'b': Result := Result + CharBackspace;
				'e', 'E': Result := Result + #$1B;
				'f': Result := Result + CharFormfeed;
				'n': Result := Result + CharLF;
				'r': Result := Result + CharCR;
				't': Result := Result + CharHT;
{				'u', 'U':
				begin
				end;}
				'v': 	Result := Result + CharVT;
				'x':
				begin
					Inc(i);
					x := 0;
					while True do
					begin
						if i <= Length(s) then
							v := HexValue[s[i]]
						else
							v := 16;
						if (v < 16) then
						begin
							x := (x shl 4) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
					end;
				end;
				'''': Result := Result + '''';
				'"': Result := Result + '"';
				'?': Result := Result + '?';
				'0'..'7': //Result := Result + Char(Ord(s[i]) - Ord('0'));//CharNull;
				begin
					x := 0;
					j := 0;
					while True do
					begin
						if (i <= Length(s)) and (j < 3) then
							v := HexValue[s[i]]
						else
							v := 8;
						if (v < 8) then
						begin
							x := (x shl 3) and $ff;
							x := (x + v) and $ff;
							Inc(i);
						end
						else
						begin
							Result := Result + Char(x);
							Dec(i);
							Break;
						end;
						Inc(j);
					end;
				end;
				else
					Result := Result + s[i];
				end;
				Special := False;
			end
			else
				Result := Result + s[i];
		Inc(i);
	end;

end;

function AddEscape(s: string): string;
var i: SG;
begin
	Result := '';
	i := 1;
	while i <= Length(s) do
	begin
		case s[i] of
		'\': Result := Result + '\\';
		CharBell: Result := Result + '\a';
		CharBackspace: Result := Result + '\b';
		#$1B: Result := Result + '\e'; // 'E'
		CharFormfeed: Result := Result + '\f';
		CharLF: Result := Result + '\n';
		CharCR: Result := Result + '\r';
		CharHT: Result := Result + '\t';
		CharVT: Result := Result + '\v';
//		'''': Result := Result + '\''';
//		#0..#6: Result := Result + '\' + Char(Ord(s[i]) + Ord('0'));//CharNull;
		#$20..#$5B, #$5D..#$7F: // ASCII
			Result := Result + s[i];
		else
		begin
			NumericBase := 8;
			Result := Result + '\' + NToS(Ord(s[i]), '000');  // NumToStr(Ord(s[i]), 8);
			NumericBase := 10;
		end;
		end;
		Inc(i);
	end;
end;

function RandomString(Size: SG): string;
var i: SG;
begin
	SetLength(Result, Size);
	for i := 1 to Size do
		Result[i] := Char(Random(256));
end;
{
var
	i: SG;
	s, s2: string;
begin
	for i := 0 to 20000 do
	begin
		s := RandomString(i);
		s2 := AddEscape(s);
		s2 := RemoveEscape(s2);
		if s <> s2 then
			Nop;
	end;

end;
}


procedure CreateLink(
	const LinkFileName: WideString;
	const Target: TFileName;
	const Arguments: string;
	const StartIn: string;
	const HotKey: Word;
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

function GetDriveInfo(const Drive: Byte): TDriveInfo;
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

///////////////////////////////////////////////////////////////////
// This function allows the user to browse for a folder
//
// Arguments:-
//    browseTitle : The title to display on the browse dialog.
//  initialFolder : Optional argument. Use to specify the folder
//                  initially selected when the dialog opens.
//
// Returns: The empty string if no folder was selected (i.e. if the
//          user clicked cancel), otherwise the full folder path.
///////////////////////////////////////////////////////////////////
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
		if Length(Path) > 0 then
			if Path[Length(Path)] <> '\' then Path := Path + '\';
		GlobalFreePtr(find_context);
	end
	else
		result := False;
end;
(*
var
	TitleName : string;
	lpItemID : PItemIDList;
	BrowseInfo : TBrowseInfo;
//	DisplayName : array[0..MAX_PATH] of char;
	TempPath : array[0..MAX_PATH] of Char;

	I: TItemIdList;
begin
	if Path = '' then Path := 'C:\';
	FillChar(BrowseInfo, SizeOf(TBrowseInfo), 0);
	BrowseInfo.hwndOwner := Handle;
//	TitleName := 'D:\';
	BrowseInfo.pszDisplayName := @Path[1];
//	BrowseInfo.pszDisplayName := @DisplayName;
	TitleName := 'Please specify a directory';
	FillChar(I, SizeOf(I), 0);
	I.mkid.cb := 1;
	I.mkid.abID[0] := Byte('C');
//	BrowseInfo.pidlRoot := @I;
	BrowseInfo.lpszTitle := @TitleName[1];
	BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS or BIF_USENEWUI;
	BrowseInfo.lpfn := nil;
	BrowseInfo.lParam := 0;
	BrowseInfo.iImage := 1;

	Move(Path[1], TempPath, Length(Path)); // := Path + CharNul;

	lpItemID := SHBrowseForFolder(BrowseInfo);
	if lpItemId <> nil then
	begin
		Result := SHGetPathFromIDList(lpItemID, TempPath);
		Path := StrPas(TempPath);
		GlobalFreePtr(lpItemID);
	end
	else
		Result := False;
end;   *)

(*
function SelectDirectory(var Dir: string): BG;
(*var OpenDialog1: TOpenDialog;
begin
	OpenDialog1 := TOpenDialog.Create(nil);
	try
		if Dir = '' then Dir := WorkDir;
		OpenDialog1.Options := OpenDialog1.Options + [ofPathMustExist];
		OpenDialog1.Options := OpenDialog1.Options - [ofFileMustExist];
		if ExecuteDialog(OpenDialog1, Dir + '*.*') then
		begin
			Result := True;
			Dir := ExtractFilePath(OpenDialog1.FileName);
		end
		else
			Result := False;
	finally
		OpenDialog1.Free;
	end;
begin
	FileCtrl.SelectDirectory('Select Direcotry...', Dir, Dir);
end;*)

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

function DeleteFileDialog(const FileName: TFileName): Boolean;
begin
	Result := False;
	if MessageD('Delete file' + LineSep + FileName, mtConfirmation, [mbYes, mbNo]) = mbYes then
		Result := DeleteFileEx(FileName);
end;


initialization
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}
end.

