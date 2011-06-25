//* File:     Lib\GUI\uSystem.pas
//* Created:  1998-01-01
//* Modified: 2007-12-24
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uSystem;

interface

uses uTypes, SysUtils, Forms, ShlObj, Controls, Classes;

function DriveTypeToStr(const DriveType: Integer): string;
function ProcessPriority(const Prior: U1): Integer;
function ThreadPriority(const Prior: U1): Integer;

procedure BeginLongOperation(const Background: BG = False);
procedure EndLongOperation(const Sound: BG = True);

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
//function WriteLinesToFile(var FileName: TFileName; const Lines: TStrings; const Append: BG): BG;
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
function GetDriveInfo(const Drive: TDriveLetter): TDriveInfo;

function SelectFolder(var Path: string; browseTitle: string = ''): BG;

implementation

uses
	Windows, Math,
	uStrings, uFiles, uDParser, uWave, uMath, uFile;

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
	FillChar(browse_info, SizeOf(browse_info), #0);
	lg_StartFolder := RepairDirectory(ExpandDir(Path));
	browse_info.pszDisplayName := @folder[0];
	if browseTitle <> '' then
		browse_info.lpszTitle := PChar('Select the folder ' + browseTitle + '.')
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

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
var
	F: TFile;
	Line: string;
begin
	Assert(Lines <> nil);
	Result := False;
	Lines.Clear;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			while not F.Eof do
			begin
				F.Readln(Line);
				Lines.Add(Line);
			end;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

{
function WriteLinesToFile(var FileName: TFileName; const Lines: TStrings; const Append: BG): BG;
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
end;}

function ReadStreamFromFile(var FileName: TFileName; Stream: TMemoryStream): BG;
var
	Buf: Pointer;
	Count: SG;
begin
	Result := ReadBufferFromFile(FileName, Buf, Count);
{	Stream.SetSize(Count);
	Stream.Seek(0, 0);}
	if Buf <> nil then
	begin
		Stream.WriteBuffer(Buf^, Count);
		Stream.Seek(0, 0);
		FreeMem(Buf);
	end;
end;

function WriteStreamToFile(var FileName: TFileName; Stream: TMemoryStream): BG;
var
	Buf: Pointer;
begin
	GetMem(Buf, Stream.Size);
	Stream.Seek(0, 0);
	Stream.ReadBuffer(Buf^, Stream.Size);
	Result := WriteBufferToFile(FileName, Buf, Stream.Size);
	FreeMem(Buf);
end;

end.
