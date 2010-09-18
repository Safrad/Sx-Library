//* File:     Lib\uFiles.pas
//* Created:  1998-01-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uFiles;

interface

uses
	uAdd, uStrings,
	SysUtils, Windows, Classes;

// File system
const
{
	Best Performance
	09/2001: 32768
	06/2003: 65536-131072
}
	DefFileBuffer = 131072;
	FileSep = CharCR + CharLF;
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

	TFileNames = array of TFileName;

	TFileMode = (fmReadOnly, fmWriteOnly, fmReadAndWrite);

	{
		Flags:
				FILE_FLAG_OVERLAPPED // Async read - not implemented yet

				FILE_FLAG_RANDOM_ACCESS
				FILE_FLAG_SEQUENTIAL_SCAN

				FILE_FLAG_WRITE_THROUGH // For write only
				FILE_FLAG_NO_BUFFERING // Be carefully for use this
	}

	TFile = class(TObject)
	private
		HFile: THANDLE;
		FFileName: ^TFileName;
		FTempFileName:  TFileName;

		FMode: TFileMode;
		// For Write only
		FProtection: Boolean;

		// For Readln only
		FBuffer: array of Char;
		FBufStart, FBufEnd: U8;
		FBufferSize: U8;
		FFilePos: U8;
		FFileSize: U8;
		function GetFileSize(var Size: U8): Boolean;
		function IsOpened: BG;
	public
		ReadCount, WriteCount: UG;
		ReadBytes, WriteBytes: U8;

		ErrorCode: U4;
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read IsOpened;
		constructor Create;
		destructor Free;
		function Open(var FileName: TFileName; const Mode: TFileMode; Flags: U4; Protection: Boolean): Boolean;
		function Seek(const Pos: U8): Boolean;
		function SeekStart: Boolean;
		function SeekEnd: Boolean;
		function BlockRead(var Buf; const Count: Cardinal): Boolean;
		function BlockWrite(var Buf; const Count: Cardinal): Boolean;
		function FillWrite(const Count: Cardinal): Boolean;
		function Readln(var Line: string): Boolean;
		function Write(var Line: string): Boolean;
		function WriteF(Line: string): Boolean;
		function Writeln(Line: string): Boolean;
		function WritelnW(Line: WideString): Boolean;
		function Close: Boolean;
		function Truncate: Boolean;
		function FlushFileBuffers: Boolean;
		function Eof: Boolean;
		function Lock(From, Count: U8): Boolean;
		function UnLock(From, Count: U8): Boolean;
	end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): Boolean;
{
	Result of Functions : False: Error, True: Ok
	Example:
	label LRetry;
	var
		F: TFile;
		FileName: TFileName;
	begin
		F := TFile.Create;
		FileName := WorkDir + 'FileName.txt';
		LRetry:
		if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			if not F.BlockRead() then goto LRetry
			if not F.Readln() then goto LRetry;

			if not F.Close then goto LRetry;
		end;
		F.Free;
	end;
}
var
	StartDir, // Dir with Ini and configuratios files (read and write)
	WorkDir, // Dir with exe file, data files (read only)
	GraphDir,
	SoundsDir,
	DataDir,
	SysDir, WinDir: string;
	ExeFileName: TFileName;

function ShortDir(const Dir: string): string;
function FullDir (Dir: string): string;
function DelFileExt(const FName: string): string;
function BackDir(const Dir: string): string;
function LegalFileName(const FileName: string): string;
procedure ReadDir(var FileNames: TFileNames; Path, Extension: string; Files, Dirs, SubDirs, Sort: Boolean);
function GetFileSiz(const FileName: TFileName): U8;

function CopyFile(const Source, Dest: TFileName; const FailExist: Boolean): Boolean;
function CreateDir(const Dir: string): Boolean;
function NewEmptyDir(var Dir: string; const CanCreate: Boolean): Boolean;
function CopyDir(const Source, Dest: string): Boolean;

function DeleteFileEx(const FileName: TFileName): Boolean;
function DeleteDir(const DirName: string): Boolean;
function DeleteDirs(DirName: string; DeleteSelf: Boolean): Boolean;

function ReadBufferFromFile(var FileName: TFileName; var Buf; var Count: SG): BG;
function WriteBufferToFile(var FileName: TFileName; var Buf; const Count: SG): BG;

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
function WriteLinesToFile(var FileName: TFileName; Lines: TStrings; Append: BG): BG;

function ReadStringFromFile(var FileName: TFileName; var Line: string): BG;
function WriteStringToFile(var FileName: TFileName; var Line: string; Append: BG): BG;

function ReadStreamFromFile(var FileName: TFileName; Stream: TMemoryStream): BG;
function WriteStreamToFile(var FileName: TFileName; Stream: TMemoryStream): BG;

function GetDriveInfo(const Drive: Byte): TDriveInfo;

{$IFDEF WIN32}
function ShortToLongFileName(const ShortName: string): string;
function ShortToLongPath(const ShortName: string): string;
function LongToShortFileName(const LongName: string): string;
function LongToShortPath(const LongName: string): string;
{$ENDIF WIN32}

procedure SelectPath(var Path: string; Handle: THandle);

{
	MapViewOfFile
	OpenFileMapping
	CreateFileMapping
}


implementation

uses
	Dialogs, ShellAPI, ShlObj,
	uError;

constructor TFile.Create;
begin
//	LineSeparator := CharCR + CharLF;
	HFile := INVALID_HANDLE_VALUE;
end;

destructor TFile.Free;
begin
	if IsOpened then Close;
end;

function TFile.IsOpened: BG;
begin
	Result := HFile <> INVALID_HANDLE_VALUE
end;

function TFile.Open(var FileName: TFileName; const Mode: TFileMode; Flags: U4; Protection: Boolean): Boolean;
label LRetry;
var
	CreationDistribution: U4;
	DesiredAccess, ShareMode: U4;
begin
	Result := False;

	if IsOpened then Close;

	FFileName := Pointer(FileName);
	FFilePos := 0;
	FMode := Mode;
	SetLength(FBuffer, 0);
	FBufferSize := DefFileBuffer;
	FBufStart := High(FBufStart);
	FBufEnd := 0;
	FProtection := Protection;

	if FProtection and (FMode <> fmReadOnly) then
	begin
		FTempFileName := ExtractFilePath(FileName) + '$' + ExtractFileName(FileName);
{   if FileExists(FTempFileName) then
		begin
			DeleteFile(FTempFileName);
		end;}
		if FileExists(FileName) then
			CopyFile(FileName, FTempFileName, False);

	end
	else
		FTempFileName := FileName;


	LRetry:
	case Mode of
	fmReadOnly:
	begin
		DesiredAccess := GENERIC_READ;
		ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
	end;
	fmWriteOnly:
	begin
		DesiredAccess := GENERIC_WRITE;
		ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
	end;
	fmReadAndWrite:
	begin
		DesiredAccess := GENERIC_READ or GENERIC_WRITE;
		ShareMode := FILE_SHARE_READ or FILE_SHARE_WRITE;
	end
	else
	begin
		DesiredAccess := 0;
		ShareMode := 0;
	end;
	end;
{ FILE_SHARE_READ
	FILE_SHARE_WRITE}

	if (FileExists(FTempFileName) = False) and (FMode <> fmReadOnly) then
		CreationDistribution := CREATE_NEW
	else
		CreationDistribution := OPEN_EXISTING;

	HFile := CreateFile(
		PChar(FTempFileName), // pointer to name of the file
		DesiredAccess,  // access (read-write) mode
		ShareMode,  // share mode
		nil,  // pointer to security attributes
		CreationDistribution, // how to create
		FILE_ATTRIBUTE_NORMAL or Flags, // file attributes
		0   // handle to file with attributes to copy
	 );
	if not IsOpened then
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if IOErrorRetry(TFileName(FTempFileName), ErrorCode) then goto LRetry;
		end
		else
		begin
			GetFileSize(FFileSize);
			Result := True;
		end;
	end
	else
	begin
		GetFileSize(FFileSize);
		Result := True;
	end;
end;

function HandleFileSize(HFile: THandle; var Size: U8): U4;
begin
	TU8(Size).D0 := Windows.GetFileSize(HFile, @TU8(Size).D1);

	if TU8(Size).D0 = $FFFFFFFF then
	begin
		Result := GetLastError;
	end
	else
		Result := NO_ERROR;
end;

function TFile.GetFileSize(var Size: U8): Boolean;
begin
	Result := HandleFileSize(HFile, Size) = NO_ERROR;
	if Result = False then
		IOError(TFileName(FTempFileName), ErrorCode);
end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): Boolean;
var
	HFile: THANDLE;
	ErrorCode: U4;
begin
	Result := False;
	U8(CreationTime) := 0;
	U8(LastAccessTime) := 0;
	U8(LastWriteTime) := 0;
	HFile := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0,  // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0   // handle to file with attributes to copy
	 );
	if HFile <> INVALID_HANDLE_VALUE then
	begin
		Result := GetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime);
		if CloseHandle(HFile) = False then
		begin
			ErrorCode := GetLastError;
			IOError(FileName, ErrorCode);
		end;
	end
	else
	begin
		ErrorCode := GetLastError;
		IOError(FileName, ErrorCode);
	end;
end;

function TFile.Seek(const Pos: U8): Boolean;
begin
	Result := False;
	if SetFilePointer(
		HFile,  // handle of file
		TU8(Pos).D0, // number of bytes to move file pointer
		@TU8(Pos).D1,  // address of high-order word of distance to move
		FILE_BEGIN    // how to move
	 ) <> $FFFFFFFF then
	begin
		Result := True;
		FFilePos := Pos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			IOError(TFileName(FTempFileName), ErrorCode);
		end
		else
			Result := True;
	end;
end;

function TFile.SeekStart: Boolean;
begin
	Result := Seek(0);
end;

function TFile.SeekEnd: Boolean;
begin
	Result := Seek(FFileSize);
end;

function TFile.BlockRead(var Buf; const Count: Cardinal): Boolean;
var Suc: U4;
begin
	if ReadFile(HFile, Buf, Count, Suc, nil) then
	begin
		Inc(FFilePos, Suc);
		Result := True;

		Inc(ReadCount);
		Inc(ReadBytes, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode)
		end
		else
			Result := True;
	end;
end;

function TFile.BlockWrite(var Buf; const Count: Cardinal): Boolean;
var Suc: U4;
begin
	if WriteFile(HFile, Buf, Count, Suc, nil) then
	begin
		Inc(FFilePos, Suc);
		Result := True;

		Inc(WriteCount);
		Inc(WriteBytes, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode)
		end
		else
			Result := True;
	end;
end;

function TFile.FillWrite(const Count: Cardinal): Boolean;
var
	Buf: Pointer;
begin
	GetMem(Buf, Count);
	FillChar(Buf^, Count, 0);
	Result := BlockWrite(Buf^, Count);
	FreeMem(Buf, Count);
end;

function TFile.Readln(var Line: string): Boolean;
label LN;
var BufPos: Integer;
begin
	Line := '';
	if Eof then
	begin
		Result := False;
		Exit;
	end;
	Result := True;
	if FBufStart = High(FBufStart) then
	begin
		SetLength(FBuffer, FBufferSize);
	end;

	while True do
	begin
		LN:
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			BufPos := FFilePos - FBufStart;
			if Eof or (FBuffer[BufPos] = CharLF) then
			begin
				Inc(FFilePos);
				Seek(FFilePos);
				Break;
			end;
		end
		else
		begin
			FBufStart := FBufferSize * (FFilePos div FBufferSize);
			FBufEnd := FBufStart + FBufferSize - 1;
			if FBufEnd > FFileSize then FBufEnd := FFileSize;
			Seek(FBufStart);
			BufPos := FFilePos;
			BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
			FFilePos := BufPos;
//      BufPos := FFilePos - FBufStart;
			goto LN;
		end;
		if FBuffer[BufPos] <> CharCR then
			Line := Line + FBuffer[BufPos];
		Inc(FFilePos);
	end;
end;

function TFile.Write(var Line: string): Boolean;
begin
	if Length(Line) > 0 then
		Result := BlockWrite(Line[1], Length(Line))
	else
		Result := True;
end;

function TFile.WriteF(Line: string): Boolean;
begin
	if Length(Line) > 0 then
		Result := BlockWrite(Line[1], Length(Line))
	else
		Result := True;
end;


function TFile.Writeln(Line: string): Boolean;
begin
	Line := Line + FileSep;
	Result := BlockWrite(Line[1], Length(Line));
end;

function TFile.WritelnW(Line: WideString): Boolean;
begin
	Line := Line + CharNul + CharLF;
	Result := BlockWrite(Line[1], 2 * Length(Line));
end;

function TFile.Close: Boolean;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	Result := False;
	if not IsOpened then
	begin
		IOErrorMessage(TFileName(FTempFileName), 'File Is Closed');
		Exit;
	end;

	SetLength(FBuffer, 0);
	if FMode <> fmReadOnly then
	begin
		if GetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime) then
		begin
			GetSystemTimeAsFileTime(LastWriteTime);
			SetFileTime(HFile, @CreationTime, @LastAccessTime, @LastWriteTime);
		end;
	end;
	if CloseHandle(HFile) then
	begin
		if FProtection and (FMode <> fmReadOnly) then
		begin
			if CopyFile(PChar(FTempFileName), PChar(FFileName), False) then
				DeleteFile(PChar(FTempFileName));
		end;
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not IOErrorRetry(TFileName(FTempFileName), ErrorCode);
		end
		else
		begin
			Result := True;
		end;
	end;
	HFile := INVALID_HANDLE_VALUE;
end;

function TFile.Truncate: Boolean;
begin
	if SetEndOfFile(HFile) then
	begin
		Result := False;
		FFileSize := FFilePos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := True;
			IOError(TFileName(FTempFileName), ErrorCode);
		end
		else
		begin
			Result := False;
			FFileSize := FFilePos;
		end;
	end;
end;

function TFile.FlushFileBuffers: Boolean;
begin
	Result := Windows.FlushFileBuffers(HFile);
	if Result = False then
		ErrorCode := GetLastError;
end;

function TFile.Eof: Boolean;
begin
	Result := FFilePos >= FFileSize;
end;

function TFile.Lock(From, Count: U8): Boolean;
begin
	Result := LockFile(HFile, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
	end;
end;

function TFile.UnLock(From, Count: U8): Boolean;
begin
	Result := UnLockFile(HFile, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
	end;
end;

// Utils

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
begin
	GetDir(0, StartDir);
	if StartDir[Length(StartDir)] <> '\' then StartDir := StartDir + '\';

	WorkDir := GetCommandLine;
	if WorkDir[1] = '"' then
	begin
		Delete(WorkDir, 1, 1);
		i := Pos('"', WorkDir);
		if i > 0 then Delete(WorkDir, i, Length(WorkDir) - i + 1);
	end
	else
	begin
		i := Pos(' ', WorkDir);
		Delete(WorkDir, i, Length(WorkDir) - i + 1);
	end;

	ExeFileName := WorkDir;

	for i := Length(WorkDir) downto 0 do
	begin
		if i = 0 then
		begin
			WorkDir := '';
			Break;
		end;
		if (WorkDir[i] = '\') then
		begin
			SetLength(WorkDir, i);
			Break;
		end;
	end;
	if (WorkDir = '') then WorkDir := StartDir;
//  SharedDir := BackDir(WorkDir) + 'Shared\';
	GraphDir := WorkDir + 'Graphics\';
	SoundsDir := WorkDir + 'Sounds\';
	DataDir := WorkDir + 'Data\';

	SetLength(SysDir, MAX_PATH);
	NewLength := GetSystemDirectory(PChar(SysDir), MAX_PATH);
	SetLength(SysDir, NewLength);
	if (Length(SysDir) > 0) and (SysDir[Length(SysDir)] <> '\') then
		SysDir := SysDir + '\';

	SetLength(WinDir, MAX_PATH);
	NewLength := GetwindowsDirectory(PChar(WinDir), MAX_PATH);
	SetLength(WinDir, NewLength);
	if (Length(WinDir) > 0) and (WinDir[Length(WinDir)] <> '\') then
		WinDir := WinDir + '\';
end;

function ShortDir(const Dir: string): string;
var
	i: Integer;
begin
	Result := Dir;
	if Length(WorkDir) <= Length(Dir) then
	begin
		for i := 1 to Length(WorkDir) do
		begin
			if UpCase(Dir[i]) <> UpCase(WorkDir[i]) then
			begin
				Exit;
			end;
		end;
		for i := 1 to Length(Dir) - Length(WorkDir) do
		begin
			Result[i] := Dir[i + Length(WorkDir)];
		end;
		SetLength(Result, Length(Dir) - Length(WorkDir));
	end;
end;

function FullDir(Dir: string): string;
var
	i: Integer;
begin
	Replace(Dir, '%SystemRoot%', Copy(WinDir, 1, Length(WinDir) - 1));
	for i := 1 to Length(Dir) do
	begin
		if Dir[i] = ':' then
		begin
			Result := Dir;
			Exit;
		end;
	end;
	Result := WorkDir + Dir;
end;

function DelFileExt(const FName: string): string;
var
	Ext: string;
begin
	Result := FName;
	Ext := ExtractFileExt(FName);
	if Length(Ext) > 0 then SetLength(Result, Length(Result) - Length(Ext));
end;

function BackDir(const Dir: string): string;
var i: Integer;
begin
	Result := Dir;
	for i := Length(Result) - 1 downto 1 do
	begin
		if Result[i] = '\' then
		begin
			SetLength(Result, i);
			Exit;
		end;
	end;
end;
{
function BackDir2(Dir: string): string;
begin
	Delete(Dir, Length(Dir), 1);
	Result := ExtractFileName(Dir) + '\';
end;}

function LegalFileName(const FileName: string): string;
var
	i: Integer;
	StrLength: Integer;
begin
	Result := FileName;
	if Length(Result) = 0 then
	begin
		Result := '';
		Exit;
	end
	else if Length(Result) > 63 then
	begin
		SetLength(Result, 63);
	end;

	i := 1;
	StrLength := Length(Result);
	while i <= StrLength do
	begin
		case Result[i] of
		'a'..'z', 'A'..'Z', '0'..'9', '_', '.', '-', ' ', #160, {special space}
		'+', '=', '`',
		'~', '!', '@', '#', '$', '%', '^', '&', '(', ')',
		'{', '}', '''', #180, ';', '[', ']', ',',
		'\', ':': // 'C:\'
		begin
			Inc(i);
		end
		else
		begin
			Delete(Result, i, 1);
			Dec(StrLength);
		end;
		end;
	end;
end;

procedure ReadDir(var FileNames: TFileNames; Path, Extension: string; Files, Dirs, SubDirs, Sort: Boolean);
var
	FilesCount: SG;
	NewSize: SG;
	IsDir, IsFile: BG;
	ErrorCode: Integer;

		procedure ReadDir2(SubPath: string);
		var
			SearchRec: TSearchRec;
		begin
			// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
			ErrorCode := FindFirst(Path + SubPath + '*.*', faAnyFile, SearchRec);
			while ErrorCode = NO_ERROR do
			begin
				IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
				IsFile := (SearchRec.Attr and faDirectory) = 0;

				if (IsDir and Dirs)
				or (IsFile and Files) then
				begin
					if (Extension = '') or (Extension = '*') or (Extension = '*.*') or
					(UpperCase(ExtractFileExt(SearchRec.Name)) = UpperCase(Extension)) then
					begin
						NewSize := FilesCount + 1;
						if AllocByEx(Length(FileNames), NewSize, SizeOf(FileNames[0])) then
							SetLength(FileNames, NewSize);
						if SubDirs then
							FileNames[FilesCount] := SubPath + SearchRec.Name
						else
							FileNames[FilesCount] := SearchRec.Name;
						Inc(FilesCount);
					end;
				end;

				if IsDir and SubDirs then
				begin
					ReadDir2(SubPath + SearchRec.Name + '\');
				end;
				ErrorCode := FindNext(SearchRec);
			end;
			if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Path + SubPath, ErrorCode);
			SysUtils.FindClose(SearchRec);
		end;
var
	i: Integer;
	Offset: Integer;
	MaxLimit: Integer;
	Switch: Integer;
	FileName: TFileName;
begin
	if Length(Extension) > 1 then
		if (Extension <> '*') and (Extension <> '*.*') then
			if Extension[1] <> '.' then Extension := '.' + Extension;
	if Length(Path) > 1 then
		if Path[Length(Path)] <> '\' then
			Path := Path + '\';

	FilesCount := Length(FileNames);
	ReadDir2('');
	SetLength(FileNames, FilesCount);

	if Sort then
	begin
		Offset := Length(FileNames) div 2;
		while Offset > 0 do
		begin
			MaxLimit := Length(FileNames) - Offset - 1;
			repeat
				Switch := 0;
				for i := 0 to MaxLimit do
				begin
					if FileNames[i] > FileNames[i + Offset] then
					begin
						FileName := FileNames[i];
						FileNames[i] := FileNames[i + Offset];
						FileNames[i + Offset] := FileName;
						Switch := i;
					end;
				end;
				MaxLimit := Switch - Offset;
			until Switch = 0;
			Offset := 2 * Offset div 3;
		end;
	end;
end;

function GetFileSiz(const FileName: TFileName): U8;
var
	HFile: THandle;
	ErrorCode: U4;
begin
	Result := 0;
	HFile := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0,  // access (read-write) mode
		0,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0 // handle to file with attributes to copy
	 );
	if HFile <> INVALID_HANDLE_VALUE then
	begin
		HandleFileSize(HFile, Result);
		if CloseHandle(HFile) = False then
		begin
			ErrorCode := GetLastError;
			IOError(FileName, ErrorCode);
		end;
	end
	else
	begin
		ErrorCode := GetLastError;
		IOError(FileName, ErrorCode);
	end;
end;

function CopyFile(const Source, Dest: TFileName; const FailExist: Boolean): Boolean;
begin
	Windows.SetFileAttributes(PChar(Dest), FILE_ATTRIBUTE_ARCHIVE);
	Result := Windows.CopyFile(PChar(Source), PChar(Dest), FailExist);
	if Result = False then
		IOError(Source + LineSep + Dest, GetLastError);
end;

function CreateDir(const Dir: string): Boolean;
begin
	if DirectoryExists(Dir) then
		Result := True
	else
	begin
		Result := CreateDirectory(PChar(Dir), nil);
		if Result = False then
			IOError(Dir, GetLastError);
	end;
end;

function NewEmptyDir(var Dir: string; const CanCreate: Boolean): Boolean;
var
	i: Integer;
	Dir2: string;
begin
	Result := False;
	Dir2 := Copy(Dir, 1, Length(Dir) - 1);
	i := 0;
	while True do
	begin
		if i > 0 then
		begin
			Dir := Dir2 + Char(Ord('a') + i - 1) + '\';
		end;

		if DirectoryExists(Dir) = False then
		begin
			if CanCreate then
				CreateDir(Dir);
			Result := True;
			Break;
		end;
		Inc(i);
		if i > Ord('z') - Ord('a') + 1 then
		begin
			MessageD('Can not create empty directory', mtError, [mbOk]);
			Break;
		end;
	end;
end;

function CopyDir(const Source, Dest: string): Boolean;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CreateDir(Dest);

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(Source + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
				CopyDir(Source + SearchRec.Name + '\', Dest + SearchRec.Name + '\');
		end
		else
		begin
			CopyFile(Source + SearchRec.Name, Dest + SearchRec.Name, False);
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Source, ErrorCode);
	SysUtils.FindClose(SearchRec);
end;

function DeleteFileEx(const FileName: TFileName): Boolean;
begin
	Windows.SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE);
	Result := DeleteFile(PChar(FileName));
	if Result = False then
		IOError(FileName, GetLastError);
end;

function DeleteDir(const DirName: string): Boolean;
begin
	Result := Windows.RemoveDirectory(PChar(DirName));
	if Result = False then
		IOError(DirName, GetLastError);
end;

function DeleteDirs(DirName: string; DeleteSelf: Boolean): Boolean;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	if DirectoryExists(DirName) = False then
	begin
		Result := False;
		Exit;
	end;
	Result := True;

	if DirName[Length(DirName)] <> '\' then DirName := DirName + '\';

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(DirName + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
			begin
				Result := DeleteDirs(DirName + SearchRec.Name + '\', True) and Result;
			end;
		end
		else
		begin
			Result := DeleteFileEx(DirName + SearchRec.Name) and Result;
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(DirName, ErrorCode);
	SysUtils.FindClose(SearchRec);

	if DeleteSelf then Result := DeleteDir(DirName) and Result;
end;

// TFile Read Write

function ReadBufferFromFile(var FileName: TFileName; var Buf; var Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		GetMem(Pointer(Buf), F.FileSize);
		if not F.BlockRead(Pointer(Buf)^, F.FileSize) then goto LRetry;
		Count := F.FileSize;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function WriteBufferToFile(var FileName: TFileName; var Buf; const Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockWrite(Pointer(Buf)^, Count) then goto LRetry;
		F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function ReadStringFromFile(var FileName: TFileName; var Line: string): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		SetLength(Line, F.FileSize);
		if not F.BlockRead(Line[1], F.FileSize) then goto LRetry;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function WriteStringToFile(var FileName: TFileName; var Line: string; Append: BG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if Append then F.SeekEnd;
		if not F.Write(Line) then goto LRetry;
		if Append = False then F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function ReadLinesFromFile(var FileName: TFileName; Lines: TStrings): BG;
label LRetry;
var
	F: TFile;
	Line: string;
begin
	{$ifopt d+}
	if not Assigned(Lines) then IE(454);
	{$endif}
	Lines.Clear;
	Result := False;
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
			if not F.WriteF(Lines[i] + FileSep) then goto LRetry;
			Inc(i);
		end;
		if Append = False then F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
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

{$IFDEF WIN32}

function ShortToLongFileName(const ShortName: string): string;
var
  Temp: TWIN32FindData;
  SearchHandle: THandle;
begin
  SearchHandle := FindFirstFile(PChar(ShortName), Temp);
  if SearchHandle <> ERROR_INVALID_HANDLE then begin
    Result := string(Temp.cFileName);
    if Result = '' then Result := string(Temp.cAlternateFileName);
  end
  else Result := '';
  Windows.FindClose(SearchHandle);
end;

function LongToShortFileName(const LongName: string): string;
var
  Temp: TWIN32FindData;
  SearchHandle: THandle;
begin
  SearchHandle := FindFirstFile(PChar(LongName), Temp);
	if SearchHandle <> ERROR_INVALID_HANDLE then begin
    Result := string(Temp.cAlternateFileName);
		if Result = '' then Result := string(Temp.cFileName);
  end
	else Result := '';
  Windows.FindClose(SearchHandle);
end;

function ShortToLongPath(const ShortName: string): string;
var
  LastSlash: PChar;
  TempPathPtr: PChar;
begin
	if FileExists(ShortName) = False then
	begin
		Result := ShortName;
		Exit;
	end;
	Result := '';
	TempPathPtr := PChar(ShortName);
	LastSlash := StrRScan(TempPathPtr, '\');
	while LastSlash <> nil do begin
		Result := '\' + ShortToLongFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			LastSlash^ := char(0);
			LastSlash := StrRScan(TempPathPtr, '\');
		end;
	end;
	Result := TempPathPtr + Result;
end;

function LongToShortPath(const LongName: string): string;
var
	LastSlash: PChar;
	TempPathPtr: PChar;
begin
	if FileExists(LongName) = False then
	begin
		Result := LongName;
		Exit;
	end;
	Result := '';
  TempPathPtr := PChar(LongName);
  LastSlash := StrRScan(TempPathPtr, '\');
  while LastSlash <> nil do begin
    Result := '\' + LongToShortFileName(TempPathPtr) + Result;
    if LastSlash <> nil then begin
      LastSlash^ := char(0);
      LastSlash := StrRScan(TempPathPtr, '\');
    end;
  end;
	Result := TempPathPtr + Result;
end;

{$ENDIF WIN32}

procedure SelectPath(var Path: string; Handle: THandle);
var
	TitleName : string;
	lpItemID : PItemIDList;
	BrowseInfo : TBrowseInfo;
//	DisplayName : array[0..MAX_PATH] of char;
	TempPath : array[0..MAX_PATH] of char;
	TempP: Shortstring;

	I: TItemIdList;
begin
	FillChar(BrowseInfo, SizeOf(TBrowseInfo), 0);
	BrowseInfo.hwndOwner := Handle;
//	TitleName := 'D:\';
//	BrowseInfo.pszDisplayName := PChar(TitleName);
//	BrowseInfo.pszDisplayName := Addr(DisplayName);
	TitleName := 'Please specify a directory';
	BrowseInfo.lpszTitle := PChar(TitleName);
//	BrowseInfo.iImage := 1;
	BrowseInfo.ulFlags := BIF_RETURNONLYFSDIRS;
	TempP:= 'D:\' + CharNul;
	I.mkid.cb := 1;
	I.mkid.abID[0] := Byte('D');
//	BrowseInfo.pidlRoot := Addr(I);
	BrowseInfo.lParam := 0;
	lpItemID := SHBrowseForFolder(BrowseInfo);
	if lpItemId <> nil then
	begin
		SHGetPathFromIDList(lpItemID, TempPath);
		Path := StrPas(TempPath);
		GlobalFreePtr(lpItemID);
	end;
end;

initialization
	InitPaths;
end.
