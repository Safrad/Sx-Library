//* File:     Lib\uFiles.pas
//* Created:  1998-01-01
//* Modified: 2005-11-13
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uFiles;

interface

uses
	uTypes, uStrings,
	{$ifndef NoGUI}Dialogs,{$endif}
	SysUtils, Windows;

// File system
const
{
	Best Performance
	1..16 * KB: very low performance
	32 * KB: lowest memory
	64..128 * KB: optimal
	32 * MB: Win API maximum
}
	DefFileBuffer = {$ifndef NoGUI}64{$else}32{$endif} * KB;
	FileSep = CharCR + CharLF;
type
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
		function Error: BG; overload;
		function Error(const EC: U4): BG; overload;
	public
		{$ifndef NoGUI}
		ErrorCode: U4;
		{$endif}
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read IsOpened;
		constructor Create;
		destructor Destroy; override;
		function Open(var FileName: TFileName; const Mode: TFileMode; Flags: U4; Protection: Boolean): Boolean;
		function Seek(const Pos: U8): Boolean;
		function SeekStart: Boolean;
		function SeekEnd: Boolean;
		function BlockRead(var Buf; const Count: UG): Boolean;
		function BlockWrite(var Buf; const Count: UG): Boolean;
		function FillWrite(Count: UG): Boolean;
		function Readln(out Line: string): Boolean;
		function Write(Line: string): Boolean;
//		function WriteF(Line: string): Boolean;
		function Writeln(Line: string): Boolean;
		function WritelnW(Line: WideString): Boolean;
		function Close(ChangeDate: BG = True): Boolean;
		function Truncate: Boolean;
		function FlushFileBuffers: Boolean;
		function Eof: Boolean;
		function Lock(From, Count: U8): Boolean;
		function UnLock(From, Count: U8): Boolean;
	end;

function ErrorMes(const ErrorCode: U4): string;
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
const
	AllFiles = '|All Files (*.*)|*.*';
	AllText = 'Text file (*.txt)|*.txt' + AllFiles;
var
	StartDir, // Dir with Ini and configuratios files (read and write)
	WorkDir, // Dir with exe file, data files (read only)
	GraphDir,
	SoundsDir,
	DataDir,
	SysDir, WinDir: string;
	ExeFileName: TFileName;
	ProgramFiles: string = 'C:\Program Files\';

	ReadCount, WriteCount: UG;
	ReadBytes, WriteBytes: U8;

	{$ifdef NoGUI}
	ErrorStr: string;
	ErrorCode: U4;
	{$endif}

function ShortDir(const Dir: string): string;
function FullDir (Dir: string): string;
function DelFileExt(const FName: string): string;
function AddAfterName(const FName: string; const Text: string): string;
function BackDir(var Dir: string): BG;
function BackDirF(Dir: string): string;
function LegalFileName(const FileName: string): string;
procedure ReadDir(var FileNames: TFileNames; var FilesCount: SG; Path, Extension: string; Files, Dirs, SubDirs, Sort: Boolean);
function GetFileSizeU(HFile: THandle): U8; overload;
function GetFileSizeU(const FileName: TFileName): U8; overload;
function GetFileSizeS(const FileName: TFileName): string;
function GetFileModified(FileName: TFileName; var LastWriteTime: TFileTime): BG;
function SetFileModified(FileName: TFileName; LastWriteTime: TFileTime): BG;

function CopyFile(Source, Dest: TFileName; const FailExist: Boolean): Boolean;
function CopyFileToDir(Source, Dest: TFileName; const FailExist: Boolean): Boolean;
function CopyDamagedFile(Source, Dest: TFileName): Boolean;
function DirectoryExists(const Directory: string): Boolean;
function CreateDir(const Dir: string): Boolean;
function NewFileOrDir(var FileOrDir: string): Boolean;
function NewFileOrDirEx(var FileOrDir: string): Boolean;
function CopyDir(const Source, Dest: string): Boolean;

function DeleteFileEx(const FileName: TFileName): Boolean;
function DeleteDir(const DirName: string): Boolean;
function DeleteDirs(DirName: string; DeleteSelf: Boolean): Boolean;


function ReadBufferFromFile(var FileName: TFileName; var Buf; var Count: SG): BG;
function WriteBufferToFile(var FileName: TFileName; var Buf; const Count: SG): BG;

function ReadBlockFromFile(var FileName: TFileName; Buf: Pointer; const Count: SG): BG;
function WriteBlockToFile(var FileName: TFileName; Buf: Pointer; const Count: SG): BG;

type
	TArrayOfString = array of string;

function ReadStringsFromFile(var FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
function WriteStringsToFile(var FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; Append: BG): BG;
function WriteStringToFileEx(var FileName: TFileName; const Line: string; Append: BG): BG;

function ReadStringFromFile(FileName: TFileName): string; overload;
function ReadStringFromFile(var FileName: TFileName; out Line: string): BG; overload;
function WriteStringToFile(var FileName: TFileName; const Line: string; Append: BG): BG;


{
	MapViewOfFile
	OpenFileMapping
	CreateFileMapping
}

{$IFDEF WIN32}
function ShortToLongFileName(const ShortName: string): string;
function ShortToLongPath(ShortName: string): string;
{function LongToShortFileName(const LongName: string): string;
function LongToShortPath(const LongName: string): string;}
{$ENDIF WIN32}

function RepairDirectory(const Dir: TFileName): TFileName;
{$ifndef NoGUI}
function ExecuteDialog(Dialog: TOpenDialog; var FileName: TFileName): BG; overload;
{$endif}
function TempFileName(FileName: TFileName): TFileName;
procedure ReplaceIfChanged(FileName: TFileName);
function DialogStr(Ext, Des: array of string): string;

implementation

uses
	Math,
	{$ifndef NoGUI}uError, {$endif}
	uFormat, uMath;

function ErrorMes(const ErrorCode: U4): string;
var
	NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := FormatMessage(
		{FORMAT_MESSAGE_ALLOCATE_BUFFER or}
		FORMAT_MESSAGE_FROM_SYSTEM or
		FORMAT_MESSAGE_IGNORE_INSERTS,
		nil,
		ErrorCode,
		LANG_NEUTRAL or SUBLANG_DEFAULT shl 10,
		@Result[1],
		MAX_PATH,
		nil);
	SetLength(Result, NewLength);
	Replace(Result, CharCR + CharLF, ' ');
	DelBESpace(Result);
	Result := Result + ' (' + NToS(ErrorCode) + ')';
end;

constructor TFile.Create;
begin
	inherited Create;
//	LineSeparator := CharCR + CharLF;
	HFile := INVALID_HANDLE_VALUE;
end;

destructor TFile.Destroy;
begin
	if IsOpened then Close;
	inherited Destroy;
end;

function TFile.Error(const EC: U4): BG;
begin
	ErrorCode := EC;
	Result := Error;
end;

function TFile.Error: BG;
begin
	{$ifndef NoGUI}
	Result := IOErrorRetry(TFileName(FTempFileName), ErrorCode);
	{$else}
	Result := False;
	ErrorStr := FTempFileName + LineSep + ErrorMes(ErrorCode);
	{$endif}
end;

procedure Error(const FileName: TFileName; const ErrorCode: U4);
begin
	{$ifndef NoGUI}
	IOError(FileName, ErrorCode);
	{$else}
	ErrorStr := FileName + LineSep + ErrorMes(ErrorCode);
	{$endif}
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
			if Error then goto LRetry;
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
	TU8(Size).D0 := GetFileSize(HFile, @TU8(Size).D1);

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
		Error;
end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): Boolean;
var
	HFile: THANDLE;
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
			Error(FileName, GetLastError);
		end;
	end
	else
	begin
		Error(FileName, GetLastError);
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
			Error(ErrorCode);
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

function TFile.BlockRead(var Buf; const Count: UG): Boolean;
var Suc: U4;
begin
	if ReadFile(HFile, Buf, Count, Suc, nil) then
	begin
		Result := True;

		Inc(ReadCount);
		Inc(ReadBytes, Suc);
		Inc(FFilePos, Count);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not Error(ErrorCode);
		end
		else
			Result := True;
		Inc(FFilePos, Count);
		Seek(FFilePos);
	end;
end;

function TFile.BlockWrite(var Buf; const Count: UG): Boolean;
var Suc: U4;
begin
	if WriteFile(HFile, Buf, Count, Suc, nil) then
	begin
		Result := True;

		Inc(WriteCount);
		Inc(WriteBytes, Suc);
		Inc(FFilePos, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			Result := not Error(ErrorCode);
		end
		else
			Result := True;
		Inc(FFilePos, Suc);
//		Seek(FFilePos);
	end;
end;

function TFile.FillWrite(Count: UG): Boolean;
var
	Buf: Pointer;
	C: UG;
begin
	C := Min(Count, DefFileBuffer);
	Buf := AllocMem(C);
	Result := True;
	while Count > 0 do
	begin
		Result := Result and BlockWrite(Buf^, C);
		Count := Count - C;
		if C > Count then C := Count;
		if Result = False then Break;
	end;

	FreeMem(Buf);
end;

function TFile.Readln(out Line: string): Boolean;
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

function TFile.Write(Line: string): Boolean;
begin
	if Length(Line) > 0 then
		Result := BlockWrite(Line[1], Length(Line))
	else
		Result := True;
end;

{
function TFile.WriteF(Line: string): Boolean;
begin
	if Length(Line) > 0 then
		Result := BlockWrite(Line[1], Length(Line))
	else
		Result := True;
end;}

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

function TFile.Close(ChangeDate: BG = True): Boolean;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	Result := False;
	if not IsOpened then
	begin
		{$ifndef NoGUI}
		IOErrorMessage(TFileName(FTempFileName), 'File Is Closed');
		{$else}
		ErrorStr := 'File Is Closed';
		{$endif}
		Exit;
	end;

	SetLength(FBuffer, 0);
	if ChangeDate then
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
			Result := not Error(ErrorCode);
		end
		else
			Result := True;
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
			Result := False;
			Error(ErrorCode);
		end
		else
		begin
			Result := True;
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

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
begin
	GetDir(0, StartDir);
	CorrectDir(StartDir);

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
	NewLength := GetSystemDirectory(@SysDir[1], MAX_PATH);
	SetLength(SysDir, NewLength);
	CorrectDir(SysDir);

	SetLength(WinDir, MAX_PATH);
	NewLength := GetWindowsDirectory(@WinDir[1], MAX_PATH);
	SetLength(WinDir, NewLength);
	CorrectDir(WinDir);
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
{var
	i: Integer;}
begin
	if Dir = '' then
		Result := ''
	else
	begin
		// %HOMEDRIVE%%HOMEPATH%
		Replace(Dir, '%SystemRoot%', Copy(WinDir, 1, Length(WinDir) - 1));
{		for i := 1 to Length(Dir) do
		begin
			if Dir[i] = ':' then
			begin
				Result := Dir;
				Exit;
			end;
		end;}
		if (Dir[1] = '\') then
			Result := WorkDir[1] + WorkDir[2] + Dir
		else if ((Length(Dir) > 1) and (Dir[2] = ':'))then
		begin
			Result := Dir;
		end
		else
			Result := WorkDir + Dir;
		Result := ShortToLongPath(Result);
	end;
end;

function DelFileExt(const FName: string): string;
var
	Ext: string;
begin
	Result := FName;
	Ext := ExtractFileExt(FName);
	if Length(Ext) > 0 then SetLength(Result, Length(Result) - Length(Ext));
end;

function AddAfterName(const FName: string; const Text: string): string;
begin
	Result := DelFileExt(FName) + Text + ExtractFileExt(FName);
end;

function BackDir(var Dir: string): BG;
var i: Integer;
begin
	Result := False;
	for i := Length(Dir) - 1 downto 3 do
	begin
		if Dir[i] = '\' then
		begin
			SetLength(Dir, i);
			Result := True;
			Exit;
		end;
	end;
end;

function BackDirF(Dir: string): string;
begin
	Result := Dir;
	BackDir(Result);
end;

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

procedure ReadDir(var FileNames: TFileNames; var FilesCount: SG; Path, Extension: string; Files, Dirs, SubDirs, Sort: Boolean);
var
	NewSize: SG;
	IsDir, IsFile: BG;
	ErrorCode: Integer;

		procedure ReadSubDir(SubPath: string);
		var
			SearchRec: TSearchRec;
		begin
			// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
			ErrorCode := FindFirst(Path + SubPath + '*.*', faAnyFile, SearchRec);
			while ErrorCode = NO_ERROR do
			begin
				IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
				if IsDir and (Dirs or SubDirs) then SearchRec.Name := SearchRec.Name + '\';
				IsFile := (SearchRec.Attr and faDirectory) = 0;

				if (IsDir and Dirs)
				or (IsFile and Files) then
				begin
					if (Extension = '') or (Extension = '*') or (Extension = '*.*') or
					(UpperCase(ExtractFileExt(SearchRec.Name)) = UpperCase(Extension)) then
					begin
						NewSize := FilesCount + 1;
						if AllocByExp(Length(FileNames), NewSize) then
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
					ReadSubDir(SubPath + SearchRec.Name);
				end;
				ErrorCode := FindNext(SearchRec);
			end;
			{$ifndef NoGUI}
			if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Path + SubPath, ErrorCode);
			{$endif}
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
	CorrectDir(Path);

	ReadSubDir('');

	if Sort then
	begin
		Offset := FilesCount div 2;
		while Offset > 0 do
		begin
			MaxLimit := FilesCount - Offset - 1;
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

function GetFileSizeU(HFile: THandle): U8;
begin
	Result := 0;
	if HFile <> INVALID_HANDLE_VALUE then
	begin
		HandleFileSize(HFile, Result);
	end;
end;

function GetFileSizeU(const FileName: TFileName): U8;
var
	HFile: THandle;
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
			Error(FileName, GetLastError);
		end;
	end
	else
	begin
		Error(FileName, GetLastError);
	end;
end;

function GetFileSizeS(const FileName: TFileName): string;
var FileSize: U8;
begin
	FileSize := GetFileSizeU(FileName);
	if FileSize = 0 then
		Result := 'N/A'
	else
		Result := BToStr(FileSize);
end;

function GetFileModified(FileName: TFileName; var LastWriteTime: TFileTime): BG;
label LRetry;
var
	F: TFile;
	ACreationTime, ALastAccessTime: TFileTime;
begin
	Result := False;
	LastWriteTime.dwLowDateTime := 0;
	LastWriteTime.dwHighDateTime := 0;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		Result := GetFileTime(F.HFile, @ACreationTime, @ALastAccessTime, @LastWriteTime);
		F.Close(False);
	end;
	F.Free;
end;


function SetFileModified(FileName: TFileName; LastWriteTime: TFileTime): BG;
label LRetry;
var
	F: TFile;
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadAndWrite, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		Result := GetFileTime(F.HFile, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
		if Result then
		begin
			Result := SetFileTime(F.HFile, @ACreationTime, @ALastAccessTime, @LastWriteTime);
			{$ifndef NoGUI}
			if Result = False then
				IOError(FileName, GetLastError);
			{$endif}
		end
		else
		begin
			{$ifndef NoGUI}
			IOError(FileName, GetLastError);
			{$endif}
		end;

		F.Close(False);
	end;
	F.Free;
end;

function CopyFile(Source, Dest: TFileName; const FailExist: Boolean): Boolean;
label LRetry;
{$ifndef NoGUI}
var Error: U4;
{$endif}
begin
	Windows.SetFileAttributes(PChar(Dest), FILE_ATTRIBUTE_ARCHIVE);
	LRetry:
	Result := Windows.CopyFile(PChar(Source), PChar(Dest), FailExist);
	{$ifndef NoGUI}
	if Result = False then
	begin
		Error := GetLastError;
		if IOErrorRetry(Source, Error) then goto LRetry;
		if IOErrorRetry(Dest, Error) then goto LRetry;
	end;
	{$endif}
end;

function CopyFileToDir(Source, Dest: TFileName; const FailExist: Boolean): Boolean;
begin
	Result := CopyFile(Source, Dest + ExtractFileName(Source), FailExist);
end;

function CopyDamagedFile(Source, Dest: TFileName): Boolean;
label LRetryS, LRetryD;
const
	Count = 512;
var
	FS, FD: TFile;
	Buf: Pointer;
begin
	Result := True;
	GetMem(Buf, Count);
	FS := TFile.Create;
	FD := TFile.Create;
	LRetryS:
	if FS.Open(Source, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		LRetryD:
		if FD.Open(Dest, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			while not FS.Eof do
			begin
				FillChar(Buf^, Count, 0);
				if not FS.BlockRead(Buf^, Count) then
					Result := False;
				FD.BlockWrite(Buf^, Count);

			end;
			FD.Truncate;
			if not FD.Close then goto LRetryD;
		end;
		if not FS.Close then goto LRetryS;
	end;
	FS.Free;
	FD.Free;
	FreeMem(Buf, Count);
end;

function DirectoryExists(const Directory: string): Boolean;
var
	Code: U4;
begin
	Code := GetFileAttributes(PChar(Directory));
	Result := (Code <> High(Code)) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function CreateDir(const Dir: string): Boolean;
begin
	if Dir = '' then
	begin
		Result := False;
		Exit;
	end;
	if DirectoryExists(Dir) then
		Result := True
	else
	begin
		Result := CreateDirectory(@Dir[1], nil);
		{$ifndef NoGUI}
		if Result = False then
			IOError(Dir, GetLastError);
		{$endif}
	end;
end;

function NewFileOrDir(var FileOrDir: string): Boolean;
var
	i: SG;
	IsDir: BG;
	DirS, DirE: string;
begin
	Result := False;
	if Length(FileOrDir) = 0 then Exit;
	IsDir := FileOrDir[Length(FileOrDir) - 1] = '\';
	if IsDir then
	begin
		DirS := Copy(FileOrDir, 1, Length(FileOrDir) - 1);
		DirE := '\';
	end
	else
	begin
		DirS := DelFileExt(FileOrDir);
		DirE := ExtractFileExt(FileOrDir);
	end;
	i := 0;
	while True do
	begin
		if i > 0 then
		begin
			FileOrDir := DirS + ' (' + IntToStr(i) + ')' + DirE;
		end;

		if IsDir then
		begin
			if DirectoryExists(FileOrDir) = False then
			begin
				Result := True;
				Break;
			end;
		end
		else
		begin
			if FileExists(FileOrDir) = False then
			begin
				Result := True;
				Break;
			end;


		end;
		Inc(i);
{		if i > Ord('z') - Ord('a') + 1 then
		begin
			MessageD('Can not create empty directory', mtError, [mbOk]);
			Break;
		end;}
	end;
end;

function NewFileOrDirEx(var FileOrDir: string): Boolean;
var D: TDateTime;
begin
	D := Date;
	FileOrDir := DelFileExt(FileOrDir) + ' ' + DateToS(D) + ExtractFileExt(FileOrDir);
	Result := NewFileOrDir(FileOrDir);
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
	{$ifndef NoGUI}
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Source, ErrorCode);
	{$endif}
	SysUtils.FindClose(SearchRec);
end;

function DeleteFileEx(const FileName: TFileName): Boolean;
begin
	Windows.SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE);
	Result := DeleteFile(PChar(FileName));
	{$ifndef NoGUI}
	if Result = False then
		IOError(FileName, GetLastError);
	{$endif}
end;

function DeleteDir(const DirName: string): Boolean;
begin
	Result := Windows.RemoveDirectory(@DirName[1]);
	{$ifndef NoGUI}
	if Result = False then
		IOError(DirName, GetLastError);
	{$endif}
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

	CorrectDir(DirName);

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
	{$ifndef NoGUI}
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(DirName, ErrorCode);
	{$endif}
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

function ReadBlockFromFile(var FileName: TFileName; Buf: Pointer; const Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockRead(Buf^, Min(Count, F.FileSize)) then goto LRetry;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function WriteBlockToFile(var FileName: TFileName; Buf: Pointer; const Count: SG): BG;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmWriteOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockWrite(Buf^, Count) then goto LRetry;
		F.Truncate;
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function ReadStringFromFile(var FileName: TFileName; out Line: string): BG; overload;
label LRetry;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	Line := '';
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		SetLength(Line, F.FileSize);
		if Length(Line) >= 1 then
			if not F.BlockRead(Line[1], F.FileSize) then goto LRetry;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function ReadStringFromFile(FileName: TFileName): string; overload;
begin
	ReadStringFromFile(FileName, Result);
end;

function WriteStringToFile(var FileName: TFileName; const Line: string; Append: BG): BG;
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


function SameDataInFile(var FileName: TFileName; const Line: string): BG;
label LRetry, LClose;
var
	F: TFile;
	Buf, P: Pointer;
	TotalBytes, ReadBytes: SG;
begin
	Result := False;
	if FileExists(FileName) then
	begin
		F := TFile.Create;
		GetMem(Buf, DefFileBuffer);
		LRetry:
		if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
		begin
			TotalBytes := F.FileSize;
			if TotalBytes <> Length(Line) then goto LClose;
			while TotalBytes > 0 do
			begin
				ReadBytes := DefFileBuffer;
				if ReadBytes > TotalBytes then ReadBytes := TotalBytes;
				if not F.BlockRead(Buf^, ReadBytes) then
				begin
					goto LClose;
				end;
				P := @Line[Length(Line) - TotalBytes + 1];
				if SameData(Buf, P, ReadBytes) = False then goto LClose;
				Dec(TotalBytes, ReadBytes);
			end;
			Result := True;
			LClose:
			if not F.Close then goto LRetry;
		end;
		FreeMem(Buf);
		F.Free;
	end;
end;

function WriteStringToFileEx(var FileName: TFileName; const Line: string; Append: BG): BG;
begin
	if (Append = False) and SameDataInFile(FileName, Line) then
		Result := True
	else
		Result := WriteStringToFile(FileName, Line, Append);
end;

function ReadStringsFromFile(var FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
label LRetry;
var
	F: TFile;
	Line: string;
	NewSize: SG;
begin
	Result := False;
	F := TFile.Create;
	LRetry:
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		while not F.Eof do
		begin
			if not F.Readln(Line) then goto LRetry;
			NewSize := LineCount + 1;
			if AllocByExp(Length(Lines), NewSize) then
				SetLength(Lines, NewSize);
			Lines[LineCount] := Line;
			Inc(LineCount);
		end;
		F.Close;
		Result := True;
	end;
	F.Free;
end;

function WriteStringsToFile(var FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; Append: BG): BG;
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
		while i < OpeningNameCount do
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

{$IFDEF WIN32}

function ShortToLongFileName(const ShortName: string): string;
var
	Temp: TWIN32FindData;
	SearchHandle: THandle;
begin
	SearchHandle := FindFirstFile(@ShortName[1], Temp);
	if SearchHandle <> ERROR_INVALID_HANDLE then begin
		Result := string(Temp.cFileName);
		if Result = '' then Result := string(Temp.cAlternateFileName);
	end
	else Result := '';
	Windows.FindClose(SearchHandle);
end;
(*
function LongToShortFileName(const LongName: string): string;
var
	Temp: TWIN32FindData;
	SearchHandle: THandle;
begin
	SearchHandle := FindFirstFile(@LongName[1], Temp);
	if SearchHandle <> ERROR_INVALID_HANDLE then begin
		Result := string(Temp.cAlternateFileName);
		if Result = '' then Result := string(Temp.cFileName);
	end
	else Result := '';
	Windows.FindClose(SearchHandle);
end;
*)


function StrRScan(const Str: string; Chr: Char): string;
var
	Index: SG;
begin
	Result := '';
	Index := Length(Str);
	while Index > 0 do
	begin
		while Str[Index] = Chr do
		begin
			Result := Copy(Str, Index, MaxInt);
			Exit;
		end;
		Dec(Index);
	end;
end;

function ShortToLongPath(ShortName: string): string;
var
	LastSlash: string;
begin
	if (FileExists(ShortName) = False) or (Length(ShortName) < 2) or (ShortName[1] = '\') then
	begin
		Result := ShortName;
		Exit;
	end;
	Result := '';
	LastSlash := StrRScan(ShortName, '\');
	while LastSlash <> '' do
	begin
		Result := '\' + ShortToLongFileName(ShortName) + Result;
		if LastSlash <> '' then
		begin
			SetLength(ShortName, Length(ShortName) - Length(LastSlash));
			LastSlash := StrRScan(ShortName, '\');
		end;
	end;
	Result := ShortName + Result;
end;
{
function ShortToLongPath(ShortName: string): string;
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
	TempPathPtr := @ShortName[1];
	LastSlash := StrRScan(TempPathPtr, '\');
	while LastSlash <> nil do begin
		Result := '\' + ShortToLongFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			LastSlash^ := char(0);
			LastSlash := StrRScan(TempPathPtr, '\');
		end;
	end;
	Result := TempPathPtr + Result;
end;}

(*
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
	TempPathPtr := @LongName[1];
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
*)
{$ENDIF WIN32}

function RepairDirectory(const Dir: TFileName): TFileName;
begin
	Result := Dir;
	while True do
	begin
		if DirectoryExists(Result) then Break;
		if BackDir(string(Result)) = False then Break;
	end;
end;

{$ifndef NoGUI}
function ExecuteDialog(Dialog: TOpenDialog; var FileName: TFileName): BG;
begin
	Dialog.FileName := ExtractFileName(FileName);
	Dialog.InitialDir := RepairDirectory(ExtractFilePath(FileName));
	Result := Dialog.Execute;
	FileName := Dialog.FileName;
end;
{$endif}

function SameFiles(FileName1, FileName2: TFileName): BG;
label LClose;
var
	TotalBytes, ReadBytes: SG;
	File1, File2: TFile;
	Buf1, Buf2: Pointer;
begin
	Result := False;
	File1 := nil;
	File2 := nil;
	Buf1 := nil;
	Buf2 := nil;

	File1 := TFile.Create;
	if not File1.Open(FileName1, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		goto LClose;
	end;
	File2 := TFile.Create;
	if not File2.Open(FileName2, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		goto LClose;
	end;
	if File1.FileSize <> File2.FileSize then goto LClose;
	GetMem(Buf1, DefFileBuffer);
	GetMem(Buf2, DefFileBuffer);

	TotalBytes := File1.FileSize;
	while TotalBytes > 0 do
	begin
		ReadBytes := DefFileBuffer;
		if ReadBytes > TotalBytes then ReadBytes := TotalBytes;
		if not File1.BlockRead(Buf1^, ReadBytes) then
		begin
			goto LClose;
		end;
		if not File2.BlockRead(Buf2^, ReadBytes) then
		begin
			goto LClose;
		end;
		Dec(TotalBytes, ReadBytes);
		if SameData(Buf1, Buf2, ReadBytes) = False then goto LClose;
	end;
	Result := True;

	LClose:
	if Assigned(File1) then
	begin
		FreeAndNil(File1);
	end;
	if Assigned(File2) then
	begin
		FreeAndNil(File2);
	end;
	FreeMem(Buf1);
	FreeMem(Buf2);
end;

function TempFileName(FileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(FileName) + '~' + ExtractFileName(FileName);
end;

procedure ReplaceIfChanged(FileName: TFileName);
var
	OrigFileName: TFileName;
begin
	OrigFileName := ExtractFileName(FileName);
	if Length(OrigFileName) <= 0 then Exit;
	if OrigFileName[1] <> '~' then Exit;
	Delete(OrigFileName, 1, 1);
	OrigFileName := ExtractFilePath(FileName) + OrigFileName;

	if FileExists(OrigFileName) and SameFiles(OrigFileName, FileName) then
		DeleteFileEx(FileName)
	else
	begin
		CopyFile(FileName, OrigFileName, False);
		DeleteFileEx(FileName);
	end;
end;

function DialogStr(Ext, Des: array of string): string;
var
	i: SG;
	s1, s2: string;
begin
	for i := 0 to Length(Ext) - 1 do
	begin
		s1 := s1 + {'*.' +} Ext[i] + ', ';
		s2 := s2 + '*.' + Ext[i] + ';';
	end;
	SetLength(s1, Length(s1) - 1);
	s1[Length(s1)] := ')';
	SetLength(s2, Length(s2) - 1);
	Result := 'Any (' + s1 + '|' + s2;
	for i := 0 to Length(Ext) - 1 do
	begin
		Result := Result + '|' + Des[i] + ' (*.' + Ext[i] + ')|*.' + Ext[i];
	end;
	Result := Result + AllFiles;
end;

initialization
	InitPaths;
end.
