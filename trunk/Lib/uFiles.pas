//* File:     Lib\uFiles.pas
//* Created:  1998-01-01
//* Modified: 2007-05-27
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uFiles;

interface

uses
	uTypes, uStrings,
	{$ifndef Console}Dialogs,{$endif}
	SysUtils, Windows;

// File System
const
{
	Best Performance
	1..16 * KB: very low performance
	32 * KB: lowest memory
	64..128 * KB: optimal
	32 * MB: Win API maximum
}
	DefFileBuffer = {$ifndef Console}64{$else}32{$endif} * KB;
	FileSep = CharCR + CharLF;
	// For write FMode only, if enabled temporary file is used first
	FProtection = True;
type
	TFileNames = array of TFileName;

	TFileMode = (fmReadOnly, fmRewrite, fmAppend, fmReadAndWrite);
var
	FileModeStr: array[TFileMode] of string;
type
	{
		Flags:
				FILE_FLAG_OVERLAPPED // Async read - not implemented yet

				FILE_FLAG_RANDOM_ACCESS
				FILE_FLAG_SEQUENTIAL_SCAN // Default value

				FILE_FLAG_WRITE_THROUGH // For write only
				FILE_FLAG_NO_BUFFERING // Be carefully for use this
	}

	TFile = class(TObject)
	private
		HFile: THANDLE;
		FFileName: TFileName;
		FTempFileName:  TFileName;

		FMode: TFileMode;

		// For Readln only
		FBuffer: array of Char;
		FBufStart, FBufEnd: U8;
		FBufferSize: U8;
		FFilePos: U8;
		FFileSize: U8;
		function GetFileSize(var Size: U8): BG;
		function IsOpened: BG;
		function ErrorRetry(const ErrorCode: U4): BG;
	public
		property FilePos: U8 read FFilePos;
		property FileSize: U8 read FFileSize;
		property Opened: BG read IsOpened;
		constructor Create;
		destructor Destroy; override;
		function Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG; overload;
		function Seek(const Pos: U8): BG;
		function SeekStart: BG;
		function SeekEnd: BG;
		function BlockRead(out Buf; const Count: UG): BG;
		function BlockWrite(const Buf; const Count: UG): BG;
		function FillWrite(Count: UG): BG;
		function Readln(out Line: string): BG;
		function Write(const Data: string): BG;
//		function WriteF(Line: string): BG;
		function Writeln(Line: string): BG;
		function WritelnW(Line: WideString): BG;
		function Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
		function Truncate: BG;
		function FlushFileBuffers: BG;
		function Eof: BG;
		function Lock(From, Count: U8): BG;
		function UnLock(From, Count: U8): BG;
	end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
{
	Result of Functions : False: Error, True: Ok
	Example:
	var
		F: TFile;
	begin
		F := TFile.Create;
		try
			if F.Open(WorkDir + 'FileName.txt', fmReadOnly) then
			begin
				F.BlockRead();
				F.Readln();
				F.Close
			end;
		finally
			F.Free;
		end;
	end;
}
const
	AllFiles = 'All Files (*.*)|*.*';
	AllText = 'Text file (*.txt)|*.txt|' + AllFiles;
var
	// Directories
	StartDir, // Actual Dir
	WorkDir, // EXE file, data files (Read only)
	GraphDir, // (Read only)
	SoundsDir, // (Read only)
	DataDir, // Input data (Read only)
	SysDir,
	WinDir, // Shared configuration files (Read and Write)
	ProgramFilesDir,
	AppDataDir, // User specific configuration files (Ini, Autosaves, Logs) (Read and Write)
	MyDocuments, // User documnets (Read and Write)
//	HomeDir, // User documnets (Read and Write)
	ApplicationDataDir, // Application Data
	CommonTempDir, TempDir: string;
	ExeFileName, MainIniFileName, MainLogFileName: TFileName;

	ReadCount, WriteCount: UG;
	ReadBytes, WriteBytes: U8;

function ShortDir(const Dir: string): string;
function RemoveEV(Dir: string): string;
function ExpandDir(Dir: string): string;
function DelFileExt(const FName: string): string;
function AddAfterName(const FName: string; const Text: string): string;
function BackDir(var Dir: string): BG;
function BackDirF(Dir: string): string;
function LegalFileName(const FileName: string): string;
procedure ReadDir(var FileNames: TFileNames; var FilesCount: SG; Path: string; Extensions: array of string; Files, Dirs, SubDirs, Sort: BG);
function GetFileSizeU(const FileName: TFileName): S8;
function GetFileSizeS(const FileName: TFileName): string;
function FileTimeToDateTime(F: TFileTime): TDateTime;
function GetFileModified(const FileName: TFileName; var LastWriteTime: TFileTime): BG; overload;
function GetFileModified(const FileName: TFileName): TFileTime; overload;
function SetFileModified(FileName: TFileName; LastWriteTime: TFileTime): BG;

function RenameFileEx(const Source, Dest: TFileName): BG;
function CopyFile(const Source, Dest: TFileName; const FailExist: BG): BG;
function CopyFileToDir(Source, Dest: TFileName; const FailExist: BG): BG;
function CopyDamagedFile(Source, Dest: TFileName): BG;
function CreateDirEx(const Dir: string): BG;
function CreateDirsEx(const Dir: string): BG;
function NewFileOrDir(var FileOrDir: string): BG;
function NewFileOrDirEx(var FileOrDir: string): BG;
function CopyDir(const Source, Dest: string): BG;

function DeleteFileEx(const FileName: TFileName): BG;
function RemoveDirEx(const DirName: string): BG;
function RemoveDirsEx(DirName: string; DeleteSelf: BG = False): BG;

function ReadBufferFromFile(const FileName: TFileName; out Buf; out Count: SG): BG;
function WriteBufferToFile(const FileName: TFileName; const Buf; const Count: SG): BG;

function ReadBlockFromFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;
function WriteBlockToFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;

type
	TArrayOfString = array of string;

function ReadStringsFromFile(const FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
function WriteStringsToFile(const FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; const Append: BG): BG;

function ReadStringFromFile(const FileName: TFileName; out Data: string): BG; overload;
function ReadStringFromFile(const FileName: TFileName): string; overload;
function WriteStringToFile(const FileName: TFileName; const Data: string; const Append: BG): BG;

{ TODO:
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
{$ifndef Console}
function ExecuteDialog(Dialog: TOpenDialog; var FileName: TFileName): BG; overload;
{$endif}
function TempFileName(const FileName: TFileName): TFileName;
procedure ReplaceIfChanged(const FileName: TFileName);
function DialogStr(Ext, Des: array of string): string;
procedure InitPaths;

function DirectoryExistsEx(const DirName: TFileName): BG;
function FileExistsEx(const FileName: TFileName): BG;
function FileOrDirExists(const FileOrDirName: string): BG;
function FileOrDirExistsEx(const FileOrDirName: string): BG;
function LastLineFromFile(const FileName: TFileName): string;

implementation

uses
	Math,
	uMsg, uProjectInfo,
	uOutputFormat, uMath, uLog, uReg;

constructor TFile.Create;
begin
	inherited Create;
	HFile := INVALID_HANDLE_VALUE;
end;

destructor TFile.Destroy;
begin
	if IsOpened then
	begin
		Warning('Forcing close of file %1.', FTempFileName);
		Close(True, True);
	end;
	inherited Destroy;
end;

function TFile.ErrorRetry(const ErrorCode: U4): BG;
begin
	Result := IOErrorRetry(FTempFileName, ErrorCode);
end;

function TempFileName(const FileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(FileName) + '~' + ExtractFileName(FileName);
end;

function TFile.IsOpened: BG;
begin
	Result := HFile <> INVALID_HANDLE_VALUE;
end;

{
function TFile.Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
var
	FileName2: TFileName;
begin
	FileName2 := FileName;
	Result := Open(FileName2, Mode, Flags, False);
end; }

function TFile.Open(const FileName: TFileName; const Mode: TFileMode; Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
label LRetry;
var
	CreationDistribution: U4;
	DesiredAccess, ShareMode: U4;
	ErrorCode: U4;
begin
	Result := False;

	if IsOpened then
	begin
		Warning('Forcing close file %1.', FileName);
		Close;
	end;

	FFileName := FileName;
	FFilePos := 0;
	FMode := Mode;
	SetLength(FBuffer, 0);
	FBufferSize := DefFileBuffer;
	FBufStart := High(FBufStart);
	FBufEnd := 0;

	if FProtection and (FMode in [fmRewrite, fmReadAndWrite]) then
	begin
		FTempFileName := TempDir + ExtractFileName(FileName);
		if FileExists(FTempFileName) then
		begin
			DeleteFileEx(FTempFileName);
		end;
		if FMode = fmReadAndWrite then
			if FileExists(FileName) then
				CopyFile(FileName, FTempFileName, False);
	end
	else
		FTempFileName := ExpandDir(FileName);
	MainLogAdd('Opening for ' + FileModeStr[Mode] + ' ' + FTempFileName, mtDebug);

	LRetry:
	ShareMode := FILE_SHARE_READ;
	case Mode of
	fmReadOnly:
	begin
		DesiredAccess := GENERIC_READ;
		ShareMode := ShareMode or FILE_SHARE_WRITE;
	end;
	fmRewrite, fmAppend:
	begin
		DesiredAccess := GENERIC_WRITE;
	end;
	fmReadAndWrite:
	begin
		DesiredAccess := GENERIC_READ or GENERIC_WRITE;
	end
	else
	begin
		DesiredAccess := GENERIC_ALL;
	end;
	end;

	if (FileExists(FTempFileName) = False) and (FMode <> fmReadOnly) then
		CreationDistribution := CREATE_NEW
	else
		CreationDistribution := OPEN_EXISTING;

	Flags := Flags and (not FILE_FLAG_OVERLAPPED);
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
			if ErrorRetry(ErrorCode) then goto LRetry;
			Exit;
		end;
	end;
	GetFileSize(FFileSize);
	if Mode = fmAppend then SeekEnd;
	if Mode = fmRewrite then Truncate;
	Result := True;
end;

function HandleFileSize(HFile: THandle): S8;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	TU8(Result).D0 := GetFileSize(HFile, @TU8(Result).D1);

	if TU8(Result).D0 = $FFFFFFFF then
	begin
		ErrorCode := GetLastError;
		if Result <> NO_ERROR then
		begin
			Result := -1;
			if ErrorRetry(ErrorCodeToStr(ErrorCode)) then goto LRetry;
		end;
	end;
end;

function TFile.GetFileSize(var Size: U8): BG;
begin
	Size :=  HandleFileSize(HFile);
	Result := Size >= 0;
end;

function GetFileDateTime(const FileName: TFileName; var CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
label LRetry;
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
			IOError(FileName, GetLastError);
		end;
	end
	else
	begin
		IOError(FileName, GetLastError);
	end;
end;

function TFile.Seek(const Pos: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
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
			if ErrorRetry(ErrorCode) then goto LRetry;
		end
		else
			Result := True;
	end;
end;

function TFile.SeekStart: BG;
begin
	Result := Seek(0);
end;

function TFile.SeekEnd: BG;
begin
	Result := Seek(FFileSize);
end;

function TFile.BlockRead(out Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
	LRetry:
	if ReadFile(HFile, Buf, Count, Suc, nil) then
	begin
		Result := True;
		Inc(ReadCount);
		Inc(ReadBytes, Suc);

		if Suc <> Count then
			Warning('Reading only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO) + ' from ' + FTempFileName)
		else
			MainLogAdd('Reading ' + BToStr(Suc, ofIO) + ' from ' + FTempFileName, mtDebug);

		Inc(FFilePos, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
			begin
				Seek(FFilePos);
				goto LRetry;
			end;
			Result := False;
		end
		else
			Result := True;
		Inc(FFilePos, Suc);
		Seek(FFilePos);
	end;
end;

function TFile.BlockWrite(const Buf; const Count: UG): BG;
label LRetry;
var
	Suc: U4;
	ErrorCode: U4;
begin
	LRetry:
	if WriteFile(HFile, Buf, Count, Suc, nil) then
	begin
		Result := True;
		Inc(WriteCount);
		Inc(WriteBytes, Suc);

		if Suc <> Count then
			Warning('Writing only ' + BToStr(Suc, ofIO) + '/' + BToStr(Count, ofIO) + ' to ' + FTempFileName)
		else
			MainLogAdd('Writing ' + BToStr(Suc, ofIO) + ' to ' + FTempFileName, mtDebug);

		Inc(FFilePos, Suc);
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then
			begin
				Seek(FFilePos);
				goto LRetry;
			end;
			Result := False;
		end
		else
			Result := True;
		Inc(FFilePos, Suc);
		Seek(FFilePos);
	end;
end;

function TFile.FillWrite(Count: UG): BG;
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

{
	File   |********************************|
	Buffer                 |*******|
}

function TFile.Readln(out Line: string): BG;
var BufPos, InLineIndex: SG;
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

	SetLength(Line, 256);
	InLineIndex := 1;
	while not Eof do
	begin
		if (FFilePos >= FBufStart) and (FFilePos <= FBufEnd) then
		begin
			BufPos := FFilePos - FBufStart;
			if {Eof or} (FBuffer[BufPos] = CharLF) then
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
			if FBufEnd >= FFileSize then FBufEnd := FFileSize - 1;
			Seek(FBufStart);
			BufPos := FFilePos;
			BlockRead(FBuffer[0], FBufEnd - FBufStart + 1);
			FFilePos := BufPos;
//      BufPos := FFilePos - FBufStart;
			Continue;
		end;
		if FBuffer[BufPos] <> CharCR then
		begin
			if InLineIndex > Length(Line) then
				SetLength(Line, 2 * (InLineIndex - 1));
			Line[InLineIndex] := FBuffer[BufPos];
			Inc(InLineIndex);
		end;
		Inc(FFilePos);
	end;
	SetLength(Line, InLineIndex - 1);
end;

function TFile.Write(const Data: string): BG;
var
	DataLength: SG;
begin
	DataLength := Length(Data);
	if DataLength > 0 then
		Result := BlockWrite(Data[1], DataLength)
	else
		Result := True;
end;

function TFile.Writeln(Line: string): BG;
begin
	Line := Line + FileSep;
	Result := BlockWrite(Line[1], Length(Line));
end;

function TFile.WritelnW(Line: WideString): BG;
begin
	Line := Line + CharNul + CharLF;
	Result := BlockWrite(Line[1], 2 * Length(Line));
end;

function TFile.Close(const ChangeDate: BG = True; const Forced: BG = False): BG;
label LRetry;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
	ErrorCode: U4;
begin
	LRetry:
	Result := False;
	if not IsOpened then
	begin
		Warning('Trying re-close file %1.', FTempFileName);
		Exit;
	end;
	MainLogAdd('Closing ' + FTempFileName, mtDebug);


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
		if FProtection and (FMode in [fmRewrite, fmReadAndWrite]) and (Forced = False) then
		begin
//		RenameFileEx(FTempFileName, FFileName); only on same disk
			if DirectoryExists(ExtractFileDir(FFileName)) = False then
				IOError(FFileName, 3)
			else
			begin
				CopyFile(FTempFileName, FFileName, False);
				DeleteFileEx(FTempFileName);
			end;
		end;
		Result := True;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorCode <> NO_ERROR then
		begin
			if ErrorRetry(ErrorCode) then goto LRetry;
			Result := False;
		end
		else
			Result := True;
	end;
	HFile := INVALID_HANDLE_VALUE;
end;

function TFile.Truncate: BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := SetEndOfFile(HFile);
	if Result then
	begin
		FFileSize := FFilePos;
	end
	else
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.FlushFileBuffers: BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := Windows.FlushFileBuffers(HFile);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.Eof: BG;
begin
	Result := FFilePos >= FFileSize;
end;

function TFile.Lock(From, Count: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := LockFile(HFile, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

function TFile.UnLock(From, Count: U8): BG;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	Result := UnLockFile(HFile, TU8(From).D0, TU8(From).D1, TU8(Count).D0, TU8(Count).D1);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCode) then goto LRetry;
	end;
end;

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
begin
	if ExeFileName <> '' then Exit;

	GetDir(0, StartDir);
	CorrectDir(StartDir);

	ExeFileName := GetCommandLine;
	WorkDir := '';
	// Remove Parameters
	if Length(ExeFileName) > 0 then
	begin
		if ExeFileName[1] = '"' then
		begin
			Delete(ExeFileName, 1, 1);
			i := Pos('"', ExeFileName);
			if i > 0 then Delete(ExeFileName, i, Length(ExeFileName) - i + 1);
		end
		else
		begin
			i := Pos(' ', ExeFileName);
			Delete(ExeFileName, i, Length(ExeFileName) - i + 1);
		end;

		// Split ExeFileName to WorkDir and InternalName
		for i := Length(ExeFileName) downto 0 do
		begin
			if i = 0 then
			begin
				Break;
			end;
			if (ExeFileName[i] = PathDelim) then
			begin
				if FProjectInfo[piInternalName] = '' then // if not initialized in ProjectInfo
					FProjectInfo[piInternalName] := DelFileExt(Copy(ExeFileName, i + 1, MaxInt));
				WorkDir := Copy(ExeFileName, 1, i);
				Break;
			end;
		end;
	end;
	if WorkDir = '' then WorkDir := StartDir;
	GraphDir := WorkDir + 'Graphics' + PathDelim;
	SoundsDir := WorkDir + 'Sounds' + PathDelim;
	DataDir := WorkDir + 'Data' + PathDelim;

	SetLength(SysDir, MAX_PATH);
	NewLength := GetSystemDirectory(PChar(SysDir), MAX_PATH);
	SetLength(SysDir, NewLength);
	CorrectDir(SysDir);

	SetLength(WinDir, MAX_PATH);
	NewLength := GetWindowsDirectory(PChar(WinDir), MAX_PATH);
	SetLength(WinDir, NewLength);
	CorrectDir(WinDir);

	ProgramFilesDir := GetEnvironmentVariable('ProgramFiles');
	if ProgramFilesDir = '' then ProgramFilesDir := 'C' + DriveDelim + PathDelim + 'Program Files' + PathDelim;
	CorrectDir(ProgramFilesDir);

	CommonTempDir := GetEnvironmentVariable('TEMP');
	if CommonTempDir = '' then TempDir := WinDir + 'Temp';
	CorrectDir(CommonTempDir);
	TempDir := CommonTempDir + '_' + GetProjectInfo(piInternalName) + PathDelim;
	CreateDirEx(TempDir);                                     

	ApplicationDataDir := GetEnvironmentVariable( 'APPDATA');
	if ApplicationDataDir = '' then ApplicationDataDir := WinDir + 'Application Data' + PathDelim;
	CorrectDir(ApplicationDataDir);
	AppDataDir := ApplicationDataDir + GetProjectInfo(piCompanyName) + PathDelim + GetProjectInfo(piInternalName) + PathDelim;
	CreateDirsEx(AppDataDir);

//	DocsDir := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH');
//	CorrectDir(DocsDir);
	MyDocuments := ShellFolder('Personal');
	CorrectDir(MyDocuments);

	MainIniFileName := WorkDir + GetProjectInfo(piInternalName) + '.ini';
	if not FileExists(MainIniFileName) then
	begin
		MainIniFileName := AppDataDir + GetProjectInfo(piInternalName) + '.ini';
		MainLogFileName := AppDataDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
	end
	else
		MainLogFileName := WorkDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
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

	// TODO: C:\Windows -> %windir%
end;

function RemoveEV(Dir: string): string;
var
	i, Start: SG;
	Variable, Value: string;
	NewLength: SG;
begin
	i := 1;
	while i <= Length(Dir) do
	begin
		if Dir[i] = '%' then
		begin
			Start := i;
			Inc(i);
			Variable := ReadToChar(Dir, i, '%');
			if (i > Length(Dir) + 1) then Break; // next % not found

			SetLength(Value, MAX_PATH);
			NewLength := GetEnvironmentVariable(PChar(Variable), PChar(Value), Max_PATH);
			SetLength(Value, NewLength);

			Delete(Dir, Start, i - Start);
			Insert(Value, Dir, Start);
			i := Start + Length(Value);
		end
		else
			Inc(i);
	end;
	Result := Dir;
end;

function ExpandDir(Dir: string): string;
begin
	if Length(Dir) = 0 then
		Result := WorkDir
{	else if StartStr('http://', Dir) then
		Result := Dir
	else if (Pos('://', Dir) <> 0) then
		Result := Dir}
	else
	begin // file://
		Dir := RemoveEV(Dir);

{		for i := 1 to Length(Dir) do
		begin
			if Dir[i] = ':' then
			begin
				Result := Dir;
				Exit;
			end;
		end;}
		if (Length(Dir) > 1) and (Dir[1] = PathDelim) and (Dir[2] = PathDelim) then
		begin
			// Network path
			Result := Dir;
		end
		else if (Length(Dir) > 0) and (Dir[1] = PathDelim) then
			Result := WorkDir[1] + WorkDir[2] + Dir
		else if ((Length(Dir) > 1) and (Dir[2] = ':'))then
		begin
			// Absolute path
			Result := Dir;
		end
		else
			// Relative path
			Result := WorkDir + Dir;

		// TODO: '..' like HTML (New Parameter WorkDir=ActualDir)
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
		if Dir[i] = PathDelim then
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
		PathDelim, DriveDelim:
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

procedure ReadDir(var FileNames: TFileNames; var FilesCount: SG; Path: string; Extensions: array of string; Files, Dirs, SubDirs, Sort: BG);
var
	NewSize: SG;
	IsDir, IsFile: BG;
	ErrorCode: Integer;

		procedure ReadSubDir(SubPath: string);
		var
			SearchRec: TSearchRec;
			Read: BG;
			i: SG;
		begin
			// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
			ErrorCode := FindFirst(Path + SubPath + '*.*', faAnyFile, SearchRec);
			while ErrorCode = NO_ERROR do
			begin
				IsDir := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..');
				if IsDir and (Dirs or SubDirs) then SearchRec.Name := SearchRec.Name + PathDelim;
				IsFile := (SearchRec.Attr and faDirectory) = 0;

				if (IsDir and Dirs)
				or (IsFile and Files) then
				begin
					if Length(Extensions) = 0 then
						Read := True
					else
					begin
						Read := False;
						for i := 0 to Length(Extensions) - 1 do
						begin
							if UpperCase(ExtractFileExt(SearchRec.Name)) = '.' + UpperCase(Extensions[i]) then
							begin
								Read := True;
								Break;
							end;
						end;
					end;
					if Read then
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
	{$ifopt d+}
	for i := 0 to Length(Extensions) - 1 do
	begin
		Assert(Length(Extensions[i]) > 0);
		Assert(Extensions[i, 1] <> '.');
	end;
	{$endif}
{						if (Extensions = '') or (Extension = '*') or (Extension = '*.*') or
	if Length(Extension) > 1 then
		if (Extension <> '*') and (Extension <> '*.*') then
			if Extension[1] <> '.' then Extension := '.' + Extension;}
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

function GetFileSizeU(const FileName: TFileName): S8;
var
	HFile: THandle;
begin
	Result := -1;
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
		Result := HandleFileSize(HFile);
		if CloseHandle(HFile) = False then
		begin
			IOError(FileName, GetLastError);
		end;
	end
	else
	begin
		IOError(FileName, GetLastError);
	end;
end;

function GetFileSizeS(const FileName: TFileName): string;
var FileSize: U8;
begin
	FileSize := GetFileSizeU(FileName);
	if FileSize < 0 then
		Result := 'N/A'
	else
		Result := BToStr(FileSize);
end;

function FileTimeToDateTime(F: TFileTime): TDateTime;
var
	SystemTime: TSystemTime;
begin
	FileTimeToLocalFileTime(F, F);
	FileTimeToSystemTime(F, SystemTime);
	Result := SystemTimeToDateTime(SystemTime);
end;

function GetFileModified(const FileName: TFileName; var LastWriteTime: TFileTime): BG;
var
	ACreationTime, ALastAccessTime: TFileTime;
begin
	Result := GetFileDateTime(FileName, ACreationTime, ALastAccessTime, LastWriteTime);
end;

function GetFileModified(const FileName: TFileName): TFileTime;
var
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	if GetFileDateTime(FileName, ACreationTime, ALastAccessTime, ALastWriteTime) then
		Result := ALastWriteTime;
end;

function SetFileModified(FileName: TFileName; LastWriteTime: TFileTime): BG;
var
	F: TFile;
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
begin
	Result := False;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadAndWrite) then
		begin
			Result := GetFileTime(F.HFile, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
			if Result then
			begin
				Result := SetFileTime(F.HFile, @ACreationTime, @ALastAccessTime, @LastWriteTime);
				if Result = False then
					IOError(FileName, GetLastError);
			end
			else
			begin
				IOError(FileName, GetLastError);
			end;

			F.Close(False);
		end;
	finally
		F.Free;
	end;
end;

function RenameFileEx(const Source, Dest: TFileName): BG;
label LRetry;
var ErrorCode: U4;
begin
	Windows.SetFileAttributes(PChar(Dest), FILE_ATTRIBUTE_ARCHIVE);
	LRetry:
	Result := Windows.MoveFileEx(PChar(Source), PChar(Dest), MOVEFILE_REPLACE_EXISTING);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCodeToStr(ErrorCode) + LineSep + Source + LineSep + Dest) then goto LRetry;
	end;
end;

function CopyFile(const Source, Dest: TFileName; const FailExist: BG): BG;
label LRetry;
var ErrorCode: U4;
begin
	Windows.SetFileAttributes(PChar(Dest), FILE_ATTRIBUTE_ARCHIVE);
	LRetry:
	Result := Windows.CopyFile(PChar(Source), PChar(Dest), FailExist);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCodeToStr(ErrorCode) + LineSep + Source + LineSep + Dest) then goto LRetry;
	end;
end;

function CopyFileToDir(Source, Dest: TFileName; const FailExist: BG): BG;
begin
	Result := CopyFile(Source, Dest + ExtractFileName(Source), FailExist);
end;

function CopyDamagedFile(Source, Dest: TFileName): BG;
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
	try
		if FS.Open(Source, fmReadOnly) then
		begin
			if FD.Open(Dest, fmRewrite) then
			begin
				while not FS.Eof do
				begin
					FillChar(Buf^, Count, 0);
					if not FS.BlockRead(Buf^, Count) then
						Result := False;
					FD.BlockWrite(Buf^, Count);
				end;
				FD.Truncate;
				FD.Close;
			end;
			FS.Close;
		end;
	finally
		FS.Free;
		FD.Free;
		FreeMem(Buf, Count);
	end;
end;

function CreateDirEx(const Dir: string): BG;
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
		Result := CreateDirectory(PChar(Dir), nil);
		if Result = False then
			IOError(Dir, GetLastError);
	end;
end;

function CreateDirsEx(const Dir: string): BG;
var i: SG;
begin
	Result := False;
	if Dir = '' then Exit;
	i := 1;
	while i < Length(Dir) do
	begin
		if Dir[i] = PathDelim then
			if CreateDirEx(Copy(Dir, 1, i)) = False then Exit;
		Inc(i);
	end;
	Result := CreateDirEx(Dir);
end;

function NewFileOrDir(var FileOrDir: string): BG;
var
	i: SG;
	IsDir: BG;
	DirS, DirE: string;
begin
	Result := False;
	if Length(FileOrDir) = 0 then Exit;
	IsDir := LastChar(FileOrDir) = PathDelim;
	if IsDir then
	begin
		DirS := Copy(FileOrDir, 1, Length(FileOrDir) - 1);
		DirE := PathDelim;
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
	end;
end;

function NewFileOrDirEx(var FileOrDir: string): BG;
begin
	FileOrDir := DelFileExt(FileOrDir) + ' ' + ReplaceF(DateTimeToS(Now, 0, ofIO), ':', '_') + ExtractFileExt(FileOrDir);
	Result := NewFileOrDir(FileOrDir);
end;

function CopyDir(const Source, Dest: string): BG;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CreateDirEx(Dest);

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(Source + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
				CopyDir(Source + SearchRec.Name + PathDelim, Dest + SearchRec.Name + PathDelim);
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

function DeleteFileEx(const FileName: TFileName): BG;
begin
	Windows.SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE);
	Result := DeleteFile(PChar(FileName));
	if Result = False then
		IOError(FileName, GetLastError);
end;

function RemoveDirEx(const DirName: string): BG;
begin
	Result := RemoveDirectory(PChar(DirName));
	if Result = False then
		IOError(DirName, GetLastError);
end;

function RemoveDirsEx(DirName: string; DeleteSelf: BG = False): BG;
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
				Result := RemoveDirsEx(DirName + SearchRec.Name + PathDelim, True) and Result;
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

	if DeleteSelf then Result := RemoveDirEx(DirName) and Result;
end;

// TFile Read Write

function ReadBufferFromFile(const FileName: TFileName; out Buf; out Count: SG): BG;
var
	F: TFile;
begin
	Result := False;
	Count := 0;
	Pointer(Buf) := nil;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			GetMem(Pointer(Buf), F.FileSize);
			F.BlockRead(Pointer(Buf)^, F.FileSize);
			Count := F.FileSize;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function WriteBufferToFile(const FileName: TFileName; const Buf; const Count: SG): BG;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	try
		if F.Open(FileName, fmRewrite) then
		begin
			F.BlockWrite(Pointer(Buf)^, Count);
			F.Truncate;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadBlockFromFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			F.BlockRead(Buf^, Min(Count, F.FileSize));
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function WriteBlockToFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;
var
	F: TFile;
begin
	Result := False;
	F := TFile.Create;
	try
		if F.Open(FileName, fmRewrite) then
		begin
			F.BlockWrite(Buf^, Count);
			F.Truncate;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadStringFromFile(const FileName: TFileName; out Data: string): BG; overload;
var
	F: TFile;
begin
	Result := False;
	Data := '';
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			SetLength(Data, F.FileSize);
			if Length(Data) >= 1 then
				F.BlockRead(Data[1], F.FileSize);
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadStringFromFile(const FileName: TFileName): string; overload;
begin
	ReadStringFromFile(FileName, Result);
end;

function SameDataInFile(const FileName: TFileName; const Line: string): BG;
label LClose;
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
		try
			if F.Open(FileName, fmReadOnly) then
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
				F.Close;
			end;
		finally
			FreeMem(Buf);
			F.Free;
		end;
	end;
end;

function WriteStringToFile(const FileName: TFileName; const Data: string; const Append: BG): BG;
var
	F: TFile;
	FileMode: TFileMode;
begin
	if (Append = False) and SameDataInFile(FileName, Data) then
		Result := True
	else
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
				F.Write(Data);
				F.Close;
				Result := True;
			end;
		finally
			F.Free;
		end;
	end;
end;

function ReadStringsFromFile(const FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
var
	F: TFile;
	Line: string;
	NewSize: SG;
begin
	Result := False;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			while not F.Eof do
			begin
				F.Readln(Line);
				NewSize := LineCount + 1;
				if AllocByExp(Length(Lines), NewSize) then
					SetLength(Lines, NewSize);
				Lines[LineCount] := Line;
				Inc(LineCount);
			end;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function WriteStringsToFile(const FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; const Append: BG): BG;
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
			while i < OpeningNameCount do
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
(*
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
*)


function StrRScan(const Str: string): string;
var
	Index: SG;
begin
	Result := '';
	Index := Length(Str);
	while Index > 0 do
	begin
		while Str[Index] in ['\', '/'] do
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
	if (FileExists(ShortName) = False) or (Length(ShortName) < 2) or (ShortName[1] = PathDelim) then
	begin
		Result := ShortName;
		Exit;
	end;
	Result := '';
	LastSlash := StrRScan(ShortName);
	while LastSlash <> '' do
	begin
		Result := PathDelim + ShortToLongFileName(ShortName) + Result;
		if LastSlash <> '' then
		begin
			SetLength(ShortName, Length(ShortName) - Length(LastSlash));
			LastSlash := StrRScan(ShortName);
		end;
	end;
	Result := UpperCase(ShortName) {c: -> C:} + Result;
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
	TempPathPtr := ShortName;
	LastSlash := StrRScan(TempPathPtr, PathDelim);
	while LastSlash <> nil do begin
		Result := PathDelim + ShortToLongFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			LastSlash^ := char(0);
			LastSlash := StrRScan(TempPathPtr, PathDelim);
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
	TempPathPtr := LongName;
	LastSlash := StrRScan(TempPathPtr, PathDelim);
	while LastSlash <> nil do begin
		Result := PathDelim + LongToShortFileName(TempPathPtr) + Result;
		if LastSlash <> nil then begin
			LastSlash^ := char(0);
			LastSlash := StrRScan(TempPathPtr, PathDelim);
		end;
	end;
	Result := TempPathPtr + Result;
end;
*)
{$ENDIF WIN32}

function RepairDirectory(const Dir: TFileName): TFileName;
begin
	Result := ShortToLongPath(ExpandDir(Dir));
	if Result = '' then Exit;
	while True do
	begin
		if DirectoryExists(Result) then Break;
		if BackDir(string(Result)) = False then Break;
	end;
end;

{$ifndef Console}
function ExecuteDialog(Dialog: TOpenDialog; var FileName: TFileName): BG;
begin
	Dialog.FileName := ExtractFileName(FileName);
	Dialog.InitialDir := RepairDirectory(ExtractFilePath(FileName));
	Result := Dialog.Execute;
	if Result then
		FileName := ShortDir(Dialog.FileName);
end;
{$endif}

function SameFiles(FileName1, FileName2: TFileName): BG;
label LClose;
var
	TotalBytes: U8;
	ReadBytes, FileBufferSize: UG;
	File1, File2: TFile;
	Buf1, Buf2: Pointer;
begin
	Result := False;
	File1 := nil;
	File2 := nil;
	Buf1 := nil;
	Buf2 := nil;

	File1 := TFile.Create;
	if not File1.Open(FileName1, fmReadOnly) then
	begin
		goto LClose;
	end;
	File2 := TFile.Create;
	if not File2.Open(FileName2, fmReadOnly) then
	begin
		goto LClose;
	end;
	if File1.FileSize <> File2.FileSize then goto LClose;
	if FileName1[1] = FileName2[1] then
		FileBufferSize := 2 * MB // Same drive
	else
		FileBufferSize := DefFileBuffer;

	GetMem(Buf1, FileBufferSize);
	GetMem(Buf2, FileBufferSize);

	TotalBytes := File1.FileSize;
	while TotalBytes > 0 do
	begin
		ReadBytes := FileBufferSize;
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
		File1.Close;
		FreeAndNil(File1);
	end;
	if Assigned(File2) then
	begin
		File2.Close;
		FreeAndNil(File2);
	end;
	FreeMem(Buf1);
	FreeMem(Buf2);
end;

procedure ReplaceIfChanged(const FileName: TFileName);
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
	if Length(Ext) > 1 then
	begin
		for i := 0 to Length(Ext) - 1 do
		begin
			s1 := s1 + {'*.' +} Ext[i] + ', ';
			s2 := s2 + '*.' + Ext[i] + ';';
		end;
		DelLastChar(s1);
		s1[Length(s1)] := ')';
		DelLastChar(s2);
		Result := 'Any (' + s1 + '|' + s2 + '|';
	end
	else
		Result := '';
	for i := 0 to Length(Ext) - 1 do
	begin
		Result := Result + Des[i] + ' (*.' + Ext[i] + ')|*.' + Ext[i] + '|';
	end;
	DelLastChar(Result);
	Result := Result + AllFiles;
end;

function FileExistsEx(const FileName: TFileName): BG;
begin
	Result := FileExists(RemoveEV(FileName));
end;
{
function DirectoryExists(const Directory: string): BG;
var
	Code: U4;
begin
	Code := GetFileAttributes(PChar(Directory));
	Result := (Code <> High(Code)) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;}

function DirectoryExistsEx(const DirName: TFileName): BG;
begin
	Result := DirectoryExists(RemoveEV(DirName));
end;

function FileOrDirExists(const FileOrDirName: string): BG;
begin
	if Length(FileOrDirName) = 0 then
		Result := False
	else if LastChar(FileOrDirName) = PathDelim then
		Result := DirectoryExists(FileOrDirName)
	else
		Result := FileExists(FileOrDirName);
end;

function FileOrDirExistsEx(const FileOrDirName: string): BG;
begin
	if Length(FileOrDirName) = 0 then
		Result := False
	else if LastChar(FileOrDirName) = PathDelim then
		Result := DirectoryExistsEx(FileOrDirName)
	else
		Result := FileExistsEx(FileOrDirName);
end;

function LastLineFromFile(const FileName: TFileName): string;
var
	i: SG;
	F: TFile;
	C: Char;
begin
	Result := '';
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly, FILE_FLAG_RANDOM_ACCESS) then
		begin
			i := F.FileSize - 1;
			while i >= 0 do
			begin
				F.Seek(i);
				F.BlockRead(C, SizeOf(C));
				if not (C in [CharCR, CharLF]) then Break;
				Dec(i);
			end;

			while i >= 0 do
			begin
				F.Seek(i);
				F.BlockRead(C, SizeOf(C));
				if C in [CharCR, CharLF] then Break;
				Result := c + Result;
				Dec(i);
			end;
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

initialization
	InitPaths;
	EnumToStr(TypeInfo(TFileMode), FileModeStr);
end.
