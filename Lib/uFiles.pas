unit uFiles;

interface

uses
	uTypes, uStrings, uFile,
	{$ifndef Console}Dialogs,{$endif}
	SysUtils, Windows;

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
//	HomeDir, // User documnets (Read and Write)
	CommonAppDataDir, // Application Data
	TempDir,
	CommonTempDir: string;
	ExeFileName, MainIniFileName, MainLogFileName: TFileName;

type
	TFileNames = array of TFileName;

function GetFileDateTime(const FileName: TFileName; out CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
function GetFileModificationDateTime(const FileName: TFileName): TFileTime;
function SetFileDateTime(const FileName: TFileName; const CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
function ShortDir(const Dir: string): string;

function RemoveEV(const Dir: string): string; overload;
function RemoveEV(Dir: string; const Environment: array of TStringPair): string; overload;
function ExpandDir(Dir: string): string;
function DelFileExt(const FName: string): string;
function DelFileName(const FName: string): string;
function AddAfterName(const FName: string; const Text: string): string;
function ParentDir(var Dir: string): BG;
function ParentDirF(Dir: string): string;
function LegalFileName(const FileName: string): string;
function LegalPath(const Path: string): string;
procedure ReadDir(var FileNames: TFileNames; var FileCount: SG; const Path: string; const Extensions: array of string; const Files, Dirs, SubDirs, Sort: BG);
function HandleFileSize(FHandle: THandle): S8;
function GetFileSizeU(const FileName: TFileName): S8;
function GetFileSizeS(const FileName: TFileName): string;
function FileTimeToDateTime(F: TFileTime): TDateTime;
function DateTimeToFileTime(const D: TDateTime): TFileTime;
function GetFileCreated(const FileName: TFileName): TFileTime; overload;
function GetFileModified(const FileName: TFileName; var LastWriteTime: TFileTime): BG; overload;
function GetFileModified(const FileName: TFileName): TFileTime; overload;
function SetFileModified(FileName: TFileName; LastWriteTime: TFileTime): BG;

function RenameFileEx(const Source, Dest: TFileName): BG;
function CopyFile(const Source, Dest: TFileName; const FailExist: BG): BG;
function CopyFileToDir(Source, Dest: TFileName; const FailExist: BG): BG;
procedure SplitFile(const Source: TFileName; const Dest: string; const MaxFileSize: U8 = GB; const CreateCRCFile: BG = False);
function CopyDamagedFile(Source, Dest: TFileName): BG;
function CreateDirEx(const Dir: string): BG;
function CreateDirsEx(const Dir: string): BG;
function NewFileOrDir(var FileOrDir: string): BG;
function NewFileOrDirEx(var FileOrDir: string): BG;
function CopyFileDateTime(const Source, Dest: string): BG;
function CopyDirOnly(const Source, Dest: string): BG;
function CopyDir(const Source, Dest: string): BG;
procedure BackupFile(const FileName: TFileName);

function DeleteFileEx(const FileName: TFileName): BG;
function RemoveDirEx(const DirName: string): BG;
function RemoveDirsEx(DirName: string; DeleteSelf: BG = False): BG;

procedure FileLinesAndSize(const FileName: TFileName; out Size, Lines: U8);
procedure CodeLinesAndSize(const Dir: string; out Size, Lines: U8);

function ReadBufferFromFile(const FileName: TFileName; out Buf; out Count: SG): BG;
function WriteBufferToFile(const FileName: TFileName; const Buf; const Count: SG): BG;

function ReadBlockFromFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;
function WriteBlockToFile(const FileName: TFileName; Buf: Pointer; const Count: SG): BG;

function ReadStringsFromFile(const FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
function WriteStringsToFile(const FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; const Append: BG; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;

function ReadStringFromFile(const FileName: TFileName; out Data: AnsiString): BG; overload;
function ReadStringFromFile(const FileName: TFileName; out Data: UnicodeString): BG; overload;
function ReadStringFromFile(const FileName: TFileName): UnicodeString; overload;
function ReadStringFromFile(const FileName: TFileName; const Limit: SG): string; overload;
function WriteStringToFile(const FileName: TFileName; const Data: AnsiString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG; overload;
function WriteStringToFile(const FileName: TFileName; const Data: UnicodeString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG; overload;

{$IFDEF WIN32}
function ShortToLongFileName(const ShortName: string): string;
function ShortToLongPath(ShortName: string): string;
{function LongToShortFileName(const LongName: string): string;
function LongToShortPath(const LongName: string): string;}
{$ENDIF WIN32}

function RepairDirectory(const Dir: TFileName): TFileName;
{$ifndef Console}
function ExecuteDialog(const Dialog: TOpenDialog; var FileName: TFileName): BG; overload;
{$endif}
function SameFiles(const FileName1, FileName2: TFileName): BG;
function TempFileName(const FileName: TFileName): TFileName;
procedure ReplaceIfChanged(const OrigFileName, TempFileName: TFileName); overload;
procedure ReplaceIfChanged(const TempFileName: TFileName); overload;
procedure InitPaths;

function DirectoryExistsEx(const DirName: TFileName): BG;
function FileExistsEx(const FileName: TFileName): BG;
function FileOrDirExists(const FileOrDirName: string): BG;
function FileOrDirExistsEx(const FileOrDirName: string): BG;
function LastLineFromFile(const FileName: TFileName): AnsiString;
function SameFileName(const FileName1, FileName2: TFileName): BG;

function DialogStrWithoutAll(const Ext, Des: array of string): string;
function DialogStr(const Ext, Des: array of string): string;
function GetFileNameFilter(const Description: string; const Extensions: array of string): string;
function GetExecutableFilter: string;
function AllFiles: string;
function AllText: string;
function AllSounds: string;

function SplitStr(const Source: string; const MaxStrings: SG): TArrayOfString;
function Installed: BG;

implementation

uses
	Math,
	uMsg, uProjectInfo, uSorts, uCharset,
	uOutputFormat, uMath, uLog;

var
	StartupEnvironment: array of TStringPair;
	GInstalled: BG;

procedure InitStartupEnvironment;
var
	EnvironmentBlock, EnvironmentBlock2: LPTSTR;
	Line: string;
	i: SG;
	InlineIndex: SG;
	NewSize: SG;
begin
	EnvironmentBlock := GetEnvironmentStrings;
	try
		if EnvironmentBlock = nil then
		begin
			Warning('GetEnvironmentStrings failed (%1)', [ErrorCodeToStr(GetLastError)]);
			Exit;
		end;

		SetLength(StartupEnvironment, 0);
		EnvironmentBlock2 := EnvironmentBlock;
		i := 0;
		while EnvironmentBlock2[1] <> #0 do
		begin
			Line := EnvironmentBlock2;
			if Line = '' then Break;
			NewSize := i + 1;
			if AllocByExp(Length(StartupEnvironment), NewSize) then
			begin
				SetLength(StartupEnvironment, NewSize);
			end;
			InlineIndex := 1;
			StartupEnvironment[i].Name := ReadToChar(Line, InlineIndex, '=');
			StartupEnvironment[i].Value := Copy(Line, InlineIndex, MaxInt);
			Inc(EnvironmentBlock2, Length(Line) + 1);
			Inc(i);
		end;
		SetLength(StartupEnvironment, i);
	finally
		FreeEnvironmentStrings(EnvironmentBlock);
	end;
end;

function Installed: BG;
begin
	Result := GInstalled;
end;

function SplitStr(const Source: string; const MaxStrings: SG): TArrayOfString;
var
	i: SG;
	EndIndex: SG;
	ResultCount: SG;
begin
	SetLength(Result, MaxStrings);
	if MaxStrings <= 0 then Exit;

	ResultCount := 0;
	i := 1;
	while i < Length(Source) do
	begin
		if Source[i] = CharSpace then
		begin
			Inc(i);
			Continue;
		end;
		if Source[i] = '"' then
		begin
			Inc(i);
			EndIndex := PosEx('"', Source, i);
		end
		else
		begin
			EndIndex := PosEx(CharSpace, Source, i + 1);
		end;
		if EndIndex = 0 then EndIndex := MaxInt - 1;

		Result[ResultCount] := Copy(Source, i, EndIndex - i);
		Inc(ResultCount);
		if ResultCount >= MaxStrings then
			Exit;

		i := EndIndex + 1;
	end;
end;

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
	All: TArrayOfString;
begin
	if ExeFileName <> '' then Exit;

	GetDir(0, StartDir);
	CorrectDir(StartDir);

	// Remove Parameters
	All := SplitStr(GetCommandLine, 1);

	ExeFileName := All[0];
	WorkDir := '';
{	if Length(ExeFileName) > 0 then
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
		end;}

		// Split ExeFileName to WorkDir and InternalName
		for i := Length(ExeFileName) downto 0 do
		begin
			if i = 0 then
			begin
				Break;
			end;
			if (ExeFileName[i] = PathDelim) then
			begin
				WorkDir := Copy(ExeFileName, 1, i);
				Break;
			end;
		end;
//	end;
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

	CommonAppDataDir := GetEnvironmentVariable( 'APPDATA');
	if CommonAppDataDir = '' then CommonAppDataDir := WinDir + 'Application Data' + PathDelim;
	CorrectDir(CommonAppDataDir);
	AppDataDir := CommonAppDataDir + GetProjectInfo(piCompanyName) + PathDelim + GetProjectInfo(piInternalName) + PathDelim;
	if DirectoryExists(AppDataDir) then
	begin
		GInstalled := True
	end
	else
	begin
		GInstalled := False;
		CreateDirsEx(AppDataDir);
	end;

//	DocsDir := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH');
//	CorrectDir(DocsDir);
	InitStartupEnvironment;

	MainIniFileName := WorkDir + GetProjectInfo(piInternalName) + '.ini';
	if not FileExists(MainIniFileName) then
	begin
		MainIniFileName := AppDataDir + GetProjectInfo(piInternalName) + '.ini';
		MainLogFileName := AppDataDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
	end
	else
		MainLogFileName := WorkDir + 'Log' + PathDelim + GetProjectInfo(piInternalName) + '.log';
end;

// i.e. C:\WINDOWS -> %systemroot%
function InsertEnvironmentVariables(const Dir: string): string;
var
	i: SG;
	n: SG;
begin
	Result := Dir;
	for i := 0 to Length(StartupEnvironment) - 1 do
	begin
		if Pos(':\', StartupEnvironment[i].Value) <> 0 then
		begin
			n := Pos(StartupEnvironment[i].Value, Result);
			if n > 0 then
			begin
				Delete(Result, n, Length(StartupEnvironment[i].Value));
				Insert('%' + StartupEnvironment[i].Name + '%', Result, n);
			end;
		end;
	end;
end;

function RemoveWorkDir(const Dir: string): string;
var
	i: SG;
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

function ShortDir(const Dir: string): string;
begin
	Result := InsertEnvironmentVariables(RemoveWorkDir(Dir));
end;

function FindEnvironmentVariable(const Variable: string; const Environment: array of TStringPair): string;
var i: SG;
begin
	Result := '';
	for i := 0 to Length(Environment) - 1 do
	begin
		if Environment[i].Name = Variable then
		begin
			Result := Environment[i].Value;
			Break;
		end;
	end;
end;

function RemoveEV(const Dir: string): string; overload;
begin
	Result := RemoveEV(Dir, []);
end;

function RemoveEV(Dir: string; const Environment: array of TStringPair): string; overload;
var
	i, Start: SG;
	Variable, Value: string;
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

			if Length(Environment) > 0 then
			begin
				Value := FindEnvironmentVariable(Variable, Environment)
			end
			else
			begin
				Value := GetEnvironmentVariable(Variable);
			end;
			if Value = '' then
			begin
				i := Start + Length(Variable);
				Continue;
			end;

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

		// TODO : '..' like HTML (New Parameter WorkDir=ActualDir)
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

function DelFileName(const FName: string): string;
var i: SG;
begin
	for i := Length(FName) downto 1 do
	begin
		if CharInSet(FName[i], ['/', '\']) then
		begin
			Result := Copy(FName, 1, i);
			Exit;
		end;
	end;
	Result := '';
end;

function AddAfterName(const FName: string; const Text: string): string;
begin
	Result := DelFileExt(FName) + Text + ExtractFileExt(FName);
end;

function ParentDir(var Dir: string): BG;
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

function ParentDirF(Dir: string): string;
begin
	Result := Dir;
	ParentDir(Result);
end;

const
	DisabledChars: array[0..8] of string = ('\', '/', ':', '*', '?', '"', '<', '>', '|');

function LegalFileName(const FileName: string): string;
begin
	Result := FileName;
	DelStrings(Result, DisabledChars);
end;

function LegalPath(const Path: string): string;
var
	i: Integer;
	StrLength: Integer;
begin
	Result := Path;
	DelStrings(Result, DisabledChars);
	if Length(Result) = 0 then
	begin
		Exit;
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

{	if Length(Result) > 63 then
	begin
		SetLength(Result, 63);
	end;}
end;

function IsDirectory(const SearchRec: TSearchRec): BG;
begin
	Result := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
end;

const
	faAll = $00000031; // faArchive or faReadOnly or faDirectory;

procedure ReadSubDir(var FileNames: TFileNames; var FilesCount: SG; const Path: string; const SubPath: string; const Extensions: array of string; const Files, Dirs, SubDirs: BG);
var
	SearchRec: TSearchRec;
	Read: BG;
	i: SG;
	ErrorCode: SG;
	IsDir, IsFile: BG;
	NewSize: SG;
begin
	// faReadOnly or faHidden or faSysFile or
	ErrorCode := FindFirst(Path + SubPath + '*.*', faAll, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := IsDirectory(SearchRec);
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
					if LowerCase(ExtractFileExt(SearchRec.Name)) = '.' + LowerCase(Extensions[i]) then
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
			ReadSubDir(FileNames, FilesCount, Path, SubPath + SearchRec.Name, Extensions, Files, Dirs, SubDirs);
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then IOError(Path + SubPath, ErrorCode);
	SysUtils.FindClose(SearchRec);
end;

type
	TSearchRecs = array of TSearchRec;
var
	GList: ^TSearchRecs;

function Compare(const Index0, Index1: SG): SG;
begin
	Result := CompareString(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
		PChar(GList^[Index0].Name), Length(GList^[Index0].Name),
		PChar(GList^[Index1].Name), Length(GList^[Index1].Name)) - 2;
end;

procedure ReadSubDirSorted(var FileNames: TFileNames; var FilesCount: SG; const Path: string; const SubPath: string; const Extensions: array of string; const Files, Dirs, SubDirs: BG);
var
	SearchRec: TSearchRec;
	Read: BG;
	i, j: SG;
	ErrorCode: SG;
	IsDir, IsFile: BG;
	NewSize: SG;
	List: array of TSearchRec;
	ListCount: SG;
	AIndex: array of SG;
begin
	ListCount := 0;
	// faReadOnly or faHidden or faSysFile or
	ErrorCode := FindFirst(Path + SubPath + '*.*', faAll, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := IsDirectory(SearchRec);
		IsFile := (SearchRec.Attr and faDirectory) = 0;

		//if (IsDir and Dirs)
		if IsDir or (IsFile and Files) then
		begin
			if IsDir or (Length(Extensions) = 0) then
				Read := True
			else
			begin
				Read := False;
				for i := 0 to Length(Extensions) - 1 do
				begin
					if LowerCase(ExtractFileExt(SearchRec.Name)) = '.' + LowerCase(Extensions[i]) then
					begin
						Read := True;
						Break;
					end;
				end;
			end;
			if Read then
			begin
				NewSize := ListCount + 1;
				if AllocByExp(Length(List), NewSize) then
					SetLength(List, NewSize);
				List[ListCount] := SearchRec;
				Inc(ListCount);
			end;
		end;
		ErrorCode := FindNext(SearchRec);
	end;
	if ErrorCode <> ERROR_NO_MORE_FILES then
		IOError(Path + SubPath, ErrorCode);
	SysUtils.FindClose(SearchRec);

	if ListCount = 0 then Exit;

	SetLength(AIndex, ListCount);
	FillOrderU4(AIndex[0], ListCount);
	GList := @TSearchRecs(List);
	Sort(PArraySG(AIndex), ListCount, Compare);

	NewSize := FilesCount + ListCount;
	if AllocByExp(Length(FileNames), NewSize) then
		SetLength(FileNames, NewSize);

	for i := 0 to ListCount - 1 do
	begin
		j := AIndex[i];
		IsDir := IsDirectory(List[j]);
		if IsDir then
		begin
			if Dirs then
			begin
				FileNames[FilesCount] := List[j].Name + PathDelim;
				Inc(FilesCount);
			end;
		end
		else
		begin
			FileNames[FilesCount] := List[j].Name;
			Inc(FilesCount);
		end;
		if SubDirs and IsDir then
		begin
			ReadSubDir(FileNames, FilesCount, Path, SubPath + List[j].Name + PathDelim, Extensions, Files, Dirs, SubDirs);
		end;
	end;
end;


procedure ReadDir(var FileNames: TFileNames; var FileCount: SG; const Path: string; const Extensions: array of string; const Files, Dirs, SubDirs, Sort: BG);
{$ifopt d+}
var
	i: SG;
{$endif}
(*	Offset: Integer;
	MaxLimit: Integer;
	Switch: Integer;
	FileName: TFileName; *)
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

	if not Sort then
		ReadSubDir(FileNames, FileCount, CorrectDirF(ExpandDir(Path)), '', Extensions, Files, Dirs, SubDirs)
	else
		ReadSubDirSorted(FileNames, FileCount, CorrectDirF(ExpandDir(Path)), '', Extensions, Files, Dirs, SubDirs);

	if MainLogWrite(mlDebug) then
		MainLogAdd(NToS(FileCount) + ' files found in folder ' + Path + '.', mlDebug);
(*	if Sort then
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
	end; *)
end;

function HandleFileSize(FHandle: THandle): S8;
label LRetry;
var ErrorCode: U4;
begin
	LRetry:
	TU8(Result).D0 := GetFileSize(FHandle, @TU8(Result).D1);

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

function GetFileSizeU(const FileName: TFileName): S8;
var
	FHandle: THandle;
begin
	Result := -1;
	FHandle := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0,  // access (read-write) mode
		0,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := HandleFileSize(FHandle);
		if CloseHandle(FHandle) = False then
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
		Result := NAStr
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

function DateTimeToFileTime(const D: TDateTime): TFileTime;
var
	SystemTime: TSystemTime;
begin
	DateTimeToSystemTime(D, SystemTime);
	SystemTimeToFileTime(SystemTime, Result);
	LocalFileTimeToFileTime(Result, Result);
end;

function GetFileModified(const FileName: TFileName; var LastWriteTime: TFileTime): BG;
var
	ACreationTime, ALastAccessTime: TFileTime;
begin
	Result := GetFileDateTime(FileName, ACreationTime, ALastAccessTime, LastWriteTime);
end;

function GetFileCreated(const FileName: TFileName): TFileTime;
var
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	if GetFileDateTime(FileName, ACreationTime, ALastAccessTime, ALastWriteTime) then
		Result := ACreationTime;
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
			Result := GetFileTime(F.Handle, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
			if Result then
			begin
				Result := SetFileTime(F.Handle, @ACreationTime, @ALastAccessTime, @LastWriteTime);
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

function GetFileDateTime(const FileName: TFileName; out CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
label LRetry;
var
	FHandle: THANDLE;
begin
	Result := False;
	U8(CreationTime) := 0;
	U8(LastAccessTime) := 0;
	U8(LastWriteTime) := 0;
	FHandle := CreateFile(
		PChar(FileName),  // pointer to name of the file
		0, // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil, // pointer to security attributes
		OPEN_EXISTING, // how to create
		FILE_FLAG_BACKUP_SEMANTICS {FILE_ATTRIBUTE_NORMAL}, // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := GetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
		if Result = False then
			IOError(FileName, GetLastError);
		if CloseHandle(FHandle) = False then
		begin
			IOError(FileName, GetLastError);
		end;
	end
	else
	begin
		IOError(FileName, GetLastError);
	end;
end;

function GetFileModificationDateTime(const FileName: TFileName): TFileTime;
var CreationTime, LastAccessTime: TFileTime;
begin
	GetFileDateTime(FileName, CreationTime, LastAccessTime, Result);
end;

function SetFileDateTime(const FileName: TFileName; const CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
label LRetry;
var
	FHandle: THANDLE;
begin
	Result := False;
	FHandle := CreateFile(
		PChar(FileName),  // pointer to name of the file
		GENERIC_WRITE, // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil, // pointer to security attributes
		OPEN_EXISTING, // how to create
		FILE_FLAG_BACKUP_SEMANTICS {FILE_ATTRIBUTE_NORMAL}, // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := SetFileTime(FHandle, @CreationTime, @LastAccessTime, @LastWriteTime);
		if Result = False then
			IOError(FileName, GetLastError);
		if CloseHandle(FHandle) = False then
		begin
			IOError(FileName, GetLastError);
		end;
	end
	else
	begin
		IOError(FileName, GetLastError);
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
	MainLogAdd('Copy file ' + AddQuoteF(Source) + ' to ' + AddQuoteF(Dest), mlDebug);
	Windows.SetFileAttributes(PChar(Dest), FILE_ATTRIBUTE_ARCHIVE);
	LRetry:
	Result := Windows.CopyFile(PChar(Source), PChar(Dest), FailExist);
	if Result = False then
	begin
		ErrorCode := GetLastError;
		if ErrorRetry(ErrorCodeToStr(ErrorCode) + LineSep + 'During copying file ' + LineSep + Source + LineSep + 'to' + LineSep + Dest) then goto LRetry;
	end;
end;

function CopyFileToDir(Source, Dest: TFileName; const FailExist: BG): BG;
begin
	Result := CopyFile(Source, Dest + ExtractFileName(Source), FailExist);
end;

procedure WriteCRCFile(const FileName: string; const FileSize: U8; const CRC32: U8; const TargetFileName: TFileName);
var
	Data: string;
begin
	NumericBase := 16;
	try
		Data := 'crc32=' + NToS(CRC32, '00000000') + FileSep;
	finally
		NumericBase := 10;
	end;

	Data := 'filename=' + FileName + FileSep +
		'size=' + IntToStr(FileSize) + FileSep +
		Data;
	WriteStringToFile(TargetFileName, Data, False, fcAnsi);
end;

var
	{Table of CRCs of all 8-bit messages}
	crc_table: Array[0..255] of Cardinal;
	{Flag: has the table been computed? Initially false}
	crc_table_computed: Boolean;
(*
const crc_table: Array[0..255] of DWord = // CRC-32-IEEE 802.3
			($00000000, $77073096, $EE0E612C, $990951BA,
			$076DC419, $706AF48F, $E963A535, $9E6495A3,
			$0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
			$09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
			$1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
			$1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
			$136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
			$14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
			$3B6E20C8, $4C69105E, $D56041E4, $A2677172,
			$3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
			$35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
			$32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
			$26D930AC, $51DE003A, $C8D75180, $BFD06116,
			$21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
			$2802B89E, $5F058808, $C60CD9B2, $B10BE924,
			$2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
			$76DC4190, $01DB7106, $98D220BC, $EFD5102A,
			$71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
			$7807C9A2, $0F00F934, $9609A88E, $E10E9818,
			$7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
			$6B6B51F4, $1C6C6162, $856530D8, $F262004E,
			$6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
			$65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
			$62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
			$4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
			$4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
			$4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
			$44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
			$5005713C, $270241AA, $BE0B1010, $C90C2086,
			$5768B525, $206F85B3, $B966D409, $CE61E49F,
			$5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
			$59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
			$EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
			$EAD54739, $9DD277AF, $04DB2615, $73DC1683,
			$E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
			$E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
			$F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
			$F762575D, $806567CB, $196C3671, $6E6B06E7,
			$FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
			$F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
			$D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
			$D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
			$D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
			$DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
			$CB61B38C, $BC66831A, $256FD2A0, $5268E236,
			$CC0C7795, $BB0B4703, $220216B9, $5505262F,
			$C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
			$C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
			$9B64C2B0, $EC63F226, $756AA39C, $026D930A,
			$9C0906A9, $EB0E363F, $72076785, $05005713,
			$95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
			$92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
			$86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
			$81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
			$88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
			$8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
			$A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
			$A7672661, $D06016F7, $4969474D, $3E6E77DB,
			$AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
			$A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
			$BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
			$BAD03605, $CDD70693, $54DE5729, $23D967BF,
			$B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
			$B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
*)
{Make the table for a fast CRC.}
procedure make_crc_table;
var
	c: Cardinal;
	n, k: Integer;
begin

	{fill the crc table}
	for n := 0 to 255 do
	begin
		c := Cardinal(n);
		for k := 0 to 7 do
		begin
			if Boolean(c and 1) then
				c := $edb88320 xor (c shr 1) // CRC-32-IEEE 802.3
			else
				c := c shr 1;
		end;
		crc_table[n] := c;
	end;

	{The table has already being computated}
	crc_table_computed := true;
end;

{ Update a running CRC with the bytes buf[0..len-1]--the CRC
	should be initialized to all 1's, and the transmitted value
	is the 1's complement of the final running CRC (see the
	crc() routine below)).}
// CRC = Cyclic Redundency Check
function CalcCRC(crc: U4; buf: pByteArray; len: Integer): Cardinal;
var
	c: Cardinal;
	n: Integer;
begin
	c := crc;

	{Create the crc table in case it has not being computed yet}
	if not crc_table_computed then make_crc_table;

	{Update}
	{$r-}
	for n := 0 to len - 1 do
		c := crc_table[(c XOR buf^[n]) and $FF] XOR (c shr 8);

	{Returns}
	Result := c;
end;

procedure SplitFile(const Source: TFileName; const Dest: string; const MaxFileSize: U8 = GB; const CreateCRCFile: BG = False);
var
	SourceFile: TFile;
	DestFile: TFile;
	FileNamePrefix: string;
	FileIndex: SG;
	RemainSource: U8;
	RemainDest: U8;
	Count: SG;
	Buf: Pointer;
	SourceFileSize: U8;
	CRC: U4;
begin
	SourceFileSize := GetFileSizeU(Source);
(*	if MaxFileSize > SourceFileSize then
	begin
		CopyFileToDir(Source, Dest, True);
		Exit;
	end; *)
	Assert((MaxFileSize >= 1));

	FileNamePrefix := Dest + DelFileExt(ExtractFileName(Source));
	SourceFile := TFile.Create;
	GetMem(Buf, DefFileBuffer);
	try
		CRC := $ffffffff;
		if SourceFile.Open(Source, fmReadOnly) then
		begin
			RemainSource := SourceFileSize;
			FileIndex := 0;
			while RemainSource > 0 do
			begin
				DestFile := TFile.Create;
				RemainDest := Min(RemainSource, MaxFileSize);
				try
					if DestFile.Open(FileNamePrefix + '.' + NToS(FileIndex + 1, '000'), fmRewrite) then
					begin
						while RemainDest > 0 do
						begin
							Count := Min(DefFileBuffer, RemainDest);
							SourceFile.BlockRead(Buf^, Count);
							CRC := CalcCRC(CRC, Buf, Count);
							DestFile.BlockWrite(Buf^, Count);
							Dec(RemainDest, Count);
						end;
					end;
					DestFile.Close;
				finally
					DestFile.Free;
				end;
				Dec(RemainSource, Min(RemainSource, MaxFileSize));
				Inc(FileIndex);
			end;
			SourceFile.Close;
		end;
		CRC := CRC xor $ffffffff;
		if CreateCRCFile then
			WriteCRCFile(ExtractFileName(Source), SourceFileSize, CRC, FileNamePrefix + '.crc');
	finally
		SourceFile.Free;
		FreeMem(Buf);
	end;
end;

function CopyDamagedFile(Source, Dest: TFileName): BG;
const
	MaxCount = 512;
var
	Total: U8;
	Count: UG;
	FS, FD: TFile;
	Buf: Pointer;
begin
	Result := True;
	Buf := nil;
	FS := nil;
	FD := nil;
	try
		GetMem(Buf, MaxCount);
		FS := TFile.Create;
		FD := TFile.Create;
		FD.Charset := fcAnsi;
		if FS.Open(Source, fmReadOnly) then
		begin
			if FD.Open(Dest, fmRewrite) then
			begin
				Total := FS.FileSize;
				while Total > 0 do
				begin
					Count := Min(Total, MaxCount);
					FillChar(Buf^, Count, 0);
					if not FS.BlockRead(Buf^, Count) then
						Result := False;
					FD.BlockWrite(Buf^, Count);
					Dec(Total, Count);
				end;
				FD.Truncate;
				FD.Close;
			end;
			FS.Close;
		end;
	finally
		FreeAndNil(FS);
		FreeAndNil(FD);
		FreeMem(Buf);
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
	while i <= 9999 do
	begin
		if i > 0 then
		begin
			FileOrDir := DirS + '_' + NToS(i, '0000') + DirE;
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
var
	FileName, Name: string;
begin
	FileName := ExtractFileName(FileOrDir);
	Name := DelFileExt(FileName);
	FileOrDir := DelFileName(FileOrDir);
	if Name <> '' then
		FileOrDir := FileOrDir + Name + ' ';
	FileOrDir := FileOrDir + ReplaceF(DateTimeToS(Now, 0, ofIO), ':', '_') + ExtractFileExt(FileName);
	Result := NewFileOrDir(FileOrDir);
end;

function CopyFileDateTime(const Source, Dest: string): BG;
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	Result := GetFileDateTime(Source, CreationTime, LastAccessTime, LastWriteTime);
	if Result then
	begin
		Result := SetFileDateTime(Dest, CreationTime, LastAccessTime, LastWriteTime);
	end;
end;

function CopyDirOnly(const Source, Dest: string): BG;
var
//	hDir: THandle;
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	Result := True;
	CreateDirectoryEx(PChar(Source), PChar(Dest), nil);
//	if Result = then
	begin
		if GetFileDateTime(Source, CreationTime, LastAccessTime, LastWriteTime) then
		begin
			SetFileDateTime(Dest, CreationTime, LastAccessTime, LastWriteTime);
		end;
	end;
end;

function CopyDir(const Source, Dest: string): BG;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CopyDirOnly(Source, Dest);

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

procedure BackupFile(const FileName: TFileName);
var FileNameD: TFileName;
begin
	if FileExists(FileName) = False then Exit;
	FileNameD := TempDir;
	if DirectoryExists(FileNameD) = False then
		CreateDirEx(FileNameD);
	FileNameD := FileNameD + ExtractFileName(FileName);
	if NewFileOrDirEx(string(FileNameD)) then
		uFiles.CopyFile(FileName, FileNameD, True);
end;

function DeleteFileEx(const FileName: TFileName): BG;
begin
	MainLogAdd('Delete file ' + AddQuoteF(FileName), mlDebug);
	Windows.SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE);
	Result := DeleteFile(PChar(FileName));
	if Result = False then
		IOError(FileName, GetLastError);
end;

function RemoveDirEx(const DirName: string): BG;
begin
	MainLogAdd('Remove directory ' + AddQuoteF(DirName), mlDebug);
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

procedure FileLinesAndSize(const FileName: TFileName; out Size, Lines: U8);
var
	F: TFile;
	Line: string;
begin
	Size := 0;
	Lines := 0;
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			Size := F.FileSize;
			while not F.EOF do
			begin
				F.Readln(Line);
				Inc(Lines);
			end;
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

procedure CodeLinesAndSize(const Dir: string; out Size, Lines: U8);
var
	FileCount: SG;
	Files: TFileNames;
	i: SG;
	FileLines, FileSize: U8;
begin
	Size := 0;
	Lines := 0;
	FileCount := 0;
	ReadDir(Files, FileCount, Dir, ['pas', 'dpr', 'inc'{, 'dfm'}], True, True, True, False);
	for i := 0 to FileCount - 1 do
	begin
		FileLinesAndSize(Dir + Files[i], FileSize, FileLines);
		Inc(Size, FileSize);
		Inc(Lines, FileLines);
	end;
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
		F.Charset := fcAnsi;
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
		if F.Open(FileName, fmRewrite, FILE_FLAG_NO_PREFIX or FILE_FLAG_SEQUENTIAL_SCAN) then
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

function ReadStringFromFile(const FileName: TFileName; out Data: AnsiString): BG; overload;
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
			begin
				F.BlockRead(Data[1], F.FileSize);
				if F.Charset = fcUTF8 then
					Data := UTF8ToAnsi(Data)
			end;
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadStringFromFile(const FileName: TFileName; out Data: UnicodeString): BG; overload;
var
	F: TFile;
	Data2: AnsiString;
begin
	Result := False;
	Data := '';
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			SetLength(Data2, F.FileSize);
			if Length(Data2) >= 1 then
			begin
				F.BlockRead(Data2[1], F.FileSize);
				if F.Charset = fcUTF8 then
					Data := ConvertUTF8ToUnicode(Data2)
				else
					Data := UnicodeString(Data2);
			end;
			SetLength(Data2, 0);
			F.Close;
			Result := True;
		end;
	finally
		F.Free;
	end;
end;

function ReadStringFromFile(const FileName: TFileName): UnicodeString; overload;
begin
	ReadStringFromFile(FileName, Result);
end;

function ReadStringFromFile(const FileName: TFileName; const Limit: SG): string;
var
	F: TFile;
	Data2: AnsiString;
begin
	Result := '';
	F := TFile.Create;
	try
		if F.Open(FileName, fmReadOnly) then
		begin
			SetLength(Data2, Min(Limit, F.FileSize));
			if Length(Data2) >= 1 then
			begin
				F.BlockRead(Data2[1], Min(Limit, F.FileSize));
				if F.Charset = fcUTF8 then
					Result := ConvertUTF8ToUnicode(Data2)
				else
					Result := UnicodeString(Data2);
			end;
			SetLength(Data2, 0);
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

function SameDataInFile(const FileName: TFileName; const Line: AnsiString): BG;
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

procedure ConvertFileCharset(const Source: AnsiString; out Dest: AnsiString; const FileCharset: TFileCharset); overload;
begin
	case FileCharset of
	fcAnsi: Dest := Source;
	fcUTF8: Dest := AnsiToUtf8(Source);
//	fcUTF16LE: Dest := UnicodeString(Source);
	else
		Warning('Unsupported charset.');
	end;
end;

procedure ConvertFileCharset(const Source: UnicodeString; out Dest: AnsiString; const FileCharset: TFileCharset); overload;
begin
	case FileCharset of
	fcAnsi: Dest := AnsiString(Source);
	fcUTF8: Dest := ConvertUnicodeToUtf8(Source);
	else
		Warning('Unsupported charset.');
	end;
end;

function WriteStringToFile(const FileName: TFileName; const Data: AnsiString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
var
	F: TFile;
	FileMode: TFileMode;
	DataA: AnsiString;
begin
	ConvertFileCharset(Data, DataA, FileCharset);

	if (Append = False) and SameDataInFile(FileName, DataA) then
		Result := True
	else
	begin
		Result := False;
		if Append then
			FileMode := fmAppend
		else
			FileMode := fmRewrite;
		F := TFile.Create;
		F.Charset := FileCharset;
		try
			if F.Open(FileName, FileMode, Flags) then
			begin
				F.WriteNoConversion(DataA);
				F.Close;
				Result := True;
			end;
		finally
			F.Free;
		end;
	end;
end;

function WriteStringToFile(const FileName: TFileName; const Data: UnicodeString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
var
	F: TFile;
	FileMode: TFileMode;
	DataA: AnsiString;
begin
	ConvertFileCharset(Data, DataA, FileCharset);
	if (Append = False) and SameDataInFile(FileName, DataA) then
		Result := True
	else
	begin
		Result := False;
		if Append then
			FileMode := fmAppend
		else
			FileMode := fmRewrite;
		F := TFile.Create;
		F.Charset := FileCharset;
		try
			if F.Open(FileName, FileMode, Flags) then
			begin
				F.WriteNoConversion(DataA);
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

function WriteStringsToFile(const FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; const Append: BG; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;
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
		if F.Open(FileName, FileMode, Flags) then
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
		while CharInSet(Str[Index], ['\', '/']) do
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
		if ParentDir(string(Result)) = False then Break;
	end;
end;

{$ifndef Console}
function ExecuteDialog(const Dialog: TOpenDialog; var FileName: TFileName): BG;
begin
	Dialog.FileName := ExtractFileName(FileName);
	Dialog.InitialDir := RepairDirectory(ExtractFilePath(FileName));
	Result := Dialog.Execute;
	if Result then
		FileName := ShortDir(Dialog.FileName);
end;
{$endif}

function CompareFiles(const File1, File2: TFile): BG;
label LClose;
var
	TotalBytes: U8;
	ReadBytes, FileBufferSize: UG;
	Buf1, Buf2: Pointer;
begin
	Result := False;
	if File1.FileSize = File2.FileSize then
	begin
		if File1.FileName[1] = File2.FileName[1] then
			FileBufferSize := 2 * MB // Same drive
		else
			FileBufferSize := DefFileBuffer;

		GetMem(Buf1, FileBufferSize);
		GetMem(Buf2, FileBufferSize);
		try
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
				if SameData(Buf1, Buf2, ReadBytes) = False then
					goto LClose;
			end;
			Result := True;
			LClose:
		finally
			FreeMem(Buf1);
			FreeMem(Buf2);
		end;
	end;
end;

function SameFiles(const FileName1, FileName2: TFileName): BG;
var
	File1, File2: TFile;
begin
	Result := False;
	File1 := TFile.Create;
	try
		if File1.Open(FileName1, fmReadOnly) then
		begin
			File2 := TFile.Create;
			try
				if File2.Open(FileName2, fmReadOnly) then
				begin
					Result := CompareFiles(File1, File2);
					File2.Close;
				end;
				File1.Close;
			finally
				File2.Free;
			end;
		end;
	finally
		File1.Free;
	end;
end;

function TempFileName(const FileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(FileName) + '~' + ExtractFileName(FileName);
end;

procedure ReplaceIfChanged(const OrigFileName, TempFileName: TFileName);
begin
	if FileExists(OrigFileName) and SameFiles(OrigFileName, TempFileName) then
		DeleteFileEx(TempFileName)
	else
	begin
		CopyFile(TempFileName, OrigFileName, False);
		DeleteFileEx(TempFileName);
	end;
end;

procedure ReplaceIfChanged(const TempFileName: TFileName);
var
	OrigFileName: TFileName;
begin
	OrigFileName := ExtractFileName(TempFileName);
	if Length(OrigFileName) <= 0 then Exit;
	if OrigFileName[1] <> '~' then Exit;
	Delete(OrigFileName, 1, 1);
	OrigFileName := ExtractFilePath(TempFileName) + OrigFileName;

	ReplaceIfChanged(OrigFileName, TempFileName);
end;

function FileExistsEx(const FileName: TFileName): BG;
begin
	Result := FileExists(ExpandDir(FileName));
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
	Result := DirectoryExists(ExpandDir(DirName));
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

function LastLineFromFile(const FileName: TFileName): AnsiString;
var
	i: SG;
	F: TFile;
	C: AnsiChar;
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
				if not CharInSet(C, [CharCR, CharLF]) then Break;
				Dec(i);
			end;

			while i >= 0 do
			begin
				F.Seek(i);
				F.BlockRead(C, SizeOf(C));
				if CharInSet(C, [CharCR, CharLF]) then Break;
				Result := c + Result;
				Dec(i);
			end;
			F.Close;
		end;
	finally
		F.Free;
	end;
end;

function SameFileName(const FileName1, FileName2: TFileName): BG;
begin
	Result := ShortToLongPath(FileName1) = ShortToLongPath(FileName2);
end;

function DialogStrWithoutAll(const Ext, Des: array of string): string;
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
		s1 := DelLastChar(s1);
		s1[Length(s1)] := ')';
		s2 := DelLastChar(s2);
		Result := 'Any (' + s1 + '|' + s2 + '|';
	end
	else
		Result := '';
	for i := 0 to Length(Ext) - 1 do
	begin
		Result := Result + Des[i] + ' (*.' + Ext[i] + ')|*.' + Ext[i] + '|';
	end;
end;

function DialogStr(const Ext, Des: array of string): string;
begin
	Result := DialogStrWithoutAll(Ext, Des);
	Result := Result + AllFiles;
end;

function GetFileNameFilter(const Description: string; const Extensions: array of string): string;
var
	i: SG;
	ExtString: string;
	s: string;
begin
	Result := Description + ' (';
	ExtString := '';
	for i := 0 to Length(Extensions) - 1 do
	begin
		if Pos('.', Extensions[i]) = 0 then
		begin
			s := '*.' + Extensions[i];
		end
		else
		begin
			s := Extensions[i];
		end;
		ExtString := ExtString + s;
		if i < Length(Extensions) - 1 then
		begin
			ExtString := ExtString + ';';
		end;
	end;
	Result := Result + ExtString + ')|' + ExtString;
end;

// @return 'All Files (*.*)|*.*'
function AllFiles: string;
begin
	Result := GetFileNameFilter('All Files', ['*']);
end;

function GetTextFilter: string;
begin
	Result := GetFileNameFilter('Text file', ['txt']);
end;

function GetExecutableFilter: string;
begin
	Result := GetFileNameFilter('Executable files', ['exe', 'com', 'bat', 'cmd']);
end;

// @return 'Text file (*.txt)|*.txt|'
function AllText: string;
begin
	Result := GetTextFilter + '|' + AllFiles;
end;

function AllSounds: string;
begin
	Result := GetFileNameFilter('Sound Wave', ['wav']);
end;

initialization
	InitPaths;
	EnumToStr(TypeInfo(TFileMode), FileModeStr);
end.
