unit uFiles;

interface

uses
	uTypes, uStrings, uFile, uBackup,
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
	UserProfileDir,
	AppDataDir, // User specific configuration files (Ini, Autosaves, Logs) (Read and Write)
	LocalAppDataDir,
  CompanyAppDataDir,
  CompanyLocalAppDataDir,
//	HomeDir, // User documnets (Read and Write)
	CommonAppDataDir, // Application Data
  CommonLocalAppDataDir: string; // Local Application Data
	ExeFileName: TFileName;
  ModuleFileName: TFileName;
  ExeParameters: string;

type
	TFileNames = array of TFileName;

function GetFileDateTime(const FileName: TFileName; out CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
function SetFileDateTime(const FileName: TFileName; const CreationTime, LastAccessTime, LastWriteTime: TFileTime): BG;
function ShortDir(const Dir: string): string;

function ExpandFile(const FileName: TFileName): string;
function ExpandFileCmd(const FileName: TFileName): string;
function ExpandDir(const Dir: string): string;
function ExpandDirCmd(const Dir: string): string;
function DelFileExt(const FName: string): string;
function DelFileName(const FName: string): string;
function AddAfterName(const FName: string; const Text: string): string;
function ParentDir(var Dir: string; Level: SG = 1): BG;
function ParentDirF(const Dir: string; const Level: SG = 1): string;
function LegalFileName(const FileName: string): string;
function LegalPath(const Path: string): string;
procedure ReadDir(var FileNames: TFileNames; var FileCount: SG; const Path: string; const Extensions: array of string; const Files, Dirs, SubDirs, Sort: BG; const FullPath: BG = False);
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
function RenameFileIfExistsEx(const Source, Dest: TFileName): BG;
function CopyFile(const Source, Dest: TFileName; const FailExist: BG): BG;
function CopyFileToDir(Source, Dest: TFileName; const FailExist: BG): BG;
function CopyDamagedFile(Source, Dest: TFileName): BG;
function CreateDirEx(const Dir: string): BG;
function CreateDirsEx(const Dir: string): BG;
function CreateLockedDir(const ADirectoryName: string): THandle;
function NewFileOrDir(var FileOrDir: string): BG;
function NewFileOrDirEx(var FileOrDir: string): BG;
function CopyFileDateTime(const Source, Dest: string): BG;
function CopyDirOnly(const Source, Dest: string): BG;
function CopyDir(const Source, Dest: string; const Attribute: SG = faAnyFile): BG;

function DeleteFileEx(const FileName: TFileName): BG;
function RemoveDirEx(const DirName: string): BG;
function RemoveDirsEx(DirName: string; DeleteSelf: BG = False): BG;

procedure FileLinesAndSize(const FileName: TFileName; out Size, Lines: U8);
procedure CodeLinesAndSize(const Dir: string; out Size, Lines, Files: U8);

function ReadBufferFromFile(const FileName: TFileName; out Buf; out Count: SG): BG;
function WriteBufferToFile(const FileName: TFileName; const Buf; const Count: UG): BG;

function ReadBlockFromFile(const FileName: TFileName; Buf: Pointer; const Count: UG): BG;
function WriteBlockToFile(const FileName: TFileName; Buf: Pointer; const Count: UG): BG;

function ReadStringsFromFile(const FileName: TFileName; var Lines: TArrayOfString; var LineCount: SG): BG;
function WriteStringsToFile(const FileName: TFileName; var Lines: TArrayOfString; OpeningNameCount: SG; const Append: BG; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN): BG;

function ReadStringFromFile(const FileName: TFileName; out Data: AnsiString): BG; overload;
function ReadStringFromFile(const FileName: TFileName; out Data: UnicodeString): BG; overload;
function ReadStringFromFile(const FileName: TFileName): UnicodeString; overload;
function ReadStringFromFile(const FileName: TFileName; const Limit: U8): string; overload;
function ReadStringFromFileEx(const FileName: TFileName; out Data: AnsiString): TFileCharset;

function WriteStringToFile(const FileName: TFileName; const Data: AnsiString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN; const Protection: BG = True; const BackupFolder: TBackupFolder = bfNone): BG; overload;
function WriteStringToFile(const FileName: TFileName; const Data: UnicodeString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN; const Protection: BG = True; const BackupFolder: TBackupFolder = bfNone): BG; overload;

function ShortToLongFileName(const ShortName: string): string;
function ShortToLongPath(ShortName: string): string;
{function LongToShortFileName(const LongName: string): string;
function LongToShortPath(const LongName: string): string;}

function RepairDirectory(const Dir: TFileName): TFileName;
function SameFiles(const FileName1, FileName2: TFileName): BG;
function SameFilesNoPrefix(const FileName1, FileName2: TFileName): BG;
function TempFileName(const FileName: TFileName): TFileName;
procedure ReplaceIfChanged(const OrigFileName, TempFileName: TFileName); overload;
procedure ReplaceIfChanged(const TempFileName: TFileName); overload;
function GetModuleFileNameFunc(const AHandle: THandle): string;
procedure InitPaths;

function DirectoryExistsEx(const DirName: TFileName): BG;
function FileExistsEx(const FileName: TFileName): BG;
procedure TestFileReadable(const AFileName: string);
function IsFileWritable(const AFileName: string): Boolean;
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

function SplitStr(Source: string; const MaxStrings: SG; out Remain: string): TArrayOfString;
function FindFileInSubDir(const AFileName: TFileName; const StartInParentDir: BG): TFileName;
function FindFilesInSubDir(const AFileName: TFileName; const StartInParentDir: BG): TFileNames;

implementation

uses
	Math,
  uEIOException,
  uStartupEnvironment,
	uChar, uMsg, uProjectInfo, uSorts, uCharset,
	uOutputFormat, uMath, uLog;

function SplitStr(Source: string; const MaxStrings: SG; out Remain: string): TArrayOfString;
var
	i: SG;
	EndIndex: SG;
	ResultCount: SG;
begin
	SetLength(Result, MaxStrings);
	if MaxStrings <= 0 then Exit;

	ResultCount := 0;
	i := 1;
	while i <= Length(Source) do
	begin
		if Source[i] = CharSpace then
		begin
			Inc(i);
			Continue;
		end;
		if Source[i] = '"' then
		begin
			Inc(i);
			EndIndex := i;
			while True do
			begin
				EndIndex := PosEx('"', Source, EndIndex);
				if CharAt(Source, EndIndex - 1) = '\' then
				begin
					Delete(Source, EndIndex - 1, 1);
				end
				else
					Break;
			end;
		end
		else
		begin
			EndIndex := PosEx(CharSpace, Source, i + 1);
		end;
		if EndIndex = 0 then EndIndex := MaxInt - 1;

		Result[ResultCount] := Copy(Source, i, EndIndex - i);
		Inc(ResultCount);
		if ResultCount >= MaxStrings then
    begin
      Remain := Copy(Source, EndIndex + 1, MaxInt);
			Exit;
    end;

		i := EndIndex + 1;
	end;
  SetLength(Result, ResultCount);
end;

function GetModuleFileNameFunc(const AHandle: THandle): string;
var
  NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := GetModuleFileName(AHandle, PChar(Result), MAX_PATH);
	SetLength(Result, NewLength);
end;

procedure InitPaths;
var
	NewLength: SG;
	i: SG;
	All: TArrayOfString;
  CommandLine: string;
  Suffix, CompanySuffix: string;
begin
  All := nil;
	if ExeFileName <> '' then Exit;

	GetDir(0, StartDir);
	CorrectDir(StartDir);

	// Remove Parameters
  CommandLine := GetCommandLine;
	All := SplitStr(CommandLine, 1, ExeParameters);
  ExeFileName := All[0];
  ModuleFileName := GetModuleFileNameFunc(HInstance);

	WorkDir := '';
  // Split ExeFileName to WorkDir and InternalName
  for i := Length(ModuleFileName) downto 0 do
  begin
    if i = 0 then
    begin
      Break;
    end;
    if (ModuleFileName[i] = PathDelim) then
    begin
      WorkDir := Copy(ModuleFileName, 1, i);
      Break;
    end;
  end;
	if WorkDir = '' then
    WorkDir := StartDir;
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
	if ProgramFilesDir = '' then
    ProgramFilesDir := 'C' + DriveDelim + PathDelim + 'Program Files' + PathDelim;
	CorrectDir(ProgramFilesDir);

	CommonAppDataDir := GetEnvironmentVariable( 'APPDATA');
	if CommonAppDataDir = '' then
    CommonAppDataDir := WinDir + 'Application Data' + PathDelim;
	CorrectDir(CommonAppDataDir);

  CompanySuffix := '';
  if GetProjectInfo(piCompanyName) <> '' then
    CompanySuffix := CompanySuffix + GetProjectInfo(piCompanyName) + PathDelim;
  CompanyAppDataDir := CommonAppDataDir + CompanySuffix;
  Suffix := CompanySuffix + GetProjectInfo(piInternalName) + PathDelim;

	AppDataDir := CommonAppDataDir + Suffix;

	UserProfileDir := StartupEnvironment.FindValue('UserProfile');
	CorrectDir(UserProfileDir);
	CommonLocalAppDataDir := StartupEnvironment.FindValue('localappdata');
	if CommonLocalAppDataDir = '' then
	begin
		CommonLocalAppDataDir := UserProfileDir;
		CorrectDir(CommonLocalAppDataDir);
		CommonLocalAppDataDir := CommonLocalAppDataDir + 'AppData\Local\';
		if not DirectoryExists(CommonLocalAppDataDir) then
			CommonLocalAppDataDir := CommonLocalAppDataDir + 'Local Settings\Application Data\' // Used for Windows XP w/o Service Pack
		else
			CommonLocalAppDataDir := CommonLocalAppDataDir + 'AppData\Local\';
	end
  else
		CorrectDir(CommonLocalAppDataDir);
	CompanyLocalAppDataDir := CommonLocalAppDataDir + CompanySuffix;
	LocalAppDataDir := CommonLocalAppDataDir + Suffix;
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
	Result := StartupEnvironment.InsertVariablesFromStart(RemoveWorkDir(Dir));
end;

function ExpandCustomDir(Dir: string; const WorkDir: string): string;
begin
	if Length(Dir) = 0 then
		Result := WorkDir
{	else if StartStr('http://', Dir) then
		Result := Dir
	else if (Pos('://', Dir) <> 0) then
		Result := Dir}
	else
	begin // file://
		Dir := StartupEnvironment.RemoveVariables(Dir);

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
			Result := ExpandFileName(Dir);
		end
		else
			// Relative path
			Result := ExpandFileName(WorkDir + Dir);
	end;
end;

function ExpandFile(const FileName: TFileName): string;
begin
	Result := ExpandCustomDir(FileName, WorkDir);
end;

function ExpandFileCmd(const FileName: TFileName): string;
begin
	Result := ExpandCustomDir(FileName, StartDir);
end;

function ExpandDir(const Dir: string): string;
begin
	Result := ExpandCustomDir(Dir, WorkDir);
end;

function ExpandDirCmd(const Dir: string): string;
begin
	Result := CorrectDirF(ExpandCustomDir(Dir, StartDir));
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

function ParentDir(var Dir: string; Level: SG = 1): BG;
var i: Integer;
begin
	Result := False;
	for i := Length(Dir) - 1 downto 3 do
	begin
		if Dir[i] = PathDelim then
		begin
      Dec(Level);
      if Level = 0 then
      begin
  			SetLength(Dir, i);
  			Result := True;
  			Exit;
      end;
		end;
	end;
//  raise Exception.Create('Could not build parent dir for ''' + Dir + '');
end;

function ParentDirF(const Dir: string; const Level: SG = 1): string;
begin
	Result := Dir;
	ParentDir(Result, Level);
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

procedure ReadSubDir(var FileNames: TFileNames; var FilesCount: SG; const Path: string; const SubPath: string; const Extensions: array of string; const Files, Dirs, SubDirs, FullPath: BG);
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
        if FullPath then
          FileNames[FilesCount] := Path + FileNames[FilesCount];
				Inc(FilesCount);
			end;
		end;

		if IsDir and SubDirs then
		begin
			ReadSubDir(FileNames, FilesCount, Path, SubPath + SearchRec.Name, Extensions, Files, Dirs, SubDirs, FullPath);
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

function Compare(const Index0, Index1: SG): TCompareResult;
begin
	Result := CompareStringLogical(GList^[Index0].Name, GList^[Index1].Name);
{	Result := TCompareResult(CompareString(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
		PChar(GList^[Index0].Name), Length(GList^[Index0].Name),
		PChar(GList^[Index1].Name), Length(GList^[Index1].Name)) - 2);}
end;

procedure ReadSubDirSorted(var FileNames: TFileNames; var FilesCount: SG; const Path: string; const SubPath: string; const Extensions: array of string; const Files, Dirs, SubDirs, FullPath: BG);
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
	FillOrderUG(AIndex[0], ListCount);
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
      if FullPath then
        FileNames[FilesCount] := Path + FileNames[FilesCount];
			Inc(FilesCount);
		end;
		if SubDirs and IsDir then
		begin
			ReadSubDir(FileNames, FilesCount, Path, SubPath + List[j].Name + PathDelim, Extensions, Files, Dirs, SubDirs, FullPath);
		end;
	end;
end;


procedure ReadDir(var FileNames: TFileNames; var FileCount: SG; const Path: string; const Extensions: array of string; const Files, Dirs, SubDirs, Sort: BG; const FullPath: BG = False);
var
	i: SG;
(*	Offset: Integer;
	MaxLimit: Integer;
	Switch: Integer;
	FileName: TFileName; *)
begin
	if IsDebug then
    for i := 0 to Length(Extensions) - 1 do
    begin
      Assert(Length(Extensions[i]) > 0);
      Assert(Extensions[i, 1] <> '.');
    end;
{						if (Extensions = '') or (Extension = '*') or (Extension = '*.*') or
	if Length(Extension) > 1 then
		if (Extension <> '*') and (Extension <> '*.*') then
			if Extension[1] <> '.' then Extension := '.' + Extension;}

	if not Sort then
		ReadSubDir(FileNames, FileCount, CorrectDirF(ExpandDir(Path)), '', Extensions, Files, Dirs, SubDirs, FullPath)
	else
		ReadSubDirSorted(FileNames, FileCount, CorrectDirF(ExpandDir(Path)), '', Extensions, Files, Dirs, SubDirs, FullPath);

	if LogDebug then
    MainLogAdd(NToS(FileCount, ofIO) + ' files found in folder ' + Path + '.', mlDebug);
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
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
  FHandle: THandle;
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
    Result := GetFileTime(FHandle, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
    if Result then
    begin
      Result := SetFileTime(FHandle, @ACreationTime, @ALastAccessTime, @LastWriteTime);
      if Result = False then
        IOError(FileName, GetLastError);
    end
    else
    begin
      IOError(FileName, GetLastError);
    end;
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

function RenameFileIfExistsEx(const Source, Dest: TFileName): BG;
begin
  if FileExists(Source) and (Source <> Dest) then
  begin
    Result := RenameFileEx(Source, Dest);
  end
  else
    Result := False;
end;

function CopyFile(const Source, Dest: TFileName; const FailExist: BG): BG;
label LRetry;
var ErrorCode: U4;
begin
	if LogDebug then
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

function CreateLockedDir(const ADirectoryName: string): THandle;
var
  DirectoryName: string;
begin
  DirectoryName := ADirectoryName;
  SetLength(DirectoryName, Length(DirectoryName) - 1);

	if ADirectoryName = '' then
	begin
		Result := INVALID_HANDLE_VALUE;
		Exit;
	end;

  CreateDirsEx(ADirectoryName);
	Result := CreateFile(PChar(DirectoryName), // pointer to name of the file
		GENERIC_ALL, // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE, // share mode
		nil, // pointer to security attributes
		OPEN_EXISTING, // creation disposition
    FILE_FLAG_BACKUP_SEMANTICS,
//		FILE_ATTRIBUTE_NORMAL or FILE_ATTRIBUTE_DIRECTORY or FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_DELETE_ON_CLOSE, // file attributes
		0 // handle to file with attributes to copy
		);
	if Result = INVALID_HANDLE_VALUE then
	begin
		IOError(DirectoryName, GetLastError);
	end;
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

function CopyDir(const Source, Dest: string; const Attribute: SG = faAnyFile): BG;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CopyDirOnly(Source, Dest);

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(Source + '*.*', Attribute, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
				CopyDir(Source + SearchRec.Name + PathDelim, Dest + SearchRec.Name + PathDelim, Attribute);
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
	if LogDebug then
    MainLogAdd('Delete file ' + AddQuoteF(FileName), mlDebug);
	Windows.SetFileAttributes(PChar(FileName), FILE_ATTRIBUTE_ARCHIVE);
	Result := DeleteFile(PChar(FileName));
	if Result = False then
		raise EIOException.Create(FileName, GetLastError);
end;

function RemoveDirEx(const DirName: string): BG;
begin
	if LogDebug then
    MainLogAdd('Remove directory ' + AddQuoteF(DirName), mlDebug);
	Result := RemoveDirectory(PChar(DirName));
	if Result = False then
		raise EIOException.Create(DirName, GetLastError);
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

procedure CodeLinesAndSize(const Dir: string; out Size, Lines, Files: U8);
var
	FileCount: SG;
	FileNames: TFileNames;
	i: SG;
	FileLines, FileSize: U8;
begin
	Size := 0;
	Lines := 0;
	FileCount := 0;
	ReadDir(FileNames, FileCount, Dir, ['pas', 'dpr', 'inc'{, 'dfm'}], True, True, True, False);
	Files := FileCount;
	for i := 0 to FileCount - 1 do
	begin
		FileLinesAndSize(Dir + FileNames[i], FileSize, FileLines);
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

function WriteBufferToFile(const FileName: TFileName; const Buf; const Count: UG): BG;
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

function ReadBlockFromFile(const FileName: TFileName; Buf: Pointer; const Count: UG): BG;
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

function WriteBlockToFile(const FileName: TFileName; Buf: Pointer; const Count: UG): BG;
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

function ReadStringFromFileEx(const FileName: TFileName; out Data: AnsiString): TFileCharset;
var
	F: TFile;
begin
	Result := fcUTF8;
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
					Data := AnsiString(UTF8ToAnsi(Data));
			end;
			Result := F.Charset;
			F.Close;
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
					Data := AnsiString(UTF8ToAnsi(Data));
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

function ReadStringFromFile(const FileName: TFileName; const Limit: U8): string;
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
	fcUTF8: Dest := AnsiToUtf8(string(Source));
	else
		Warning('Unsupported charset.');
	end;
end;

procedure ConvertFileCharset(const Source: UnicodeString; out Dest: AnsiString; const FileCharset: TFileCharset); overload;
var
  Size: SG;
  u: UnicodeString;
  i: SG;
begin
	case FileCharset of
	fcAnsi: Dest := AnsiString(Source);
	fcUTF8: Dest := ConvertUnicodeToUtf8(Source);
  fcUTF16BE:
  begin
    Size := Length(Source) * SizeOf(WideChar);
    u := Source;
    for i := 1 to Length(u) do
    begin
      u[i] := WideChar(Swap(Ord(u[i])));
    end;
		SetLength(Dest, Size);
		Move(u[1], Dest[1], Size);
  end;
  fcUTF16LE:
  begin
    Size := Length(Source) * SizeOf(WideChar);
		SetLength(Dest, Size);
		Move(Source[1], Dest[1], Size);
  end;
	else
		Warning('Unsupported charset.');
	end;
end;

function WriteStringToFile(const FileName: TFileName; const Data: AnsiString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN; const Protection: BG = True; const BackupFolder: TBackupFolder = bfNone): BG;
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
		try
  		F.Charset := FileCharset;
      F.Protection := Protection;
      F.SkipSameData := False;
      F.BackupFolder := BackupFolder;
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

function WriteStringToFile(const FileName: TFileName; const Data: UnicodeString; const Append: BG; const FileCharset: TFileCharset = DefaultFileCharset; const Flags: U4 = FILE_FLAG_SEQUENTIAL_SCAN; const Protection: BG = True; const BackupFolder: TBackupFolder = bfNone): BG;
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
		try
  		F.Charset := FileCharset;
      F.Protection := Protection;
      F.SkipSameData := False;
      F.BackupFolder := BackupFolder;
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

function SameFilesNoPrefix(const FileName1, FileName2: TFileName): BG;
var
	File1, File2: TFile;
begin
	Result := False;
	File1 := TFile.Create;
	try
		if File1.Open(FileName1, fmReadOnly, FILE_FLAG_NO_PREFIX) then
		begin
			File2 := TFile.Create;
			try
				if File2.Open(FileName2, fmReadOnly, FILE_FLAG_NO_PREFIX) then
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

procedure TestFileReadable(const AFileName: string);
var
  H: THandle;
begin
  H := CreateFile(PChar(AFileName), GENERIC_READ, 0, nil, OPEN_EXISTING, 0, 0);
  if H <> INVALID_HANDLE_VALUE then
  begin
    CloseHandle(H);
  end
  else
  begin
    raise Exception.Create(ReplaceParam(ErrorCodeToStr(GetLastError) + LineSep + '%1', [AFileName]));
  end;
end;

function IsFileWritable(const AFileName: string): Boolean;
var
  H: THandle;
begin
  H := CreateFile(PChar(AFileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, 0, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(H);
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

function FindFileInSubDir(const AFileName: TFileName; const StartInParentDir: BG): TFileName;
var
  Dir: string;
  FileNameOnly: string;
  FileName: string;
begin
  Result := '';
  Dir := ExtractFilePath(AFileName);
  if StartInParentDir then
    Dir := ParentDirF(Dir);
  FileNameOnly := ExtractFileName(AFileName);
  while True do
  begin
    FileName := Dir + FileNameOnly;
    if FileExistsEx(FileName) then
    begin
      Result := FileName;
      Exit;
    end;
    if (Length(Dir) <= 3) or (PosEx(PathDelim, Dir, 4) = 0) then
      Break;
    Dir := ParentDirF(Dir);
  end;
end;

function FindFilesInSubDir(const AFileName: TFileName; const StartInParentDir: BG): TFileNames;
var
  Dir: string;
  Dir2: string;
  FileNameOnly: string;
  FileName: string;
begin
  SetLength(Result, 0);
  Dir := ExtractFilePath(AFileName);
  if StartInParentDir then
    Dir := ParentDirF(Dir);
  FileNameOnly := ExtractFileName(AFileName);
  while True do
  begin
    FileName := Dir + FileNameOnly;
    if FileExistsEx(FileName) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := FileName;
    end;
    if (Length(Dir) <= 3) or (PosEx(PathDelim, Dir, 4) = 0) then
      Break;
    Dir2 := ParentDirF(Dir);
    if Dir = Dir2 then Break;
    Dir := Dir2;
  end;
end;

initialization
{$IFNDEF NoInitialization}
	InitPaths;
	EnumToStr(TypeInfo(TFileMode), FileModeStr);
{$ENDIF NoInitialization}
end.
