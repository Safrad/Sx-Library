unit uFiles;

{$ZEROBASEDSTRINGS OFF}

interface

uses
{$ifdef MSWINDOWS}
	Winapi.Windows,
{$else}
  System.IOUtils,
{$endif}
  SysUtils,

  uTypes,
  uFileCharset;

type
{$ifdef MSWINDOWS}
  TFileTime = Winapi.Windows.TFileTime;
{$else}
  TFileTime = TDateTime;
const
  NO_ERROR = 0;
type
{$endif}

	TFileNames = array of TFileName;

function ShortDir(const ADirectoryPath: string): string;

function ExpandFile(const AFileName: TFileName): string;
function ExpandFileCmd(const AFileName: TFileName): string;
function ExpandDir(const ADirectoryPath: string): string;
function ExpandDirCmd(const ADirectoryPath: string): string;
function DelFileExt(const AFileName: string): string;
function AddAfterName(const AFileName: string; const AText: string): string;
function ParentDir(var ADirectoryPath: string; ALevel: SG = 1): BG;
function ParentDirF(const ADirectoryPath: string; const ALevel: SG = 1): string;
function LegalFileName(const AFileName: string): string;
procedure ReadDir(
  var AFileNames: TFileNames;
  var AFileCount: SG;
  const APath: string;
  const AExtensions: array of string;
  const AFiles, ADirs, ASubDirs, ASort: BG;
  const AFullPath: BG = False); deprecated 'use TFolder';

function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
function DateTimeToFileTime(const ADateTime: TDateTime): TFileTime;
procedure GetFileDateTime(const AFileName: TFileName; out ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime);
procedure SetFileDateTime(const AFileName: TFileName; const ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime);
function GetFileSizeU(const AFileName: TFileName): S8;
function GetFileSizeS(const AFileName: TFileName): string;
function GetFileCreated(const AFileName: TFileName): TFileTime; overload;
function GetFileModified(const AFileName: TFileName): TFileTime; overload;
procedure SetFileModified(FileName: TFileName; LastWriteTime: TFileTime);

procedure RenameFileEx(const ASource, ADestination: TFileName);
function RenameFileIfExistsEx(const ASource, ADestination: TFileName): BG;
procedure CopyFile(const ASource, ADestination: TFileName; const AFailExist: BG);
procedure CopyFileToDir(ASource, ADestination: TFileName; const AFailExist: BG);
procedure CopyDamagedFile(ASource, ADestination: TFileName);
function CreateDirEx(const ADirectoryPath: string): BG;
function CreateDirsEx(const ADirectoryPath: string): BG;
function CreateLockedDir(const ADirectoryName: string): THandle;
function NewFileOrDir(var AFileOrDirectory: string): BG;
function NewFileOrDirEx(var AFileOrDirectory: string): BG;
procedure CopyFileDateTime(const ASourceFile, ADestinationFile: string);
procedure CopyDirOnly(const ASourceDirectory, ADestinationDirectory: string);
function CopyDir(const ASourceDirectory, ADestinationDirectory: string; const AAttribute: SG = faAnyFile): BG;

function DeleteFileEx(const AFileName: TFileName): BG;
procedure RemoveDirEx(const ADirectoryPath: string);
function RemoveDirsEx(ADirectoryPath: string; ADeleteSelf: BG = False): BG;

procedure FileLinesAndSize(const AFileName: TFileName; out ASize, ALines: U8);

procedure ReadBufferFromFile(const AAFileName: TFileName; out ABuffer; out ACount: SG);
procedure WriteBufferToFile(const AFileName: TFileName; const ABuffer; const ACount: UG);

procedure ReadBlockFromFile(const AFileName: TFileName; ABuffer: Pointer; const ACount: UG);
procedure WriteBlockToFile(const AFileName: TFileName; ABuffer: Pointer; const ACount: UG);

procedure ReadStringsFromFile(const AFileName: TFileName; var ALines: TArrayOfString; var ALineCount: SG);
procedure WriteStringsToFile(const AFileName: TFileName; var ALines: TArrayOfString; const AOpeningNameCount: SG; const AAppend: BG);

procedure ReadStringFromFile(const AFileName: TFileName; out AData: string); overload;
function ReadStringFromFile(const AFileName: TFileName): string; overload;
function ReadStringFromFile(const AFileName: TFileName; const ALimit: U8): string; overload;
function ReadStringFromFileEx(const AFileName: TFileName; out AData: string): TFileCharset;

procedure WriteStringToFile(const AFileName: TFileName; const AData: string; const AAppend: BG; const AFileCharset: TFileCharset = DefaultFileCharset; const AProtection: BG = True); overload;

function ShortToLongFileName(const AShortFileName: string): string;
function ShortToLongPath(const AShortFileName: string): string;

function RepairDirectory(const ADirectoryPath: TFileName): TFileName;
function SameFiles(const AFileName1, AFileName2: TFileName): BG;
function TempFileName(const AFileName: TFileName): TFileName;
procedure ReplaceIfChanged(const AOriginalFileName, ATempFileName: TFileName); overload;
procedure ReplaceIfChanged(const ATempFileName: TFileName); overload;
function GetModuleFileNameFunc(const AHandle: THandle): string;

function DirectoryExistsEx(const ADirectoryName: string): BG;
procedure CheckDirectory(const ADirectoryName: string);

function FileExistsEx(const AFileName: TFileName): BG;
procedure RaiseExceptionIfFileNotExists(const AFileName: string);

procedure TestFileReadable(const AFileName: string);
function IsFileWritable(const AFileName: string): Boolean;
function FileOrDirExists(const AFileOrDirectoryPath: string): BG;
function FileOrDirExistsEx(const AFileOrDirectoryPath: string): BG;
{$ifdef MSWINDOWS}
function LastLineFromFile(const FileName: TFileName): AnsiString;
{$endif}
function SameFileName(const AFileName1, AFileName2: TFileName): BG;

function DialogStrWithoutAll(const Ext, Des: array of string): string;
function DialogStr(const AExtensions, ADescriptions: array of string): string;
function GetFileNameFilter(const ADescription: string; const AExtensions: array of string): string;
function GetExecutableFilter: string;

/// <returns>Return 'All Files (*.*)|*.*'</summareturnsry>
function AllFiles: string;

/// <returns>Return 'Text file (*.txt)|*.txt|'</returns>
function AllText: string;

function AllSounds: string;

function SplitCommandLine(ASource: string): TStringPair;
function FindFileInSubDir(const AFileName: TFileName; const AStartInParentDir: BG): TFileName;
function FindFilesInSubDir(const AFileName: TFileName; const AStartInParentDir: BG): TFileNames;

function IsActualOrParentDirectoryName(const AName: string): BG;

implementation

uses
	Math,
{$ifdef MSWINDOWS}
  uWindowsFileAPI,
  uErrorCodeToStr,
{$endif}

  uStrings,
  uRawFile,
  uTextFile,
  uEIOException,
  uStartupEnvironment,
	uChar,
  uProjectInfo,
  uSorts,
  uCharset,
	uOutputFormat,
  uMath,
  uMainLog,

  uSystemPaths;

function SplitCommandLine(ASource: string): TStringPair;
var
	i: SG;
	EndIndex: SG;
begin
	i := 1;
  SkipSpace(ASource, i);
  if CharAt(ASource, i) = '"' then
  begin
    Inc(i);
    EndIndex := i;
    while True do
    begin
      EndIndex := PosEx('"', ASource, EndIndex);
      if CharAt(ASource, EndIndex - 1) = '\' then
      begin
        Delete(ASource, EndIndex - 1, 1);
      end
      else
        Break;
    end;
  end
  else
  begin
    EndIndex := i + 1;
    ReadToChar(ASource, EndIndex, CharSpace);
    Dec(EndIndex);
  end;

  Result.Name := Copy(ASource, i, EndIndex - i);
  Result.Value := Copy(ASource, EndIndex + 1, MaxInt);
end;

function GetModuleFileNameFunc(const AHandle: THandle): string;
var
  NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := GetModuleFileName(AHandle, PChar(Result), MAX_PATH);
	SetLength(Result, NewLength);
end;

function RemoveWorkDir(const Dir: string): string;
var
	i: SG;
begin
	Result := Dir;
	if Length(SystemPaths.WorkDir) <= Length(Dir) then
	begin
		for i := 1 to Length(SystemPaths.WorkDir) do
		begin
			if UpCase(Dir[i]) <> UpCase(SystemPaths.WorkDir[i]) then
			begin
				Exit;
			end;
		end;
		for i := 1 to Length(Dir) - Length(SystemPaths.WorkDir) do
		begin
			Result[i] := Dir[i + Length(SystemPaths.WorkDir)];
		end;
		SetLength(Result, Length(Dir) - Length(SystemPaths.WorkDir));
	end;
end;

function ShortDir(const ADirectoryPath: string): string;
begin
	Result := StartupEnvironment.InsertVariablesFromStart(RemoveWorkDir(ADirectoryPath));
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

function ExpandFile(const AFileName: TFileName): string;
begin
	Result := ExpandCustomDir(AFileName, SystemPaths.WorkDir);
end;

function ExpandFileCmd(const AFileName: TFileName): string;
begin
	Result := ExpandCustomDir(AFileName, SystemPaths.StartDir);
end;

function ExpandDir(const ADirectoryPath: string): string;
begin
	Result := ExpandCustomDir(ADirectoryPath, SystemPaths.WorkDir);
end;

function ExpandDirCmd(const ADirectoryPath: string): string;
begin
	Result := CorrectDirF(ExpandCustomDir(ADirectoryPath, SystemPaths.StartDir));
end;

function DelFileExt(const AFileName: string): string;
var
	Ext: string;
begin
	Result := AFileName;
	Ext := ExtractFileExt(AFileName);
	if Length(Ext) > 0 then SetLength(Result, Length(Result) - Length(Ext));
end;

function AddAfterName(const AFileName: string; const AText: string): string;
begin
	Result := DelFileExt(AFileName) + AText + ExtractFileExt(AFileName);
end;

function ParentDir(var ADirectoryPath: string; ALevel: SG = 1): BG;
var i: Integer;
begin
	Result := False;
	for i := Length(ADirectoryPath) - 1 downto 3 do
	begin
		if ADirectoryPath[i] = PathDelim then
		begin
      Dec(ALevel);
      if ALevel = 0 then
      begin
  			SetLength(ADirectoryPath, i);
  			Result := True;
  			Exit;
      end;
		end;
	end;
//  raise Exception.Create('Could not build parent dir for ''' + Dir + '');
end;

function ParentDirF(const ADirectoryPath: string; const ALevel: SG = 1): string;
begin
	Result := ADirectoryPath;
	ParentDir(Result, ALevel);
end;

const
	DisabledChars: array[0..8] of string =
    ('\', '/', ':', '*', '?', '"', '<', '>', '|');
	NewChars: array[0..8] of string =
    ('-', '∕', 'ː', '✱', '︖', '＂', '＜', '＞', '｜');

function LegalFileName(const AFileName: string): string;
begin
	Result := AFileName;
	Replace(Result, DisabledChars, NewChars);
end;

function IsDirectory(const SearchRec: TSearchRec): BG;
begin
	Result := ((SearchRec.Attr and faDirectory) <> 0);
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
    if (not IsDir) or (not IsActualOrParentDirectoryName(SearchRec.Name)) then
    begin
      if IsDir and (Dirs or SubDirs) then
        SearchRec.Name := SearchRec.Name + PathDelim;
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
    end;
		ErrorCode := FindNext(SearchRec);
	end;
  {$ifdef MSWINDOWS}
	if ErrorCode <> ERROR_NO_MORE_FILES then
    raise EIOException.Create(Path + SubPath, ErrorCode);
  {$endif}
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
    if (not IsDir) or (not IsActualOrParentDirectoryName(SearchRec.Name)) then
    begin
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
    end;
		ErrorCode := FindNext(SearchRec);
	end;
  {$ifdef MSWINDOWS}
	if ErrorCode <> ERROR_NO_MORE_FILES then
		raise EIOException.Create(Path + SubPath, ErrorCode);
  {$endif}
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

procedure CheckExtensions(const Extensions: array of string);
var
  i: Integer;
begin
  for i := 0 to Length(Extensions) - 1 do
  begin
    Assert(Length(Extensions[i]) > 0);
    Assert(Extensions[i, 1] <> '.');
  end;
end;

procedure ReadDir(
  var AFileNames: TFileNames;
  var AFileCount: SG;
  const APath: string;
  const AExtensions: array of string;
  const AFiles, ADirs, ASubDirs, ASort: BG;
  const AFullPath: BG = False);
(*	Offset: Integer;
	MaxLimit: Integer;
	Switch: Integer;
	FileName: TFileName; *)
begin
	if IsDebug then
    CheckExtensions(AExtensions);
{						if (Extensions = '') or (Extension = '*') or (Extension = '*.*') or
	if Length(Extension) > 1 then
		if (Extension <> '*') and (Extension <> '*.*') then
			if Extension[1] <> '.' then Extension := '.' + Extension;}

	if not ASort then
		ReadSubDir(AFileNames, AFileCount, CorrectDirF(ExpandDir(APath)), '', AExtensions, AFiles, ADirs, ASubDirs, AFullPath)
	else
		ReadSubDirSorted(AFileNames, AFileCount, CorrectDirF(ExpandDir(APath)), '', AExtensions, AFiles, ADirs, ASubDirs, AFullPath);

	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add(NToS(AFileCount, ofIO) + ' files found in folder ' + APath + '.', mlDebug);
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

function GetFileSizeU(const AFileName: TFileName): S8;
{$ifdef MSWINDOWS}
var
	FHandle: THandle;
begin
	FHandle := CreateFile(
		PChar(AFileName),  // pointer to name of the file
		0,  // access (read-write) mode
		0,  // share mode
		nil,  // pointer to security attributes
		OPEN_EXISTING,  // how to create
		FILE_ATTRIBUTE_NORMAL,  // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := HandleFileSize(FHandle, AFileName);
		if CloseHandle(FHandle) = False then
		begin
      raise EIOException.Create(AFileName, GetLastError);
		end;
	end
	else
	begin
 		raise EIOException.Create(AFileName, GetLastError);
	end;
{$else}
var
  F: File;
begin
  AssignFile(F, AFileName);
  Reset(F, 1);
  try
    Result := FileSize(F);
  finally
    CloseFile(f);
  end;
{$endif}
end;

function GetFileSizeS(const AFileName: TFileName): string;
begin
	Result := BToStr(GetFileSizeU(AFileName));
end;

function FileTimeToDateTime(AFileTime: TFileTime): TDateTime;
{$ifdef MSWINDOWS}
var
	SystemTime: TSystemTime;
begin
	FileTimeToLocalFileTime(AFileTime, AFileTime);
	FileTimeToSystemTime(AFileTime, SystemTime);
	Result := SystemTimeToDateTime(SystemTime);
{$else}
begin
  Result := AFileTime;
{$endif}
end;

function DateTimeToFileTime(const ADateTime: TDateTime): TFileTime;
{$ifdef MSWINDOWS}
var
	SystemTime: TSystemTime;
begin
	DateTimeToSystemTime(ADateTime, SystemTime);
	SystemTimeToFileTime(SystemTime, Result);
	LocalFileTimeToFileTime(Result, Result);
{$else}
begin
  Result := ADateTime;
{$endif}
end;

function GetFileModified(const AFileName: TFileName): TFileTime;
{$ifdef MSWINDOWS}
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	Result.dwLowDateTime := 0;
	Result.dwHighDateTime := 0;
	GetFileDateTime(AFileName, CreationTime, LastAccessTime, LastWriteTime);
	Result := LastWriteTime;
{$else}
begin
  Result := TFile.GetLastWriteTime(AFileName);
{$endif}
end;

function GetFileCreated(const AFileName: TFileName): TFileTime;
{$ifdef MSWINDOWS}
var
	LastAccessTime, LastWriteTime: TFileTime;
begin
	GetFileDateTime(AFileName, Result, LastAccessTime, LastWriteTime);
{$else}
begin
  Result := TFile.GetCreationTime(AFileName);
{$endif}
end;

procedure SetFileModified(FileName: TFileName; LastWriteTime: TFileTime);
{$ifdef MSWINDOWS}
var
	ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime;
  FHandle: THandle;
  Result: BG;
begin
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
        raise EIOException.Create(FileName, GetLastError);
    end
    else
    begin
      raise EIOException.Create(FileName, GetLastError);
    end;
		if CloseHandle(FHandle) = False then
		begin
			raise EIOException.Create(FileName, GetLastError);
		end;
	end
	else
	begin
		raise EIOException.Create(FileName, GetLastError);
	end;
{$else}
begin
  TFile.SetLastWriteTime(FileName, LastWriteTime);
{$endif}
end;

procedure GetFileDateTime(const AFileName: TFileName; out ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime);
{$ifdef MSWINDOWS}
var
	FHandle: THANDLE;
  Result: BG;
begin
	U8(ACreationTime) := 0;
	U8(ALastAccessTime) := 0;
	U8(ALastWriteTime) := 0;
	FHandle := CreateFile(
		PChar(AFileName),  // pointer to name of the file
		0, // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil, // pointer to security attributes
		OPEN_EXISTING, // how to create
		FILE_FLAG_BACKUP_SEMANTICS {FILE_ATTRIBUTE_NORMAL}, // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := GetFileTime(FHandle, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
		if Result = False then
			raise EIOException.Create(AFileName, GetLastError);
		if CloseHandle(FHandle) = False then
		begin
			raise EIOException.Create(AFileName, GetLastError);
		end;
	end
	else
	begin
		raise EIOException.Create(AFileName, GetLastError);
	end;
{$else}
begin
  ACreationTime := TFile.GetCreationTime(AFileName);
  ALastWriteTime := TFile.GetLastWriteTime(AFileName);
  ALastAccessTime := TFile.GetLastAccessTime(AFileName);
{$endif}
end;

procedure SetFileDateTime(const AFileName: TFileName; const ACreationTime, ALastAccessTime, ALastWriteTime: TFileTime);
{$ifdef MSWINDOWS}
var
	FHandle: THANDLE;
  Result: BG;
begin
	FHandle := CreateFile(
		PChar(AFileName),  // pointer to name of the file
		GENERIC_WRITE, // access (read-write) mode
		FILE_SHARE_READ or FILE_SHARE_WRITE,  // share mode
		nil, // pointer to security attributes
		OPEN_EXISTING, // how to create
		FILE_FLAG_BACKUP_SEMANTICS {FILE_ATTRIBUTE_NORMAL}, // file attributes
		0 // handle to file with attributes to copy
	);
	if FHandle <> INVALID_HANDLE_VALUE then
	begin
		Result := SetFileTime(FHandle, @ACreationTime, @ALastAccessTime, @ALastWriteTime);
		if Result = False then
			raise EIOException.Create(AFileName, GetLastError);
		if CloseHandle(FHandle) = False then
		begin
			raise EIOException.Create(AFileName, GetLastError);
		end;
	end
	else
	begin
		raise EIOException.Create(AFileName, GetLastError);
	end;
{$else}
begin
  TFile.SetCreationTime(AFileName, ACreationTime);
  TFile.SetLastAccessTime(AFileName, ALastWriteTime);
  TFile.SetLastAccessTime(AFileName, ALastAccessTime);
{$endif}
end;

procedure RenameFileEx(const ASource, ADestination: TFileName);
var
  Result: BG;
begin
{$ifdef MSWINDOWS}
	Winapi.Windows.SetFileAttributes(PChar(ADestination), FILE_ATTRIBUTE_ARCHIVE);
	Result := Winapi.Windows.MoveFileEx(PChar(ASource), PChar(ADestination), MOVEFILE_REPLACE_EXISTING);
	if Result = False then
	begin
 		raise EInOutError.Create(ErrorCodeToStr(GetLastError) + LineSep + ASource + LineSep + ADestination);
	end;
{$else}
  Result := SysUtils.RenameFile(ASource, ADestination);
	if Result = False then
	begin
 		raise EInOutError.Create('Can not rename file' + LineSep + ASource + LineSep + ADestination);
	end;
{$endif}
end;

function RenameFileIfExistsEx(const ASource, ADestination: TFileName): BG;
begin
  if FileExists(ASource) and (ASource <> ADestination) then
  begin
    RenameFileEx(ASource, ADestination);
    Result := True;
  end
  else
    Result := False;
end;

procedure CopyFile(const ASource, ADestination: TFileName; const AFailExist: BG);
{$ifdef MSWINDOWS}
var
  Result: BG;
begin
	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Copy file ' + AddQuoteF(ASource) + ' to ' + AddQuoteF(ADestination), mlDebug);
	Winapi.Windows.SetFileAttributes(PChar(ADestination), FILE_ATTRIBUTE_ARCHIVE);
	Result := Winapi.Windows.CopyFile(PChar(ASource), PChar(ADestination), AFailExist);
	if Result = False then
	begin
    raise EInOutError.Create(ErrorCodeToStr(GetLastError) + LineSep + 'During copying file ' + LineSep + ASource + LineSep + 'to' + LineSep + ADestination);;
	end;
{$else}
begin
  TFile.Copy(ASource, ADestination, {Overwrite: }not AFailExist);
{$endif}
end;

procedure CopyFileToDir(ASource, ADestination: TFileName; const AFailExist: BG);
begin
	CopyFile(ASource, ADestination + ExtractFileName(ASource), AFailExist);
end;

procedure CopyDamagedFile(ASource, ADestination: TFileName);
const
	MaxCount = 512;
var
	Total: U8;
	Count: UG;
	FS, FD: TRawFile;
	Buf: Pointer;
begin
	Buf := nil;
	FS := nil;
	FD := nil;
	try
		GetMem(Buf, MaxCount);
		FS := TRawFile.Create;
		FD := TRawFile.Create;
    FS.FileName := ASource;
    FS.FileMode := fmReadOnly;
    FD.FileName := ADestination;
    FD.FileMode := fmReadOnly;
    FS.Open;
    FD.Open;
    Total := FS.FileSize;
    while Total > 0 do
    begin
      Count := Min(Total, MaxCount);
      ClearMemory(Buf^, Count);
      FS.BlockRead(Buf^, Count);
      FD.BlockWrite(Buf^, Count);
      Dec(Total, Count);
    end;
    FD.Truncate;
    FD.Close;
	  FS.Close;
	finally
		FreeAndNil(FS);
		FreeAndNil(FD);
		FreeMem(Buf);
	end;
end;

function CreateDirEx(const ADirectoryPath: string): BG;
begin
	if ADirectoryPath = '' then
	begin
		Result := False;
		Exit;
	end;
	if DirectoryExists(ADirectoryPath) then
		Result := True
	else
	begin
{$ifdef MSWINDOWS}
		Result := CreateDirectory(PChar(ADirectoryPath), nil);
		if Result = False then
			raise EIOException.Create(ADirectoryPath, GetLastError);
{$else}
    TDirectory.CreateDirectory(ADirectoryPath);
    Result := True;
{$endif}
	end;
end;

function CreateDirsEx(const ADirectoryPath: string): BG;
var i: SG;
begin
	Result := False;
	if ADirectoryPath = '' then Exit;
	i := 1;
	while i < Length(ADirectoryPath) do
	begin
		if ADirectoryPath[i] = PathDelim then
			if CreateDirEx(Copy(ADirectoryPath, 1, i)) = False then Exit;
		Inc(i);
	end;
	Result := CreateDirEx(ADirectoryPath);
end;

function CreateLockedDir(const ADirectoryName: string): THandle;
{$ifdef MSWINDOWS}
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
		raise EIOException.Create(DirectoryName, GetLastError);
	end;
{$else}
begin
  CreateDirsEx(ADirectoryName);
  Result := 0;
{$endif}
end;

function NewFileOrDir(var AFileOrDirectory: string): BG;
var
	i: SG;
	IsDir: BG;
	DirS, DirE: string;
begin
	Result := False;
	if Length(AFileOrDirectory) = 0 then Exit;
	IsDir := LastChar(AFileOrDirectory) = PathDelim;
	if IsDir then
	begin
		DirS := Copy(AFileOrDirectory, 1, Length(AFileOrDirectory) - 1);
		DirE := PathDelim;
	end
	else
	begin
		DirS := DelFileExt(AFileOrDirectory);
		DirE := ExtractFileExt(AFileOrDirectory);
	end;
	i := 0;
	while i <= 9999 do
	begin
		if i > 0 then
		begin
			AFileOrDirectory := DirS + '_' + NToS(i, ofIO) + DirE;
		end;

		if IsDir then
		begin
			if DirectoryExists(AFileOrDirectory) = False then
			begin
				Result := True;
				Break;
			end;
		end
		else
		begin
			if FileExists(AFileOrDirectory) = False then
			begin
				Result := True;
				Break;
			end;
		end;
		Inc(i);
	end;
end;

function NewFileOrDirEx(var AFileOrDirectory: string): BG;
var
	FileName, Name: string;
begin
	FileName := ExtractFileName(AFileOrDirectory);
	Name := DelFileExt(FileName);
	AFileOrDirectory := ExtractFilePath(AFileOrDirectory);
	if Name <> '' then
		AFileOrDirectory := AFileOrDirectory + Name + ' ';
	AFileOrDirectory := AFileOrDirectory + ReplaceF(DateTimeToS(Now, 0, ofIO), ':', '_') + ExtractFileExt(FileName);
	Result := NewFileOrDir(AFileOrDirectory);
end;

procedure CopyFileDateTime(const ASourceFile, ADestinationFile: string);
var
	CreationTime, LastAccessTime, LastWriteTime: TFileTime;
begin
	GetFileDateTime(ASourceFile, CreationTime, LastAccessTime, LastWriteTime);
	SetFileDateTime(ADestinationFile, CreationTime, LastAccessTime, LastWriteTime);
end;

procedure CopyDirOnly(const ASourceDirectory, ADestinationDirectory: string);
begin
  {$ifdef MSWINDOWS}
	CreateDirectoryEx(PChar(ASourceDirectory), PChar(ADestinationDirectory), nil);
  {$else}
  TDirectory.CreateDirectory(ADestinationDirectory);
  {$endif}
  CopyFileDateTime(ASourceDirectory, ADestinationDirectory);
end;

function CopyDir(const ASourceDirectory, ADestinationDirectory: string; const AAttribute: SG = faAnyFile): BG;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	Result := True;

	CopyDirOnly(ASourceDirectory, ADestinationDirectory);

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(ASourceDirectory + '*.*', AAttribute, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if not IsActualOrParentDirectoryName(SearchRec.Name) then
				CopyDir(ASourceDirectory + SearchRec.Name + PathDelim, ADestinationDirectory + SearchRec.Name + PathDelim, AAttribute);
		end
		else
		begin
			CopyFile(ASourceDirectory + SearchRec.Name, ADestinationDirectory + SearchRec.Name, False);
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
  {$ifdef MSWINDOWS}
	if ErrorCode <> ERROR_NO_MORE_FILES then
    raise EIOException.Create(ASourceDirectory, ErrorCode);
  {$endif}
	SysUtils.FindClose(SearchRec);
end;

function DeleteFileEx(const AFileName: TFileName): BG;
begin
	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Delete file ' + AddQuoteF(AFileName), mlDebug);
  {$ifdef MSWINDOWS}
  // Readonly file can not be deleted
	Winapi.Windows.SetFileAttributes(PChar(AFileName), FILE_ATTRIBUTE_ARCHIVE);
  {$endif}
	Result := DeleteFile(PChar(AFileName));
	if Result = False then
		raise EIOException.Create(AFileName, GetLastError);
end;

procedure RemoveDirEx(const ADirectoryPath: string);
begin
	if MainLog.IsLoggerFor(mlDebug) then
    MainLog.Add('Remove directory ' + AddQuoteF(ADirectoryPath), mlDebug);
  {$ifdef MSWINDOWS}
	if RemoveDirectory(PChar(ADirectoryPath)) = False then
		raise EIOException.Create(ADirectoryPath, GetLastError);
  {$else}
  TDirectory.Delete(ADirectoryPath);
  {$endif}
end;

function RemoveDirsEx(ADirectoryPath: string; ADeleteSelf: BG = False): BG;
var
	SearchRec: TSearchRec;
	ErrorCode: Integer;
begin
	if DirectoryExists(ADirectoryPath) = False then
	begin
		Result := False;
		Exit;
	end;
	Result := True;

	CorrectDir(ADirectoryPath);

	// faReadOnly or faHidden or faSysFile or faArchive or faDirectory
	ErrorCode := FindFirst(ADirectoryPath + '*.*', faAnyFile, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		if (SearchRec.Attr and faDirectory) <> 0 then
		begin
			if not IsActualOrParentDirectoryName(SearchRec.Name) then
			begin
				Result := RemoveDirsEx(ADirectoryPath + SearchRec.Name + PathDelim, True) and Result;
			end;
		end
		else
		begin
			Result := DeleteFileEx(ADirectoryPath + SearchRec.Name) and Result;
		end;
		ErrorCode := SysUtils.FindNext(SearchRec);
	end;
  {$ifdef MSWINDOWS}
	if ErrorCode <> ERROR_NO_MORE_FILES then
    raise EIOException.Create(ADirectoryPath, ErrorCode);
  {$endif}
	SysUtils.FindClose(SearchRec);

	if ADeleteSelf then
    RemoveDirEx(ADirectoryPath);
end;

procedure FileLinesAndSize(const AFileName: TFileName; out ASize, ALines: U8);
var
	F: TTextFile;
	Line: string;
begin
	ASize := 0;
	ALines := 0;
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    ASize := F.FileSize;
    while not F.EOF do
    begin
      F.ReadLine(Line);
      Inc(ALines);
    end;
    F.Close;
	finally
		F.Free;
	end;
end;

procedure ReadBufferFromFile(const AAFileName: TFileName; out ABuffer; out ACount: SG);
var
	F: TRawFile;
begin
	ACount := 0;
	Pointer(ABuffer) := nil;
	F := TRawFile.Create;
  F.FileName := AAFileName;
  F.FileMode := fmReadOnly;
	try
		F.Open;
    GetMem(Pointer(ABuffer), F.FileSize);
    try
      F.BlockRead(Pointer(ABuffer)^, F.FileSize);
      ACount := F.FileSize;
      F.Close;
    except
      FreeMem(Pointer(ABuffer));
      Pointer(ABuffer) := nil;
    end;
	finally
		F.Free;
	end;
end;

procedure WriteBufferToFile(const AFileName: TFileName; const ABuffer; const ACount: UG);
var
	F: TRawFile;
begin
	F := TRawFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmRewrite;
		F.Open;
    F.BlockWrite(Pointer(ABuffer)^, ACount);
    F.Truncate;
    F.Close;
	finally
		F.Free;
	end;
end;

procedure ReadBlockFromFile(const AFileName: TFileName; ABuffer: Pointer; const ACount: UG);
var
	F: TRawFile;
begin
	F := TRawFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
    F.Open;
    F.BlockRead(ABuffer^, Min(ACount, F.FileSize));
    F.Close;
	finally
		F.Free;
	end;
end;

procedure WriteBlockToFile(const AFileName: TFileName; ABuffer: Pointer; const ACount: UG);
var
	F: TRawFile;
begin
	F := TRawFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmRewrite;
		F.Open;
    F.BlockWrite(ABuffer^, ACount);
    F.Truncate;
    F.Close;
	finally
		F.Free;
	end;
end;

function ReadStringFromFileEx(const AFileName: TFileName; out AData: string): TFileCharset;
var
	F: TTextFile;
begin
	AData := '';
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
    F.DefaultCharset := fcAnsi;
		F.Open;
    AData := F.ReadAsString;
    Result := F.Charset;
    F.Close;
	finally
		F.Free;
	end;
end;

procedure ReadStringFromFile(const AFileName: TFileName; out AData: string); overload;
var
	F: TTextFile;
begin
	AData := '';
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    AData := F.ReadAsString;
    F.Close;
	finally
		F.Free;
	end;
end;

function ReadStringFromFile(const AFileName: TFileName): string; overload;
begin
	ReadStringFromFile(AFileName, Result);
end;

function ReadStringFromFile(const AFileName: TFileName; const ALimit: U8): string;
var
	F: TTextFile;
begin
	Result := '';
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
    F.Open;
    F.ReadAsString(ALimit);
    F.Close;
	finally
		F.Free;
	end;
end;

function SameDataInFile(const AFileName: TFileName; const ALine: RawByteString): BG;
var
	F: TRawFile;
	Buf: Pointer;

  function InternalSameDataInFile: BG;
  var
	  TotalBytes, ReadBytes: SG;
    P: Pointer;
  begin
    TotalBytes := F.FileSize;
    if TotalBytes <> Length(ALine) then
    begin
      Result := False;
      Exit;
    end;
    while TotalBytes > 0 do
    begin
      ReadBytes := DefFileBuffer;
      if ReadBytes > TotalBytes then
        ReadBytes := TotalBytes;
      F.BlockRead(Buf^, ReadBytes);
      P := @ALine[Length(ALine) - TotalBytes + 1];
      if SameData(Buf, P, ReadBytes) = False then
      begin
        Result := False;
        Exit;
      end;
      Dec(TotalBytes, ReadBytes);
    end;
    Result := True;
  end;
begin
	Result := False;
	if FileExists(AFileName) then
	begin
		F := TRawFile.Create;
		GetMem(Buf, DefFileBuffer);
		try
      F.FileName := AFileName;
      F.FileMode := fmReadOnly;
			F.Open;
      Result := InternalSameDataInFile;
  		F.Close;
		finally
			FreeMem(Buf);
			F.Free;
		end;
	end;
end;

procedure ConvertFileCharset(const ASource: RawByteString; out ADestination: RawByteString; const AFileCharset: TFileCharset); overload;
begin
	case AFileCharset of
	fcAnsi: ADestination := ASource;
	fcUTF8: ADestination := AnsiToUtf8(string(ASource));
	else
    raise ENotSupportedException.Create('Unsupported charset.');
	end;
end;

procedure ConvertFileCharset(const ASource: UnicodeString; out ADestination: RawByteString; const AFileCharset: TFileCharset); overload;
var
  Size: SG;
  u: UnicodeString;
  i: SG;
begin
	case AFileCharset of
	fcAnsi: ADestination := RawByteString(ASource);
	fcUTF8: ADestination := ConvertUnicodeToUtf8(ASource);
  fcUTF16BE:
  begin
    Size := Length(ASource) * SizeOf(WideChar);
    u := ASource;
    for i := 1 to Length(u) do
    begin
      u[i] := WideChar(Swap(Ord(u[i])));
    end;
		SetLength(ADestination, Size);
		Move(u[1], ADestination[1], Size);
  end;
  fcUTF16LE:
  begin
    Size := Length(ASource) * SizeOf(WideChar);
		SetLength(ADestination, Size);
		Move(ASource[1], ADestination[1], Size);
  end;
	else
    raise ENotSupportedException.Create('Unsupported charset');
	end;
end;

procedure WriteStringToFile(
  const AFileName: TFileName;
  const AData: string;
  const AAppend: BG;
  const AFileCharset: TFileCharset = DefaultFileCharset;
  const AProtection: BG = True);
var
	F: TTextFile;
	DataA: RawByteString;
begin
	ConvertFileCharset(AData, DataA, AFileCharset);
	if AAppend or (not SameDataInFile(AFileName, DataA)) then
	begin
		F := TTextFile.Create;
		try
      F.FileName := AFileName;
      if AAppend then
        F.FileMode := fmReadAndWrite // Needs to read BOM
      else
        F.FileMode := fmRewrite;
  		F.DefaultCharset := AFileCharset;
      F.Protection := AProtection;
      F.SkipSameData := False;
			F.Open;
      if F.FileMode <> fmRewrite then
        F.SeekEnd;
      F.WriteNoConversion(DataA);
      F.Close;
		finally
			F.Free;
		end;
	end;
end;

procedure ReadStringsFromFile(const AFileName: TFileName; var ALines: TArrayOfString; var ALineCount: SG);
var
	F: TTextFile;
	Line: string;
	NewSize: SG;
begin
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    while not F.Eof do
    begin
      F.ReadLine(Line);
      NewSize := ALineCount + 1;
      if AllocByExp(Length(ALines), NewSize) then
        SetLength(ALines, NewSize);
      ALines[ALineCount] := Line;
      Inc(ALineCount);
    end;
    F.Close;
	finally
		F.Free;
	end;
end;

procedure WriteStringsToFile(const AFileName: TFileName; var ALines: TArrayOfString; const AOpeningNameCount: SG; const AAppend: BG);
var
	F: TTextFile;
	i: SG;
begin
	F := TTextFile.Create;
	try
    F.FileName := AFileName;
    if AAppend then
      F.FileMode := fmReadAndWrite
    else
      F.FileMode := fmRewrite;
		F.Open;
    if AAppend then
      F.SeekEnd;
    i := 0;
    while i < AOpeningNameCount do
    begin
      F.Write(ALines[i] + FileSep);
      Inc(i);
    end;
    F.Close;
	finally
		F.Free;
	end;
end;

function ShortToLongFileName(const AShortFileName: string): string;
{$ifdef MSWINDOWS}
var
	Temp: TWIN32FindData;
	SearchHandle: THandle;
begin
	SearchHandle := FindFirstFile(PChar(AShortFileName), Temp);
	if SearchHandle <> ERROR_INVALID_HANDLE then begin
		Result := string(Temp.cFileName);
		if Result = '' then Result := string(Temp.cAlternateFileName);
	end
	else Result := '';
	Winapi.Windows.FindClose(SearchHandle);
{$else}
begin
  Result := AShortFileName;
{$endif}
end;

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

function ShortToLongPath(const AShortFileName: string): string;
var
	LastSlash: string;
  ShortFileName: string;
begin
	if (FileExists(AShortFileName) = False) or (Length(AShortFileName) < 2) or (AShortFileName[1] = PathDelim) then
	begin
		Result := AShortFileName;
		Exit;
	end;
	Result := '';
	LastSlash := StrRScan(AShortFileName);
  ShortFileName := AShortFileName;
	while LastSlash <> '' do
	begin
		Result := PathDelim + ShortToLongFileName(ShortFileName) + Result;
		if LastSlash <> '' then
		begin
			SetLength(ShortFileName, Length(ShortFileName) - Length(LastSlash));
			LastSlash := StrRScan(ShortFileName);
		end;
	end;
	Result := UpperCase(ShortFileName) {c: -> C:} + Result;
end;

function RepairDirectory(const ADirectoryPath: TFileName): TFileName;
begin
	Result := ShortToLongPath(ExpandDir(ADirectoryPath));
	if Result = '' then Exit;
	while True do
	begin
		if DirectoryExists(Result) then Break;
		if ParentDir(string(Result)) = False then Break;
	end;
end;

function CompareFiles(const File1, File2: TRawFile): BG;
var
	Buf1, Buf2: Pointer;
  FileBufferSize: UG;

  function InternalCompareFiles: BG;
  var
  	ReadBytes: UG;
  	TotalBytes: U8;
  begin
    TotalBytes := File1.FileSize;
    while TotalBytes > 0 do
    begin
      ReadBytes := FileBufferSize;
      if ReadBytes > TotalBytes then ReadBytes := TotalBytes;
      File1.BlockRead(Buf1^, ReadBytes);
      File2.BlockRead(Buf2^, ReadBytes);
      Dec(TotalBytes, ReadBytes);
      if SameData(Buf1, Buf2, ReadBytes) = False then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;

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
			Result := InternalCompareFiles;
		finally
			FreeMem(Buf1);
			FreeMem(Buf2);
		end;
	end;
end;

function SameFiles(const AFileName1, AFileName2: TFileName): BG;
var
	File1, File2: TRawFile;
begin
	File1 := TRawFile.Create;
	try
    File1.FileName := AFileName1;
    File1.FileMode := fmReadOnly;
		File1.Open;
		File2 := TRawFile.Create;
    try
      File2.FileName := AFileName2;
      File2.FileMode := fmReadOnly;
      File2.Open;
      Result := CompareFiles(File1, File2);
      File2.Close;
      File1.Close;
    finally
      File2.Free;
    end;
	finally
		File1.Free;
	end;
end;

function TempFileName(const AFileName: TFileName): TFileName;
begin
	Result := ExtractFilePath(AFileName) + '~' + ExtractFileName(AFileName);
end;

procedure ReplaceIfChanged(const AOriginalFileName, ATempFileName: TFileName);
begin
	if FileExists(AOriginalFileName) and SameFiles(AOriginalFileName, ATempFileName) then
		DeleteFileEx(ATempFileName)
	else
	begin
		CopyFile(ATempFileName, AOriginalFileName, False);
		DeleteFileEx(ATempFileName);
	end;
end;

procedure ReplaceIfChanged(const ATempFileName: TFileName);
var
	OrigFileName: TFileName;
begin
	OrigFileName := ExtractFileName(ATempFileName);
	if Length(OrigFileName) <= 0 then Exit;
	if OrigFileName[1] <> '~' then Exit;
	Delete(OrigFileName, 1, 1);
	OrigFileName := ExtractFilePath(ATempFileName) + OrigFileName;

	ReplaceIfChanged(OrigFileName, ATempFileName);
end;

function FileExistsEx(const AFileName: TFileName): BG;
begin
	Result := FileExists(ExpandDir(AFileName));
end;

procedure TestFileReadable(const AFileName: string);
{$ifdef MSWINDOWS}
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
{$else}
var
  F: File;
begin
  AssignFile(F, AFileName);
  Reset(F, 1);
  CloseFile(F);
{$endif}
end;

function IsFileWritable(const AFileName: string): Boolean;
{$ifdef MSWINDOWS}
var
  H: THandle;
begin
  H := CreateFile(PChar(AFileName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_ALWAYS, 0, 0);
  Result := H <> INVALID_HANDLE_VALUE;
  if Result then
    CloseHandle(H);
{$else}
var
  F: File;
begin
  AssignFile(F, AFileName);
  try
    Reset(F, 1);
    CloseFile(F);
    Result := True;
  except
    Result := False;
  end;
{$endif}
end;
{
function DirectoryExists(const Directory: string): BG;
var
	Code: U4;
begin
	Code := GetFileAttributes(PChar(Directory));
	Result := (Code <> High(Code)) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;}

function DirectoryExistsEx(const ADirectoryName: string): BG;
begin
	Result := DirectoryExists(ExpandDir(ADirectoryName));
end;

procedure CheckDirectory(const ADirectoryName: string);
begin
  if not DirectoryExists(ADirectoryName) then
    raise EDirectoryNotFoundException.Create('Path ' + AddQuoteF(ADirectoryName) + ' not found.');
end;

procedure RaiseExceptionIfFileNotExists(const AFileName: string);
begin
  if not FileExistsEx(AFileName) then
    raise EFileNotFoundException.Create('File ' + AddQuoteF(AFileName) + ' not found.');
end;

function FileOrDirExists(const AFileOrDirectoryPath: string): BG;
begin
	if Length(AFileOrDirectoryPath) = 0 then
		Result := False
	else if LastChar(AFileOrDirectoryPath) = PathDelim then
		Result := DirectoryExists(AFileOrDirectoryPath)
	else
		Result := FileExists(AFileOrDirectoryPath);
end;

function FileOrDirExistsEx(const AFileOrDirectoryPath: string): BG;
begin
	if Length(AFileOrDirectoryPath) = 0 then
		Result := False
	else if LastChar(AFileOrDirectoryPath) = PathDelim then
		Result := DirectoryExistsEx(AFileOrDirectoryPath)
	else
		Result := FileExistsEx(AFileOrDirectoryPath);
end;

{$ifdef MSWINDOWS}
function LastLineFromFile(const FileName: TFileName): AnsiString;
var
	i: SG;
	F: TTextFile;
	C: AnsiChar;
begin
	Result := '';
	F := TTextFile.Create;
	try
    F.FileName := FileName;
    F.FileMode := fmReadOnly;
    F.RandomAccess := True;
		F.Open;
    i := F.FileSize - 1;
    while i >= 0 do
    begin
      F.Seek(i);
      F.BlockRead(C, SizeOf(C));
      if not CharInSet(C, [CharCR, CharLF]) then
        Break;
      Dec(i);
    end;

    while i >= 0 do
    begin
      F.Seek(i);
      F.BlockRead(C, SizeOf(C));
      if CharInSet(C, [CharCR, CharLF]) then
        Break;
      Result := c + Result;
      Dec(i);
    end;
    F.Close;
	finally
		F.Free;
	end;
end;
{$endif}

function SameFileName(const AFileName1, AFileName2: TFileName): BG;
begin
	Result := ShortToLongPath(AFileName1) = ShortToLongPath(AFileName2);
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
		s1 := DelLastChar(s1, 2);
		s1 := s1 + ')';
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

function DialogStr(const AExtensions, ADescriptions: array of string): string;
begin
	Result := DialogStrWithoutAll(AExtensions, ADescriptions);
	Result := Result + AllFiles;
end;

function GetFileNameFilter(const ADescription: string; const AExtensions: array of string): string;
var
	i: SG;
	ExtString: string;
	s: string;
begin
	Result := ADescription + ' (';
	ExtString := '';
	for i := 0 to Length(AExtensions) - 1 do
	begin
		if Pos('.', AExtensions[i]) = 0 then
		begin
			s := '*.' + AExtensions[i];
		end
		else
		begin
			s := AExtensions[i];
		end;
		ExtString := ExtString + s;
		if i < Length(AExtensions) - 1 then
		begin
			ExtString := ExtString + ';';
		end;
	end;
	Result := Result + ExtString + ')|' + ExtString;
end;

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

function AllText: string;
begin
	Result := GetTextFilter + '|' + AllFiles;
end;

function AllSounds: string;
begin
	Result := GetFileNameFilter('Sound Wave', ['wav']);
end;

function FindFileInSubDir(const AFileName: TFileName; const AStartInParentDir: BG): TFileName;
var
  Dir: string;
  FileNameOnly: string;
  FileName: string;
begin
  Result := '';
  Dir := ExtractFilePath(AFileName);
  if AStartInParentDir then
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

function FindFilesInSubDir(const AFileName: TFileName; const AStartInParentDir: BG): TFileNames;
var
  Dir: string;
  Dir2: string;
  FileNameOnly: string;
  FileName: string;
begin
  SetLength(Result, 0);
  Dir := ExtractFilePath(AFileName);
  if AStartInParentDir then
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

function IsActualOrParentDirectoryName(const AName: string): BG;
var
  NameLength: SG;
begin
  NameLength := Length(AName);
  if NameLength > 2 then
    Result := False
  else if NameLength = 2 then
    Result := (AName[1] = '.') and (AName[2] = '.')
  else if NameLength = 1 then
    Result := AName[1] = '.'
  else // 0
    Result := False;
end;

end.
