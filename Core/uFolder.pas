unit uFolder;

{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Generics.Collections,

	SysUtils,
  Classes,

	uTypes,
  uFiles,
  uRelativeFileId;

type
	TSortBy = (fsNone, fsFileName, fsDateTime, fsFileSize);

	TSortOrder = (soAscending, soDescending);

type
  TFolderItem = record
    Name: string;
    DateTime: TDateTime;
    Size: U8;
    Attr: S4;
    function IsDirectory: BG;
  end;
  TFolderList = TList<TFolderItem>;

	TFileItem = record
    RelativeFileId: TRelativeFileId;
		DateTime: TDateTime;
		Attr: S4;
		Size: U8;
    function IsDirectory: BG;
	end;
  TFileList = TList<TFileItem>;

	TDirectoryItem = record
		FullPath: string;
		DateTime: TDateTime;
		Attr: S4;
    DepthLevel: S4;
	end;
  TDirectoryList = TList<TDirectoryItem>;

	TOnAddFile = procedure(FileName: TFileName) of object;
	TOnAddFileP = procedure(FileName: TFileName);

	TOnAddFileEx = procedure(const AFileItem: TFileItem) of object;
	TOnAddDirectoryEx = procedure(const ADirectoryItem: TDirectoryItem) of object;

	TFolder = class
	private
    FDepthLevel: SG;

    // All read
    FFiles: TFileList;
    FDirectories: TDirectoryList;

    // Progressive
    FCount: U8;

    // Properties
    // Input
    FPath: string;
    FAborted: BG;
    FOnAddFileEx: TOnAddFileEx;
    FOnAddDirectoryEx: TOnAddDirectoryEx;
    FDirMask: string;
    FFileMask: string;
    FExtensions: TArrayOfString;

    // Output
    FItemIndex: U8;
    FActualPath: string;
    FSortBy: TSortBy;
    FAttributeMask: SG;
    FAcceptFiles: BG;
    FSubDirs: BG;
    FAcceptDirs: BG;
    FSortOrder: TSortOrder;

    procedure SetAborted(const Value: BG);
    procedure SetOnAddFileEx(const Value: TOnAddFileEx);
    procedure SetOnAddDirectoryEx(const Value: TOnAddDirectoryEx);
    procedure SetAcceptDirs(const Value: BG);
    procedure SetAcceptFiles(const Value: BG);
    procedure SetAttributeMask(const Value: SG);
    procedure SetSortBy(const Value: TSortBy);
    procedure SetSortOrder(const Value: TSortOrder);
    procedure SetSubDirs(const Value: BG);
    const
      CountNotInitialized = High(UG);
    var
		FOnAddFile: TOnAddFile;
		FOnAddFileP: TOnAddFileP;

		function GetCount: U8;
    procedure ReadFolderList(const ASubPath: string; const AFolderList: TFolderList);
		procedure ReadSubDirSorted(const ASubPath: string);
    procedure SetAllMask(const Value: string);
    procedure SetPath(const Value: string);
    function CountFiles(const APath: string): U8;
    function AcceptExtension(const AExtension: string): BG;
    procedure SetExtensions(const Value: TArrayOfString);
    procedure RecursiveCall(const ASubPath: string; const AFolderList: TFolderList; const AIndexes: TArrayOfSG);
    procedure AddItems(const ASubPath: string; const AFolderList: TFolderList; const AIndexes: TArrayOfSG);
    procedure SortItems(const FolderList: TFolderList; out AIndexes: TArrayOfSG);
    procedure AddDirectory(const AFolderItem: TFolderItem; const ASubPath: string);
    procedure AddFile(const AFolderItem: TFolderItem; const ASubPath: string);
	public
		constructor Create;
		destructor Destroy; override;

    // Input
		property Path: string read FPath write SetPath;
    property FileMask: string read FFileMask write FFileMask;
    property DirMask: string read FDirMask write FDirMask;
    property AllMask: string write SetAllMask;
		property AcceptFiles: BG read FAcceptFiles write SetAcceptFiles;
		property AcceptDirs: BG read FAcceptDirs write SetAcceptDirs;
		property SubDirs: BG read FSubDirs write SetSubDirs;
		property SortBy: TSortBy read FSortBy write SetSortBy;
		property SortOrder: TSortOrder read FSortOrder write SetSortOrder;
		property AttributeMask: SG read FAttributeMask write SetAttributeMask;
		property OnAddFile: TOnAddFile read FOnAddFile write FOnAddFile;
		property OnAddFileP: TOnAddFileP read FOnAddFileP write FOnAddFileP;
    property OnAddFileEx: TOnAddFileEx read FOnAddFileEx write SetOnAddFileEx;
    property OnAddDirectoryEx: TOnAddDirectoryEx read FOnAddDirectoryEx write SetOnAddDirectoryEx;
		property Extensions: TArrayOfString read FExtensions write SetExtensions;

    // Process
    procedure Clear;
		procedure Read;
    property Aborted: BG read FAborted write SetAborted;

    // Output
    property ItemIndex: U8 read FItemIndex;
		property Count: U8 read GetCount;
		function FirstFileName: TFileName;
		property Files: TFileList read FFiles;
    property Directories: TDirectoryList read FDirectories;
    property ActualPath: string read FActualPath;
	end;

implementation

uses
	uMath,
  uSorts,
  uStrings;

const
	faAll = $00000031;

var
	GList: TFolderList;

function CompareFileName(const Index0, Index1: SG): TCompareResult;
begin
	Result := CompareStringLogical(GList[Index0].Name, GList[Index1].Name);

{	Result := CompareString(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
		PChar(GList^[Index0].Name), Length(GList^[Index0].Name),
		PChar(GList^[Index1].Name), Length(GList^[Index1].Name)) - 2;}
end;

function CompareDateTime(const Index0, Index1: SG): TCompareResult;
begin
	if GList[Index0].DateTime > GList[Index1].DateTime then
		Result := crFirstGreater
	else if GList[Index0].DateTime < GList[Index1].DateTime then
		Result := crFirstLess
	else
		Result := crBothSame;
end;

function CompareFileSize(const Index0, Index1: SG): TCompareResult;
begin
	if GList[Index0].Size > GList[Index1].Size then
		Result := crFirstGreater
	else if GList[Index0].Size < GList[Index1].Size then
		Result := crFirstLess
	else
		Result := crBothSame;
end;

{ TFolder }

procedure TFolder.ReadFolderList(const ASubPath: string; const AFolderList: TFolderList);
var
	SearchRec: TSearchRec;
	ErrorCode: SG;
	IsDir, IsFile: BG;
  FolderItem: TFolderItem;
begin
  SearchRec := Default(TSearchRec);
  ErrorCode := FindFirst(ASubPath + '*.*', AttributeMask, SearchRec);
  while ErrorCode = 0 do
  begin
    IsDir := (SearchRec.Attr and faDirectory) <> 0;
    if (not IsDir) or (not IsActualOrParentDirectoryName(SearchRec.Name)) then
    begin
      IsFile := (SearchRec.Attr and faDirectory) = 0;

      if
        (IsFile and FAcceptFiles and AcceptExtension(ExtractFileExt(SearchRec.Name)) and FileMatch(SearchRec.Name, FFileMask))
        or
        (IsDir and FileMatch(SearchRec.Name, FDirMask)) then
      begin
        FolderItem := Default(TFolderItem);
        FolderItem.Name := SearchRec.Name;
        FolderItem.DateTime := SearchRec.TimeStamp;
        FolderItem.Size := SearchRec.Size;
        FolderItem.Attr := SearchRec.Attr;
        AFolderList.Add(FolderItem);
      end;
    end;
    ErrorCode := FindNext(SearchRec);
  end;
  SysUtils.FindClose(SearchRec);
end;

procedure TFolder.ReadSubDirSorted(const ASubPath: string);
var
	FolderList: TFolderList;
  FIndexes: TArrayOfSG;
begin
  Inc(FDepthLevel);
  FActualPath := ASubPath;
	FolderList := TFolderList.Create;
  try
    ReadFolderList(ASubPath, FolderList);

    if FolderList.Count > 0 then
    begin
      SortItems(FolderList, FIndexes);
      if FAborted then
        Exit;

      AddItems(ASubPath, FolderList, FIndexes);
      if FAborted then
        Exit;

      RecursiveCall(ASubPath, FolderList, FIndexes);
      if FAborted then
        Exit;
    end;
  finally
    FolderList.Free;
  end;
  Dec(FDepthLevel);
end;

function TFolder.AcceptExtension(const AExtension: string): BG;
var
  i: SG;
  LowerCaseExtension: string;
begin
  if Length(FExtensions) = 0 then
    Result := True
  else
  begin
    Result := False;
    LowerCaseExtension := LowerCase(DelFirstChar(AExtension));
    for i := 0 to Length(FExtensions) - 1 do
    begin
      if LowerCaseExtension = LowerCase(FExtensions[i]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TFolder.Clear;
begin
  FFiles.Free;
  FDirectories.Free;
  FAborted := False;
end;

function TFolder.CountFiles(const APath: string): U8;
var
	SearchRec: TSearchRec;
  ErrorCode: SG;
  IsDir: BG;
  IsFile: BG;
begin
  Result := 0;

  SearchRec := Default(TSearchRec);
	ErrorCode := FindFirst(APath + '*.*', AttributeMask, SearchRec);
	while ErrorCode = 0 do
	begin
		IsDir := (SearchRec.Attr and faDirectory) <> 0;
    if (not IsDir) or (not IsActualOrParentDirectoryName(SearchRec.Name)) then
    begin
      IsFile := (SearchRec.Attr and faDirectory) = 0;
      if IsFile and FAcceptFiles and FileMatch(SearchRec.Name, FFileMask) and AcceptExtension(ExtractFileExt(SearchRec.Name)) then
      begin
        Inc(Result);
      end;

      if IsDir and FileMatch(SearchRec.Name, FDirMask) then
        Inc(Result, CountFiles(APath + SearchRec.Name + PathDelim));
    end;
		ErrorCode := FindNext(SearchRec);
	end;
	SysUtils.FindClose(SearchRec);
end;

constructor TFolder.Create;
begin
	inherited;

	FAcceptFiles := True;
	FAcceptDirs := True;
	FSubDirs := True;
	FFileMask := '*.*';
	FDirMask := '*.*';
	FAttributeMask := faAll;

  FCount := CountNotInitialized;
end;

destructor TFolder.Destroy;
begin
  try
    Clear;
  finally
	  inherited;
  end;
end;

function TFolder.FirstFileName: TFileName;
begin
	if FFiles.Count > 0 then
		Result := FFiles.First.RelativeFileId.RelativePathAndName
	else
		Result := '';
end;

procedure TFolder.AddFile(const AFolderItem: TFolderItem; const ASubPath: string);
var
  FileItem: TFileItem;
begin
  Inc(FItemIndex);
  if Assigned(FOnAddFile) then
  begin
    FOnAddFile(ASubPath + AFolderItem.Name);
  end
  else if Assigned(FOnAddFileP) then
  begin
    FOnAddFileP(ASubPath + AFolderItem.Name);
  end
  else
  begin
    FileItem := Default(TFileItem);
    FileItem.RelativeFileId.RelativePath := ASubPath;
    FileItem.RelativeFileId.Name := AFolderItem.Name;
    FileItem.DateTime := AFolderItem.DateTime;
    FileItem.Size := AFolderItem.Size;
    if Assigned(FOnAddFileEx) then
    begin
      FOnAddFileEx(FileItem);
    end
    else
    begin
      FFiles.Add(FileItem);
    end;
  end;
end;

procedure TFolder.AddDirectory(const AFolderItem: TFolderItem; const ASubPath: string);
var
  DirectoryItem: TDirectoryItem;
begin
  Inc(FItemIndex);
  if Assigned(FOnAddFile) then
  begin
    FOnAddFile(ASubPath + AFolderItem.Name + PathDelim);
  end
  else if Assigned(FOnAddFileP) then
  begin
    FOnAddFileP(ASubPath + AFolderItem.Name + PathDelim);
  end
  else
  begin
    DirectoryItem := Default(TDirectoryItem);
    DirectoryItem.FullPath := ASubPath + AFolderItem.Name + PathDelim;
    DirectoryItem.DateTime := AFolderItem.DateTime;
    DirectoryItem.Attr := AFolderItem.Attr;
    DirectoryItem.DepthLevel := FDepthLevel;
    if Assigned(FOnAddDirectoryEx) then
    begin
      FOnAddDirectoryEx(DirectoryItem);
    end
    else
    begin
      if FDirectories = nil then
      begin
        FDirectories := TDirectoryList.Create;
      end;
      FDirectories.Add(DirectoryItem);
    end;
  end;
end;

procedure TFolder.SortItems(const FolderList: TFolderList; out AIndexes: TArrayOfSG);
begin
  SetLength(AIndexes, FolderList.Count);
  FillOrderUG(AIndexes[0], FolderList.Count);
  if SortBy <> fsNone then
  begin
    GList := FolderList;
    case SortBy of
      fsFileName:
        Sort(PArraySG(AIndexes), FolderList.Count, CompareFileName, SortOrder = soDescending);
      fsDateTime:
        Sort(PArraySG(AIndexes), FolderList.Count, CompareDateTime, SortOrder = soDescending);
      fsFileSize:
        Sort(PArraySG(AIndexes), FolderList.Count, CompareFileSize, SortOrder = soDescending);
    end;
  end;
end;

procedure TFolder.AddItems(const ASubPath: string; const AFolderList: TFolderList; const AIndexes: TArrayOfSG);
var
  i: SG;
  j: SG;
begin
  for i := 0 to AFolderList.Count - 1 do
  begin
    j := AIndexes[i];
    if AFolderList[j].IsDirectory then
    begin
      if FAcceptDirs then
      begin
        AddDirectory(AFolderList[j], ASubPath);
      end;
    end
    else
    begin
      AddFile(AFolderList[j], ASubPath);
    end;
    if FAborted then
      Break;
  end;
end;

procedure TFolder.RecursiveCall(const ASubPath: string; const AFolderList: TFolderList; const AIndexes: TArrayOfSG);
var
  i, j: SG;
begin
  for i := 0 to AFolderList.Count - 1 do
  begin
    j := AIndexes[i];
    if SubDirs and AFolderList[j].IsDirectory then
    begin
      ReadSubDirSorted(ASubPath + AFolderList[j].Name + PathDelim);
      if FAborted then
        Break;
    end;
  end;
end;

function TFolder.GetCount: U8;
begin
  if Assigned(FFiles) then
  	Result := FFiles.Count
  else
  begin
    if FCount = CountNotInitialized then
    begin
      FCount := CountFiles(FPath);
    end;
    Result := FCount;
  end;
end;

procedure TFolder.Read;
begin
  Assert(FPath <> '');
  CheckDirectory(FPath);
  if not (Assigned(FOnAddFileEx) or Assigned(FOnAddFile) or Assigned(FOnAddFileP)) then
  begin
    if FFiles = nil then
    begin
      FFiles := TFileList.Create;
    end;
  end;
  FDepthLevel := 0;
  ReadSubDirSorted(FPath);
end;

procedure TFolder.SetAborted(const Value: BG);
begin
  FAborted := Value;
end;

procedure TFolder.SetAcceptDirs(const Value: BG);
begin
  FAcceptDirs := Value;
end;

procedure TFolder.SetAcceptFiles(const Value: BG);
begin
  FAcceptFiles := Value;
end;

procedure TFolder.SetAllMask(const Value: string);
begin
	FileMask := Value;
  DirMask := Value;
end;

procedure TFolder.SetAttributeMask(const Value: SG);
begin
  FAttributeMask := Value;
end;

procedure TFolder.SetExtensions(const Value: TArrayOfString);
begin
  FExtensions := Value;
end;

procedure TFolder.SetOnAddDirectoryEx(const Value: TOnAddDirectoryEx);
begin
  FOnAddDirectoryEx := Value;
end;

procedure TFolder.SetOnAddFileEx(const Value: TOnAddFileEx);
begin
  FOnAddFileEx := Value;
end;

procedure TFolder.SetPath(const Value: string);
begin
  FPath := Value;
end;

procedure TFolder.SetSortBy(const Value: TSortBy);
begin
  FSortBy := Value;
end;

procedure TFolder.SetSortOrder(const Value: TSortOrder);
begin
  FSortOrder := Value;
end;

procedure TFolder.SetSubDirs(const Value: BG);
begin
  FSubDirs := Value;
end;

{ TFolderItem }

function TFolderItem.IsDirectory: BG;
begin
  Result := (Attr and faDirectory) <> 0;
end;

{ TFileItem }

function TFileItem.IsDirectory: BG;
begin
  Result := (Attr and faDirectory) <> 0;
end;

end.
