unit uFolder;

interface

uses
	SysUtils,
	uTypes, uFiles, uData;

type
	TSortBy = (fsNone, fsFileName, fsDateTime, fsFileSize);

	TSortOrder = (soAscending, soDescending);

	TFileItem = class
		Name: string;
		DateTime: TDateTime;
		Size: U8;
	end;

	TOnAddFile = procedure(FileName: TFileName) of object;

	TFolder = class
	private
		FOnAddFile: TOnAddFile;
		function GetCount: SG;
		procedure ReadSubDirSorted(const SubPath: string);
	public
		Path: string;
		Mask: string;
		Extensions: TArrayOfString; //array of string;
		AcceptFiles: BG;
		AcceptDirs: BG;
		SubDirs: BG;
		SortBy: TSortBy;
		SortOrder: TSortOrder;
		AttributeMask: SG;

		Files: TData;
		property Count: SG read GetCount;

		constructor Create;
		destructor Destroy; override;
		procedure Read;
		function FirstFileName: TFileName;
		property OnAddFile: TOnAddFile read FOnAddFile write FOnAddFile;
	published

	end;

implementation

uses
	Windows,
	uMath, uMsg, uSorts;

const
	faAll = $00000031; // faArchive or faReadOnly or faDirectory;

function IsDirectory(const SearchRec: TSearchRec): BG;
begin
	Result := ((SearchRec.Attr and faDirectory) <> 0) and (SearchRec.Name <> '.') and (SearchRec.Name <> '..')
end;

type
	TSearchRecs = array of TSearchRec;
var
	GList: ^TSearchRecs;

function CompareFileName(const Index0, Index1: SG): SG;
begin
	Result := CompareString(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
		PChar(GList^[Index0].Name), Length(GList^[Index0].Name),
		PChar(GList^[Index1].Name), Length(GList^[Index1].Name)) - 2;
end;

function CompareDateTime(const Index0, Index1: SG): SG;
begin
	if GList^[Index0].Time > GList^[Index1].Time then
		Result := 1
	else if GList^[Index0].Time < GList^[Index1].Time then
		Result := -1
	else
		Result := 0;
end;

function CompareFileSize(const Index0, Index1: SG): SG;
begin
	if GList^[Index0].Size > GList^[Index1].Size then
		Result := 1
	else if GList^[Index0].Size < GList^[Index1].Size then
		Result := -1
	else
		Result := 0;
end;

{ TFolder }

procedure TFolder.ReadSubDirSorted(const SubPath: string);
var
	SearchRec: TSearchRec;
	Read: BG;
	i, j, k: SG;
	ErrorCode: SG;
	IsDir, IsFile: BG;
	NewSize: SG;
	List: array of TSearchRec;
	ListCount: SG;
	AIndex: array of SG;
	FileItem: TFileItem;
begin
	ListCount := 0;
	// faReadOnly or faHidden or faSysFile or
	ErrorCode := FindFirst(Path + SubPath + Mask, AttributeMask, SearchRec);
	while ErrorCode = NO_ERROR do
	begin
		IsDir := IsDirectory(SearchRec);
		IsFile := (SearchRec.Attr and faDirectory) = 0;

		//if (IsDir and Dirs)
		if IsDir or (IsFile and AcceptFiles) then
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
	if SortBy <> fsNone then
	begin
		GList := @TSearchRecs(List);
		case SortBy of
		fsFileName:
			Sort(PArraySG(AIndex), ListCount, CompareFileName, SortOrder = soDescending);
		fsDateTime:
			Sort(PArraySG(AIndex), ListCount, CompareDateTime, SortOrder = soDescending);
		fsFileSize:
			Sort(PArraySG(AIndex), ListCount, CompareFileSize, SortOrder = soDescending);
		end;
	end;

	k := Files.Count;

	for i := 0 to ListCount - 1 do
	begin
		j := AIndex[i];
		IsDir := IsDirectory(List[j]);
		if IsDir then
		begin
			if AcceptDirs then
			begin
				if Assigned(OnAddFile) then
				begin
					OnAddFile(SubPath + List[j].Name + PathDelim);
				end
				else
				begin
					FileItem := TFileItem.Create;
					FileItem.Name := List[j].Name + PathDelim;
					FileItem.DateTime := FileDateToDateTime(List[j].Time);
					FileItem.Size := List[j].Size;
					Files.SetCount(k + 1);
					Files.ReplaceObject(k, FileItem);
					Inc(k);
				end;
			end;
		end
		else
		begin
			if Assigned(OnAddFile) then
			begin
				OnAddFile(SubPath + List[j].Name);
			end
			else
			begin
				FileItem := TFileItem.Create;
				FileItem.Name := List[j].Name;
				FileItem.DateTime := FileDateToDateTime(List[j].Time);
				FileItem.Size := List[j].Size;
				Files.SetCount(k + 1);
				Files.ReplaceObject(k, FileItem);
				Inc(k);
			end;
		end;
	end;

	for i := 0 to ListCount - 1 do
	begin
		j := AIndex[i];
		IsDir := IsDirectory(List[j]);
		if SubDirs and IsDir then
		begin
			ReadSubDirSorted(SubPath + List[j].Name + PathDelim);
		end;
	end;
end;

constructor TFolder.Create;
begin
	inherited;

	AcceptFiles := True;
	AcceptDirs := True;
	SubDirs := True;
	Mask := '*.*';
	AttributeMask := faAll;
	Files := TData.Create;
end;

destructor TFolder.Destroy;
begin
	FreeAndNil(Files);
	inherited;
end;

function TFolder.FirstFileName: TFileName;
begin
	if Files.Count > 0 then
		Result := Path + TFileItem(Files.GetObject(0)).Name
	else
		Result := '';
end;

function TFolder.GetCount: SG;
begin
	Result := Files.Count;
end;

procedure TFolder.Read;
begin
	ReadSubDirSorted('');
end;

end.
