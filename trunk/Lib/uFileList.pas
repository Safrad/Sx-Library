unit uFileList;

interface

uses
	uTypes, uFiles, uData,
	SysUtils;

type
	TFileChange = (fcLeastUsed, fcLeastUsedTime, fcRandom, fcAscending {+1}, fcDescending {-1});

var
	FileChangeNames: array [TFileChange] of string;

type
	PWD = ^TWD;

	TWD = record
		Name: string;
		CountOn: U4;
		TimeOn: U4;
		PathIndex: SG;
	end;

	TPathFileName = class
		FileName: TFileName;
		WDIndex: SG;
		Selected: BG;
	end;

	TFileList = class
	private
		FFilter: TArrayOfString;
		FPath: string;
		FFileChange: TFileChange;
		FPathFileNames: TData; // array of TPathFileName;

		FDatabaseFileName: TFileName;
		WDs: array of TWD;
		FWDCount: SG;

		function SelectFileName: SG;
		procedure SetPath(Value: string);
		procedure FreeData;
		procedure OnAddFile(FileName: TFileName);
		function GetCount: SG;
	public
		procedure UpdateFileList;
		procedure ReadFileListDir;
		procedure RWOptions(const Save: BG);
		function GetPWD(const Index: SG): PWD;
		function FindWDIndexByFileName(const FileName: string): SG;

		procedure SelectNewFileNames(Count: SG);
		function GetFileNames: TFileNames;
		procedure SetFileNames(const FileNames: TFileNames);

		function GetFileName: TFileName;
		procedure Reload;

		function IsWDSelected(const Index: SG): BG;
		function IsWDExists(const Index: SG): BG;

		function GetPathFileNameFromWDIndex(const WDIndex: SG): TPathFileName;

		procedure ReadDatabase(const CSVFileName: TFileName);
		procedure SaveDatabase;

		constructor Create;
		destructor Destroy; override;
		procedure Event(const Duration: U4);
		procedure SetFilter(const A: array of string);
		property Path: string read FPath write SetPath;
		property Count: SG read GetCount;
		property WDCount: SG read FWDCount;

		property FileChange: TFileChange read FFileChange write FFileChange;
	end;

implementation

uses
	Math,
	uDIniFile, uFolder,
	uStrings, uMath, uCSVFile, uInputFormat, uFile, uOutputFormat, uWatch;

{ TFileList }

procedure TFileList.UpdateFileList;
var
	s: string;
	P: PWD;
	Index: SG;
	NewSize: SG;
	PathFileName: TPathFileName;
begin
	PathFileName := TPathFileName(FPathFileNames.First);
	while PathFileName <> nil do
	begin
		s := ExtractFileName(PathFileName.FileName);
		Index := FindWDIndexByFileName(s);
		P := GetPWD(Index);
		if P = nil then
		begin
			// Insert new WP into database
			NewSize := FWDCount + 1;
			if AllocByExp(Length(WDs), NewSize) then
				SetLength(WDs, NewSize);
			WDs[FWDCount].Name := s;
			WDs[FWDCount].CountOn := 0;
			WDs[FWDCount].TimeOn := 0;
			WDs[FWDCount].PathIndex := FPathFileNames.Index;
			PathFileName.WDIndex := FWDCount;
			Inc(FWDCount);
		end
		else
		begin
			P.PathIndex := FPathFileNames.Index;
			PathFileName.WDIndex := Index;
		end;
		PathFileName := TPathFileName(FPathFileNames.Next);
	end;
end;

function TFileList.GetPathFileNameFromWDIndex(const WDIndex: SG): TPathFileName;
begin
	if WDs[WDIndex].PathIndex <> -1 then
		Result := TPathFileName(FPathFileNames.GetObject(WDs[WDIndex].PathIndex))
	else
		Result := nil;
end;

function TFileList.GetPWD(const Index: SG): PWD;
begin
	if Index < 0 then
		Result := nil
	else
		Result := @WDs[Index];
end;

function TFileList.IsWDSelected(const Index: SG): BG;
begin
	if WDs[Index].PathIndex <> -1 then
		Result := TPathFileName(FPathFileNames.GetObject(WDs[Index].PathIndex)).Selected
	else
		Result := False;
end;

function TFileList.IsWDExists(const Index: SG): BG;
begin
	if WDs[Index].PathIndex <> -1 then
		Result := True
	else
		Result := False;
end;

function TFileList.FindWDIndexByFileName(const FileName: string): SG;
var
	i: SG;
begin
	Result := -1;
	for i := 0 to FWDCount - 1 do
	begin
		if FileName = WDs[i].Name then
		begin
			Result := i;
			Break;
		end;
	end;
end;

procedure TFileList.OnAddFile(FileName: TFileName);
var
	PathFileName: TPathFileName;
begin
	PathFileName := TPathFileName.Create;
	PathFileName.FileName := FileName;
	FPathFileNames.Add(PathFileName);
end;

procedure TFileList.ReadFileListDir;
var
	Folder: TFolder;
begin
	FPathFileNames.Clear;
	Folder := TFolder.Create;
	try
		Folder.Path := FPath;
		Folder.Extensions := FFilter;
		Folder.OnAddFile := OnAddFile;
		Folder.AcceptDirs := False;
		Folder.Read;
	finally
		Folder.Free;
	end;
end;

function TFileList.GetCount: SG;
begin
	Result := FPathFileNames.Count;
end;

function TFileList.GetFileName: TFileName;
var
	FileNames: TFileNames;
begin
	FileNames := GetFileNames;
	if Length(FileNames) > 0 then
		Result := GetFileNames[0]
	else
		Result := '';
end;

procedure TFileList.SelectNewFileNames(Count: SG);
var
	i: SG;
	Index: SG;
	PathFileName: TPathFileName;
	Offset: SG;
	P: PWD;
	SelectedIndexes: array of SG;
begin
	Count := Min(FPathFileNames.Count, Count);
	SetLength(SelectedIndexes, Count);

	PathFileName := TPathFileName(FPathFileNames.First);
	i := 0;
	while PathFileName <> nil do
	begin
		if PathFileName.Selected then
		begin
			PathFileName.Selected := False;

			if i < Count then
			begin
				SelectedIndexes[i] := FPathFileNames.Index;
				Inc(i);
			end;
		end;
		PathFileName := TPathFileName(FPathFileNames.Next);
	end;

	case FFileChange of
	fcDescending:
		Offset := -1;
	else
		{ fcAscending: } Offset := +1;
	end;

	for i := 0 to Count - 1 do
	begin
		case FFileChange of
		fcAscending, fcDescending:
			Index := UnsignedMod(SelectedIndexes[i] + Offset, FPathFileNames.Count);
		else
			Index := SelectFileName;
		end;
		if Index <> -1 then
		begin
			PathFileName := TPathFileName(FPathFileNames.GetObject(Index));
			PathFileName.Selected := True;
			P := @WDs[PathFileName.WDIndex];
			if P <> nil then
				Inc(P.CountOn);
		end;
	end;
	SaveDatabase;
end;

function TFileList.GetFileNames: TFileNames;
var
	PathFileName: TPathFileName;
begin
	PathFileName := TPathFileName(FPathFileNames.First);
	while PathFileName <> nil do
	begin
		if PathFileName.Selected then
		begin
			SetLength(Result, Length(Result) + 1);
			Result[Length(Result) - 1] := PathFileName.FileName;
		end;
		PathFileName := TPathFileName(FPathFileNames.Next);
	end;
end;

procedure TFileList.SetFileNames(const FileNames: TFileNames);
var
	PathFileName: TPathFileName;
	i: SG;
begin
	PathFileName := TPathFileName(FPathFileNames.First);
	while PathFileName <> nil do
	begin
		PathFileName.Selected := False;
		for i := 0 to Length(FileNames) - 1 do
		begin
			if PathFileName.FileName = FileNames[i] then
			begin
				PathFileName.Selected := True;
				Break;
			end;
		end;
		PathFileName := TPathFileName(FPathFileNames.Next);
	end;
end;

procedure TFileList.SetPath(Value: string);
begin
	if Value <> FPath then
	begin
		FPath := Value;
	end;
end;

procedure TFileList.Reload;
var
	FileNames: TFileNames;
begin
	FileNames := GetFileNames;
	ReadFileListDir;
	UpdateFileList;
	SetFileNames(FileNames);
end;

function TFileList.SelectFileName: SG;
var
	i: SG;
	MinValue, Value: U4;
begin
	Result := -1;
	case FFileChange of
	fcLeastUsed, fcLeastUsedTime:
		begin
			MinValue := MaxInt;
			for i := 0 to FWDCount - 1 do
			begin
				case FFileChange of
				fcLeastUsed:
					Value := WDs[i].CountOn;
				fcLeastUsedTime:
					Value := WDs[i].TimeOn;
				else
					Value := 0;
				end;

				if (Value < MinValue) and (WDs[i].PathIndex >= 0) and
					(TPathFileName(FPathFileNames.GetObject(WDs[i].PathIndex)).Selected = False) then
				begin
					Result := WDs[i].PathIndex;
					MinValue := Value;
				end;
			end;
		end;
	fcRandom:
		begin
			Result := Random(FPathFileNames.Count);
		end;
	end;
end;

procedure TFileList.ReadDatabase(const CSVFileName: TFileName);
var
	i: SG;
	CSVFile: TCSVFile;
	NewSize: SG;
	Values: TArrayOfString;
begin
	Values := nil;
	FDatabaseFileName := CSVFileName;
	for i := 0 to FWDCount - 1 do
	begin
		WDs[i].Name := '';
	end;
	SetLength(WDs, 0);
	FWDCount := 0;
	if FileExists(CSVFileName) then
	begin
		WatchRemoveFile(CSVFileName);
		CSVFile := TCSVFile.Create(4);
		try
			if CSVFile.Open(CSVFileName) then
			begin
				CSVFile.RemapColumns(['Name', 'CountOn', 'TimeOn']);
				while not CSVFile.EOF do
				begin
					Values := CSVFile.ReadLine;
					if Length(Values) >= 4 then
					begin
						NewSize := FWDCount + 1;
						if AllocByExp(Length(WDs), NewSize) then
							SetLength(WDs, NewSize);
						Assert(Values[0] <> '');
						WDs[FWDCount].Name := Values[0];
						WDs[FWDCount].CountOn := StrToValI(Values[1], False, 0, 0, MaxInt, 1);
						WDs[FWDCount].TimeOn := StrToValI(Values[2], False, 0, 0, MaxInt, 1);
						WDs[FWDCount].PathIndex := -1;
						Inc(FWDCount);
					end;
				end;
				CSVFile.Close;
			end;
		finally
			CSVFile.Free;
		end;
		WatchAddFile(CSVFileName, ReadDatabase);
	end;
end;

procedure TFileList.SaveDatabase;
var
	i: SG;
	F: TFile;
begin
	WatchRemoveFile(FDatabaseFileName);
	F := TFile.Create;
	try
		if F.Open(FDatabaseFileName, fmRewrite) then
		begin
			F.Writeln(CSVRemark + 'FileName' + CSVSep + 'CountOn' + CSVSep + 'TimeOn' + CSVSep +
					'Exists');
			for i := 0 to FWDCount - 1 do
			begin
				if (WDs[i].Name <> '') and ((WDs[i].CountOn > 0) or (WDs[i].TimeOn > 0)) then
					F.Writeln(CSVCell(WDs[i].Name) + CSVSep + CSVCell(NToS(WDs[i].CountOn, ofIO))
							+ CSVSep + CSVCell(NToS(WDs[i].TimeOn, ofIO)) + CSVSep + CSVCell
							(NToS(SG(WDs[i].PathIndex), ofIO)));
			end;
			F.Truncate;
			F.Close;
		end;
	finally
		F.Free;
		WatchAddFile(FDatabaseFileName, ReadDatabase);
	end;
end;

constructor TFileList.Create;
begin
	inherited;

	FPathFileNames := TData.Create;
end;

destructor TFileList.Destroy;
begin
	SaveDatabase;
	RWOptions(True);
	WatchRemoveFile(FDatabaseFileName);
	FreeData;
	FreeAndNil(FPathFileNames);
	inherited;
end;

procedure TFileList.RWOptions(const Save: BG);
const
	Section = 'FileList';
var
	FSelectedFileNames: TFileNames;
begin
	if Save then
		FSelectedFileNames := GetFileNames;

	MainIni.RWFileNames(Section, 'FileNames', FSelectedFileNames, Save);

	if Save = False then
		SetFileNames(FSelectedFileNames);
end;

procedure TFileList.FreeData;
begin
	FreeAndNil(FPathFileNames);

	SetLength(WDs, 0);
	FWDCount := 0;
end;

procedure TFileList.Event(const Duration: U4);
var
	PathFileName: TPathFileName;
	P: PWD;
begin
	PathFileName := TPathFileName(FPathFileNames.First);
	while PathFileName <> nil do
	begin
		if PathFileName.Selected then
		begin
			P := @WDs[PathFileName.WDIndex];
			if P <> nil then
				Inc(P.TimeOn, Duration);
		end;
		PathFileName := TPathFileName(FPathFileNames.Next);
	end;
end;

procedure TFileList.SetFilter(const A: array of string);
var
	i: SG;
begin
	SetLength(FFilter, Length(A));
	for i := 0 to Length(FFilter) - 1 do
		FFilter[i] := A[i];
end;

initialization

EnumToStr(TypeInfo(TFileChange), FileChangeNames);

end.
