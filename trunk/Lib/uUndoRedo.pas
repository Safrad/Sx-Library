unit uUndoRedo;

interface

uses
	Menus,
	uTypes, uUndo, uOptions;

type
	TUndoRedo = class(TObject)
	private
		FUndos: array of TUndo; // TODO : Tree - Branching
		FCount: SG;
		FIndex: SG;
		FSavedIndex: SG;
		function GetRedoCount: SG;
		function GetUndoCount: SG;
		function GetSaved: BG;
		procedure SetSaved(const Value: BG);
		procedure FillItem(const Undo: BG; const MenuItem: TMenuItem; const Count: UG);
	public
		constructor Create(const Owner: TObject);
		destructor Destroy; override;

		procedure InitMenu;

		function GetSize: U8;
		function GetUndoSize: U8;
		function GetRedoSize: U8;

		function GetUndoRedo(const DoUndo: BG): TUndo;
		procedure ApplyUndo(const Undo: TUndo; const DoUndo: BG);

		procedure ClearUndo;
		procedure ClearRedo;
		procedure AddUndo(const Undo: TUndo);

		property RedoCount: SG read GetRedoCount;
		property UndoCount: SG read GetUndoCount;
		property Saved: BG read GetSaved write SetSaved;
	end;

{type
	TUndoSettings = (
		usUndoCountLimit,
		usUndoCountSizePercentOfRAM);}
var
{	UndoOptions: array [TUndoSettings] of TOption = (
		(
			Typ: vsSpin; Default: MaxInt; Minimum: 0; Maximum: MaxInt), //
		(Typ: vsSpin; Default: 20; Minimum: 0; Maximum: 100) //
		);
	UndoParams: array [TUndoSettings] of TParam;}

	Undo1, Redo1: TMenuItem;
	UndoCountLimit: SG;
	UndoSizePercentOfRAM: UG;

implementation

uses
	uDictionary, uSysInfo,
	SysUtils;

{ TUndoRedo }

procedure TUndoRedo.AddUndo(const Undo: TUndo);
var i: SG;
begin
	ClearRedo;

	if (FIndex >= UndoCountLimit) or (100 * (GetUndoSize + Undo.GetSize) > UndoSizePercentOfRAM * U8(GSysInfo.MS.dwTotalPhys)) then
	begin
		FUndos[0].Free;
		for i := 0 to FCount - 2 do
			FUndos[i] := FUndos[i + 1];
	end
	else
	begin
		Inc(FIndex);
		Inc(FCount);
		SetLength(FUndos, FCount);
	end;
	FUndos[FCount - 1] := Undo;
	InitMenu;
end;

procedure TUndoRedo.ApplyUndo(const Undo: TUndo; const DoUndo: BG);
begin
	if DoUndo then
	begin
		if FIndex = -1 then
		begin
			Exit;
		end;
		FUndos[FIndex] := Undo;
		Dec(FIndex)
	end
	else
	begin
		if FIndex >= FCount - 1 then
		begin
			Exit;
		end;
		Inc(FIndex);
		FUndos[FIndex] := Undo;
	end;

	InitMenu;
end;

procedure TUndoRedo.ClearRedo;
var
	i: SG;
begin
	for i := FIndex + 1 to FCount - 1 do
	begin
		FreeAndNil(FUndos[i]);
	end;
	FCount := FIndex + 1;
	InitMenu;
end;

procedure TUndoRedo.ClearUndo;
var
	i: SG;
begin
	for i := 0 to FIndex do
		FreeAndNil(FUndos[i]);

	for i := FIndex + 1 to FCount - 1 do
	begin
		FUndos[i - FIndex] := FUndos[i];
	end;
	Dec(FCount, FIndex + 1);
	FIndex := -1;
	InitMenu;
end;

constructor TUndoRedo.Create(const Owner: TObject);
begin
	FIndex := -1;
	FSavedIndex := -1;
	InitMenu;
end;

destructor TUndoRedo.Destroy;
var i: SG;
begin
	for i := 0 to FCount - 1 do
		FreeAndNil(FUndos[i]);
end;

procedure TUndoRedo.FillItem(const Undo: BG; const MenuItem: TMenuItem; const Count: UG);
var
	s: string;
begin
	MenuItem.Enabled := Count > 0;
	s := MenuItem.Caption;
	if Undo then
	begin
		s := Translate('Undo');
		if (Count > 0) and (FUndos[FIndex].Name <> '') then
			s := s + ' - ' + FUndos[FIndex].Name;
	end
	else
	begin
		s := Translate('Redo');
	end;
	MenuItem.Caption := s;
	MenuItem.Enabled := Count > 0;
end;

function TUndoRedo.GetRedoSize: U8;
var
	i: SG;
begin
	Result := 0;
	for i := FIndex + 1 to FCount - 1 do
		Inc(Result, FUndos[i].GetSize);
end;

function TUndoRedo.GetSaved: BG;
begin
	Result := FIndex = FSavedIndex;
end;

function TUndoRedo.GetSize: U8;
var
	i: SG;
begin
	Result := 0;
	for i := 0 to Length(FUndos) - 1 do
	begin
		Inc(Result, FUndos[i].GetSize);
	end;
end;

function TUndoRedo.GetUndoSize: U8;
var
	i: SG;
begin
	Result := 0;
	for i := 0 to FIndex do
		Inc(Result, FUndos[i].GetSize);
end;

procedure TUndoRedo.InitMenu;
begin
	if Assigned(Undo1) then
		FillItem(True, Undo1, GetUndoCount);
	if Assigned(Redo1) then
		FillItem(False, Redo1, GetRedoCount);
end;

procedure TUndoRedo.SetSaved(const Value: BG);
begin
	if Value = True then
		FSavedIndex := FIndex;
end;

function TUndoRedo.GetRedoCount: SG;
begin
	Result := FCount - FIndex - 1;
end;

function TUndoRedo.GetUndoCount: SG;
begin
	Result := FIndex + 1;
end;

function TUndoRedo.GetUndoRedo(const DoUndo: BG): TUndo;
begin
	Result := nil;
	if DoUndo then
	begin
		if FIndex = -1 then
		begin
			Exit;
		end;
		Result := FUndos[FIndex];
	end
	else
	begin
		if FIndex >= FCount - 1 then
		begin
			Exit;
		end;
		Result := FUndos[FIndex + 1];
	end;
end;

initialization
	FillMemoryStatus(GSysInfo);
end.
