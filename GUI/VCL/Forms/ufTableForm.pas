unit ufTableForm;

interface

uses
	SysUtils,
  Classes,
  Variants,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
	Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ComCtrls,

	uTypes,
  uIRow,
  uITable,
  uDForm,
  uDImage, uDView, uDWinControl;

type
	TRowAction = procedure(const ASelectedRows: TArrayOfBG) of object;

	TfTableForm = class(TDForm)
		DViewTable: TDView;
		PopupMenu: TPopupMenu;
		Copy1: TMenuItem;
		N1: TMenuItem;
		Delete1: TMenuItem;
		Edit1: TMenuItem;
		StatusBar: TStatusBar;
		Cut1: TMenuItem;
		Paste1: TMenuItem;
		N2: TMenuItem;
		ImportFromCSV1: TMenuItem;
		ExportToCSV1: TMenuItem;
		Add1: TMenuItem;
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure Copy1Click(Sender: TObject);
		procedure Delete1Click(Sender: TObject);
		procedure Edit1Click(Sender: TObject);
		procedure Cut1Click(Sender: TObject);
		procedure Paste1Click(Sender: TObject);
		procedure ImportFromCSV1Click(Sender: TObject);
		procedure ExportToCSV1Click(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure DViewTableDblClick(Sender: TObject);
		procedure PopupMenuPopup(Sender: TObject);
		procedure Add1Click(Sender: TObject);
    procedure DViewTableGetDataEx(Sender: TObject; var Data: Variant; ColIndex, RowIndex: Integer; Rect: TRect);
	private
		{ Private declarations }
		FTableModel: ITable;
		FRowActions: array of TRowAction;
		FReadOnly: BG;
		procedure OnMenuClick(Sender: TObject);
		procedure OnCellClick(Sender: TObject; ColumnIndex, RowIndex: SG; Shift: TShiftState);
		procedure RWOptions(const Save: BG);
		function AddEditOptions(const ACaption: string; ARow: IRow): BG;
		procedure UpdateStatusBar;
	public
		{ Public declarations }
//		constructor Create(AOwner: TComponent); overload; override;
		procedure AddAction(const Title: string; RowAction: TRowAction);
		constructor Create(const ATable: ITable; const AReadOnly: BG); reintroduce;
	end;

implementation

{$R *.dfm}
uses
	Vcl.ClipBrd,

  uOutputInfo,
  uRow,
  uTable,
	uReg,
  uDIniFile, uMenus, uOutputFormat, uStrings, uData, ufOptions, uMsg;

procedure TfTableForm.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

constructor TfTableForm.Create(const ATable: ITable; const AReadOnly: BG);
var
	i : SG;
begin
  Assert(ATable <> nil);

	inherited Create(nil);

	Background := baNone;

	ActiveControl := DViewTable;
	DViewTable.OnCellClick := OnCellClick;

	FTableModel := ATable;
	FReadOnly := AReadOnly;
	Caption := ATable.GetCaption;
	for i := 0 to ATable.GetColumnCount - 1 do
	begin
    DViewTable.Columns.Add(ATable.GetColumn(i));
	end;
  DViewTable.Columns.OwnsObjects := False;
  DViewTable.UpdateColumnCount;

	if Assigned(MainIni) then
	begin
		MainIni.RegisterRW(RWOptions);
	end;
end;

procedure TfTableForm.RWOptions(const Save: BG);
begin
	MainIni.RWFormPos(Self, Save);
	DViewTable.Serialize(MainIni, Save);
end;

procedure TfTableForm.Copy1Click(Sender: TObject);
begin
	DViewTable.CopySelection;
end;

procedure TfTableForm.Delete1Click(Sender: TObject);
var
	i, j: SG;
begin
	if Confirmation('Delete ' + NToS(DViewTable.SelCount) + ' item' + Plural(DViewTable.SelCount) + '?', [mbYes, mbNo]) = mbYes then
	begin
		j := 0;
		for i := 0 to DViewTable.RowCount - 1 do
		begin
			if DViewTable.SelectedRows[i] then
			begin
				DViewTable.SelectedRows[i] := False;
				FTableModel.RemoveRow(j);
			end
			else
				Inc(j);
		end;
		DViewTable.RowCount := FTableModel.GetRowCount;
		DViewTable.DataChanged;
		UpdateStatusBar;
	end;
end;

procedure TfTableForm.Edit1Click(Sender: TObject);
var
	i: SG;
begin
  for i := 0 to DViewTable.RowCount - 1 do
  begin
    if DViewTable.SelectedRows[i] then
    begin
      if AddEditOptions('Edit', FTableModel.GetRow(i)) then
        Invalidate;
    end;
  end;
  DViewTable.DataChanged;
end;

procedure TfTableForm.OnCellClick(Sender: TObject; ColumnIndex, RowIndex: SG; Shift: TShiftState);
begin
	UpdateStatusBar;
end;

procedure TfTableForm.Cut1Click(Sender: TObject);
begin
	Copy1Click(Sender);
	Delete1Click(Sender);
end;

procedure TfTableForm.Paste1Click(Sender: TObject);
{var
	Lines: string;
	InLineIndex: SG;}
begin
{	Lines := Clipboard.AsText;
	InLineIndex := 1;
	while InLineIndex < Length(Lines) do
		FTableModel.AddRow(ReadToNewLine(Lines, InLineIndex));
	DViewTable.RowCount := FTableModel.GetRowCount;
	DViewTable.DataChanged;
	UpdateStatusBar; TODO }
end;

procedure TfTableForm.ImportFromCSV1Click(Sender: TObject);
begin
	// TODO :
end;

procedure TfTableForm.ExportToCSV1Click(Sender: TObject);
begin
	// TODO :
end;

procedure TfTableForm.FormDestroy(Sender: TObject);
begin
	if Assigned(MainIni) then
		MainIni.UnregisterRW(RWOptions);
end;

procedure TfTableForm.OnMenuClick(Sender: TObject);
begin
	if (Length(FRowActions) > 0) and (DViewTable.SelCount > 0) then
		FRowActions[TMenuItem(Sender).Tag](DViewTable.SelectedRows);
end;

procedure TfTableForm.AddAction(const Title: string; RowAction: TRowAction);
var
	M: TMenuItem;
	i: SG;
begin
	M := TMenuItem.Create(Self);
	M.Caption := Title;
	i := Length(FRowActions);
	M.Default := i = 0;
	M.Tag := i;
	SetLength(FRowActions, i + 1);
	FRowActions[i] := RowAction;
	M.OnClick:= OnMenuClick;
	PopupMenu.Items.Insert(i, M);
end;

procedure TfTableForm.FormShow(Sender: TObject);
begin
	MenuSet(PopupMenu);
	DViewTable.RowCount := FTableModel.GetRowCount;
	DViewTable.DataChanged;
	UpdateStatusBar;
end;

procedure TfTableForm.DViewTableDblClick(Sender: TObject);
begin
	if (Length(FRowActions) > 0) and (DViewTable.SelCount >= 0) then
		FRowActions[0](DViewTable.SelectedRows);
end;

procedure TfTableForm.DViewTableGetDataEx(Sender: TObject; var Data: Variant; ColIndex, RowIndex: Integer; Rect: TRect);
begin
	Data := FTableModel.GetCell(ColIndex, RowIndex).GetData;
  // TODO : Colors
end;

procedure TfTableForm.PopupMenuPopup(Sender: TObject);
var
	B: BG;
	i: SG;
begin
//	B := DViewTable.ActualRow >= 0;
	B := (DViewTable.SelCount > 0) and (DViewTable.Where = vaRow);
	for i := 0 to Length(FRowActions) - 1 do
	begin
		PopupMenu.Items[i].Enabled := B;
	end;
	Edit1.Enabled := B and (not FReadOnly);
	Cut1.Enabled := B and (not FReadOnly);
	Copy1.Enabled := B;
	Paste1.Enabled := (Clipboard.GetAsHandle(CF_TEXT) <> 0) and (not FReadOnly);
	Add1.Enabled := Paste1.Enabled;
	Delete1.Enabled := B and (not FReadOnly);
end;

procedure TfTableForm.Add1Click(Sender: TObject);
var
	Row: TRow;
begin
	Row := TRow.Create(FTableModel.GetColumnCount);
	if AddEditOptions('Add', Row) then
	begin
    FTableModel.AddRow(Row);
		DViewTable.RowCount := FTableModel.GetRowCount;
		DViewTable.DataChanged;
	end;
end;

function TfTableForm.AddEditOptions(const ACaption: string;
	ARow: IRow): BG;
begin
	Result := False; // ShowOptions(Caption, FTableModel.Options, FTableModel.OptionCount, Params, nil); TODO
end;

procedure TfTableForm.UpdateStatusBar;
var
	n: SG;
	s: string;
begin
	if DViewTable.SelCount = 0 then
	begin
		n := DViewTable.RowCount;
	end
	else
	begin
		n := DViewTable.SelCount;
	end;
	s := NToS(n) + ' item' + Plural(n);
	if DViewTable.SelCount <> 0 then
		s := s + ' selected';
	StatusBar.SimpleText := s;
end;

end.
