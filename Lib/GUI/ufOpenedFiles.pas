unit ufOpenedFiles;

interface

uses
	uTypes, uOpenedFiles,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, uDImage, uDView, uDWinControl;

type
	TfOpenedFiles = class(TForm)
		DViewOpenedFiles: TDView;
		procedure FormCreate(Sender: TObject);
		procedure DViewOpenedFilesKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure DViewOpenedFilesGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure DViewOpenedFilesKeyPress(Sender: TObject; var Key: Char);
		procedure FormDestroy(Sender: TObject);
		procedure DViewOpenedFilesKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure DViewOpenedFilesColumnClick(Sender: TObject;
			Column: TColumn);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FOpenedFiles: TOpenedFiles;
		procedure RWOptions(const Save: BG);
	public
		{ Public declarations }
		procedure TabKey(const Direction: SG);
		{$WARNINGS OFF}
		constructor Create(const OpenedFiles: TOpenedFiles);
	end;
	{$WARNINGS ON}

var
	fOpenedFiles: TfOpenedFiles;

implementation

{$R *.dfm}
uses
	uDIniFile, uOutputFormat, uFiles, uSorts, uMath, uDictionary, uOpenedFileItem;

procedure TfOpenedFiles.FormCreate(Sender: TObject);
begin
	inherited;
	DViewOpenedFiles.AddColumn('#', 32, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('File name', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Path', DViewOpenedFiles.Width div 3, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Last Write Time', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Save Count', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Created', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Modified', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Work Time', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Modification Time', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Save Time', DViewOpenedFiles.Width div 8, taLeftJustify, True);
	MainIni.RegisterRW(RWOptions);
end;

procedure TfOpenedFiles.DViewOpenedFilesKeyUp(Sender: TObject;
	var Key: Word; Shift: TShiftState);
begin
	if Key = VK_CONTROL then
	begin
		if DViewOpenedFiles.ActualRow = -1 then
			FOpenedFiles.Index := -1
		else
			FOpenedFiles.Index := DViewOpenedFiles.RowOrder[DViewOpenedFiles.ActualRow];
		FOpenedFiles.Window1.Items[FOpenedFiles.Index + 1].Checked := True;

		FOpenedFiles.OpenedFileChangeFile(Sender);
		Close;
	end;
end;

procedure TfOpenedFiles.DViewOpenedFilesGetData(Sender: TObject;
	var Data: String; ColIndex, RowIndex: Integer; Rect: TRect);
var
	Item: TOpenedFileItem;
begin
	if RowIndex = FOpenedFiles.Index then
		DViewOpenedFiles.Bitmap.Canvas.Font.Style := [fsBold]
	else
		DViewOpenedFiles.Bitmap.Canvas.Font.Style := [];
	Item := FOpenedFiles.GetItem(RowIndex);
	case ColIndex of
	0: Data := NToS(RowIndex + 1);
	1: Data := ExtractFileName(Item.FileName);
	2: Data := ExtractFilePath(FOpenedFiles.GetItem(RowIndex).FileName);
	3: Data := DateTimeToS(FileTimeToDateTime(FOpenedFiles.GetItem(RowIndex).LastWriteTime), 0, ofDisplay);
	4: Data := NToS(FOpenedFiles.GetItem(RowIndex).SaveCount);
	5: Data := DateTimeToS(FOpenedFiles.GetItem(RowIndex).Created, 0, ofDisplay);
	6: Data := DateTimeToS(FOpenedFiles.GetItem(RowIndex).Modified, 0, ofDisplay);
	7: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).WorkTime, diSD);
	8: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).ModificationTime, diSD);
	9: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).SaveTime, diSD);
	end;
end;

constructor TfOpenedFiles.Create(const OpenedFiles: TOpenedFiles);
begin
	inherited Create(nil);

	FOpenedFiles := OpenedFiles;
end;

procedure TfOpenedFiles.DViewOpenedFilesKeyPress(Sender: TObject;
	var Key: Char);
begin
	if Key = Char(VK_ESCAPE) then
		Close;
end;

procedure TfOpenedFiles.FormDestroy(Sender: TObject);
begin
	MainIni.UnregisterRW(RWOptions);
end;

procedure TfOpenedFiles.DViewOpenedFilesKeyDown(Sender: TObject;
	var Key: Word; Shift: TShiftState);
var
	Direction: SG;
begin
	if Key = VK_TAB then
	begin
		if ssShift in Shift then
			Direction := -1
		else
			Direction := 1;
		TabKey(Direction);
	end
end;

procedure TfOpenedFiles.TabKey(const Direction: SG);
const MinValue = -1;
var i: SG;
begin
	i := DViewOpenedFiles.ActualRow + Direction;
	if i >= FOpenedFiles.Count then
		i := MinValue
	else if i < MinValue then
		i := FOpenedFiles.Count - 1;
	DViewOpenedFiles.RowCount := FOpenedFiles.Count;
	DViewOpenedFiles.ActualRow := i;
	DViewOpenedFiles.DataChanged;
end;

procedure TfOpenedFiles.DViewOpenedFilesColumnClick(Sender: TObject;
	Column: TColumn);
var
	i: SG;
	SortS: array of string;
	SortN: array of F8;
	Item: TOpenedFileItem;
begin
	case DViewOpenedFiles.SortBy of
	0, 1: SetLength(SortS, DViewOpenedFiles.RowCount);
	else SetLength(SortN, DViewOpenedFiles.RowCount);
	end;

//	FillOrderUG(DViewOpenedFiles.RowOrder[0], DViewOpenedFiles.RowCount);
	for i := 0 to DViewOpenedFiles.RowCount - 1 do
	begin
		Item := FOpenedFiles.GetItem(i);
		case DViewOpenedFiles.SortBy of
		1: SortS[i] := ExtractFileName(Item.FileName);
		2: SortS[i] := ExtractFilePath(Item.FileName);
		3: SortN[i] := FileTimeToDateTime(Item.LastWriteTime);
		4: SortN[i] := Item.SaveCount;
		5: SortN[i] := Item.Created;
		6: SortN[i] := Item.Modified;
		7: SortN[i] := Item.WorkTime;
		8: SortN[i] := Item.ModificationTime;
		9: SortN[i] := Item.SaveTime;
		end;
	end;
	case DViewOpenedFiles.SortBy of
	0, 1:
	begin
		SortStr(PArraySG(DViewOpenedFiles.RowOrder), PArrayString(SortS), DViewOpenedFiles.RowCount);
	end
	else
	begin
		SortF8(False, False, PArraySG(DViewOpenedFiles.RowOrder), PArrayF8(SortN), DViewOpenedFiles.RowCount);
	end;
	end;
	SetLength(SortS, 0);
	SetLength(SortN, 0);
end;

procedure TfOpenedFiles.FormShow(Sender: TObject);
begin
	DViewOpenedFiles.DeselectAll;
end;

procedure TfOpenedFiles.RWOptions(const Save: BG);
begin
	MainIni.RWFormPos(Self, Save);
	DViewOpenedFiles.Serialize(MainIni, Save);
end;

end.
