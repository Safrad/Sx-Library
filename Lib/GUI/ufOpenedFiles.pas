//* File:     Lib\GUI\ufOpenedFiles.pas
//* Created:  2007-11-25
//* Modified: 2008-01-20
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit ufOpenedFiles;

interface

uses
	uTypes,	uOpenedFiles,
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
	public
		{ Public declarations }
		constructor Create(const OpenedFiles: TOpenedFiles);
		procedure TabKey(const Direction: SG);
	end;

var
	fOpenedFiles: TfOpenedFiles;

implementation

{$R *.dfm}
uses
	uDIniFile, uOutputFormat, uFiles, uSorts, uMath;

procedure TfOpenedFiles.FormCreate(Sender: TObject);
begin
	inherited;
	DViewOpenedFiles.AddColumn('File name', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Path', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('LastWriteTime', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('SaveCount', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Created', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('Modified', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('WorkTime', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('ModificationTime', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	DViewOpenedFiles.AddColumn('SaveTime', DViewOpenedFiles.Width div 2, taLeftJustify, True);
	MainIni.RWFormPos(Self, False);
	DViewOpenedFiles.Serialize(MainIni, False);
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
begin
	if RowIndex = FOpenedFiles.Index then
		DViewOpenedFiles.Bitmap.Canvas.Font.Style := [fsBold]
	else
		DViewOpenedFiles.Bitmap.Canvas.Font.Style := [];
	case ColIndex of
	0: Data := ExtractFileName(FOpenedFiles.GetItem(RowIndex).FileName);
	1: Data := ExtractFilePath(FOpenedFiles.GetItem(RowIndex).FileName);
	2: Data := DateTimeToS(FileTimeToDateTime(FOpenedFiles.GetItem(RowIndex).LastWriteTime), 0, ofDisplay);
	3: Data := NToS(FOpenedFiles.GetItem(RowIndex).SaveCount);
	4: Data := DateTimeToS(FOpenedFiles.GetItem(RowIndex).Created, 0, ofDisplay);
	5: Data := DateTimeToS(FOpenedFiles.GetItem(RowIndex).Modified, 0, ofDisplay);
	6: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).WorkTime, diSD);
	7: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).ModificationTime, diSD);
	8: Data := MsToStr(FOpenedFiles.GetItem(RowIndex).SaveTime, diSD);
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
	MainIni.RWFormPos(Self, True);
	DViewOpenedFiles.Serialize(MainIni, True);
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
	Item: POpenedFileItem;
begin
	case DViewOpenedFiles.SortBy of
	0, 1: SetLength(SortS, DViewOpenedFiles.RowCount);
	else SetLength(SortN, DViewOpenedFiles.RowCount);
	end;

	FillOrderU4(DViewOpenedFiles.RowOrder[0], DViewOpenedFiles.RowCount);
	for i := 0 to DViewOpenedFiles.RowCount - 1 do
	begin
		Item := FOpenedFiles.GetItem(i);
		case DViewOpenedFiles.SortBy of
		0: SortS[i] := ExtractFileName(Item.FileName);
		1: SortS[i] := ExtractFilePath(Item.FileName);
		2: SortN[i] := FileTimeToDateTime(Item.LastWriteTime);
		3: SortN[i] := Item.SaveCount;
		4: SortN[i] := Item.Created;
		5: SortN[i] := Item.Modified;
		6: SortN[i] := Item.WorkTime;
		7: SortN[i] := Item.ModificationTime;
		8: SortN[i] := Item.SaveTime;
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

end.
