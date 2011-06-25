//* File:     Lib\GUI\uCSVAssign.pas
//* Created:  2004-01-04
//* Modified: 2008-03-14
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

// TODO : DNW

unit uCSVAssign; deprecated;

interface

uses
	uDForm, uTypes, uData,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDButton, uDLabel, uMain, uDWinControl;

type
	TfFormats = class(TDForm)
		ButtonOK: TDButton;
		LabelType: TDLabel;
		LabelFunction: TDLabel;
		ButtonCancel: TDButton;
		LabelName: TDLabel;
		procedure ButtonOKClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormCreate(Sender: TObject);
		procedure FormDblClick(Sender: TObject);
	private
		{ Private declarations }
		CSVFileName: TFileName;
		PanelNames: array of TDLabel;
		PanelTypes: array of TDLabel;
		ComboBoxes: array of TComboBox;
	public
		{ Public declarations }
		procedure FillComp;
	end;

var
	fFormats: TfFormats;

type
	TCSVValueType = (
		vtUnknown,
		vtNumeric,
		vtDecimal,
		vtInteger,
		vtSmallInt,
		vtFloat,
		vtReal,
		vtDouble,
		vtDoublePrecision,
		vtTinyInt,
		vtMediumInt,
		vtBigInt,
		vtDateTime,
		vtDate,
		vtTimeStamp,
		vtTime,
		vtYear,
		vtChar,
		vtVarChar,
		vtTinyBlob,
		vtBlob,
		vtMediumBlob,
		vtLongBlob,
		vtTinyText,
		vtText,
		vtMediumText,
		vtLongText,
		vtEnum,
		vtSet,

		vtLogical,
		vtMemo,
		vtOLE,
		vtBinary
		);
	PCSVFormat = ^TCSVFormat;
	TCSVFormat = packed record // 16
		Name: string; // 4
		ValueType: TCSVValueType; // 1
		Res0: array[0..1] of U1; // 3
		Precision: U2; // Numeric, Decimal
		Scale: U2;  // Numeric, Decimal
		Size: U2; // Float, TimeStamp, Char, VarChar,
		Unsigned: B1; // Integer, SmallInt, Float, Real, Double, DoublePrecision, TinyInt, MediumInt, BigInt
//		Dec{Flo Only}: U1; // 1
// Params: string; // 4 Enum, Set
		Reserve: array[0..0] of U1; // 1
	end;
var
	CSVFormats: TData;//array of TCSVFormat;
	CSVReqFormats: TData;//array of TCSVFormat;
	Columns: array of string;
	Indexes: array of SG;

	CSVSep: Char = ';';
	CSVFileName: TFileName;

function CSVRead(FileName: TFileName; Sep: Char): BG;
function CSVReadLine: BG;
function CSVClose: BG;

implementation

{$R *.DFM}
uses
	uDIni, uMsg, uFiles, uStrings, uFormat;

var
	CSVFile: TFile;

function CSVRead(FileName: TFileName; Sep: Char): BG;
label LRetry;
var
	i: SG;
	Line: string;
	InLineIndex: SG;
	Format: PCSVFormat;
begin
	Result := False;
	CSVSep := Sep;
	CSVFileName := FileName;
	if CSVReqFormats.Count = 0 then Exit;
	if not Assigned(fFormats) then
		fFormats := TfFormats.Create(nil);
	fFormats.CSVFileName := FileName;
	CSVFormats.Clear;
	CSVFile := TFile.Create;
	if CSVFile.Open(CSVFileName, fmReadOnly) then
	begin
		if not CSVFile.Readln(Line) then goto LRetry;

		InLineIndex := 1;
		while InLineIndex < Length(Line) do
		begin
			Format := CSVFormats.Add;
			Format.Name := ReadToChar(Line, InLineIndex, CSVSep);
			Format.ValueType := vtUnknown;
			Format.Precision := 0;
			Format.Scale := 0;
			Format.Size := 0;
			Format.Unsigned := False;
		end;
	end;
	SetLength(Columns, 0);
	SetLength(Columns, CSVFormats.Count);
	SetLength(Indexes, 0);
	SetLength(Indexes, CSVFormats.Count);
	for i := 0 to CSVFormats.Count - 1 do
		Indexes[i] := -1;

	fFormats.FillComp;
	fFormats.ShowModal;
	Result := fFormats.ModalResult = mrOk;
end;

function CSVReadLine: BG;
label LRetry;
var
	Line: string;
	i, InLineIndex: SG;
begin
	Result := False;
	if Assigned(CSVFile) and (CSVFile.Opened) then
	begin
		LRetry:
		if CSVFile.Readln(Line) then
		begin
			if Line = '' then goto LRetry;
			InLineIndex := 1;
			for i := 0 to CSVReqFormats.Count - 1 do
			begin
				if Indexes[i] >= 0 then
					Columns[Indexes[i]] := DelQuoteF(ReadToChar(Line, InLineIndex, CSVSep))
				else
					ReadToChar(Line, InLineIndex, CSVSep);
			end;
			Result := True;
		end;
	end;
end;

function CSVClose: BG;
begin
	Result := CSVFile.Close;
	FreeAndNil(CSVFile);
	SetLength(Columns, 0);
	SetLength(Indexes, 0);
end;

function IsSubTyp(F1, F2: PCSVFormat): BG; // ? F1 < F2
begin
{	if (F1.ValueType = vtUnknown)
	or (F2.ValueType = vtUnknown) then
		Result := True;}

	case F1.ValueType of
	vtChar:
	begin
		case F2.ValueType of
		vtChar: Result := F1.Size <= F2.Size;
		vtNumeric: Result := False;
		vtText: Result := True;
		else Result := True;
		end;
	end;
	vtText:
	begin
		case F2.ValueType of
		vtChar: Result := True;
		vtNumeric: Result := False;
		vtText: Result := True;
		else Result := True;
		end;
	end;
	vtNumeric:
	begin
		case F2.ValueType of
		vtChar: Result := F1.Precision + SG(F1.Scale <> 0) <= F2.Size;
		vtNumeric: Result := True;
		vtText: Result := True;
		else Result := True;
		end;
	end;
	else
		Result := True;
	end;
end;

procedure TfFormats.FillComp;
var
	i, j, k, l: Integer;
	Found: BG;
	s: string;
	Format, ReqFormat: PCSVFormat;
begin
	Caption := ExtractFileName(CSVFileName);

	SetLength(PanelNames, CSVReqFormats.Count);
	SetLength(PanelTypes, CSVReqFormats.Count);
	SetLength(ComboBoxes, CSVReqFormats.Count);
	SetLength(Indexes, CSVReqFormats.Count);
	ReqFormat := CSVReqFormats.GetFirst;
	for i := 0 to CSVReqFormats.Count - 1 do
	begin
		PanelNames[i] := TDLabel.Create(Self);
		PanelNames[i].SetBounds(LabelName.Left, 28 + 21 * i, LabelName.Width, 21);
		PanelNames[i].Alignment := taLeftJustify;
		PanelNames[i].BevelOuter := bvNone;
		PanelNames[i].BorderStyle := bsSingle;
		PanelNames[i].FontShadow := 1;
		PanelNames[i].Caption := NToS(i + 1) + ': ' + ReqFormat.Name;

		PanelTypes[i] := TDLabel.Create(Self);
		PanelTypes[i].SetBounds(LabelType.Left, 28 + 21 * i, LabelType.Width, 21);
		PanelTypes[i].Alignment := taLeftJustify;
		PanelTypes[i].BevelOuter := bvNone;
		PanelTypes[i].BorderStyle := bsSingle;
		PanelTypes[i].FontShadow := 1;

		case ReqFormat.ValueType of
		vtUnknown: s := 'Unknown';
		vtFloat:
		begin
			s := 'Float (' + NToS(ReqFormat.Size) + ')';
		end;
		vtNumeric:
		begin
			s := 'Numeric (' + NToS(ReqFormat.Precision) + ',' +
				NToS(ReqFormat.Scale) + ')';
		end;
		vtChar:
		begin
			s := 'Char (' + NToS(ReqFormat.Size) + ')';
		end;
		else
			s := 'N/A';
		end;
		PanelTypes[i].Caption := s;

		ComboBoxes[i] := TComboBox.Create(Self);
		ComboBoxes[i].SetBounds(LabelFunction.Left, PanelNames[i].Top, LabelFunction.Width, ComboBoxes[i].Height);
		ComboBoxes[i].Style := csDropDownList;
		ComboBoxes[i].DropDownCount := 24;


		InsertControl(ComboBoxes[i]);
		k := 0;
		Format := CSVFormats.GetFirst;
		for j := 0 to CSVFormats.Count - 1 do
		begin
			if IsSubTyp(Format, ReqFormat) then
			begin
				ComboBoxes[i].Items.Add(Format.Name);

				Found := False;
				for l := 0 to i - 1 do
				begin
					if Indexes[l] = j then
					begin
						Found := True;
						Break;
					end;
				end;

				if Found = False then
				begin
					if k = 0 then
					begin
						Indexes[i] := j;
						ComboBoxes[i].ItemIndex := j;
					end;
					Inc(k);
				end;
			end;
		Inc(SG(Format), CSVFormats.ItemMemSize);
		end;

		InsertControl(PanelNames[i]);
		InsertControl(PanelTypes[i]);
		Inc(SG(ReqFormat), CSVReqFormats.ItemMemSize);
	end;
	ButtonOk.Top := PanelNames[CSVReqFormats.Count - 1].Top + PanelNames[CSVReqFormats.Count - 1].Height + 8;
	ButtonCancel.Top := ButtonOk.Top;
	ClientHeight := ButtonOk.Top + ButtonOk.Height + 8;
end;

procedure TfFormats.ButtonOKClick(Sender: TObject);
var
	i, j: SG;
	ReqFormat: PCSVFormat;
begin
	ReqFormat := CSVReqFormats.GetFirst;
	for i := 0 to CSVReqFormats.Count - 1 do
	begin
		Indexes[i] := ComboBoxes[i].ItemIndex; // TODO : Func
	end;

	for i := 0 to CSVReqFormats.Count - 1 do
	begin
		if ComboBoxes[i].ItemIndex = -1 then
		begin
			Warning('Please select all columns.');
			Exit;
		end;

		for j := i + 1 to CSVReqFormats.Count - 1 do
			if Indexes[i] = Indexes[j] then
			begin
				Warning('Columns collision ' + NToS(i + 1) + ' - ' + NToS(j + 1) + '.');
				Exit;
			end;

//		Format.Func := ComboIndex[i][ComboBoxs[i].ItemIndex];
		Inc(SG(ReqFormat), CSVReqFormats.ItemMemSize);
	end;
	ModalResult := mrOk;
end;

procedure TfFormats.FormClose(Sender: TObject; var Action: TCloseAction);
//var i: Integer;
begin
{	TODO : for i := 0 to CSVReqFormats.Count - 1 do
	begin
		RemoveControl(PanelNames[i]);
		FreeAndNil(PanelNames[i]);
		RemoveControl(PanelTypes[i]);
		FreeAndNil(PanelTypes[i]);
		RemoveControl(ComboBoxes[i]);
		FreeAndNil(ComboBoxes[i]);
		SetLength(PanelNames, 0);
		SetLength(PanelTypes, 0);
		SetLength(ComboBoxes, 0);
	end;}
	MainIni.RWFormPos(Self, True);
end;

procedure TfFormats.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	MainIni.RWFormPos(Self, False);
end;

procedure TfFormats.FormDblClick(Sender: TObject);
begin
	// Auto Assign
end;

initialization
	CSVFormats := TData.Create;
	CSVFormats.ItemSize := SizeOf(TCSVFormat);
	CSVReqFormats := TData.Create;
	CSVReqFormats.ItemSize := SizeOf(TCSVFormat);
finalization
	FreeAndNil(CSVReqFormats);
	FreeAndNil(CSVFormats);
end.
