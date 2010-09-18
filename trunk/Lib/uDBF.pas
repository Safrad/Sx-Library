//* File:     Lib\uDBF.pas
//* Created:  1999-12-01
//* Modified: 2004-10-31
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDBF;

interface

uses SysUtils, uAdd;

const
	PreHeadSize = 16;
type
	PColumn = ^TColumn;
	TColumn = record // 16
		Name: string;
		Typ: Variant;
		Width: S4;
		Items: array of Variant;
	end;

	TDBF = class
	private
		Columns: array of TColumn;
		ColumnCount: UG;

	public

		DbItemCount: SG;
		FileName: TFileName;
		IsNew: Boolean;



		constructor Create;
		destructor Destroy; override;
//		procedure New(const NewDbDataSize: Integer; const HeadId: TDbHeadId; const HeadVersion: LongWord);

		function FindColumn(Name: string): PColumn;
		procedure Close;
		procedure SetItems(ItemsCount: Integer);
		procedure CopyItem(Source, Dest: Integer);
		procedure FreeItem(Item: Integer);

		function LoadFromFile(FName: TFileName): Boolean;
		function SaveToFile(FName: TFileName): Boolean;
	end;

implementation

uses
	Windows,
	uFiles, uStrings, uInput;
{
procedure TDBF.New(const NewDbDataSize: Integer; const HeadId: TDbHeadId; const HeadVersion: LongWord);
begin
	DbItemCount := -1;
	DbItemSize := NewDbDataSize;
	SetItems(0);
	Id := HeadId;
	Version := HeadVersion;
	Head.Id := HeadId;
	Head.Version := HeadVersion;
	Head.HeadSize := SizeOf(TDbHead);
	Head.ItemSize := SizeOf(TDbItem) - 4 + DbItemSize;
	Head.SaveCount := 0;
	Head.Modified := 0;
	IsNew := True;
end;}

procedure TDBF.Close;
begin
	SetItems(-1);
end;

procedure TDBF.CopyItem(Source, Dest: Integer);
begin

end;

procedure TDBF.FreeItem(Item: Integer);
begin

end;

procedure TDBF.SetItems(ItemsCount: Integer);
var i: Integer;
begin
	if ItemsCount = DbItemCount then Exit;
	if ItemsCount < DbItemCount then
	begin
		for i := ItemsCount + 1 to DbItemCount do
		begin
			FreeItem(i);
		end;
	end;
{	SetLength(DbItems, ItemsCount + 1);
	if ItemsCount > DbItemCount then
	begin
		for i := DbItemCount + 1 to ItemsCount do
		begin
			FillChar(DbItems[i], SizeOf(DbItems[i]), 0);
			GetMem(DbItems[i].PData, DbItemSize);
			FillChar(DbItems[i].PData^, DbItemSize, 0);
		end;
	end;
	DbItemCount := ItemsCount;}
end;

function TDBF.LoadFromFile(FName: TFileName): Boolean;
label LRetry, LExit;
type
	TColumn = packed record // 32
		Name: array[0..10] of Char; // 11
		Typ: Char; // 1
		Offset: U4;
		Width: U1;
		Decimal: U1;
		Reserved0: array[0..5] of U1;
		Reserved: U8;
	end;

var
	F: TFile;
	j, k: SG;
	s: string;
	Column: TColumn;
	NewSize, Offset: SG;
	Row: PArrayChar;
	RowSize: SG;
begin
	Result := False;
	Row := nil;
	F := TFile.Create;
	LRetry:
	Close;
	FileName := FName;
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		// Head
		F.Seek(32);
		ColumnCount := 0;
		RowSize := 0;
		while True do
		begin
			F.BlockRead(Column, SizeOf(TColumn));
			if (U1(Column.Name[0]) = $0d) and (U1(Column.Name[1]) = $20) then
			begin
				F.Seek(F.FilePos - 30);
				Break;
			end;
			if F.Eof then goto LExit;

			NewSize := ColumnCount + 1;
			if AllocByEx(Length(Columns), NewSize, SizeOf(Columns[0])) then
				SetLength(Columns, NewSize);
			Columns[ColumnCount].Name := Column.Name;
			case Column.Typ of
			'C': Columns[ColumnCount].Typ := vtChar;
			'N': Columns[ColumnCount].Typ := vtInteger;
{ 		'D': Columns[ColumnCount].Typ := vtDate;

	vtBoolean    = 1;
	vtChar       = 2;
	vtExtended   = 3;
	vtString     = 4;
	vtPointer    = 5;
	vtPChar      = 6;
	vtObject     = 7;
	vtClass      = 8;
	vtWideChar   = 9;
	vtPWideChar  = 10;
	vtAnsiString = 11;
	vtCurrency   = 12;
	vtVariant    = 13;
	vtInterface  = 14;
	vtWideString = 15;
	vtInt64      = 16;}
			else Columns[ColumnCount].Typ := vtPointer;
			end;
			DelEndSpace(Columns[ColumnCount].Name);
			Columns[ColumnCount].Width := Column.Width;
			Inc(RowSize, Column.Width);
			Inc(ColumnCount);
		end;

		// Data
		Inc(RowSize);// := 2 * ((RowSize + 1) div 2);
		GetMem(Row, RowSize);
		DbItemCount := 0;
		while True do
		begin
			F.BlockRead(Row^, RowSize);
			if F.Eof then goto LExit;

			Offset := 0;
			for j := 0 to ColumnCount - 1 do
			begin
				NewSize := DbItemCount + 1;
				if AllocByEx(Length(Columns[j].Items), NewSize, SizeOf(Columns[j].Items[0])) then
					SetLength(Columns[j].Items, NewSize);

				s := '';
				for k := 0 to Columns[j].Width - 1 do
					s := s + Row^[k + Offset];

				case Columns[j].Typ of
				vtChar: Columns[j].Items[DbItemCount] := s;
				vtInteger:
				begin
					if Pos('.', s) <> 0 then
					begin
						Columns[j].Typ := vtExtended;
						Columns[j].Items[DbItemCount] := StrToFA(s);
					end
					else
						Columns[j].Items[DbItemCount] := StrToSG(s); //StrToValI(s, False, MinInt, 0, MaxInt, 1)

				end;
				vtExtended:
						Columns[j].Items[DbItemCount] := StrToFA(s);
//				vtDate:                                   


				end;
				Inc(Offset, Columns[j].Width);
			end;
			Inc(DbItemCount);
		end;
		LExit:
		FreeMem(Row);
		if not F.Close then goto LRetry;
		Result := True;
	end;
	F.Free;
end;

function TDBF.SaveToFile(FName: TFileName): Boolean;
begin
	Result := False;
end;

constructor TDBF.Create;
begin
	inherited;
	IsNew := True;
end;

destructor TDBF.Destroy;
begin
	Close;
  inherited;
end;

function TDBF.FindColumn(Name: string): PColumn;
var
	i: SG;
	NameU: string;
begin
	Result := nil;
	NameU := UpperCase(Name);
	for i := 0 to ColumnCount - 1 do
	begin
		if NameU = Columns[i].Name then
		begin
			Result := @Columns[i];
		end;
	end;
end;

end.
