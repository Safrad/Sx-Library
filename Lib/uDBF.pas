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

type
	PColumn = ^TColumn;
	TColumn = record // 16
		Name: string;
		Typ: TVarType; // 2
		Reserved: U2;
		Width: S4;
		Items: array of Variant;
	end;

	TDBF = class
	private
		FileName: TFileName;
		Columns: array of TColumn;
		ColumnCount: SG;
	public
		Rows: array of BG; // True if row is enabled
		DbItemCount: SG;

		constructor Create;
		destructor Destroy; override;

		function FindColumn(Name: string): PColumn;
		procedure Close;

		function LoadFromFile(FName: TFileName): Boolean;
		function SaveToFile(FName: TFileName): Boolean;
	end;

implementation

uses
	Windows, Variants,
	uFiles, uStrings, uInput, uError;

procedure TDBF.Close;
var i, j: SG;
begin
	for i := 0 to ColumnCount - 1 do
	begin
		Columns[i].Name := '';
		Columns[i].Typ := varNull;
		for j := 0 to DbItemCount - 1 do
		begin
			Finalize(Columns[i].Items[j]);
		end;
	end;
	SetLength(Columns, 0);
	ColumnCount := 0;
	SetLength(Rows, 0);
	DbItemCount := 0;
	FileName := '';
end;

function TDBF.LoadFromFile(FName: TFileName): Boolean;
label LRetry, LExit;
type
	THead = packed record // 32
		YearOffset: U1; // $03=1900, $30=2000
		Year: U1;
		Month: U1;
		Day: U1;
		ItemsCount: U4;
		DataOffset: U2;
		RowSize: U2;
		R1, R2, R3, R4, R5: U4;
	end;
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

	DataWStr: WideString;
	DataStr: ShortString;

	Head: THead;
	Column: TColumn;
	NewSize: SG;
	Row, SRow, CRow: Pointer;
	RowSize, RowsSize: SG;
begin
	Result := False;
	FillChar(DataStr, SizeOf(DataStr), 0);
	F := TFile.Create;
	LRetry:
	Close;
	FileName := FName;
	SRow := nil;
	if F.Open(FileName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		// Header
		F.BlockRead(Head, SizeOf(Head));
		// Columns
		ColumnCount := 0;
		RowSize := 0;
		while True do
		begin
			F.BlockRead(Column, SizeOf(TColumn));
			if (U1(Column.Name[0]) = $0d) {and ((U1(Column.Name[1]) = $20) or (U1(Column.Name[1]) = $00))} then
			begin
				F.Seek(Head.DataOffset);
(*				while not F.Eof do
				begin
					F.BlockRead(c, 1);
					if c <> #0 {(c = $20) or (c = $2A)} then
					begin
						F.Seek(F.FilePos - 1); // Enable/Disable
						Break;
					end;
				end;*)

				Break;
			end;
			if F.Eof then goto LExit;

			NewSize := ColumnCount + 1;
			if AllocByExp(Length(Columns), NewSize) then
				SetLength(Columns, NewSize);
			Columns[ColumnCount].Name := Column.Name;
			case Column.Typ of
			'C'{ars}: Columns[ColumnCount].Typ := varString;
			'N'{umber}: Columns[ColumnCount].Typ := varInteger; // varDouble
			'L'{ogical}: Columns[ColumnCount].Typ := varBoolean;
			'W'{ide string}: Columns[ColumnCount].Typ := varOleStr;
//			'M'{Memo}: Columns[ColumnCount].Typ := varPointer;
{ 		'D': Columns[ColumnCount].Typ := varDate;}

			else Columns[ColumnCount].Typ := varUnknown;
			end;
			DelEndSpace(Columns[ColumnCount].Name);
			Columns[ColumnCount].Width := Column.Width;
			Inc(RowSize, Column.Width);
			Inc(ColumnCount);
		end;

		// Data
		Inc(RowSize);// Enable/Disable := 2 * ((RowSize + 1) div 2);
		if RowSize <> Head.RowSize then
		begin
			Head.RowSize := RowSize;
			IOErrorMessage(FileName, 'Row Size Mishmash');
		end;
		RowsSize := Head.ItemsCount * Head.RowSize;
		if RowsSize > F.FileSize - F.FilePos then
		begin
			Head.ItemsCount := (F.FileSize - F.FilePos) div RowSize;
			IOErrorMessage(FileName, 'File Truncated');
		end;
		if RowsSize + 1 < F.FileSize - F.FilePos then
			IOErrorMessage(FileName, 'File Too big');


		GetMem(SRow, RowsSize);
		F.BlockRead(SRow^, RowsSize);
		if not F.Close then goto LRetry;

		SetLength(Rows, Head.ItemsCount);
		for j := 0 to ColumnCount - 1 do
		begin
			SetLength(Columns[j].Items, Head.ItemsCount);
		end;
		DbItemCount := 0;
		CRow := SRow;
		while SG(CRow) < SG(SRow) + RowsSize do
		begin
(*			if F.FilePos >= F.FileSize then goto LExit; // All readed
			if F.FilePos + RowSize > F.FileSize then
			begin
				{$ifopt d+}
				IOErrorMessage(FileName, 'File Truncated');
				{$endif}
//				MessageD('File too short'., [mbOk]);
				goto LExit; // Cutted file
			end;
//			F.BlockRead(SRow^, RowsSize);*)

{			NewSize := DbItemCount + 1;
			if AllocByExp(Length(Rows), NewSize) then
				SetLength(Rows, NewSize);}

			Rows[DbItemCount] := Char(CRow^) = ' '; // 2A = Disabled
			CRow := Pointer(SG(CRow) + 1);
			for j := 0 to ColumnCount - 1 do
			begin
{				NewSize := DbItemCount + 1;
				if AllocByExp(Length(Columns[j].Items), NewSize) then
					SetLength(Columns[j].Items, NewSize);}

				Row := CRow;
				case Columns[j].Typ of
				varOleStr:
				begin
					for k := 0 to Columns[j].Width div 2 - 1 do
					begin
						if U2(Row^) = 0 then Break;
						Inc(SG(Row), 2)
					end;
					DataWStr := '';
					SetLength(DataWStr, k);
					if k >= 1 then
						Move(CRow^, DataWStr[1], 2 * k);
				end
				else
				begin
{					DataStr[0] := Char(Columns[j].Width);
					for k := 0 to Columns[j].Width - 1 do
					begin
						if Char(Row^) = #0 then Break;
//						DataStr[k + 1] := Char(Row^);
						Inc(SG(Row));
					end;}
					k := Columns[j].Width;
					DataStr[0] := Char(k);
					if k >= 1 then
						Move(CRow^, DataStr[1], k);
				end;
				end;
				CRow := Pointer(SG(CRow) + Columns[j].Width);

				case Columns[j].Typ of
				varString:
				begin
					Columns[j].Items[DbItemCount] := DataStr;
				end;
{				varPointer:
					Columns[j].Items[DbItemCount] := SG(s[1]) + SG(s[2]) shl 8 + SG(s[3]) shl 16 + SG(s[4]) shl 24;}
				varInteger:
				begin
					if Pos('.', DataStr) <> 0 then
					begin
						Columns[j].Typ := varDouble;
						Columns[j].Items[DbItemCount] := StrToF8(DataStr);
					end
					else
						Columns[j].Items[DbItemCount] := StrToSG(DataStr); //StrToValI(s, False, MinInt, 0, MaxInt, 1)

				end;
				varDouble:
					Columns[j].Items[DbItemCount] := StrToF8(DataStr);
				varBoolean: Columns[j].Items[DbItemCount] := (DataStr <> 'F');
				varOleStr:
				begin
					Columns[j].Items[DbItemCount] := DataWStr;
				end;
//				varDate:
				end;
			end;
			Inc(DbItemCount);
		end;
		LExit:
		FreeMem(SRow);
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
	inherited Create;
end;

destructor TDBF.Destroy;
begin
	Close;
	inherited Destroy;
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
