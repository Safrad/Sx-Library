unit uDBFReader;

interface

uses
  SysUtils,

  uTypes,
  uRawFile,
  uDBF;

type
  TDBFFileType = (
    FoxBASE = $02,
    /// <summary>FoxBASE+/Dbase III plus, no memo</summary>
    FoxBASEdBase3NoMemo = $03,
    /// <summary>Visual FoxPro</summary>
    VisualFoxPro = $30,
    /// <summary>Visual FoxPro, autoincrement enabled</summary>
    VisualFoxProAutoincrement = $31,
    /// <summary>Visual FoxPro with field type Varchar or Varbinary</summary>
    VisualFoxProWithFieldTypeVarcharOrVarbinary = $32,
    /// <summary>dBASE IV SQL table files, no memo</summary>
    dBASEIVSQLTableFilesNoMemo = $43,
    /// <summary>dBASE IV SQL system files, no memo</summary>
    dBASEIVSQLSystemFilesNoMemo = $63,
    /// <summary>FoxBASE+/dBASE III PLUS, with memo</summary>
    FoxBASEdBASEIIIPLUSWithMemo = $83,
    /// <summary>dBASE IV with memo</summary>
    dBASEIVWithMemo = $8B,
    /// <summary>dBASE IV SQL table files, with memo</summary>
    dBASEIVSQLTableFilesWithMemo = $CB,
    /// <summary>FoxPro 2.x (or earlier) with memo</summary>
    FoxPro2OrEarlierWithMemo = $F5,
    /// <summary>HiPer-Six format with SMT memo file</summary>
    HiPerSixFormatWithSMTMemoFile = $E5,
    FoxBASEX = $FB
    );

  TTableFlag = (
    /// <summary>File has a structural .cdx</summary>
    StructuralCDX,
    /// <summary>File has a Memo field</summary>
    MemoField,
    /// <summary>File is a database (.dbc)</summary>
    DatabaseDBC4
    );

  TTableFlags = set of TTableFlag;

	TDBFHeader = packed record // 32
		FileType: TDBFFileType;
		Year: U1;
		Month: U1;
		Day: U1;
		ItemsCount: U4;
		DataOffset: U2;
		RowSize: U2;
		R1, R2, R3, R4: U4;
    TableFlags: TTableFlags;
    CodePageMark: U1;
    Reserved: U2;
	end;

  TColumnTag = (
    /// <summary>Not visible to user</summary>
    SystemColumn,
    /// <summary>Column can store null values</summary>
    NullValues,
    /// <summary>Binary column (for CHAR and MEMO only)<summary>
    Binary,
    /// <summary>Column is autoincrementing</summary>
    Autoincrement
    );

  TColumnTags = set of TColumnTag;

	TBinaryColumn = packed record // 32
		Name: array[0..10] of AnsiChar; // 11
		Typ: AnsiChar; // 1
		Offset: U4;
		Width: U1;
		Decimal: U1;
    Flags: TColumnTags;
    ValueOfAutoincrementNextValue: U4;
    ValueOfAutoincrementStepValue: U1;
		Reserved: U8;
	end;

  TDBFReader = class
  private
    F: TRawFile;
    Header: TDBFHeader;
    FFileEncoding: TEncoding;
    FDBF: TDBF;
    FFileName: TFileName;
    FPTSize: UG;
    FPT: RawByteString;
    FRowSize: UG;

    procedure ReadHeader;
    procedure ReadColumns;

    function CodePageMarkToCodePage(const ACodePageMark: U1): U2;
    function GetOleStringCharCount(const AMaxWidth: SG; ARow: Pointer): UG;
    function ReadOleString(ARow: Pointer; const AMaxWidth: SG): string;
    function ReadString(ARow: Pointer; const AMaxWidth: SG): string;
    procedure ReadCell(const AItemIndex: SG; const ARow: Pointer; const ADBFColumn: TDBFColumn);
    procedure ReadRow(const AItemIndex: SG; var ARow: Pointer);
    procedure ReadRows;

    procedure SetDBF(const Value: TDBF);
    procedure SetFileName(const Value: TFileName);
  public
    property DBF: TDBF read FDBF write SetDBF;
    property FileName: TFileName read FFileName write SetFileName;
    procedure Read;
    class procedure ReadDBFFromFile(const ADBF: TDBF; const AFileName: TFileName);
  end;

implementation

uses
  uFiles, uStrings, uInputFormat, uMsg, uMath;

{ TDBFReader }

function TDBFReader.CodePageMarkToCodePage(const ACodePageMark: U1): U2;
begin
  case ACodePageMark of
  $00: Result := 852; // Default - Eastern European MS-DOS
  $01: Result := 437; // US MS-DOS
  $02: Result := 850; // International MS-DOS
  $03: Result := 1252; // Windows ANSI
  $04: Result := 10000; // Standard MacIntosh
  $64: Result := 852; // Eastern European MS-DOS
  $65: Result := 866; // Russian MS-DOS
  $66: Result := 865; // Nordic MS-DOS
  $67: Result := 861; // Icelandic MS-DOS
  $68: Result := 895; // Kamenicky (Czech) MS-DOS
  $69: Result := 620; // Mazovia (Polish) MS-DOS
  $6A: Result := 737; // Greek MS-DOS (437G)
  $6B: Result := 857; // Turkish MS-DOS
  $78: Result := 950; // Chinese (Hong Kong SAR, Taiwan) Windows
  $79: Result := 949; // Korean Windows
  $7A: Result := 936; // Chinese (PRC, Singapore) Windows
  $7B: Result := 932; // Japanese Windows
  $7C: Result := 874; // Thai Windows
  $7D: Result := 1255; // Hebrew Windows
  $7E: Result := 1256; // Arabic Windows
  $96: Result := 10007; // Russian MacIntosh
  $97: Result := 10029; // MacIntosh EE
  $98: Result := 10006; // Greek MacIntosh
  $C8: Result := 1250; // Eastern European Windows
  $C9: Result := 1251; // Russian Windows
  $CA: Result := 1254; // Turkish Windows
  $CB: Result := 1253; // Greek Windows
  else
    raise EArgumentException.Create('Invalid code page mark.');
  end;
end;

procedure TDBFReader.Read;
begin
	FDBF.Clear;

	F := TRawFile.Create;
	try
    F.FileName := FFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    ReadHeader;
    FFileEncoding := TEncoding.GetEncoding(CodePageMarkToCodePage(Header.CodePageMark));
    try
      ReadColumns;
      if F.Eof then
         Exit;
      ReadRows;
    finally
      FFileEncoding.Free;
    end;
	finally
		F.Free;
	end;
end;

function TDBFReader.GetOleStringCharCount(const AMaxWidth: SG; ARow: Pointer): UG;
var
  i: SG;
begin
  Result := AMaxWidth div SizeOf(Char);
  for i := 0 to AMaxWidth div SizeOf(Char) - 1 do
  begin
    if U2(ARow^) = 0 then
    begin
      Result := i;
      Break;
    end;
    Inc(PByte(ARow), SizeOf(Char));
  end;
end;

function TDBFReader.ReadOleString(ARow: Pointer; const AMaxWidth: SG): string;
var
  CharCount: SG;
begin
  CharCount := GetOleStringCharCount(AMaxWidth, ARow);

  if CharCount >= 1 then
  begin
    SetLength(Result, CharCount);
    Move(ARow^, Result[1], SizeOf(Char) * CharCount);
  end;
end;

procedure TDBFReader.ReadCell(const AItemIndex: SG; const ARow: Pointer; const ADBFColumn: TDBFColumn);
var
  DataAsString: string;
  Tmp: RawByteString;
  Size: SG;
  Index: UG;
begin
  case ADBFColumn.Typ of
    varString:
      begin
        ADBFColumn.Items[AItemIndex] := ReadString(ARow, ADBFColumn.Width);
      end;
    varInteger:
      begin
        DataAsString := ReadString(ARow, ADBFColumn.Width);
        if Pos('.', DataAsString) <> 0 then
        begin
          ADBFColumn.Typ := varDouble;
          ADBFColumn.Items[AItemIndex] := StrToF8(DataAsString, ifIO);
        end
        else
          ADBFColumn.Items[AItemIndex] := StrToSG(DataAsString, ifIO);
      end;
    varDouble:
      ADBFColumn.Items[AItemIndex] := StrToF8(ReadString(ARow, ADBFColumn.Width), ifIO);
    varBoolean:
      ADBFColumn.Items[AItemIndex] := (ReadString(ARow, ADBFColumn.Width) <> 'F');
    varOleStr:
      begin
        ADBFColumn.Items[AItemIndex] := ReadOleString(ARow, ADBFColumn.Width);
      end;
    varUnknown:
      begin
        Index := U4(ARow^);
        if Index <> 0 then
        begin
          if IsDebug then
            Assert(SwapU4(PU4(@FPT[FPTSize * Index + 1])^) = 1);
          Size := SwapU4(PU4(@FPT[FPTSize * Index + 5])^);
          SetLength(Tmp, Size);
          ClearMemory(Tmp[1], SizeOf(Tmp[1]) * Size);
          if Size > 0 then
            Move(FPT[FPTSize * Index + 9], Tmp[1], Size);
          ADBFColumn.Items[AItemIndex] := string(Tmp);
          Tmp := '';
        end
        else
          ADBFColumn.Items[AItemIndex] := '';
    end;
  end;
end;

procedure TDBFReader.ReadRow(const AItemIndex: SG; var ARow: Pointer);
var
  DBFColumn: TDBFColumn;
begin
  // 2A = Disabled
  FDBF.EnabledRows[AItemIndex] := AnsiChar(ARow^) = ' ';

  ARow := Pointer(SG(ARow) + 1);
  for DBFColumn in FDBF.Columns do
  begin
    ReadCell(AItemIndex, ARow, DBFColumn);
    ARow := Pointer(SG(ARow) + DBFColumn.Width);
  end;
end;

procedure TDBFReader.ReadRows;
var
  AllRowsSize: UG;
  AllRows: Pointer;
  Row: Pointer;
  FItemCount: SG;
begin
  Inc(FRowSize);
  // Enable/Disable := 2 * ((RowSize + 1) div 2);
  if FRowSize <> Header.RowSize then
  begin
    Header.RowSize := FRowSize;
    IOErrorMessage(FFileName, 'Row size mishmash.');
  end;
  AllRowsSize := Header.ItemsCount * Header.RowSize;
  if AllRowsSize > F.FileSize - F.FilePos then
  begin
    Header.ItemsCount := (F.FileSize - F.FilePos) div FRowSize;
    IOErrorMessage(FFileName, 'File truncated.');
  end;
  if AllRowsSize + 1 < F.FileSize - F.FilePos then
    IOErrorMessage(FFileName, 'File too large.');
  GetMem(AllRows, AllRowsSize);
  try
    F.BlockRead(AllRows^, AllRowsSize);
    F.Close;
    FDBF.RowCount := Header.ItemsCount;
    FItemCount := 0;
    Row := AllRows;
    while UG(Row) < UG(AllRows) + AllRowsSize do
    begin
      ReadRow(FItemCount, Row);
      Inc(FItemCount);
    end;
  finally
    FreeMem(AllRows);
  end;
  FPT := '';
end;

function TDBFReader.ReadString(ARow: Pointer; const AMaxWidth: SG): string;
var
  DataAsBytes: TBytes;
begin
  SetLength(DataAsBytes, AMaxWidth);
  if AMaxWidth >= 1 then
    Move(ARow^, DataAsBytes[0], AMaxWidth);
  Result := TEncoding.Unicode.GetString(TEncoding.Convert(FFileEncoding, TEncoding.Unicode, DataAsBytes));
end;

procedure TDBFReader.ReadColumns;
var
  Column: TBinaryColumn;
  DBFColumn: TDBFColumn;
begin
	FPTSize := 0;
  FDBF.Columns.Clear;
  FRowSize := 0;
  while True do
  begin
    F.BlockRead(Column, SizeOf(TBinaryColumn));
    if (U1(Column.Name[0]) = $D) then
    {and ((U1(Column.Name[1]) = $20) or (U1(Column.Name[1]) = $00))}
    begin
      F.Seek(Header.DataOffset);
      Break;
    end;
    DBFColumn := TDBFColumn.Create;
    DBFColumn.Name := string(Column.Name);
    case Column.Typ of
      'C':
        // Chars
        DBFColumn.Typ := varString;
      'N':
        // Number
        DBFColumn.Typ := varInteger;
      'L':
        // Logical
        DBFColumn.Typ := varBoolean;
      'W':
        // Wide string
        DBFColumn.Typ := varOleStr;
      'M':
        // Memo
        begin
          DBFColumn.Typ := varUnknown;
          if FPTSize = 0 then
          begin
            FPT := RawByteString(ReadStringFromFile(DelFileExt(FFileName) + '.fpt'));
            if Length(FPT) >= 8 then
            begin
              FPTSize := Ord(FPT[5]) shl 24 + Ord(FPT[6]) shl 16 + Ord(FPT[7]) shl 8 + Ord(FPT[8]);
            end
            else
              FPTSize := 0;
          end;
        end;
      'D':
        DBFColumn.Typ := varString;
    else
      DBFColumn.Typ := varNull;
    end;
    DelEndSpace(DBFColumn.Name);
    DBFColumn.Width := Column.Width;
    Inc(FRowSize, Column.Width);
    FDBF.Columns.Add(DBFColumn);
  end;
end;

class procedure TDBFReader.ReadDBFFromFile(const ADBF: TDBF; const AFileName: TFileName);
var
  DBFReader: TDBFReader;
begin
  DBFReader := TDBFReader.Create;
  try
    DBFReader.DBF := ADBF;
    DBFReader.FileName := AFileName;
    DBFReader.Read;
  finally
    DBFReader.Free;
  end;
end;

procedure TDBFReader.ReadHeader;
var
  Year: SG;
begin
  F.BlockRead(Header, SizeOf(Header));
  case Header.FileType of
    TDBFFileType.FoxBASEdBase3NoMemo:
      Year := 1900;
  else
    Year := 2000;
  end;
  FDBF.FileDate := EncodeDate(Header.Year + Year, Header.Month, Header.Day);
end;

procedure TDBFReader.SetDBF(const Value: TDBF);
begin
  FDBF := Value;
end;

procedure TDBFReader.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

initialization
  Assert(SizeOf(TDBFHeader) = 32);
  Assert(SizeOf(TBinaryColumn) = 32);
end.
