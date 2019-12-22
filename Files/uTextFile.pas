unit uTextFile;

{$ZEROBASEDSTRINGS OFF}

interface

uses
  uTypes,
  uFileCharset,
  uBufferedRawFile;

type
  TTextFile = class(TBufferedRawFile)
  private
    FLastCharCR: BG;
		FCharset: TFileCharset;

    // Used if BOM not found
		FDefaultCharset: TFileCharset;

    FUseByteOrderMark: BG;
    FDataSize: U8;
		procedure ReadPrefix;
    procedure SetDefaultCharsetIfNotInFile;
		procedure ForceWritePrefix;
    procedure SetUseByteOrderMark(const Value: BG);
    function ConvertBuffer(const ABuffer: array of U1): string;
    procedure RaiseUnsupportedCharset;
    procedure WritePrefixIfEnabled;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
		property DefaultCharset: TFileCharset read FDefaultCharset write FDefaultCharset;
    property UseByteOrderMark: BG read FUseByteOrderMark write SetUseByteOrderMark;

    // Process
    procedure Open; override;

{$ifdef MSWINDOWS}
		procedure ReadLine(out Line: AnsiString); overload;
{$endif}
		procedure ReadLine(out Line: string); overload;

{$ifdef MSWINDOWS}
		procedure Write(const Line: AnsiString); overload;
{$endif}
		procedure Write(const Line: UnicodeString); overload;
		procedure WriteNoConversion(const Line: PByte; const LineLength: SG); overload;
		procedure WriteNoConversion(const Line: RawByteString); overload;
{$ifdef MSWINDOWS}
		procedure WriteLine(const Line: AnsiString); overload;
{$endif}
		procedure WriteLine(const Line: UnicodeString); overload;

    function ReadAsString(const ALimit: U8 = 0): string;

    // Output
		property Charset: TFileCharset read FCharset;
    property DataSize: U8 read FDataSize;
  end;

implementation

uses
  SysUtils,
  uRawFile,
  Math,
  uChar,
  uCharset,
  uStrings,
  uEIOException;

{ TTextFile }

constructor TTextFile.Create;
begin
  inherited;

	FDefaultCharset := fcUnknown;
	FCharset := fcUnknown;
  FUseByteOrderMark := True;
end;

destructor TTextFile.Destroy;
begin

  inherited;
end;

procedure TTextFile.Open;
begin
  inherited;

	case FileMode of
	fmReadOnly, fmReadAndWrite:
		begin
			if FileSize > 0 then
			begin
        ReadPrefix;
        Seek(Length(ByteOrderMarks[FCharset]));
        SetDefaultCharsetIfNotInFile;
      end
			else if FileMode = fmReadAndWrite then
      begin
        WritePrefixIfEnabled;
      end;
		end;
	fmRewrite:
		begin
      WritePrefixIfEnabled;
		end;
	end;
end;

procedure TTextFile.ReadLine(out Line: string);
const
  StartBufferSize = 256;
var
	InLineIndex: SG;
	BufferSize: SG;
  ReadedU1: U1;
  ReadedU2: U2;
  Buffer: array of Byte;
  CR: U2;
  LF: U2;
begin
	Line := '';
	if Eof then
	begin
		Exit;
	end;

  case FCharset of
  fcUTF16LE:
  begin
    CR := U2(CharCR);
    LF := U2(CharLF);
  end;
  fcUTF16BE:
  begin
    CR := Swap(U2(CharCR));
    LF := Swap(U2(CharLF));
  end;
  fcAscii, fcAnsi, fcUTF8:
  begin
    CR := 0;
    LF := 0;
  end;
  else
    RaiseUnsupportedCharset;
    Exit;
  end;

	BufferSize := StartBufferSize;
  SetLength(Buffer, BufferSize);
	InLineIndex := 0;
	while not Eof do
	begin
    case FCharset of
    fcAscii, fcAnsi, fcUTF8:
    begin
      ReadedU1 := ReadU1;
      if ReadedU1 = U1(CharCR) then
      begin
        FLastCharCR := True;
        Break; // Line ready
      end
      else if ReadedU1 = U1(CharLF) then
      begin
        if FLastCharCR then
        begin
          FLastCharCR := False;
          Continue; // Skip CharLF
        end
        else
        begin
          FLastCharCR := False;
          Break; // Line ready
        end;
      end
      else
      begin
        FLastCharCR := False;

        Buffer[InLineIndex] := ReadedU1;
        Inc(InLineIndex);
      end;
    end;
    fcUTF16BE, fcUTF16LE:
    begin
      ReadedU2 := ReadU2;

      if ReadedU2 = CR then
      begin
        FLastCharCR := True;
        Break; // Line ready
      end
      else if ReadedU2 = LF then
      begin
        if FLastCharCR then
        begin
          FLastCharCR := False;
          Continue; // Skip CharLF
        end
        else
        begin
          FLastCharCR := False;
          Break; // Line ready
        end;
      end
      else
      begin
        FLastCharCR := False;
        Buffer[InLineIndex] := TU2(ReadedU2).B0;
        Buffer[InLineIndex + 1] := TU2(ReadedU2).B1;
        Inc(InLineIndex, 2);
      end;
		end;
    end;

    if InLineIndex + 1 > BufferSize then
    begin
      BufferSize := 2 * BufferSize;
      SetLength(Buffer, BufferSize);
    end;
	end;
  SetLength(Buffer, InLineIndex);
  Line := ConvertBuffer(Buffer);
end;

{$ifdef MSWINDOWS}
procedure TTextFile.ReadLine(out Line: AnsiString);
var
  s: string;
begin
  ReadLine(s);
  Line := AnsiString(s); // Convert to Ansi
end;
{$endif}

procedure TTextFile.RaiseUnsupportedCharset;
begin
  raise ENotSupportedException.Create(ReplaceParam('Unsupported charset in file %1', [FileName]));
end;

function TTextFile.ReadAsString(const ALimit: U8 = 0): string;
var
	Buffer: TBytes;
  DataToRead: U8;
begin
  DataToRead := DataSize;

  if ALimit > 0 then
    DataToRead := Min(ALimit, DataSize);

  if DataToRead = 0 then
    Exit;

  case FCharset of
    fcAscii, fcAnsi:
    begin
      SetLength(Buffer, DataToRead);
      BlockRead(Buffer[0], DataToRead);
    end;
    fcUTF8:
    begin
      SetLength(Buffer, DataToRead);
      BlockRead(Buffer[0], DataToRead);
    end;
    fcUTF16BE, fcUTF16LE:
    begin
      SetLength(Buffer, (DataToRead + 1) div 2);
      BlockRead(Buffer[0], DataToRead);
    end
    else
      RaiseUnsupportedCharset;
  end;

  Result := ConvertBuffer(Buffer);
end;

procedure TTextFile.WritePrefixIfEnabled;
begin
  if FUseByteOrderMark then
  begin
    FCharset := FDefaultCharset;
    ForceWritePrefix;
  end;
end;

function TTextFile.ConvertBuffer(const ABuffer: array of U1): string;
begin
  case FCharset of
    fcAscii:
      Result := TEncoding.ASCII.GetString(ABuffer);
    fcAnsi:
      Result := TEncoding.ANSI.GetString(ABuffer);
    fcUTF8:
      Result := TEncoding.UTF8.GetString(ABuffer);
    fcUTF16LE:
      Result := TEncoding.Unicode.GetString(ABuffer);
    fcUTF16BE:
      Result := TEncoding.BigEndianUnicode.GetString(ABuffer);
  else
    RaiseUnsupportedCharset;
    Result := '';
  end;
end;

procedure TTextFile.ReadPrefix;
var
	ByteOrderMark: TByteOrderMark;
begin
  if FUseByteOrderMark and (FileSize > 0) then
  begin
    SetLength(ByteOrderMark, Min(FileSize, MaxByteOrderMarkSize));
    BlockRead(@ByteOrderMark[0], Length(ByteOrderMark));

    FCharset := FindFileCharset(ByteOrderMark);
    FDataSize := FileSize - U8(Length(ByteOrderMarks[FCharset]));
  end
  else
    FCharset := fcUnknown;
end;

procedure TTextFile.WriteNoConversion(const Line: PByte; const LineLength: SG);
begin
  BlockWrite(Line, LineLength);
end;

procedure TTextFile.WriteNoConversion(const Line: RawByteString);
begin
  if Line <> '' then
    BlockWrite(PByte(Line), Length(Line));
end;

{$ifdef MSWINDOWS}
procedure TTextFile.Write(const Line: AnsiString);
var
	u: UnicodeString;
  i: SG;
begin
	if Length(Line) = 0 then
	begin
		Exit;
	end;

	case FCharset of
	fcAnsi:
		begin
			WriteNoConversion(Line);
		end;
	fcUTF8:
		begin
			WriteNoConversion(AnsiToUTF8(string(Line)));
		end;
	fcUTF16BE, fcUTF16LE:
		begin
			u := UnicodeString(Line);
      if FCharset = fcUTF16BE then
        for i := 1 to Length(u) do
        begin
          u[i] := WideChar(Swap(Ord(u[i])));
        end;
			WriteNoConversion(PByte(@u), Length(Line) * SizeOf(WideChar));
		end;
	else
		RaiseUnsupportedCharset;
	end;
end;
{$endif}

procedure TTextFile.Write(const Line: UnicodeString);
var
  u: UnicodeString;
  i: SG;
begin
	if Length(Line) = 0 then
	begin
		Exit;
	end;

	case FCharset of
	fcUTF8:
		begin
			WriteNoConversion(ConvertUnicodeToUTF8(Line));
		end;
	fcUTF16BE:
		begin
      u := Line;
      for i := 1 to Length(u) do
      begin
        u[i] := WideChar(Swap(Ord(u[i])));
      end;
			WriteNoConversion(PByte(@u[1]), Length(u) * SizeOf(WideChar));
		end;
  fcUTF16LE:
		begin
			WriteNoConversion(PByte(@Line[1]), Length(Line) * SizeOf(WideChar));
		end;
	fcAscii, fcAnsi:
		begin
			WriteNoConversion(RawByteString(Line));
		end;
	else
    RaiseUnsupportedCharset;
	end;
end;

{$ifdef MSWINDOWS}
procedure TTextFile.WriteLine(const Line: AnsiString);
begin
	Write(Line + FileSep);
end;
{$endif}

procedure TTextFile.WriteLine(const Line: UnicodeString);
begin
	Write(Line + FileSep);
end;

procedure TTextFile.ForceWritePrefix;
var
	L: SG;
begin
	L := Length(ByteOrderMarks[FCharset]);
	if L > 0 then
		BlockWrite(@ByteOrderMarks[FCharset][0], L);
end;

procedure TTextFile.SetDefaultCharsetIfNotInFile;
begin
  if FCharset = fcUnknown then
  begin
    if FDefaultCharset = fcUnknown then
      FCharset := fcAnsi
    else
      FCharset := FDefaultCharset;
  end;
end;

procedure TTextFile.SetUseByteOrderMark(const Value: BG);
begin
  FUseByteOrderMark := Value;
end;

end.
