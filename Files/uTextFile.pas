unit uTextFile;

interface

uses
  uTypes,
  uFIleCharset,
  uBufferedRawFile;

type
  TTextFile = class(TBufferedRawFile)
  private
    FLastCharCR: BG;
		FCharset: TFileCharset;

    // Used if BOM not found
		FDefaultCharset: TFileCharset;

    FUseByteOrderMark: BG;
		procedure ReadPrefix;
		procedure WritePrefix;
    procedure SetUseByteOrderMark(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    // Input
		property DefaultCharset: TFileCharset read FDefaultCharset write FDefaultCharset;
    property UseByteOrderMark: BG read FUseByteOrderMark write SetUseByteOrderMark;

    // Process
    procedure Open; override;

		property Charset: TFileCharset read FCharset;

		procedure ReadLineNoConversion(out Line: AnsiString);
		procedure ReadLine(out Line: AnsiString); overload;
		procedure ReadLine(out Line: UnicodeString); overload;

		procedure Write(const Line: AnsiString); overload;
		procedure Write(const Line: UnicodeString); overload;
		procedure WriteNoConversion(const Line: PAnsiChar; const LineLength: SG); overload;
		procedure WriteNoConversion(const Line: AnsiString); overload;
		procedure WriteLine(const Line: AnsiString); overload;
		procedure WriteLine(const Line: UnicodeString); overload;
  end;

implementation

uses
  SysUtils,
  Winapi.Windows,
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
  		ReadPrefix;
      Seek(Length(ByteOrderMarks[FCharset]));
		end;
	fmAppend:
		begin
			if FileSize > 0 then
			begin
        SeekBegin;
 				ReadPrefix;
				SeekEnd;
			end
			else if FUseByteOrderMark then
      begin
        FCharset := FDefaultCharset;
				WritePrefix;
      end;
		end;
	fmRewrite:
		begin
			Truncate;
			if FUseByteOrderMark then
      begin
        FCharset := FDefaultCharset;
				WritePrefix;
      end;
		end;
	end;
end;

procedure TTextFile.ReadLineNoConversion(out Line: AnsiString);
var
	InLineIndex: SG;
	LineLength: SG;
  ReadedAnsiChar: AnsiChar;
  ReadedWideChar: WideChar;
begin
	Line := '';
	if Eof then
	begin
		Exit;
	end;

	LineLength := 256;
	SetLength(Line, LineLength);
	InLineIndex := 1;
	while not Eof do
	begin
    case FCharset of
    fcAnsi, fcUTF8:
    begin
      ReadedAnsiChar := ReadAnsiChar;
      if ReadedAnsiChar = CharCR then
      begin
        FLastCharCR := True;
        Break; // Line ready
      end
      else if ReadedAnsiChar = CharLF then
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
        Line[InLineIndex] := ReadedAnsiChar;
        Inc(InLineIndex);
      end;
    end;
    fcUTF16BE, fcUTF16LE:
    begin
      ReadedWideChar := ReadWideChar;
      if FCharset = fcUTF16BE then
        ReadedWideChar := WideChar(Swap(U2(ReadedWideChar)));

      if ReadedWideChar = CharCR then
      begin
        FLastCharCR := True;
        Break; // Line ready
      end
      else if ReadedWideChar = CharLF then
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
        Line[InLineIndex] := AnsiChar(TU2(ReadedWideChar).B0);
        Inc(InLineIndex);
        Line[InLineIndex] := AnsiChar(TU2(ReadedWideChar).B1);
        Inc(InLineIndex);
      end;
		end;
    end;

    if InLineIndex + 1 > LineLength then
    begin
      LineLength := 2 * (InLineIndex - 1);
      SetLength(Line, LineLength);
    end;
	end;

	SetLength(Line, InLineIndex - 1);
end;

procedure TTextFile.ReadLine(out Line: AnsiString);
begin
	ReadLineNoConversion(Line);
	case FCharset of
	fcAnsi: ;
	fcUTF8: Line := AnsiString(Utf8ToAnsi(Line));
	else
		Line := '';
		raise ENotSupportedException.Create(ReplaceParam('Unsupported charset in file %1', [FileName]));
	end;
end;

procedure TTextFile.ReadLine(out Line: UnicodeString);
var
	LineA: AnsiString;
begin
	ReadLineNoConversion(LineA);
	case FCharset of
	fcAnsi:
		Line := UnicodeString(LineA);
	fcUTF8:
  	Line := ConvertUtf8ToUnicode(LineA);
  fcUTF16BE, fcUTF16LE:
		begin
			SetLength(Line, Length(LineA) div SizeOf(WideChar));
			Move(LineA[1], Line[1], Length(LineA));
		end;
	else
		begin
			Line := '';
  		raise ENotSupportedException.Create(ReplaceParam('Unsupported charset in file %1', [FileName]));
		end;
	end;
end;

procedure TTextFile.ReadPrefix;
var
	ByteOrderMark: array[0..MaxByteOrderMarkSize - 1] of AnsiChar;
begin
  if FUseByteOrderMark then
  begin
  	ByteOrderMark := '    ';
  	BlockRead(@ByteOrderMark, Min(FileSize, MaxByteOrderMarkSize));

    FCharset := FindFileCharset(ByteOrderMark);
  end
  else
    FCharset := fcUnknown;

  if FCharset = fcUnknown then
  begin
    if FDefaultCharset = fcUnknown then
    begin
      FCharset := fcUTF8
    end
    else
      FCharset := FDefaultCharset;
  end;
end;

procedure TTextFile.WriteNoConversion(const Line: PAnsiChar; const LineLength: SG);
begin
  BlockWrite(Line, LineLength);
end;

procedure TTextFile.WriteNoConversion(const Line: AnsiString);
begin
  if Line <> '' then
    BlockWrite(PAnsiChar(Line), Length(Line));
end;

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
			WriteNoConversion(PAnsiChar(@u), Length(Line) * SizeOf(WideChar));
		end;
	else
		raise ENotSupportedException.Create(ReplaceParam('Unsupported charset in file %1', [FileName]));
	end;
end;

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
			WriteNoConversion(AnsiString(ConvertUnicodeToUTF8(Line)));
		end;
	fcUTF16BE:
		begin
      u := Line;
      for i := 1 to Length(u) do
      begin
        u[i] := WideChar(Swap(Ord(u[i])));
      end;
			WriteNoConversion(PAnsiChar(@u[1]), Length(u) * SizeOf(WideChar));
		end;
  fcUTF16LE:
		begin
			WriteNoConversion(PAnsiChar(@Line[1]), Length(Line) * SizeOf(WideChar));
		end;
	fcAnsi:
		begin
			WriteNoConversion(AnsiString(Line));
		end;
	else
		raise ENotSupportedException.Create(ReplaceParam('Unsupported charset in file %1', [FileName]));
	end;
end;

procedure TTextFile.WriteLine(const Line: AnsiString);
begin
	Write(Line + FileSep);
end;

procedure TTextFile.WriteLine(const Line: UnicodeString);
begin
	Write(Line + FileSep);
end;

procedure TTextFile.WritePrefix;
var
	L: SG;
begin
	L := Length(ByteOrderMarks[FCharset]);
	if L > 0 then
		BlockWrite(@ByteOrderMarks[FCharset][1], L);
end;

procedure TTextFile.SetUseByteOrderMark(const Value: BG);
begin
  FUseByteOrderMark := Value;
end;

end.
