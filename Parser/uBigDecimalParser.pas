unit uBigDecimalParser;

interface

uses
  Velthuis.BigDecimals,

  uTypes,
  uSxCustomParser;

type
  TBigDecimalParser = class
  private
    FValue: BigDecimal;
    FActualDigitDivisor: BigDecimal;
    FBase: SG;
    FSxParser: TSxCustomParser;
    FFAcceptFloatNumber: BG;
    FThousandSep: Char;
    FDecimalSep: Char;
    function ParseDecimalNumber: SG;
    function ReadInteger: SG;
    procedure ParseNumberMantisaBase;
    procedure ParseMantisaValue;
    procedure ParseExponentValue;
    procedure ParsePercentAndPromile;
    procedure ApplyDigitValue(const ADigitValue: SG);
    procedure SetSxParser(const Value: TSxCustomParser);
    procedure SetFAcceptFloatNumber(const Value: BG);
    procedure SetDecimalSep(const Value: Char);
    procedure SetThousandSep(const Value: Char);
  public
    constructor Create;

    // Input
    property SxParser: TSxCustomParser read FSxParser write SetSxParser;
    property FAcceptFloatNumber: BG read FFAcceptFloatNumber write SetFAcceptFloatNumber;
    property DecimalSep: Char read FDecimalSep write SetDecimalSep;
    property ThousandSep: Char read FThousandSep write SetThousandSep;

    // Process
    procedure Parse;

    // Output
    property Value: BigDecimal read FValue;
  end;

implementation

uses
  SysUtils,
  uChar,
  uStrings,
  uEParseError;

{ TBigDecimalParser }

procedure TBigDecimalParser.ApplyDigitValue(const ADigitValue: SG);
begin
  if FActualDigitDivisor.IsZero then
  begin
    FValue := FValue * FBase + ADigitValue;
  end
  else
  begin
    FActualDigitDivisor := FActualDigitDivisor * FBase;
    FValue := FValue + ADigitValue / FActualDigitDivisor;
  end;
end;

constructor TBigDecimalParser.Create;
begin
  inherited;

  FAcceptFloatNumber := True;
end;

procedure TBigDecimalParser.ParseMantisaValue;
var
	Minus: BG;
  Digit: SG;
  UpCaseActualChar: Char;
begin
  FActualDigitDivisor := 0;

	Minus := False;

	case SxParser.ActualChar of
	'-':
		begin
			Minus := True;
      SxParser.ReadNextChar;
		end;
	'+':
		begin
      // Skip
      SxParser.ReadNextChar;
		end;
	end;

	while not SxParser.EndOfInput do
	begin
		if (FActualDigitDivisor.IsZero) and (SxParser.ActualChar = FDecimalSep) or
			((FThousandSep <> '.') and (SxParser.ActualChar = '.')) and (FAcceptFloatNumber) then
			FActualDigitDivisor := 1
		else if
      (SxParser.ActualChar = ThousandSep) or
      ((ThousandSep = CharUnbrokableSpace) and (SxParser.ActualChar = CharSpace)) or
      ((ThousandSep = CharSpace) and (SxParser.ActualChar = CharUnbrokableSpace)) then
		begin
      // Skip
		end
		else
		begin
			case SxParser.ActualChar of
			'0'..'9':
        Digit := CharToDigit(SxParser.ActualChar);
			'A'..'Z', 'a'..'z':
      begin
        UpCaseActualChar := UpCase(SxParser.ActualChar);
        Digit := Ord(UpCaseActualChar) - Ord('A') + 10;
        if (Digit >= FBase) and
          ((UpCaseActualChar = 'E') or (UpCaseActualChar = 'P')) then
        begin
           ParseExponentValue;
           Continue;
        end;
      end
			else
				Break;
			end;
      if Digit < FBase then
      begin
        ApplyDigitValue(Digit);
      end
      else
        raise EParseError.Create('Invalid number character ''' + SxParser.ActualChar + '''.');
		end;
    SxParser.ReadNextChar;
	end;
	if Minus then
		FValue := -FValue;
end;

procedure TBigDecimalParser.ParseExponentValue;
var
  N: SG;
	Radix: BigDecimal;
begin
	if UpCase(SxParser.ActualChar) = 'E' then
		Radix := BigDecimal(10)
  else // if UpCase(SxParser.ActualChar) = 'P' then
    Radix := BigDecimal(2);
  SxParser.ReadNextChar;
  N := ParseDecimalNumber;
  if FValue <> 0 then
  begin
    FValue := FValue * Radix.IntPower(N);
  end;
end;

procedure TBigDecimalParser.ParsePercentAndPromile;
begin
  if SxParser.ActualChar = '%' then
  begin
    FValue := FValue / 100;
    SxParser.ReadNextChar;
  end
  else if SxParser.ActualChar = '‰' then
  begin
    FValue := FValue / 1000;
    SxParser.ReadNextChar;
  end;
end;

function TBigDecimalParser.ReadInteger: SG;
var
  C: Char;
begin
  Result := 0;
  while True do
  begin
    C := SxParser.ActualChar;
    if (C < '0') or (C > '9') then
      Break;
    Result := Result * 10 + CharToDigit(C);
    SxParser.ReadNextChar;
  end;
end;

procedure TBigDecimalParser.ParseNumberMantisaBase;
begin
	case SxParser.ActualChar of
    '#':
      begin
        FBase := 2;
        SxParser.ReadNextChar;
      end;
    '$':
      begin
        FBase := 16;
        SxParser.ReadNextChar;
      end;
    '0':
      begin
        SxParser.ReadNextChar;
        case SxParser.ActualChar of
        'b', 'B':
          begin
            FBase := 2;
            SxParser.ReadNextChar;
          end;
        'd', 'D':
          begin
            FBase := 10;
            SxParser.ReadNextChar;
          end;
        'x', 'X':
          begin
            FBase := 16;
            SxParser.ReadNextChar;
          end;
        'o', 'O', 'k', 'K':
          begin
            FBase := 8;
            SxParser.ReadNextChar;
          end;
          else
            FBase := 10;
        end;
      end;
    '%':
    begin
      SxParser.ReadNextChar;
      FBase := ReadInteger;
      if (FBase < 2) or (FBase > 36) then
        raise EArgumentException.Create('Invalid base value.');
      SxParser.ReadExpectedChar(['R', 'r']);
    end
    else
      FBase := 10;
	end;
end;

procedure TBigDecimalParser.Parse;
begin
  FValue := 0;

  ParseNumberMantisaBase;
	ParseMantisaValue;
  ParsePercentAndPromile;
end;

function TBigDecimalParser.ParseDecimalNumber: SG;
var
  C: Char;
  Minus: BG;
begin
  Result := 0;
  Minus := False;

  case SxParser.ActualChar of
    '-':
    begin
      Minus := True;
      SxParser.ReadNextChar;
    end;
    '+':
      SxParser.ReadNextChar;
  end;

  while True do
  begin
    C := SxParser.ActualChar;
    if (C >= '0') and (C <= '9') then
    begin
      Result := Result  * 10 + CharToDigit(SxParser.ActualChar);
      SxParser.ReadNextChar;
    end
    else
      Break;
  end;

  if Minus then
    Result := -Result;
end;

procedure TBigDecimalParser.SetDecimalSep(const Value: Char);
begin
  FDecimalSep := Value;
end;

procedure TBigDecimalParser.SetFAcceptFloatNumber(const Value: BG);
begin
  FFAcceptFloatNumber := Value;
end;

procedure TBigDecimalParser.SetSxParser(const Value: TSxCustomParser);
begin
  FSxParser := Value;
end;

procedure TBigDecimalParser.SetThousandSep(const Value: Char);
begin
  FThousandSep := Value;
end;

end.
