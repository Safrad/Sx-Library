unit uCommonOutputFormat;

interface

uses
  SysUtils,

	uTypes,
  uNegativeNumberFormat;

type
  TCommonOutputFormat = class
  private
    function NToSInternal(const Num: S8; const Negative: BG; const UseFormat: string; const ANumericBase: U1): string; overload;
    function NToSInternal(const Num: U8; const Negative: BG; const UseFormat: string; const ANumericBase: U1): string; overload;
    procedure MsToHMSD(const T: S8; out GH, GM, GS, GD: U4);
  protected
    function NToSInternal(const Num: S8; const Negative: BG; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; overload; virtual;
    function NToSInternal(const Num: U8; const Negative: BG; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; overload; virtual;
  public
    type
      TDisplay = (diSD, diMSD, diHMSD, diHHMSD, diDHMSD);
      TFloatFormat = (ffAuto, ffScientic, ffNormal);
    const
      MaxNumericBase = 36;
      NumberTable: array[0..MaxNumericBase - 1] of Char = (
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
        'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T',
        'U', 'V', 'W', 'X', 'Y', 'Z'{, 'a', 'b', 'c', 'd',
        'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x',
        'y', 'z', '-', '*'});
    function NumToStr(const ANum: S8; const ANumericBase: U1): string; overload;
    function NumToStr(const ANum: U8; const ANumericBase: U1): string; overload;

    function NToS(const Num: S8; const UseFormat: string; const ANumericBase: U1 = 10): string; overload;
    function NToS(const Num: U8; const UseFormat: string; const ANumericBase: U1 = 10): string; overload;

    function NToS(const Num: S4; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; overload;
    function NToS(const Num: S8; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; overload;
    function NToS(const Num: U8; const Decimals: SG = 0; const ANumericBase: U1 = 10): string; overload;

    function NToSBase(const Num: S4; const ANumericBase: U1 = 10): string; overload;
    function NToSBase(const Num: S8; const ANumericBase: U1 = 10): string; overload;
    function NToSBase(const Num: U8; const ANumericBase: U1 = 10): string; overload;

    function FloatToDecimalString(const Value: FM; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TFloatFormat = ffAuto): string;
    function FToS(const Num: FM): string;

    function BToStr(const Bytes: S4): string; overload;
    function BToStr(const Bytes: S8): string; overload;
    function NodesToS(const Value: U8): string;
    function ExitCodeToString(const AExitCode: U4): string;

    /// <param name="Decimals">
    /// -3: 0:34.34
    /// +3: 0:34.340
    /// </param>
    function MsToStr(DT: S8; Display: TDisplay = diDHMSD;
      const Decimals: SG = -3; const FixedWidth: Boolean = False): string;

    function DateToS(const Year, Month, Day: U2): string; overload; virtual;
    function DateToS(const D: TDateTime): string; overload; virtual;
    function TimeToS(const T: TDateTime; const Decimals: SG): string;
    function DateTimeToS(const DT: TDateTime; const Decimals: SG): string;
    function PhoneToStr(const Phone: U8): string;

    function AddThousandSeparators(const AValue: string; const AFormatSettings: TFormatSettings; const AExponentPrefix: string): string;
  end;

var
  CommonOutputFormat: TCommonOutputFormat;

implementation

uses
  Math,

	uMath,
  uStrings,
  uEnglishFormatSettings;

{ TCommonOuptutFormat }

function TCommonOutputFormat.NumToStr(const ANum: S8; const ANumericBase: U1): string;
begin
	if ANum < 0 then
	begin
		Result := NumToStr(U8(-ANum), ANumericBase);
	end
  else
		Result := NumToStr(U8(ANum), ANumericBase);
end;

function TCommonOutputFormat.NumToStr(const ANum: U8; const ANumericBase: U1): string;
var
	Modulo: SG;
  Reminder: U8;
begin
	Assert((ANumericBase >= 2) and (ANumericBase <= MaxNumericBase));

	Result := '';
  Reminder := ANum;
	repeat
		Modulo := Reminder mod ANumericBase;
		Reminder := Reminder div ANumericBase;
		Result := NumberTable[Modulo] + Result;
	until Reminder = 0;
end;

function TCommonOutputFormat.NToSInternal(const Num: S8; const Negative: BG; const UseFormat: string; const ANumericBase: U1): string;
var
	Nums: string;
	i, j: SG;
	PointPos: SG;
	NumFound: BG;
begin
	Result := '';

{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
	if ANumericBase = 10 then
  begin
    if Negative and (Num < 0) then
  		Nums := IntToStr(U8(-Num))
    else
  		Nums := UIntToStr(U8(Num))
  end
	else
  begin
    if Negative and (Num < 0) then
  		Nums := NumToStr(U8(-Num), ANumericBase)
    else
  		Nums := NumToStr(U8(Num), ANumericBase);
  end;
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}

	j := Length(Nums);
	PointPos := Pos('.', UseFormat);
	if PointPos = 0 then PointPos := High(PointPos);
	NumFound := False;
	for i := Length(UseFormat) downto 1 do
	begin
		if (UseFormat[i] = '0') or (UseFormat[i] = '#') then
		begin
			if j >= 1 then
			begin
				if Nums[j] <> '0' then
				begin
					NumFound := True;
					Result := Nums[j] + Result;
				end
				else
				begin
					if (UseFormat[i] = '#') and (((i > PointPos) and (NumFound = False))) then
						Result := ' ' + Result
					else
					begin
						NumFound := True;
						Result := '0' + Result;
					end;
				end;
			end
			else
			begin
				if (UseFormat[i] = '#') and ((i < PointPos) or (NumFound = False)) then
					Result := ' ' + Result
				else
				begin
					NumFound := True;
					Result := '0' + Result;
				end;
			end;
			Dec(j);
		end
		else if (UseFormat[i] = '.') then
		begin
			if NumFound then
			begin
				Result := '.' + Result
			end
			else
				Result := ' ' + Result;
		end
		else if (UseFormat[i] = ',') then
		begin
			if j > 0 then
			begin
				Result := UseFormat[i] + Result
			end
			else
				Result := ' ' + Result;
		end
		else if UseFormat[i] = ' ' then
		begin
			Result := ' ' + Result;
		end
		else if UseFormat[i] = '+' then
		begin
			if Negative and (Num < 0) then
				Result := '-' + Result
			else
				Result := '+' + Result;

		end
		else if UseFormat[i] = '-' then
		begin
			if Negative and (Num < 0) then
				Result := '-' + Result
			else
				Result := ' ' + Result;
		end
		else if IsDebug then
			Assert(False, 'Unknown char in format string');
	end;
end;

function TCommonOutputFormat.NToS(const Num: S8; const UseFormat: string; const ANumericBase: U1 = 10): string;
begin
  Result := NToSInternal(Num, True, UseFormat, ANumericBase);
end;

function TCommonOutputFormat.NToS(const Num: U8; const UseFormat: string; const ANumericBase: U1 = 10): string;
begin
{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
  Result := NToSInternal(Num, False, UseFormat, ANumericBase);
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
end;

function TCommonOutputFormat.NToSBase(const Num: S4; const ANumericBase: U1 = 10): string;
begin
  if ANumericBase = 10 then
    Result := IntToStr(Num)
  else
    Result := NToS(S8(Num), 0);
end;

function TCommonOutputFormat.NToS(const Num: S4; const Decimals: SG = 0; const ANumericBase: U1 = 10): string;
begin
  if (ANumericBase = 10) and (Decimals = 0) then
    Result := IntToStr(Num)
  else
    Result := NToS(S8(Num), Decimals);
end;

// 454,545,455.456465; 0.045
function TCommonOutputFormat.NToSInternal(const Num: U8; const Negative: BG; const Decimals: SG = 0; const ANumericBase: U1 = 10): string;
var
	DecimalSep, ThousandSep: string;
	ThousandGr, FractionGr: SG;

	Nums: string;
	i, M: SG;
	FirstNotZero: BG;
	c: Char;
begin
	Result := '';

  if (ANumericBase = 10) and (Decimals = 0) then
  begin
    if Negative then
{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
      Result := IntToStr(S8(Num))
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
    else
      Result := UIntToStr(Num);
    Exit;
  end
  else
  begin
    DecimalSep := '.';
    ThousandSep := '';
    ThousandGr := 3;
    FractionGr := 3;
  end;

	if Num = 0 then
	begin
//		if OutputFormat = ofHTML then Nums := nbsp else Nums := ''
		Nums := '';
	end
	else if ANumericBase = 10 then
  begin
    if Negative then
  		Nums := IntToStr(Abs(Num))
    else
      Nums := UIntToStr(Num);
  end
	else
		Nums := NumToStr(Abs(Num), ANumericBase);

	M := -Abs(Decimals);
	i := Length(Nums);
	FirstNotZero := Decimals >= 0;
	while True do
	begin
		if i > 0 then
		begin
			c := Nums[i];
		end
		else
			c := '0';

		if c = '0' then
		begin
			if FirstNotZero then
				Result := '0' + Result;
		end
		else
		begin
			FirstNotZero := True;
			Result := c + Result
		end;

		Dec(i);
		Inc(M);

		if (i < 1) and (M > 0) then Break;

		if M = 0 then
		begin
			if FirstNotZero then
				Result := DecimalSep + Result;
			FirstNotZero := True;
			if i < 1 then
			begin
				if EnglishFormatSettings.LeadingZero = 1 then
					Result := '0' + Result;
				Break;
			end;
		end
		else if (M < 0) then
		begin
			if (FractionGr > 0) and (FirstNotZero) then
				if Abs(M) mod FractionGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end
		else if (M > 0) then
		begin
			if ThousandGr > 0 then
				if Abs(M) mod ThousandGr = 0 then
				begin
					Result := ThousandSep + Result
				end;
		end;
	end;

	if Negative and (S8(Num) < 0) then
	begin
		Result := '-' + Result;
	end;
end;

function TCommonOutputFormat.NToSInternal(const Num: U8; const Negative: BG; const UseFormat: string;
  const ANumericBase: U1): string;
begin
{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
  Result := NToSInternal(S8(Num), True, UseFormat, ANumericBase);
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
end;

function TCommonOutputFormat.NToSInternal(const Num: S8; const Negative: BG; const Decimals: SG;
  const ANumericBase: U1): string;
begin
{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
  Result := NToSInternal(U8(Num), True, Decimals, ANumericBase);
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}
end;

function TCommonOutputFormat.NToS(const Num: S8; const Decimals: SG = 0; const ANumericBase: U1 = 10): string;
begin
  Result := NToSInternal(Num, True, Decimals, ANumericBase);
end;

function TCommonOutputFormat.NToS(const Num: U8; const Decimals: SG = 0; const ANumericBase: U1 = 10): string;
begin
  Result := NToSInternal(Num, False, Decimals, ANumericBase);
end;

function TCommonOutputFormat.NToSBase(const Num: S8; const ANumericBase: U1 = 10): string;
begin
	Result := NToSInternal(Num, True, 0, ANumericBase);
end;

function TCommonOutputFormat.NToSBase(const Num: U8; const ANumericBase: U1 = 10): string;
begin
	Result := NToSInternal(Num, False, 0, ANumericBase);
end;

function TCommonOutputFormat.FloatToDecimalString(const Value: FM; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TFloatFormat = ffAuto): string;
var
  digits: string;
  s: string;
  floatRec: TFloatRec;
  Scientific: Boolean;
  i: SG;
begin
    FloatToDecimal(floatRec, Value, fvExtended, Precision, 9999);

  case FloatFormat of
  ffScientic: Scientific := True;
  ffNormal: Scientific := False;
  else{ffAuto}
    Scientific := Abs(floatRec.Exponent) > 10;
  end;

    //convert the array of byte into an easy to access string
    for i := 0 to Length(floatRec.Digits) - 1 do
    begin
      if floatRec.Digits[i] = 0 then
        Break;
      digits := digits + Char(floatRec.Digits[i]);
    end;

    if floatRec.Exponent > 0 then
    begin
        //Check for positive or negative infinity (exponent = 32767)
        if floatRec.Exponent = 32767 then //David Heffernan says that currency can never be infinity. Even though i can't test it, i can at least try to handle it
        begin
            if floatRec.Negative = False then
                Result := 'INF'
            else
                Result := '-INF';
            Exit;
        end;

        {
            digits:    1234567 89
              exponent--------^ 7=7 digits on left of decimal mark
        }
        if Scientific then
          s := s + digits + 'e' + IntToStr(floatRec.Exponent - Length(digits))
        else
        begin
        {
            for the value 10000:
                digits:   "1"
                exponent: 5
            Add enough zero's to digits to pad it out to exponent digits
        }
        s := Copy(digits, 1, floatRec.Exponent);
        if Length(s) < floatRec.Exponent then
            s := s+StringOfChar('0', floatRec.Exponent-Length(s));

        if Length(digits) > floatRec.Exponent then
            s := s+'.'+Copy(digits, floatRec.Exponent+1, Decimals);
        end;
    end
    else if floatRec.Exponent < 0 then
    begin
        //check for NaN (Exponent = -32768)
        if floatRec.Exponent = -32768 then  //David Heffernan says that currency can never be NotANumber. Even though i can't test it, i can at least try to handle it
        begin
            Result := 'NAN';
            Exit;
        end;
                        
        {
            digits:   .000123456789
                         ^---------exponent
        }
        if Scientific then
          s := s + digits + 'e' + IntToStr(floatRec.Exponent - Length(digits))
        else
        //Add zero, or more, "0"'s to the left
        s := '0.'+Copy(StringOfChar('0', -floatRec.Exponent)+digits, 1, Decimals);
    end
    else { floatRec.Exponent = 0 }
    begin
        {
            Exponent is zero.

            digits:     .123456789
                            ^
        }
        if length(digits) > 0 then
            s := '0.'+Copy(digits, 1, Decimals)
        else
            s := '0';
    end;

    if floatRec.Negative then
        s := '-'+s;

    Result := s;
end;

function TCommonOutputFormat.AddThousandSeparators(const AValue: string; const AFormatSettings: TFormatSettings; const AExponentPrefix: string): string;
var
  LastNumberPosition, ExponentPrefixPosition: SG;
  ThousandGroupPosition: SG;
  Index: SG;
  CopySize: SG;
  StringBuilder: TStringBuilder;
begin
  if AValue = '' then
    Exit;

  if EnglishFormatSettings.ThousandGroup <= 1 then
  begin
    Result := AValue;
    Exit;
  end;

  LastNumberPosition := Pos(AFormatSettings.DecimalSeparator, AValue) - 1;
  if LastNumberPosition = -1 then
    LastNumberPosition := Length(AValue);

  ExponentPrefixPosition := Pos(AExponentPrefix, AValue) - 1;
  if ExponentPrefixPosition <> -1 then
    LastNumberPosition := Min(ExponentPrefixPosition, LastNumberPosition);

  ThousandGroupPosition := (LastNumberPosition - 1) mod EnglishFormatSettings.ThousandGroup;
  StringBuilder := TStringBuilder.Create;
  try
    StringBuilder.Capacity := Length(AValue) + ((LastNumberPosition - 2) div EnglishFormatSettings.ThousandGroup);
    Result := AValue;
    for Index := 1 to LastNumberPosition - 1 do
    begin
      StringBuilder.Append(AValue[Index]);

      if ThousandGroupPosition = 0 then
      begin
        ThousandGroupPosition := EnglishFormatSettings.ThousandGroup;
        if (AValue[Index] >= '0') and (AValue[Index] <= '9') then
          StringBuilder.Append(AFormatSettings.ThousandSeparator);
      end;
      Dec(ThousandGroupPosition);
    end;
    CopySize := Length(AValue) - LastNumberPosition + 1;
    if CopySize > 0  then
      StringBuilder.Append(AValue, LastNumberPosition - 1, CopySize);
    Result := StringBuilder.ToString;
  finally
    StringBuilder.Free;
  end;
end;

function TCommonOutputFormat.FToS(const Num: FM): string;
begin
  Result := FloatToStr(Num, EnglishFormatSettings.IOFormatSettings);
end;

const
	Sep = ' ';
	iB = 'iB';

function TCommonOutputFormat.BToStr(const Bytes: S4): string;
var
	B: S4;
begin
	B := Abs(Bytes);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0) + Sep;
		Result := Result + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2) + Sep + 'K' + iB;
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1) + Sep + 'K' + iB;
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0) + Sep + 'K' + iB;
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2) + Sep + 'M' + iB;
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1) + Sep + 'M' + iB;
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0) + Sep + 'M' + iB;
	end
	else //if B < GB then
		Result := NToS((100 * (B div 128) + GB div 256) div (GB div 128), -2) + Sep + 'G' + iB;
	if B < 0 then Result := '-' + Result;
end;

function TCommonOutputFormat.BToStr(const Bytes: S8): string;
var
	B: S8;
begin
	B := Abs(Bytes);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0) + Sep + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2) + Sep + 'K' + iB; // Kilo
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1) + Sep + 'K' + iB;
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0) + Sep + 'K' + iB;
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2) + Sep + 'M' + iB; // Mega
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1) + Sep + 'M' + iB;
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0) + Sep + 'M' + iB;
	end
	else if B < 10737418240 then
	begin
		Result := NToS((100 * B + GB div 2) div GB, -2) + Sep + 'G' + iB; // Giga
	end
	else if B < 107374182400 then
	begin
		Result := NToS((10 * B + GB div 2) div GB, -1) + Sep + 'G' + iB;
	end
	else if B < 1099511627776 then //2^40 ($10 000 000 000)
	begin
		Result := NToS((B + GB div 2) div GB, 0) + Sep + 'G' + iB;
	end
	else if B < 10995116277760 then
	begin
		Result := NToS((100 * B + 1099511627776 div 2) div 1099511627776, -2) + Sep + 'T' + iB; // Tera
	end
	else if B < 109951162777600 then
	begin
		Result := NToS((10 * B + 1099511627776 div 2) div 1099511627776, -1) + Sep + 'T' + iB;
	end
	else if B < 1125899906842624 then //2^50 ($4 000 000 000 000)
	begin
		Result := NToS((B + 1099511627776 div 2) div 1099511627776, 0) + Sep + 'T' + iB;
	end
	else if B < 11258999068426240 then
	begin;
		Result := NToS((100 * B + 1125899906842624 div 2) div 1125899906842624, -2) + Sep + 'P' + iB; // Peta
	end
	else if B < 112589990684262400 then
	begin
		Result := NToS((10 * B + 1125899906842624 div 2) div 1125899906842624, -1) + Sep + 'P' + iB;
	end
	else if B < 1152921504606846976 then //2^60 ($1 000 000 000 000 000)
	begin
		Result := NToS((B + 1125899906842624 div 2) div 1125899906842624, 0) + Sep + 'P' + iB;
	end
	else //if B < 11529215046068469760 then
		Result := NToS((100 * (B div 128) + 1152921504606846976 div 256) div (1152921504606846976 div 128), -2) + Sep + 'E' + iB; // Exa
	// ZB
	// YB
	if B < 0 then Result := '-' + Result;
end;

function TCommonOutputFormat.NodesToS(const Value: U8): string;
const
	K = 1000;
	M = 1000 * K;
	G = 1000 * M;
	T = 1000 * U8(G);
	E = 1000 * T;
begin
	if Value < 0 then
		Result := '> 9E18'
	else if Value < M then
		Result := NToS(Value)
	else if Value < G then
		Result := NToS((Value + K div 2) div K) + ' k'
	else if Value < T then
		Result := NToS((Value + M div 2) div M) + ' M'
	else if Value < E then
		Result := NToS((Value + G div 2) div G) + ' G'
	else
		Result := NToS((Value + T div 2) div T) + ' T';
end;

function TCommonOutputFormat.ExitCodeToString(const AExitCode: U4): string;
begin
  Result := NToSBase(AExitCode, 16) + ' h';
end;

procedure TCommonOutputFormat.MsToHMSD(const T: S8; out GH, GM, GS, GD: U4);
var
	DW: U4;
begin
	if Abs(T) >= 1000 * U8(High(U4)) then
	begin
		GH := 999;
		GM := 59;
		GS := 59;
		GD := 999;
	end
	else
	begin
		DivModU8(Abs(T), 1000, DW, GD);
		DivModU8(DW, 60, DW, GS);
		DivModU8(DW, 60, GH, GM);
	end;
end;


function TryEncodeDate(Year, Month, Day: U2; out Date: TDateTime): Boolean;
var
	I: Integer;
	DayTable: PDayTable;
begin
	Result := False;
	DayTable := @MonthDays[IsLeapYear(Year)];
	if (Year >= 1) and (Year <= 9999) and (Month >= 1) and (Month <= 12) and
		(Day >= 1) and (Day <= DayTable^[Month]) then
	begin
		for I := 1 to Month - 1 do Inc(Day, DayTable^[I]);
		I := Year - 1;
		Date := I * 365 + I div 4 - I div 100 + I div 400 + Day - DateDelta;
		Result := True;
	end;
end;

function TCommonOutputFormat.MsToStr(DT: S8; Display: TDisplay = diDHMSD;
	const Decimals: SG = -3; const FixedWidth: Boolean = False): string;
var
	h, m, s, d: U4;
	Year, Week, Day: SG;
	Res, Rem: U1;

	TimeSep, DecimalSep, ListSep: string;
begin
  TimeSep := EnglishFormatSettings.IOFormatSettings.TimeSeparator;
  DecimalSep := EnglishFormatSettings.IOFormatSettings.DecimalSeparator;
  ListSep := EnglishFormatSettings.IOFormatSettings.ListSeparator;

{	case Abs(Decimals) of
	0:
	begin
		DT := 1000 * ((DT + 500) div 1000);
	end;
	1:
	begin
		DT := 100 * ((DT + 50) div 100);
	end;
	2:
	begin
		DT := 10 * ((DT + 5) div 10);
	end;
	end;}
	if DT < 0 then
	begin
		DT := -DT;
		Result := '-'
	end
	else
		Result := '';
	if (DT >= Minute) and (Display = diSD) then
		Display := diMSD;

	if Display = diDHMSD then
	begin
		if DT >= MSecsPerYear then
		begin
			Year := DT div S8(MSecsPerYear);
			Result := Result + IntToStr(Year) + CharSpace + 'year' + Plural(Year) + ListSep;
			DT := DT mod S8(MSecsPerYear);
		end;
		if DT >= MSecsPerWeek then
		begin
			Week := DT div MSecsPerWeek;
			Result := Result + IntToStr(Week) + CharSpace + 'week' + Plural(Week) + ListSep;
			DT := DT mod MSecsPerWeek;
		end;
		if DT >= MSecsPerDay then
		begin
			Day := DT div MSecsPerDay;
			Result := Result + IntToStr(Day) + CharSpace + 'day' + Plural(Day) + ListSep;
			DT := DT mod MSecsPerDay;
		end;
	end;
	MsToHMSD(DT, h, m, s, d);
	if Display = diDHMSD then
		h := h mod 24;

	case Display of
	diHHMSD, diHMSD, diDHMSD:
	begin
		if (h < 10) and (Display <> diHHMSD) then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + ' ';
			Result := Result + Char(h + Ord('0')) + TimeSep;
		end
		else if h < 100 then
		begin
			DivModU2(h, 10, Res, Rem);
			Result := Result + Char(Res + Ord('0')) + Char(Rem + Ord('0')) + TimeSep
		end
		else
			Result := Result + IntToStr(h) + TimeSep;
	end;
	diMSD:
	begin
		if h = 0 then
		begin
			if FixedWidth then Result := Result + StringOfChar(CharSpace, 3);
		end
		else if h < 10 then
		begin
			if (DT >= 0) and FixedWidth then Result := Result + CharSpace;
			Result := Result + Char(h + Ord('0')) + TimeSep
		end
		else if h < 100 then
		begin
			DivModU2(h, 10, Res, Rem);
			Result := Result + Char(Res + Ord('0')) + Char(Rem + Ord('0')) + TimeSep
		end
		else
			Result := Result + IntToStr(h) + TimeSep;
	end;
	end;

	if Display <> diSD then
		if m < 10 then
		begin
			if (h = 0) and (not (Display in [diHHMSD, diHMSD, diDHMSD])) then
			begin
				if FixedWidth then Result := Result + ' ';
				Result := Result + Char(m + Ord('0')) + TimeSep
			end
			else
				Result := Result + '0' + Char(m + Ord('0')) + TimeSep;
		end
		else
		begin
			DivModU2(m, 10, Res, Rem);
			Result := Result + Char(Res + Ord('0')) + Char(Rem + Ord('0')) + TimeSep;
		end;

	if Display = diSD then
	begin
		Result := Result + IntToStr(3600 * h + 60 * m + s);
	end
	else
		if s < 10 then
		begin
			Result := Result + '0' + Char(s + Ord('0'));
		end
		else
		begin
			DivModU2(s, 10, Res, Rem);
			Result := Result + Char(Res + Ord('0')) + Char(Rem + Ord('0'));
		end;

	if (d = 0) and (Decimals <= 0) then
		Exit;

	case Abs(Decimals) of
	1:
	begin
		d := d div 100;
		if (Decimals > 0) or (d <> 0) then
			Result := Result + DecimalSep + Char(d + Ord('0'));
	end;
	2:
	begin
		d := d div 10;
		if (Decimals > 0) then
			Result := Result + DecimalSep + NToS(d, '00')
		else
		begin
			Result := Result + DecimalSep + NToS(d, '00');
			if FixedWidth = False then DelEndChars(Result, ['0']);
		end;
	end;
	3:
	begin
		if (Decimals > 0) then
			Result := Result + DecimalSep + NToS(d, '000')
		else
		begin
			Result := Result + DecimalSep + NToS(d, '000');
			if FixedWidth = False then DelEndChars(Result, ['0']);
		end;
	end;
	end;
end;

function TCommonOutputFormat.DateToS(const Year, Month, Day: U2): string;
begin
	Result := NToS(Year, '0000') + '-' + NToS(Month, '00') + '-' + NToS(Day, '00');
end;

function TCommonOutputFormat.DateToS(const D: TDateTime): string;
var Year, Month, Day: U2;
begin
	if D = 0 then
		Result := ''
	else
	begin
    DecodeDate(D, Year, Month, Day);
    Result := DateToS(Year, Month, Day);
	end;
end;

function TCommonOutputFormat.TimeToS(const T: TDateTime; const Decimals: SG): string;
var
  TimeInMs: FG;
begin
  TimeInMs := T * MSecsPerDay;
  Result := MsToStr(Trunc(TimeInMs), diHHMSD, Min(Decimals, 3), False);
  if Decimals > 3 then
    Result := Result + NToS(Trunc(Frac(TimeInMs) * Power10(1, Decimals - 3)), StringOfChar('0', Decimals - 3));
end;

function TCommonOutputFormat.DateTimeToS(const DT: TDateTime; const Decimals: SG): string;
begin
	if DT = 0 then
		Result := ''
	else
	begin
		// YYYY-MM-DD HH:MM:SS
		Result := DateToS(Trunc(DT)) + ' ' + TimeToS(Frac(DT), Decimals);
	end;
end;

function TCommonOutputFormat.PhoneToStr(const Phone: U8): string;
begin
	if Phone = 0 then
	begin
		Result := '';
		Exit;
	end;
	if Phone div 1000000000 = 0 then
		Result := '+420-'
	else
		Result := '+' + NToS(Phone div 1000000000, '000') + '-';
	Result := Result + NToS(Phone mod 1000000000, '000000000');
end;

initialization
  CommonOutputFormat := TCommonOutputFormat.Create;
finalization
  FreeAndNil(CommonOutputFormat);
end.
