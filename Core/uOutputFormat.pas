unit uOutputFormat;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
	uTypes,
  SysUtils;

type
	TOutputFormat = (ofIO{Disk File Input/Output}, ofHTML{Disk HTML}, ofDisplay{Windows Locale});

// Number Format
var
	NativeSymbols: string;

	DecimalSeparator: string; // Decimal symbol
	DigitsAfterDecimal: SG; // No. of digits after decimal
	ThousandSeparator: string; // Digit grouping symbol
	UseThousandSeparator: BG = True; // Custom
	ThousandGroup: SG; // Digit grouping
	FractionGroup: SG;
	NegSymbol: string; // Negatove sing symbol
	PosSymbol: string; // Negatove sing symbol
	NegFormat: SG; // Negative number format
	LeadingZero: SG; // Display leading zeros
	ListSeparator: string; // List separator

// Time Format
	TimeSeparator: string;

  IOFormatSettings: TFormatSettings; // English format settings
{
NToS(S8, ...); <-> StrToValI(SG,UG), StrToValS8, U1(..., False, ...);

FToS(F10, ...) <-> StrToValExt StrToE(..., False);

2,102,454,545.45644;

IntToStr	StrToInt ; 2102454545;  Windows Registry, IE

}
{
	Decimals:
	-2	2.2	Maximum decimals
	+2	2.20	Fixed decimals
}
{
		20030923
		2003-09-23
		2003-9-23
		09/23/2003
		9/23/2003
		23.09.2003
		23.9.2003
}
// Data To Str
var
	NumericBase: U1 = 10;
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

function NumToStr(Num: S8; const Base: SG): string;

function NToS(const Num: S4; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NToS(const Num: S4; const OutputFormat: TOutputFormat): string; overload;

function NToS(const Num: S8; const UseFormat: string): string; overload;
function NToS(const Num: S8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NToS(const Num: S8; const OutputFormat: TOutputFormat): string; overload;

type
  TFloatFormat= (ffAuto, ffScientic, ffNormal);

function FloatToDecimalString(const Value: Extended; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TFloatFormat = ffAuto): string;
function FToS(const Num: Extended; const OutputFormat: TOutputFormat = ofDisplay): string;

function BToStr(const Bytes: S4; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function BToStr(const Bytes: S8; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;

procedure msToHMSD(const T: S8; out GH, GM, GS, GD: U4);
type
	TDisplay = (diSD, diMSD, diHMSD, diHHMSD, diDHMSD);

{Decimals
-3: 0:34.34
3: 0:34.340
}


function MsToStr(DT: S8; Display: TDisplay = diDHMSD;
	const Decimals: SG = -3; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function PhoneToStr(const Phone: U8): string;

implementation

uses
	Windows, Math,
	uMath, uStrings, uDictionary;

procedure AddMinusStr(var Result: string; const OutputFormat: TOutputFormat);
begin
	if OutputFormat = ofDisplay then
	begin
		case NegFormat of
		0: Result := '(' + Result + ')';
		1: Result := NegSymbol + Result;
		2: Result := NegSymbol + ' ' + Result;
		3: Result := Result + NegSymbol;
		4: Result := Result + ' ' + NegSymbol;
		else Result := NegSymbol + Result;
		end;
	end
	else
		Result := '-' + Result;
end;

function NumToStr(Num: S8; const Base: SG): string;
var
	M: SG;
//	Minus: BG;
begin
	Result := '';
	if Num < 0 then
	begin
		Num := -Num;
//		Minus := True;
	end;
{	else
		Minus := False;}
	Assert((Base >= 2) and (Base <= MaxNumericBase));
	while True do
	begin
//		DivModS64(Num, Base, D, M);
		M := Num mod Base;
		Num := Num div Base;
		Result := NumberTable[M] + Result;
		if Num = 0 then Break;
	end;
//	if Minus then AddMinusStr(Result);
end;

function NToS(const Num: S8; const UseFormat: string): string;
var
	Nums: string;
	i, j: SG;
	PointPos: SG;
	NumFound: BG;
begin
	Result := '';
	if (Num = Low(Num)) or (Num = High(Num)) then
	begin
		Result := 'Out of 64-bit range';
		Exit;
	end;

	if NumericBase = 10 then
		Nums := IntToStr(Abs(Num))
	else
		Nums := NumToStr(Abs(Num), NumericBase);
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
			if Num < 0 then
				Result := '-' + Result
			else
				Result := '+' + Result;

		end
		else if UseFormat[i] = '-' then
		begin
			if Num < 0 then
				Result := '-' + Result
			else
				Result := ' ' + Result;
		end
		else if IsDebug then
			Assert(False, 'Unknown char in format string');
	end;
end;

function NToS(const Num: S4; const OutputFormat: TOutputFormat): string; overload;
begin
	case OutputFormat of
	ofDisplay:
		Result := NToS(S8(Num), 0, OutputFormat);
	ofIO:
		if NumericBase = 10 then
			Result := IntToStr(Num)
		else
			Result := NToS(S8(Num), 0, OutputFormat);
	else
		Result := NToS(S8(Num), 0, OutputFormat);
	end;
end;

function NToS(const Num: S4; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
begin
	case OutputFormat of
	ofDisplay:
		Result := NToS(S8(Num), Decimals, OutputFormat);
	ofIO:
		if (NumericBase = 10) and (Decimals = 0) then
			Result := IntToStr(Num)
		else
			Result := NToS(S8(Num), Decimals, OutputFormat);
	else
		Result := NToS(S8(Num), Decimals, OutputFormat);
	end;
end;

// 454,545,455.456465; 0.045
function NToS(const Num: S8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
var
	DecimalSep, ThousandSep: string;
	ThousandGr, FractionGr: SG;

	Nums: string;
	i, M: SG;
	FirstNotZero: BG;
	c: Char;
begin
	Result := '';
	if (Num = Low(Num)) or (Num = High(Num)) then
	begin
		Result := 'Out of 64-bit range';
		Exit;
	end;

	case OutputFormat of
	ofDisplay:
	begin
		DecimalSep := DecimalSeparator;
		ThousandSep := ThousandSeparator;
		ThousandGr := ThousandGroup;
		FractionGr := FractionGroup;
	end;
	ofIO:
	begin
		if (NumericBase = 10) and (Decimals = 0) then
		begin
			Result := IntToStr(Num);
			Exit;
		end
		else
		begin
			DecimalSep := '.';
			ThousandSep := '';
			ThousandGr := 3;
			FractionGr := 3;
		end;
	end;
	else {ofHTML:}
	begin
		DecimalSep := '.';
		ThousandSep := ',';
		ThousandGr := 3;
		FractionGr := 3;
	end;
	end;
	if UseThousandSeparator = False then ThousandSep := '';

	if Num = 0 then
	begin
//		if OutputFormat = ofHTML then Nums := nbsp else Nums := ''
		Nums := '';
	end
	else if NumericBase = 10 then
		Nums := IntToStr(Abs(Num))
	else
		Nums := NumToStr(Abs(Num), NumericBase);

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
				if OutputFormat = ofDisplay then
				begin
					if LeadingZero = 1 then
						Result := '0' + Result;
				end
				else
				begin
					Result := '0' + Result;
				end;
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

	if Num < 0 then
	begin
		AddMinusStr(Result, OutputFormat);
	end;
end;

function NToS(const Num: S8; const OutputFormat: TOutputFormat): string; overload;
begin
	Result := NToS(Num, 0, OutputFormat);
end;

function FloatToDecimalString(const Value: Extended; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TFloatFormat = ffAuto): string;
var
    digits: string;
    s: string;
    floatRec: TFloatRec;
    Scientific: Boolean;
begin
    FloatToDecimal(floatRec, Value, fvExtended, Precision, 9999);

  case FloatFormat of
  ffScientic: Scientific := True;
  ffNormal: Scientific := False;
  else{ffAuto}
    Scientific := Abs(floatRec.Exponent) > 10;
  end;

    //convert the array of char into an easy to access string
    digits := PChar(Addr(floatRec.Digits[0]));

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

function AddThrousandSeparator(const s: string): string;
var
	dp: SG;
  i, M: SG;
begin
  Result := s;
	if UseThousandSeparator then
  begin
    dp := Pos(DecimalSeparator, s);
    if dp = 0 then
    begin
      if Pos('E', s) <> 0 then
      begin
      	Exit;
      end
      else
      	dp := Length(s);
    end
    else
    	Dec(dp);
    M := 0;
    for i := dp downto 2 do
    begin
    	if M = ThousandGroup - 1 then
      begin
        M := 0;
        Insert(ThousandSeparator, Result, i);
      end
      else
      	Inc(M);
    end;
  end;
end;

function FToS(const Num: Extended; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
  if OutputFormat = ofIO then
	  Result := FloatToStr(Num, IOFormatSettings)
  else
  	Result := AddThrousandSeparator(FloatToStr(Num)); //FloatToStrF(Num, ffNumber, 99, 99); // Format('%.99n', [Num]);//FloatToStr(Num);
end;

{
function Using(const Typ: string; const Num: S8): string;
var
	inp: string;
	inpP: Integer;
	FixedSign: Boolean;
	Poin: Integer;
	DelSpace: Boolean;
	i: Integer;
	Fra: Boolean;
begin
	Result := '';
	if UseFormat = '' then
	begin
		Exit;
	end;

	Poin := Pos('.', UseFormat);
	if LastChar(UseFormat) = '~' then DelSpace := True else DelSpace := False;

	if Num = 0 then inp := '' else inp := IntToStr(Abs(Num));
	inpP := Length(inp);

	FixedSign := False;
	for i := 1 to Length(UseFormat) do
	begin
		if (UseFormat[i] = '+') or (UseFormat[i] = '-') then
		begin
			FixedSign := True;
			Break;
		end;
	end;

	Fra := False;
	for i := Length(UseFormat) downto 1 do
	begin
		case UseFormat[i] of
		'#':
		begin
			if inpP > 0 then
			begin
				if (inp[inpP] <> '0') or (Fra = True) then
				begin
					Result := inp[inpP] + Result;
					Fra := True;
				end
				else
					if DelSpace = False then Result := ' ' + Result;
				Dec(inpP);
			end
			else
			begin
				if (i < Poin) and (FixedSign = False) and (num < 0) then
				begin
					FixedSign := True;
					Result := '-' + Result;
				end
				else
				begin
					if (Fra = True) and ((i > Poin) and (Poin <> 0)) then
					begin
						Result := '0' + Result;
						Fra := True;
					end
					else
						if DelSpace = False then Result := ' ' + Result;
				end;
			end;
		end;
		'0':
		begin
			if inpP > 0 then
			begin
				Result := inp[inpP] + Result;
				Fra := True;
				Dec(inpP);
			end
			else
			begin
				Result := '0' + Result;
				Fra := True;
			end;
		end;
		'~':
		begin
			if i = 1 then
			begin
				while Length(Result) > 0 do
				begin
					if Result[1] = ' ' then Delete(Result, 1, 1) else goto LExit;
				end;
				goto LExit;
			end;
		end;
		'+':
		begin
			if num = 0 then
				Result := ' ' + Result
			else
			if num > 0 then
				Result := '+' + Result
			else
				Result := '-' + Result;
		end;
		'-':
		begin
			if num >= 0 then
				Result := ' ' + Result
			else
				Result := '-' + Result;
		end;
		'.':
		begin
			if Fra = False then
			begin
				if DelSpace = False then Result := Result + ' ';
				fra := True;
			end
			else
			begin
				if UseWinFormat then
					Result := DecimalSeparator + Result
				else
					Result := '.' + Result;
			end;
			if UseFormat[1] = '~' then DelSpace := True else DelSpace := False;
		end;
		' ':
		begin
			Result := ' ' + Result;
		end;
		',':
		begin
				if (inpP > 0) then
				begin
					if UseWinFormat then
						Result := ThousandSeparator + Result
					else
						Result := ',' + Result;
				end
				else
				begin
					if DelSpace = False then Result := ' ' + Result;
				end;
		end;
		end;
	end;
	LExit:
end;

{function StrToI(s: string): SG;
begin
	Result := StrToI(s, 0);
{var
	i: SG;
	Minus: Boolean;
begin
	Result := 0;
	Minus := False;
	for i := 1 to Length(s) do
	begin
		case s[i] of
		'-': Minus := not Minus;
		'0'..'9':
		begin
			Result := Result * 10;
			if Minus then
				Result := Result - (Ord(s[i]) - Ord('0'))
			else
				Result := Result + (Ord(s[i]) - Ord('0'));
		end;
		'.': Break;
		end;
	end;
end;
}

const
	Sep = ' ';
	iB = 'iB';

function BToStr(const Bytes: S4; const OutputFormat: TOutputFormat = ofDisplay): string;
var
	B: S4;
begin
	B := Abs(Bytes);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0, OutputFormat) + Sep;
		if OutputFormat = ofDisplay then
			Result := Result + Translate('byte' + Plural(B))
		else
			Result := Result + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2, OutputFormat) + Sep + 'K' + iB;
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1, OutputFormat) + Sep + 'K' + iB;
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0, OutputFormat) + Sep + 'K' + iB;
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2, OutputFormat) + Sep + 'M' + iB;
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1, OutputFormat) + Sep + 'M' + iB;
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0, OutputFormat) + Sep + 'M' + iB;
	end
	else //if B < GB then
		Result := NToS((100 * (B div 128) + GB div 256) div (GB div 128), -2, OutputFormat) + Sep + 'G' + iB;
	if B < 0 then Result := '-' + Result;
end;

function BToStr(const Bytes: S8; const OutputFormat: TOutputFormat = ofDisplay): string;
var
	B: S8;
begin
	B := Abs(Bytes);
	if B < KB then // 2^10 ($400)
	begin
		Result := NToS(B, 0, OutputFormat) + Sep + 'byte' + Plural(B);
	end
	else if B < 10 * KB then
	begin
		Result := NToS((100 * B + KB div 2) div KB, -2, OutputFormat) + Sep + 'K' + iB; // Kilo
	end
	else if B < 100 * KB then
	begin
		Result := NToS((10 * B + KB div 2) div KB, -1, OutputFormat) + Sep + 'K' + iB;
	end
	else if B < MB then // 2^20 ($100 000)
	begin
		Result := NToS((B + KB div 2) div KB, 0, OutputFormat) + Sep + 'K' + iB;
	end
	else if B < 10 * MB then
	begin
		Result := NToS((100 * B + MB div 2) div MB, -2, OutputFormat) + Sep + 'M' + iB; // Mega
	end
	else if B < 100 * MB then
	begin
		Result := NToS((10 * B + MB div 2) div MB, -1, OutputFormat) + Sep + 'M' + iB;
	end
	else if B < GB then // 2^30 ($40 000 000)
	begin
		Result := NToS((B + MB div 2) div MB, 0, OutputFormat) + Sep + 'M' + iB;
	end
	else if B < 10737418240 then
	begin
		Result := NToS((100 * B + GB div 2) div GB, -2, OutputFormat) + Sep + 'G' + iB; // Giga
	end
	else if B < 107374182400 then
	begin
		Result := NToS((10 * B + GB div 2) div GB, -1, OutputFormat) + Sep + 'G' + iB;
	end
	else if B < 1099511627776 then //2^40 ($10 000 000 000)
	begin
		Result := NToS((B + GB div 2) div GB, 0, OutputFormat) + Sep + 'G' + iB;
	end
	else if B < 10995116277760 then
	begin
		Result := NToS((100 * B + 1099511627776 div 2) div 1099511627776, -2, OutputFormat) + Sep + 'T' + iB; // Tera
	end
	else if B < 109951162777600 then
	begin
		Result := NToS((10 * B + 1099511627776 div 2) div 1099511627776, -1, OutputFormat) + Sep + 'T' + iB;
	end
	else if B < 1125899906842624 then //2^50 ($4 000 000 000 000)
	begin
		Result := NToS((B + 1099511627776 div 2) div 1099511627776, 0, OutputFormat) + Sep + 'T' + iB;
	end
	else if B < 11258999068426240 then
	begin;
		Result := NToS((100 * B + 1125899906842624 div 2) div 1125899906842624, -2, OutputFormat) + Sep + 'P' + iB; // Peta
	end
	else if B < 112589990684262400 then
	begin
		Result := NToS((10 * B + 1125899906842624 div 2) div 1125899906842624, -1, OutputFormat) + Sep + 'P' + iB;
	end
	else if B < 1152921504606846976 then //2^60 ($1 000 000 000 000 000)
	begin
		Result := NToS((B + 1125899906842624 div 2) div 1125899906842624, 0, OutputFormat) + Sep + 'P' + iB;
	end
	else //if B < 11529215046068469760 then
		Result := NToS((100 * (B div 128) + 1152921504606846976 div 256) div (1152921504606846976 div 128), -2, OutputFormat) + Sep + 'E' + iB; // Exa
	// ZB
	// YB
	if B < 0 then Result := '-' + Result;
end;

function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;
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
		Result := NToS(Value, OutputFormat)
	else if Value < G then
		Result := NToS((Value + K div 2) div K, OutputFormat) + ' k'
	else if Value < T then
		Result := NToS((Value + M div 2) div M, OutputFormat) + ' M'
	else if Value < E then
		Result := NToS((Value + G div 2) div G, OutputFormat) + ' G'
	else
		Result := NToS((Value + T div 2) div T, OutputFormat) + ' T';
end;

procedure MsToHMSD(const T: S8; out GH, GM, GS, GD: U4);
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

function MsToStr(DT: S8; Display: TDisplay = diDHMSD;
	const Decimals: SG = -3; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;
var
	h, m, s, d: U4;
	Year, Week, Day: SG;
	Res, Rem: U1;

	TimeSep, DecimalSep, ListSep: string;
begin
	case OutputFormat of
	ofDisplay:
	begin
		TimeSep := TimeSeparator;
		DecimalSep := DecimalSeparator;
		ListSep := ListSeparator;
	end
	else {ofIO, ofHTML:}
	begin
		TimeSep := ':';
		DecimalSep := '.';
		ListSep := '; ';
	end;
	end;

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
			Year := DT div MSecsPerYear;
			Result := Result + IntToStr(Year) + CharSpace + Translate('year' + Plural(Year)) + ListSep;
			DT := DT mod MSecsPerYear;
		end;
		if DT >= MSecsPerWeek then
		begin
			Week := DT div MSecsPerWeek;
			Result := Result + IntToStr(Week) + CharSpace + Translate('week' + Plural(Week)) + ListSep;
			DT := DT mod MSecsPerWeek;
		end;
		if DT >= MSecsPerDay then
		begin
			Day := DT div MSecsPerDay;
			Result := Result + IntToStr(Day) + CharSpace + Translate('day' + Plural(Day)) + ListSep;
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

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
begin
	Assert(OutputFormat <> ofDisplay);
	Result := NToS(Year, '0000') + '-' + NToS(Month, '00') + '-' + NToS(Day, '00');
end;

function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
var Year, Month, Day: U2;
begin
	if D = 0 then
		Result := ''
	else
	begin
		case OutputFormat of
		ofDisplay:
		begin
			try
				Result := DateToStr(D);
			except
				Result := 'unknown';
			end;
		end
		else
		begin
			DecodeDate(D, Year, Month, Day);
			Result := DateToS(Year, Month, Day, OutputFormat);
		end;
		end;
	end;
end;

function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	case OutputFormat of
	ofDisplay:
		try
			Result := TimeToStr(T);
		except
			Result := 'unknown';
		end;
	else
		Result := MsToStr(RoundN(T * MSecsPerDay), diHHMSD, Decimals, False, OutputFormat);
	end;
end;

function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	if DT = 0 then
		Result := ''
	else
	begin
		case OutputFormat of
		ofDisplay:
		begin
			try
				Result := DateTimeToStr(DT);
			except
				Result := 'unknown';
			end;
		end
		else
			// YYYY-MM-DD HH:MM:SS
			Result := DateToS(Trunc(DT), OutputFormat) + ' ' + TimeToS(Frac(DT), Decimals, OutputFormat);
		end;
	end;
end;

function PhoneToStr(const Phone: U8): string;
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

procedure GetLocale;
{$ifndef LINUX}
var
	s: string;
	InLineIndex: SG;
{$endif}
begin
	{$ifndef LINUX}
	NativeSymbols := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNATIVEDIGITS, '0123456789');

	DecimalSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SDECIMAL, '.');
	DigitsAfterDecimal := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_IDIGITS, '0'), 0);
	ThousandSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_STHOUSAND, ',');

	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SGROUPING, '0');
	InLineIndex := 1;
	ThousandGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);
	FractionGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 0);

	NegSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNEGATIVESIGN, '-');
	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SPOSITIVESIGN, '+');
{	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGSIGNPOSN, '1');
	PosSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGSIGNPOSN, '1');}
	NegFormat := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGNUMBER, '1'), 1);
	LeadingZero := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ILZERO, '2'), 2);
	ListSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SLIST, ';');

// Time Format
	TimeSeparator := GetLocaleStr(SysLocale.DefaultLCID{ GetThreadLocale}, LOCALE_STIME, ':');
{	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ICENTURY, '0');
	InLineIndex := 1;
	ICentury := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);}
	{$ELSE}
	NativeSymbols := '0123456789';

	DecimalSeparator := '.';
	DigitsAfterDecimal := 0;
	ThousandSeparator := ',';

	ThousandGroup := 3;
	FractionGroup := 0;

	NegSymbol := '-';
	PosSymbol := '+';
	NegFormat := 1;
	LeadingZero := 2;
	ListSeparator := ';';

// Time Format
	TimeSeparator := ':';

	{$ENDIF}
end;

function GetEnglishSetting: TFormatSettings;
begin
  Result.CurrencyFormat := $00; // 0 = '$1'
  Result.NegCurrFormat := $00; //0 = '($1)'
  Result.CurrencyString := '$';
  Result.CurrencyDecimals := 2;

  Result.ThousandSeparator := ',';
  Result.DecimalSeparator := '.';

  Result.DateSeparator := '/';
  Result.ShortDateFormat := 'M/d/yyyy';
  Result.LongDateFormat := 'dddd, MMMM dd, yyyy';

  Result.TimeSeparator := ':';
  Result.TimeAMString := 'AM';
  Result.TimePMString := 'PM';
  Result.LongTimeFormat := 'h:mm:ss AMPM';
  Result.ShortTimeFormat := 'h:mm AMPM';

  Result.ShortMonthNames[1] := 'Jan';
  Result.ShortMonthNames[2] := 'Feb';
  Result.ShortMonthNames[3] := 'Mar';
  Result.ShortMonthNames[4] := 'Apr';
  Result.ShortMonthNames[5] := 'May';
  Result.ShortMonthNames[6] := 'Jun';
  Result.ShortMonthNames[7] := 'Jul';
  Result.ShortMonthNames[8] := 'Aug';
  Result.ShortMonthNames[9] := 'Sep';
  Result.ShortMonthNames[10] := 'Oct';
  Result.ShortMonthNames[11] := 'Nov';
  Result.ShortMonthNames[12] := 'Dec';

  Result.LongMonthNames[1] := 'January';
  Result.LongMonthNames[2] := 'February';
  Result.LongMonthNames[3] := 'March';
  Result.LongMonthNames[4] := 'April';
  Result.LongMonthNames[5] := 'May';
  Result.LongMonthNames[6] := 'June';
  Result.LongMonthNames[7] := 'July';
  Result.LongMonthNames[8] := 'August';
  Result.LongMonthNames[9] := 'September';
  Result.LongMonthNames[10] := 'October';
  Result.LongMonthNames[11] := 'November';
  Result.LongMonthNames[12] := 'December';

  Result.ShortDayNames[1] := 'Sun';
  Result.ShortDayNames[2] := 'Mon';
  Result.ShortDayNames[3] := 'Tue';
  Result.ShortDayNames[4] := 'Wed';
  Result.ShortDayNames[5] := 'Thu';
  Result.ShortDayNames[6] := 'Fri';
  Result.ShortDayNames[7] := 'Sat';

  Result.LongDayNames[1] := 'Sunday';
  Result.LongDayNames[2] := 'Monday';
  Result.LongDayNames[3] := 'Tuesday';
  Result.LongDayNames[4] := 'Wednesday';
  Result.LongDayNames[5] := 'Thursday';
  Result.LongDayNames[6] := 'Friday';
  Result.LongDayNames[7] := 'Saturday';

  Result.ListSeparator := ',';
end;

initialization
{$IFNDEF NoInitialization}
	GetLocale;
  IOFormatSettings := GetEnglishSetting;
{$ENDIF NoInitialization}
end.
