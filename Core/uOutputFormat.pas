unit uOutputFormat;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  SysUtils,

  uTypes,
  uCommonOutputFormat,
  uLocaleOutputFormat;

type
	TOutputFormat = (
    /// <summary>
    /// Disk File Input/Output
    /// </summary>
    ofIO,
    /// <summary>
    /// Disk HTML
    /// </summary>
    ofHTML,
    /// <summary>
    /// Windows Locale
    /// </summary>
    ofDisplay);

const
  /// <summary>
  /// Non-dividable Blank Space.
  /// </summary>
	nbsp = '&nbsp;';

type
  TDisplay = TCommonOutputFormat.TDisplay;

function NumToStr(Num: S8; const ANumericBase: U1): string;

function NToS(const Num: S8; const UseFormat: string; const ANumericBase: U1 = 10): string; overload;
function NToS(const Num: U8; const UseFormat: string; const ANumericBase: U1 = 10): string; overload;

function NToS(const Num: S4; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;
function NToS(const Num: S8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;
function NToS(const Num: U8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;

function NToS(const Num: S4; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;
function NToS(const Num: S8; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;
function NToS(const Num: U8; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;

function FloatToDecimalString(const Value: FM; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TCommonOutputFormat.TFloatFormat = ffAuto): string;
function FToS(const Num: FM; const OutputFormat: TOutputFormat = ofDisplay): string;

function BToStr(const Bytes: S4; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function BToStr(const Bytes: S8; const OutputFormat: TOutputFormat = ofDisplay): string; overload;
function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;
function ExitCodeToString(const AExitCode: U4; const AOutputFormat: TOutputFormat): string;

function MsToStr(DT: S8; Display: TCommonOutputFormat.TDisplay = diDHMSD;
	const Decimals: SG = -3; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
function PhoneToStr(const Phone: U8): string;

function AddThousandSeparators(const AValue: string; const AFormatSettings: TFormatSettings; const AExponentPrefix: string): string;

implementation

function NumToStr(Num: S8; const ANumericBase: U1): string;
begin
  Result := CommonOutputFormat.NumToStr(Num, ANumericBase);
end;

function NToS(const Num: S8; const UseFormat: string; const ANumericBase: U1 = 10): string;
begin
  Result := CommonOutputFormat.NToS(Num, UseFormat, ANumericBase);
end;

function NToS(const Num: U8; const UseFormat: string; const ANumericBase: U1 = 10): string;
begin
  Result := CommonOutputFormat.NToS(Num, UseFormat, ANumericBase);
end;

function NToS(const Num: S4; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;
begin
	case OutputFormat of
	ofIO:
		Result := CommonOutputFormat.NToSBase(Num, ANumericBase);
	else
		Result := LocaleOutputFormat.NToSBase(Num, ANumericBase);
  end;
end;

function NToS(const Num: S4; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;
begin
	case OutputFormat of
	ofIO:
    Result := CommonOutputFormat.NToS(Num, Decimals, ANumericBase);
  else
    Result := LocaleOutputFormat.NToS(Num, Decimals, ANumericBase);
	end;
end;

function NToS(const Num: S8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.NToS(Num, Decimals, ANumericBase);
  else
    Result := LocaleOutputFormat.NToS(Num, Decimals, ANumericBase);
  end;
end;

function NToS(const Num: U8; const Decimals: SG = 0; const OutputFormat: TOutputFormat = ofDisplay; const ANumericBase: U1 = 10): string; overload;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.NToS(Num, Decimals, ANumericBase);
  else
    Result := LocaleOutputFormat.NToS(Num, Decimals, ANumericBase);
  end;
end;

function NToS(const Num: S8; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.NToSBase(Num, ANumericBase);
  else
    Result := LocaleOutputFormat.NToSBase(Num, ANumericBase);
  end;
end;

function NToS(const Num: U8; const OutputFormat: TOutputFormat; const ANumericBase: U1 = 10): string; overload;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.NToSBase(Num, ANumericBase);
  else
    Result := LocaleOutputFormat.NToSBase(Num, ANumericBase);
  end;
end;

function FloatToDecimalString(const Value: FM; const Precision: Integer = 16; const Decimals: Integer = 20; const FloatFormat: TCommonOutputFormat.TFloatFormat = ffAuto): string;
begin
  Result := CommonOutputFormat.FloatToDecimalString(Value, Precision, Decimals, FloatFormat);
end;

function AddThousandSeparators(const AValue: string; const AFormatSettings: TFormatSettings; const AExponentPrefix: string): string;
begin
  Result := CommonOutputFormat.AddThousandSeparators(AValue, AFormatSettings, AExponentPrefix);
end;

function FToS(const Num: FM; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.FToS(Num);
  else
    Result := LocaleOutputFormat.FToS(Num);
  end;
end;

function BToStr(const Bytes: S4; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.BToStr(Bytes);
  else
    Result := LocaleOutputFormat.BToStr(Bytes);
  end;
end;

function BToStr(const Bytes: S8; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.BToStr(Bytes);
  else
    Result := LocaleOutputFormat.BToStr(Bytes);
  end;
end;

function NodesToS(const Value: U8; const OutputFormat: TOutputFormat): string;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.NodesToS(Value);
  else
    Result := LocaleOutputFormat.NodesToS(Value);
  end;
end;

function ExitCodeToString(const AExitCode: U4; const AOutputFormat: TOutputFormat): string;
begin
  case AOutputFormat of
  ofIO:
    Result := CommonOutputFormat.ExitCodeToString(AExitCode);
  else
    Result := LocaleOutputFormat.ExitCodeToString(AExitCode);
  end;
end;

function MsToStr(DT: S8; Display: TCommonOutputFormat.TDisplay = diDHMSD;
	const Decimals: SG = -3; const FixedWidth: Boolean = False; const OutputFormat: TOutputFormat = ofDisplay): string;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.MsToStr(DT, Display, Decimals, FixedWidth);
  else
    Result := LocaleOutputFormat.MsToStr(DT, Display, Decimals, FixedWidth);
  end;

end;

function DateToS(const Year, Month, Day: U2; const OutputFormat: TOutputFormat): string; overload;
begin
  case OutputFormat of
  ofIO:
    Result := CommonOutputFormat.DateToS(Year, Month, Day);
  else
    Result := LocaleOutputFormat.DateToS(Year, Month, Day);
  end;
end;

function DateToS(const D: TDateTime; const OutputFormat: TOutputFormat): string; overload;
begin
	case OutputFormat of
	ofIO:
    Result := CommonOutputFormat.DateToS(D);
  else
    Result := LocaleOutputFormat.DateToS(D);
  end;
end;

function TimeToS(const T: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	case OutputFormat of
	ofIO:
    Result := CommonOutputFormat.TimeToS(T, Decimals);
  else
    Result := LocaleOutputFormat.TimeToS(T, Decimals);
  end;
end;

function DateTimeToS(const DT: TDateTime; const Decimals: SG; const OutputFormat: TOutputFormat): string;
begin
	case OutputFormat of
	ofIO:
    Result := CommonOutputFormat.DateTimeToS(DT, Decimals);
  else
    Result := LocaleOutputFormat.DateTimeToS(DT, Decimals);
  end;
end;

function PhoneToStr(const Phone: U8): string;
begin
  Result := CommonOutputFormat.PhoneToStr(Phone);
end;

end.
