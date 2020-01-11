unit uEnglishFormatSettings;

interface

uses
  SysUtils,

  uTypes,
  uNegativeNumberFormat;

type
  TEnglishFormatSettings = class
  protected
    var
      FIOFormatSettings: TFormatSettings;

      // Only Char in TFormatSettings
      FDecimalSeparator: string;
      FThousandSeparator: string;
      FListSeparator: string;
      FTimeSeparator: string;

      FThousandGroup: SG;
      FFractionGroup: SG;
      FLeadingZero: SG;
      FDigitsAfterDecimal: SG;

      FPositiveSymbol: string;
      FNegativeNumberFormat: TNegativeNumberFormat;
  public
    constructor Create;

    /// English format settings
    property IOFormatSettings: TFormatSettings read FIOFormatSettings;

    property DecimalSeparator: string read FDecimalSeparator;

    property ThousandSeparator: string read FThousandSeparator;

    property ListSeparator: string read FListSeparator;

    /// Digit grouping
    property ThousandGroup: SG read FThousandGroup;

    property FractionGroup: SG read FFractionGroup;

    /// Display leading zeros
    property LeadingZero: SG read FLeadingZero;

    property NegativeNumberFormat: TNegativeNumberFormat read FNegativeNumberFormat;
  end;

function EnglishFormatSettings: TEnglishFormatSettings;

implementation

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

  Result.ListSeparator := ';';
end;

var
  GEnglishFormatSettings: TEnglishFormatSettings;

function EnglishFormatSettings: TEnglishFormatSettings;
begin
  if GEnglishFormatSettings = nil then
    GEnglishFormatSettings := TEnglishFormatSettings.Create;

  Result := GEnglishFormatSettings;
end;

{ TEnglishFormatSettings }

constructor TEnglishFormatSettings.Create;
begin
  inherited;

  FIOFormatSettings := GetEnglishSetting;

  // Not in TFormatSettings
	FDigitsAfterDecimal := 0;

	FThousandGroup := 3;
	FFractionGroup := 0;
	FLeadingZero := 1; // 0.3 TODO : 2?

	FPositiveSymbol := '+';
	FNegativeNumberFormat.Symbol := '-';
	FNegativeNumberFormat.Style := TNegativeNumberStyle.BeforeNumber;
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GEnglishFormatSettings);
{$ENDIF NoFinalization}
end.
