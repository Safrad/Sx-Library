unit uLocaleFormatSettings;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uEnglishFormatSettings;

type
  TLocaleFormatSettings = class(TEnglishFormatSettings)
  public
    constructor Create;
  end;

function LocaleFormatSettings: TLocaleFormatSettings;

implementation

uses
  SysUtils,
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}

  uTypes,
  uStrings,
  uNegativeNumberFormat;

var
  GLocaleFormatSettings: TLocaleFormatSettings;

function LocaleFormatSettings: TLocaleFormatSettings;
begin
  if GLocaleFormatSettings = nil then
    GLocaleFormatSettings := TLocaleFormatSettings.Create;

  Result := GLocaleFormatSettings;
end;

{ TLocaleFormatSettings }

constructor TLocaleFormatSettings.Create;
{$IFDEF MSWINDOWS}
var
	s: string;
	InLineIndex: SG;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
	//FNativeSymbols := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNATIVEDIGITS, '0123456789');
//	FIOFormatSettings.DigitsAfterDecimal := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_IDIGITS, '0'), 0);

	FDecimalSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SDECIMAL, FDecimalSeparator);
  if Length(FDecimalSeparator) = 1 then
    FIOFormatSettings.DecimalSeparator := FirstChar(FDecimalSeparator);

	FThousandSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_STHOUSAND, FThousandSeparator);
  if Length(FThousandSeparator) = 1 then
    FIOFormatSettings.ThousandSeparator := FirstChar(FThousandSeparator);

	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SGROUPING, '3;0');
	InLineIndex := 1;
	FThousandGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);
	FFractionGroup := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 0);

	FPositiveSymbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SPOSITIVESIGN, '+');
  FNegativeNumberFormat.Symbol := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SNEGATIVESIGN, FNegativeNumberFormat.Symbol);
  FNegativeNumberFormat.Style := TNegativeNumberStyle(StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_INEGNUMBER, IntToStr(SG(FNegativeNumberFormat.Style))), SG(FNegativeNumberFormat.Style)));
	FLeadingZero := StrToIntDef(GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ILZERO, IntToStr(SG(FLeadingZero))), SG(FLeadingZero));
	FListSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_SLIST, FListSeparator);
  if Length(FListSeparator) = 1 then
    FIOFormatSettings.ListSeparator := FirstChar(FListSeparator);

// Time Format
	FTimeSeparator := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_STIME, FTimeSeparator);
  if Length(FTimeSeparator) = 1 then
    FIOFormatSettings.TimeSeparator := FirstChar(FTimeSeparator);
{	s := GetLocaleStr(SysLocale.DefaultLCID, LOCALE_ICENTURY, '0');
	InLineIndex := 1;
	ICentury := StrToIntDef(ReadToChar(s, InLineIndex, ';'), 3);}
{$ENDIF}
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GLocaleFormatSettings);
{$ENDIF NoFinalization}
end.
