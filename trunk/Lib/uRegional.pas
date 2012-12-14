unit uRegional;

interface

uses uTypes;

function DateToCS(const D: TDateTime): string; // Birthday, Players desription
function DateToCSLong(const D: TDateTime): string; // Chart legend
function DateToCSShort(const D: TDateTime): string; // Events

function IsNewYear(const ADate: TDate): BG;
function IsChristmas(const ADate: TDate): BG;

function IsNewYearOnStartup: BG;
function IsChristmasOnStartup: BG;

implementation

uses
  SysUtils, DateUtils;

function DateToCS(const D: TDateTime): string; // Birthday, Players desription
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'd.m.yyyy', D);
end;

function DateToCSLong(const D: TDateTime): string; // Chart legend
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'dd.mm.yyyy', D);
end;

function DateToCSShort(const D: TDateTime): string; // Events
begin
	if D = 0 then
		Result := '-'
	else
		DateTimeToString(Result, 'd.m.', D);
end;

function IsNewYear(const ADate: TDate): BG;
var Day, Month, Year: U2;
begin
  DecodeDate(ADate, Year, Month, Day);
  Result := (Day = 1) and (Month = 1);
end;

function IsChristmas(const ADate: TDate): BG;
var Day, Month, Year: U2;
begin
  DecodeDate(ADate, Year, Month, Day);
  Result := (Day = 24) and (Month = 12);
end;

var
  GIsChristmasOnStartup: BG;
  GIsNewYearOnStartup: BG;

function IsNewYearOnStartup: BG;
begin
  Result := GIsNewYearOnStartup;
end;

function IsChristmasOnStartup: BG;
begin
  Result := GIsChristmasOnStartup;
end;

initialization
  GIsNewYearOnStartup := IsNewYear(Date);
  GIsChristmasOnStartup := IsChristmas(Date);
end.
