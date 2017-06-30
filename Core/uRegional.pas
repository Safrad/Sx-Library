unit uRegional;

interface

uses uTypes, Controls;

function DateToCS(const D: TDateTime): string; // Birthday, Players desription
function DateToCSLong(const D: TDateTime): string; // Chart legend
function DateToCSShort(const D: TDateTime): string; // Events

function IsNewYear(const ADate: TDate): BG;
function IsChristmas(const ADate: TDate): BG;
function GetEasterSunday(const AYear: SG): TDateTime;
function GetEasterMonday(const AYear: SG): TDateTime;
function IsEaster(const ADate: TDate): BG;
function IsCzechPublicHolidayDay(const Date: TDateTime): BG;

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

function GetEasterSundayGaussian(const AYear: SG): TDateTime;
var
  A, BC, D, E, K, P, Q, M, N, MarchDay: Integer;
begin
  A := AYear mod 19;
  BC := (AYear + AYear div 4) mod 7;
  K := AYear div 100;
  P := (13 + 8 * K) div 25;
  Q := K div 4;
  M := 15 - P + K - Q;
  N := 4 + K - Q;
  D := (19 * A + M) mod 30;

  if D = 29 then
    Dec(D);
  if (D = 28) and (A > 10) then
    Dec(D);

  E := (35 + N - BC - D) mod 7;

  MarchDay := D + E + 22;
  if MarchDay <= 31 then
    Result := EncodeDate(AYear, 3, MarchDay)
  else
    Result := EncodeDate(AYear, 4, MarchDay - 31);
end;

function GetEasterSundayGaussianOld(const AYear: SG): TDateTime;
const
  M = 24;
  N = 5;
var
  A, B, C, D, E, AprilDay: Integer;
begin
  A := AYear mod 19;
  B := AYear mod 4;
  C := AYear mod 7;
  D := (19 * A + M) mod 30;
  E := (N + 2 * B + 4 * C + 6 * D) mod 7;
  AprilDay := D + E - 9;

  if (AprilDay = 25) and (D = 28) and (E = 6) and (A > 10) then
  begin
    Result := EncodeDate(AYear, 4, 18)
  end
  else if (AprilDay >= 1) and (AprilDay <= 25) then
  begin
    Result := EncodeDate(AYear, 4, AprilDay);
  end
  else if AprilDay > 25 then
  begin
    Result := EncodeDate(AYear, 4, AprilDay - 7);
  end
  else // if AprilDay < 0
  begin
    Result := EncodeDate(AYear, 3, AprilDay + 31);
  end;
end;

function GetEasterSunday(const AYear: SG): TDateTime;
begin
  Result := GetEasterSundayGaussian(AYear);
//  Result := GetEasterSundayGaussianOld(AYear);
end;

function GetEasterMonday(const AYear: SG): TDateTime;
begin
  Result := GetEasterSunday(AYear) + 1;
end;

function IsEaster(const ADate: TDate): BG;
var
  EasterMonday: TDateTime;
  Year: SG;
begin
  Year := YearOf(ADate);
  EasterMonday := GetEasterMonday(Year);
  Result := ADate = EasterMonday;
  if (Result = False) and (Year >= 2016) then
  begin
    // Newly Easter Friday
    Result := ADate = EasterMonday - 3;
  end;
end;

function IsCzechPublicHolidayDay(const Date: TDateTime): BG;
var
  Day, Month, Year: U2;
begin
  Result := False;
  DecodeDate(Date, Year, Month, Day);

  case Month of
  1:
  begin
    if Day in [1] then
      Result := True;
  end;
  3, 4:
  begin
    Result := IsEaster(Date);
  end;
  5:
  begin
    if Day in [1, 8] then
      Result := True;
  end;
  7:
  begin
    if Day in [5, 6] then
      Result := True;
  end;
  9:
  begin
    if Day in [28] then
      Result := True;
  end;
  10:
  begin
    if Day in [28] then
      Result := True;
  end;
  11:
  begin
    if Day in [17] then
      Result := True;
  end;
  12:
  begin
    if Day in [24, 25, 26] then
      Result := True;
  end;
  end;
end;

initialization
  GIsNewYearOnStartup := IsNewYear(Date);
  GIsChristmasOnStartup := IsChristmas(Date);
end.
