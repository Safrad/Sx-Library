unit uTimeInterval;

interface

uses
  uTypes;

type
  TTimePeriod = (
    tpAll,
    tpThisYear,
    tpPreviousYear,
    tpCustomYear,
    tpThisMonth,
    tpPreviousMonth,
    tpCustomMonth,
    tpThisQuarter,
    tpPreviousQuarter,
    tp1stQuarter,
    tp2ndQuarter,
    tp3rdQuarter,
    tp4thQuarter,
    tpCustomRange);
var
  TimePeriodStr: array[TTimePeriod] of string;

type
  TTimeInterval = class
  private
    FFromDateTime: TDateTime;
    FToDateTime: TDateTime;
  public
    procedure SetYear(const AYear: SG);
    function Length: F8;
    function LengthInMs: S8;
    function ToPeriod: string;
    function ToYear: string;
    function Contains(const DateTime: TDateTime): BG;
  published
    property FromDateTime: TDateTime read FFromDateTime write FFromDateTime;
    property ToDateTime: TDateTime read FToDateTime write FToDateTime;
  end;

implementation

uses
  SysUtils,
  uStrings;

{ TTimeInterval }

function TTimeInterval.Contains(const DateTime: TDateTime): BG;
begin
  Result := (DateTime >= FromDateTime) and (DateTime <= ToDateTime);
end;

function TTimeInterval.Length: F8;
begin
  Result := ToDateTime - FromDateTime;
end;

function TTimeInterval.LengthInMs: S8;
begin
  Result := Round(Day * Length);
end;

procedure TTimeInterval.SetYear(const AYear: SG);
begin
  FromDateTime := EncodeDate(AYear, 1, 1);
  ToDateTime := EncodeDate(AYear, 12, 31);
end;

function YearInterval(const FY, TY: SG): string;
begin
  if FY = TY then
    Result := IntToStr(FY)
  else
    Result := IntToStr(FY) + '-' + IntToStr(TY);
end;

function TTimeInterval.ToPeriod: string;
var
  FD, FM, FY: U2;
  TD, TM, TY: U2;
begin
  DecodeDate(FromDateTime, FY, FM, FD);
  DecodeDate(ToDateTime, TY, TM, TD);
  if FromDateTime = ToDateTime then
    Result := DateTimeToStr(FromDateTime)
  else
  begin
    if (FD = 1) and (FM = 1) and (TD = 31) and (TM = 12) then
    begin
      Result := YearInterval(FY, TY);
    end
    else if (FD = 1) then // TODO : TD = last day in month
    begin
      if (FM = TM) and (FY = TY) then
        Result := IntToStr(FM) + '/' + IntToStr(FY)
      else
        Result := IntToStr(FM) + '/' + IntToStr(FY) + '-' +
          IntToStr(TM) + '/' + IntToStr(TY);
    end
    else
      Result := DateTimeToStr(FromDateTime) + '-' + DateTimeToStr(ToDateTime);
  end;
end;

function GetYear(const DateTime: TDateTime): SG;
var
  FD, FM, FY: U2;
begin
  DecodeDate(DateTime, FY, FM, FD);
  Result := FY;
end;

function TTimeInterval.ToYear: string;
var
  FY, TY: U2;
begin
  FY := GetYear(FromDateTime);
  TY := GetYear(ToDateTime);
  Result := YearInterval(FY, TY);
end;

initialization
  EnumToStr(TypeInfo(TTimePeriod), TimePeriodStr);

end.
