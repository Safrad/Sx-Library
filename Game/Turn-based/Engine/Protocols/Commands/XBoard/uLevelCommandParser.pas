unit uLevelCommandParser;

interface

uses
  uTypes,
  uTimeSpan,
  uCustomParser;

type
  TLevelCommandParser = class(TInterfacedObject, ICustomParser)
  private
    FMoveIncrementTime: TTimeSpan;
    FIncrementTime: TTimeSpan;
    FMoveCount: UG;
    function ReadXBoardTime(const AText: string; const ANumberIsMinutes: BG): TTimeSpan;
    procedure SetIncrementTime(const Value: TTimeSpan);
    procedure SetMoveCount(const Value: UG);
    procedure SetMoveIncrementTime(const Value: TTimeSpan);
  protected
  public
    // Process
    procedure Parse(const AText: string);

    // Output
    function GetSyntax: string;
    property MoveCount: UG read FMoveCount write SetMoveCount;
    property IncrementTime: TTimeSpan read FIncrementTime write SetIncrementTime;
    property MoveIncrementTime: TTimeSpan read FMoveIncrementTime write SetMoveIncrementTime;
    function ParametersToString: string;
  end;

implementation

uses
  SysUtils,

  uInputFormat,
  uOutputFormat,
  uStrings;

{ TLevelCommandParser }

function TLevelCommandParser.ParametersToString: string;
begin
  Result :=
    IntToStr(FMoveCount) + CharSpace +
    MsToStr(FIncrementTime.Milliseconds, diMSD, 0) + CharSpace +
    FToS(FMoveIncrementTime.SecondsAsF);
end;

procedure TLevelCommandParser.Parse(const AText: string);
var
  InLineIndex: SG;
begin
  inherited;

  InLineIndex := 1;

  FMoveCount := ReadSGFast(AText, InLineIndex);
  SkipSpace(AText, InLineIndex);
  FIncrementTime := ReadXBoardTime(ReadToChar(AText, InLineIndex, CharSpace), True);
  FMoveIncrementTime := ReadXBoardTime(ReadToChar(AText, InLineIndex, CharSpace), False);
end;

function TLevelCommandParser.GetSyntax: string;
begin
  Result := '[RemainMoves BaseTimeInMinutes TimeIncrementInSeconds]';
end;

function TLevelCommandParser.ReadXBoardTime(const AText: string; const ANumberIsMinutes: BG): TTimeSpan;
const
  MaximalValueInMinutes = 10 * 365 * 24 * 60; // 10 years
  MaximalValueInSeconds = 10 * 365 * 24 * 60 * 60; // 10 years
var
  MinValue, DefValue, MaxValue: TTimeSpan;
begin
  if Pos(':', AText) <= 0 then
  begin
    if ANumberIsMinutes then
    begin
      // Simple number represents minutes => convert seconds to minutes
      Result.MinutesAsBD := StrToValBD(AText, False, 0, 0, MaximalValueInMinutes);
    end
    else
    begin
      Result.SecondsAsBD := StrToValBD(AText, False, MaximalValueInSeconds);
    end;
  end
  else
  begin
    MinValue.Ticks := 0;
    DefValue.Ticks := 0;
    MaxValue.Days := 1;
    Result := StrToMs(AText, MinValue, DefValue, MaxValue, False);
  end;
end;

procedure TLevelCommandParser.SetIncrementTime(const Value: TTimeSpan);
begin
  FIncrementTime := Value;
end;

procedure TLevelCommandParser.SetMoveCount(const Value: UG);
begin
  FMoveCount := Value;
end;

procedure TLevelCommandParser.SetMoveIncrementTime(const Value: TTimeSpan);
begin
  FMoveIncrementTime := Value;
end;

end.

