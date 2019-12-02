unit uLevelCommand;

interface

uses
  uTypes,
  uTimeSpan,
  uTimeControlLevel,
  uEngineCommand;

type
  TLevelCommand = class(TEngineCommand)
  private
    FTimeControlLevel: TTimeControlLevel;
    function ReadXBoardTime(const AText: string; const ANumberIsMinutes: BG): TTimeSpan;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,
  uInputFormat,
  uStrings;

{ TLevelCommand }

constructor TLevelCommand.Create;
begin
  inherited;

  Description := 'In conventional clock mode, every time control period is the same. That is, if the time control is 40 moves in 5 minutes, then after each side has made 40 moves, they each get an additional 5 minutes, and so on, ad infinitum.';
end;

destructor TLevelCommand.Destroy;
begin
  try
    inherited;
  finally
    FTimeControlLevel.Free;
  end;
end;

procedure TLevelCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
begin
  inherited;

  InLineIndex := 1;
  FTimeControlLevel := TTimeControlLevel.Create;
  FTimeControlLevel.MoveCount := ReadSGFast(AParameters, InLineIndex);

  SkipSpace(AParameters, InLineIndex);
  FTimeControlLevel.MaximalMoveOverhead.Milliseconds := InternalEngine.CommonOptions.MaximalMoveOverhead.Value;
  FTimeControlLevel.IncrementTime := ReadXBoardTime(ReadToChar(AParameters, InLineIndex, CharSpace), True);
  FTimeControlLevel.MoveIncrementTime := ReadXBoardTime(ReadToChar(AParameters, InLineIndex, CharSpace), False);
  FTimeControlLevel.FreeTime.Ticks := 0;
  FTimeControlLevel.MoveIndex := 0;
  FTimeControlLevel.TimeUsage := InternalEngine.CommonOptions.TimeUsage.Value;
  FTimeControlLevel.FixedMoveTime := InternalEngine.CommonOptions.FixedMoveTime.Value;
  FTimeControlLevel.MinimalTime.Milliseconds := InternalEngine.CommonOptions.MinimalMoveTime.Value;

  FTimeControlLevel.AnalysisInfo := InternalEngine.AnalysisInfo;
  InternalEngine.LevelManager.MyLevel := FTimeControlLevel;
end;

function TLevelCommand.GetSyntax: string;
begin
  Result := '[RemainMoves BaseTimeInMinutes TimeIncrementInSeconds]';
end;

function TLevelCommand.ReadXBoardTime(const AText: string; const ANumberIsMinutes: BG): TTimeSpan;
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

end.

