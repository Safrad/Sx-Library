unit uLevelCommand;

interface

uses
  uEngineCommand,
  uLevelCommandParser,
  uTimeControlLevel;

type
  TLevelCommand = class(TEngineCommand)
  private
    FTimeControlLevel: TTimeControlLevel;
    FLevelCommandParser: TLevelCommandParser;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const AParameters: string); override;
  end;

implementation

{ TLevelCommand }

constructor TLevelCommand.Create;
begin
  inherited;

  Description := 'In conventional clock mode, every time control period is the same. That is, if the time control is 40 moves in 5 minutes, then after each side has made 40 moves, they each get an additional 5 minutes, and so on, ad infinitum.';
  FTimeControlLevel := TTimeControlLevel.Create;
  FLevelCommandParser := TLevelCommandParser.Create;
end;

destructor TLevelCommand.Destroy;
begin
  try
    inherited;
  finally
    FLevelCommandParser.Free;
    FTimeControlLevel.Free;
  end;
end;

procedure TLevelCommand.Execute(const AParameters: string);
begin
  inherited;

  FLevelCommandParser.Parse(AParameters);

  FTimeControlLevel.TimeUsage := InternalEngine.CommonOptions.TimeUsage.Value;
  FTimeControlLevel.FixedMoveTime := InternalEngine.CommonOptions.FixedMoveTime.Value;
  FTimeControlLevel.MinimalTime.Milliseconds := InternalEngine.CommonOptions.MinimalMoveTime.Value;
  FTimeControlLevel.MaximalMoveOverhead.Milliseconds := InternalEngine.CommonOptions.MaximalMoveOverhead.Value;

  FTimeControlLevel.MoveCount := FLevelCommandParser.MoveCount;
  FTimeControlLevel.IncrementTime := FLevelCommandParser.IncrementTime;
  FTimeControlLevel.MoveIncrementTime := FLevelCommandParser.MoveIncrementTime;
  FTimeControlLevel.FreeTime.Ticks := 0;
  FTimeControlLevel.MoveIndex := 0; // Init time limits

  FTimeControlLevel.AnalysisInfo := InternalEngine.AnalysisInfo;
  InternalEngine.LevelManager.MyLevel := FTimeControlLevel;
end;

function TLevelCommand.GetSyntax: string;
begin
  Result := FLevelCommandParser.GetSyntax;
end;

end.

