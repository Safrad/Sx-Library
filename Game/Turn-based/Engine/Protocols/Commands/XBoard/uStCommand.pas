unit uStCommand;

interface

uses
  uEngineCommand,
  uMoveTimeLevel;

type
  TStCommand = class(TEngineCommand)
  private
    FMoveTimeLevel: TMoveTimeLevel;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes,
  uStrings,
  uEParseError,
  uInputFormat,

  uCustomLevel;

{ TStCommand }

constructor TStCommand.Create;
begin
  inherited;

  Description := 'Set an exact number of seconds per move.';
  FMoveTimeLevel := TMoveTimeLevel.Create;
  FMoveTimeLevel.ValueType := lvtEqual;
end;

destructor TStCommand.Destroy;
begin
  try
    FMoveTimeLevel.Free;
  finally
    inherited;
  end;
end;

procedure TStCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
begin
  inherited;

  if AParameters = '' then
  begin
    raise EParseError.Create(['exact time per move in seconds'], '');
  end;

  InLineIndex := 1;
  FMoveTimeLevel.Value.SecondsAsBD := StrToValBD(ReadToNewLine(AParameters, InLineIndex), False, 0, 1, 60 * 60 * 24);
  FMoveTimeLevel.AnalysisInfo := InternalEngine.AnalysisInfo;

  InternalEngine.LevelManager.MyLevel := FMoveTimeLevel;
end;

function TStCommand.GetSyntax: string;
begin
  Result := '[exact time per move in seconds]';
end;

end.

