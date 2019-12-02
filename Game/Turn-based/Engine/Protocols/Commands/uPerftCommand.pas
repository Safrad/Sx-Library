unit uPerftCommand;

interface

uses
  uTypes,
  uEngineCommand;

type
  TPerftCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,

  uStrings,
  uOutputFormat,
  uEParseError,
  uStopwatch,
  uMath;

{ TPerftCommand }

constructor TPerftCommand.Create;
begin
  inherited;

  Description :=
    'The complete game tree up to depth "Value" plies will be computed from the momentary position. ' +
    'No evaluations or tree-cuts will be done. ' +
    'This is just for testing the correctness and speed of the move generator.';
end;

procedure TPerftCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
  Depth: SG;
begin
  inherited;

  if AParameters = '' then
  begin
    raise EParseError.Create(['maximal depth'], '');
  end;

  InLineIndex := 1;
  Depth := ReadSGFast(AParameters, InLineIndex);
  InternalEngine.GetPerft(Depth);
end;

function TPerftCommand.GetSyntax: string;
begin
  Result := '[maximal depth]';
end;


end.
