unit uDebugCommand;

interface

uses
  uEngineCommand;

type
  TDebugCommand = class(TEngineCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes,
  uStrings,
  uEParseError;

{ TDebugCommand }

constructor TDebugCommand.Create;
begin
  inherited;

  Description := 'Switch the debug mode of the engine on and off.';
end;

procedure TDebugCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
  Value: string;
begin
  inherited;

  InLineIndex := 1;
  Value := ReadToChar(AParameters, InLineIndex, CharSpace);
  if Value = 'on' then
    InternalEngine.Output.DebugMode := True
  else if Value = 'off' then
    InternalEngine.Output.DebugMode := False
  else
    raise EParseError.Create(['on', 'off'], Value);
end;

function TDebugCommand.GetSyntax: string;
begin
  Result := '[on | off]';
end;

end.
