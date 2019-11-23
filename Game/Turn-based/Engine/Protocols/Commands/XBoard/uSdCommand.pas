unit uSdCommand;

interface

uses
  uEngineCommand;

type
  TSdCommand = class(TEngineCommand)
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
  uEParseError,

  uInfiniteLevel;

{ TSdCommand }

constructor TSdCommand.Create;
begin
  inherited;

  Description :=
    'The engine should limit its thinking to DEPTH ply. ' +
    'The commands "level" or "st" and "sd" can be used together in an orthogonal way. ' +
    'If both are issued, the engine should observe both limitations: ' +
    'In the protocol, the "sd" command isn''t a time control. ' +
    'It doesn''t say that your engine has unlimited time but must search to exactly the given depth. ' +
    'It says that you should pay attention to the time control as normal, ' +
    'but cut off the search at the specified depth even if you have time to search deeper.';
end;

procedure TSdCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
begin
  inherited;

  if AParameters = '' then
  begin
    raise EParseError.Create(['maximal depth'], '');
  end;

  InLineIndex := 1;
  InternalEngine.StopManager.LimitMaxDepth := ReadSGFast(AParameters, InLineIndex);

  if InternalEngine.LevelManager.MyLevel = nil then
  begin
    InternalEngine.LevelManager.MyLevel := TInfiniteLevel.Create;
  end;
end;

function TSdCommand.GetSyntax: string;
begin
  Result := '[maximal depth]';
end;

end.

