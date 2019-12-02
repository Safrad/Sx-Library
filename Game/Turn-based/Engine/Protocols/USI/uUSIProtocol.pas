// http://hgm.nubati.net/usi.html
unit uUSIProtocol;

interface

uses
  uTypes,

  uUCIProtocol;

type
  TUSIProtocol = class(TUCIProtocol)
  public
    constructor Create;
    procedure Initialize; override;
  end;

implementation

uses
  uStrings;

{ TUSIProtocol }

constructor TUSIProtocol.Create;
begin
  inherited;

  InitializationReply := 'usiok';
end;

procedure TUSIProtocol.Initialize;
var
  i: SG;
begin
  FShowCurrentMoveArgument.Shortcut := ReplaceF(FShowCurrentMoveArgument.Shortcut, CharSpace, '');
  FShowSelectiveDepthArgument.Shortcut := ReplaceF(FShowSelectiveDepthArgument.Shortcut, CharSpace, '');
  for i := 0 to InternalEngine.Options.DefinedCount - 1 do
  begin
    InternalEngine.Options[i].Shortcut := ReplaceF(InternalEngine.Options[i].Shortcut, CharSpace, '');
  end;

  inherited;

  NewGameCommand.Shortcut := 'usinewgame';
end;

end.
