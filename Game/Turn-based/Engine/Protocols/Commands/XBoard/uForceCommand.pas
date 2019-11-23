unit uForceCommand;

interface

uses
  uXBoardProtocolCommand;

type
  TForceCommand = class(TXBoardProtocolCommand)
  protected
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  uTypes;

{ TForceCommand }

constructor TForceCommand.Create;
begin
  inherited;

  Description := 'Set the engine to play neither color ("force mode"). Stop clocks. The engine should check that moves received in force mode are legal and made in the proper turn, but should not think, ponder, or make moves of its own.';
end;

procedure TForceCommand.Execute(const AParameters: string);
begin
  inherited;

  XBoardProtocol.AutoPlay := False;
end;

function TForceCommand.GetSyntax: string;
begin
  Result := '';
end;

end.

