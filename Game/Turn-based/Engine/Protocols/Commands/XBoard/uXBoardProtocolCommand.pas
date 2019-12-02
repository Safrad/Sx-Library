// Ancestor for Commands

unit uXBoardProtocolCommand;

interface

uses
  uEngineCommand,
  uXBoardProtocol;

type
  TXBoardProtocolCommand = class(TEngineCommand)
  private
    FXBoardProtocol: TXBoardProtocol;
    procedure SetXBoardProtocol(const Value: TXBoardProtocol);
  public
    property XBoardProtocol: TXBoardProtocol read FXBoardProtocol write SetXBoardProtocol;
  end;

implementation

{ TXBoardProtocolCommand }

procedure TXBoardProtocolCommand.SetXBoardProtocol(const Value: TXBoardProtocol);
begin
  FXBoardProtocol := Value;
end;

end.
