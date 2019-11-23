// Ancestor for protocol specification commands (uci, xboard)

unit uProtocolCommand;

interface

uses
  uConsoleEngine,
  uSimpleCommand;

type
  TProtocolCommand = class(TSimpleCommand)
  private
    FConsoleEngine: TConsoleEngine;
    procedure SetConsoleEngine(const Value: TConsoleEngine);
  public
    property ConsoleEngine: TConsoleEngine read FConsoleEngine write SetConsoleEngine;
  end;

implementation

{ TProtocolCommand }

procedure TProtocolCommand.SetConsoleEngine(const Value: TConsoleEngine);
begin
  FConsoleEngine := Value;
end;

end.
