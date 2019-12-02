unit uXBoardCommand;

interface

uses
  uProtocolCommand;

type
  TXBoardCommand = class(TProtocolCommand)
  public
    constructor Create;
    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uXBoardProtocol,
  uXBoardEngineOutput;

{ TXBoardCommand }

constructor TXBoardCommand.Create;
begin
  inherited;

  Description := 'Use the XBoard interface.';
end;

procedure TXBoardCommand.ExecuteNoParam;
begin
  inherited;

  ConsoleEngine.InternalEngine.Output.Free; // TNoProtocolOutput.Free
  ConsoleEngine.InternalEngine.Output := TXBoardEngineOutput.Create;
  TXBoardEngineOutput(ConsoleEngine.InternalEngine.Output).AnalysisInfo := ConsoleEngine.InternalEngine.AnalysisInfo;

  ConsoleEngine.Protocol := TXBoardProtocol.Create;
end;

end.

