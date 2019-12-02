unit uUSICommand;

interface

uses
  uProtocolCommand;

type
  TUSICommand = class(TProtocolCommand)
  public
    constructor Create;
    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uUSIProtocol,
  uUSIEngineOutput,
  uGameVariant,
  uGameVariants;

{ TUSICommand }

constructor TUSICommand.Create;
begin
  inherited;

  Description := 'Use the USI (universal shogi interface).';
end;

procedure TUSICommand.ExecuteNoParam;
begin
  inherited;

  ConsoleEngine.InternalEngine.Output.Free; // TNoProtocolOutput.Free
  ConsoleEngine.InternalEngine.Output := TUSIEngineOutput.Create;

  TUSIEngineOutput(ConsoleEngine.InternalEngine.Output).AnalysisInfo := ConsoleEngine.InternalEngine.AnalysisInfo;
  ConsoleEngine.Protocol := TUSIProtocol.Create;

  ConsoleEngine.InternalEngine.GameVariant := GameVariants.FindByName('shogi');
end;

end.
