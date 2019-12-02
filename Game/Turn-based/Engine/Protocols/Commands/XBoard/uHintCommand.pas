unit uHintCommand;

interface

uses
  uSimpleEngineCommand;

type
  THintCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uTextType,

  uAnalysis;

{ THintCommand }

constructor THintCommand.Create;
begin
  inherited;

  Description := 'Asks for a hint.';
end;

procedure THintCommand.ExecuteNoParam;
var
  LastAnalysis: PAnalysis;
begin
  inherited;

  InternalEngine.Output.StartWrite;
  try
    InternalEngine.Output.Write('Hint: ', ccKeyword);
    LastAnalysis := InternalEngine.AnalysisInfo.ActualAnalysis;
    if (LastAnalysis <> nil) and (Length(LastAnalysis.Moves) >= 2) then
    begin
      InternalEngine.Output.WriteLine(LastAnalysis.Moves[2], ccValue);
    end
    else
      InternalEngine.Output.WriteLine(InternalEngine.Output.NullMoveStr, ccValue);
  finally
    InternalEngine.Output.StopWrite;
  end;
end;

end.

