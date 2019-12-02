unit uDotCommand;

interface

uses
  uSimpleEngineCommand;

type
  TDotCommand = class(TSimpleEngineCommand)
  public
    constructor Create;

    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uTypes;

{ TDotCommand }

constructor TDotCommand.Create;
begin
  inherited;

  Description := 'Get a search status update.';
end;

procedure TDotCommand.ExecuteNoParam;
begin
  inherited;

  InternalEngine.Output.DrawNodes;
  if InternalEngine.CommonOptions.ShowCurrLine.Value then
    InternalEngine.Output.DrawCurrLine(InternalEngine.GetCurrentLine);
end;

end.
