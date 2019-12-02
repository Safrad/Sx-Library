// Ancestor for Commands

unit uSimpleEngineCommand;

interface

uses
  uInternalEngine,
  uSimpleCommand;

type
  TSimpleEngineCommand = class(TSimpleCommand)
  private
    FInternalEngine: TInternalEngine;
    procedure SetInternalEngine(const Value: TInternalEngine);
  public
    property InternalEngine: TInternalEngine read FInternalEngine write SetInternalEngine;
  end;

implementation

{ TSimpleEngineCommand }

procedure TSimpleEngineCommand.SetInternalEngine(const Value: TInternalEngine);
begin
  FInternalEngine := Value;
end;

end.
