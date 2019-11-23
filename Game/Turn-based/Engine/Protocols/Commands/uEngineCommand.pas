// Ancestor for Commands

unit uEngineCommand;

interface

uses
  uInternalEngine,
  uCustomCommand;

type
  TEngineCommand = class(TCustomCommand)
  private
    FInternalEngine: TInternalEngine;
    procedure SetInternalEngine(const Value: TInternalEngine);
  public
    property InternalEngine: TInternalEngine read FInternalEngine write SetInternalEngine;
  end;

implementation

{ TEngineCommand }

procedure TEngineCommand.SetInternalEngine(const Value: TInternalEngine);
begin
  FInternalEngine := Value;
end;

end.
