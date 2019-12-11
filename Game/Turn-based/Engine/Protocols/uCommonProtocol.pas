// Ancestor for TXBoardProtocol in uXBoardProtocol and TUCIProtocol in uUCIProtocol

unit uCommonProtocol;

interface

uses
  uTypes,
  uCustomCommand,
  uCommands,
  uInternalEngine;

type
  TCommonProtocol = class
  private
    FCommands: TCommandsList;
    FInternalEngine: TInternalEngine;
    FAnalyzeMode: BG;
    procedure SetInternalEngine(const Value: TInternalEngine);
    procedure CreateCommands;
  protected
    procedure SetAnalyzeMode(const Value: BG); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Initialize; virtual;
    procedure AddCommand(const ACommand: TCustomCommand);

    property Commands: TCommandsList read FCommands;
    property InternalEngine: TInternalEngine read FInternalEngine write SetInternalEngine;
    property AnalyzeMode: BG read FAnalyzeMode write SetAnalyzeMode;

    // Returns
    //   true: command is successfully handled
    //   false: unknown command error is displayed
    function UnknownCommand(const AText: string): BG; virtual;
  end;

implementation

uses
  Classes,

  uEngineCommand,
  uSimpleEngineCommand;

{ TCommonProtocol }

procedure TCommonProtocol.AddCommand(const ACommand: TCustomCommand);
begin
  Assert(ACommand is TCustomCommand);
  if ACommand is TSimpleEngineCommand then
  begin
    Assert(FInternalEngine <> nil);
    TSimpleEngineCommand(ACommand).InternalEngine := FInternalEngine;
  end
  else if ACommand is TEngineCommand then
  begin
    Assert(FInternalEngine <> nil);
    TEngineCommand(ACommand).InternalEngine := FInternalEngine;
  end;
  FCommands.Add(ACommand);
end;

constructor TCommonProtocol.Create;
begin
  inherited;

end;

procedure TCommonProtocol.CreateCommands;
begin
  FCommands := TCommandsList.Create;
  FCommands.OwnsObjects := False;
end;

destructor TCommonProtocol.Destroy;
begin
  try
    FCommands.Free;
  finally
    inherited;
  end;
end;

procedure TCommonProtocol.Initialize;
begin
  CreateCommands;
end;

procedure TCommonProtocol.SetAnalyzeMode(const Value: BG);
begin
  FAnalyzeMode := Value;
end;

procedure TCommonProtocol.SetInternalEngine(const Value: TInternalEngine);
begin
  FInternalEngine := Value;
end;

function TCommonProtocol.UnknownCommand(const AText: string): BG;
begin
  Result := False;
end;

end.
