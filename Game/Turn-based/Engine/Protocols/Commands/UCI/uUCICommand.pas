unit uUCICommand;

interface

uses
  uTypes,
  uComboArgument,
  uSwitchArgument,
  uStringArgument,

  uProtocolCommand;

type
  TUCICommand = class(TProtocolCommand)
  private
    FindChess960: SG;
    FUCI_Variant: TComboArgument;
    FUCI_Chess960: TSwitchArgument;
    FUCI_AnalyseMode: TSwitchArgument;
    FUCI_Opponent: TStringArgument;
    procedure AddOptions;
    procedure OnChess960Change(Sender: TObject);
    procedure OnVariantChange(Sender: TObject);
    procedure OnAnalyseModeChange(Sender: TObject);
    procedure OnOpponentChange(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ExecuteNoParam; override;
  end;

implementation

uses
  uUCIProtocol,
  uUCIEngineOutput,
  uGameVariants;

{ TUCICommand }

procedure TUCICommand.AddOptions;
var
  i: SG;
begin
  FindChess960 := -1;

  if GameVariants.Count = 0 then
    Exit;

  FUCI_Variant := TComboArgument.Create;
  FUCI_Variant.Shortcut := 'UCI_Variant';
  FUCI_Variant.Description := '';
  for i := 0 to GameVariants.Count - 1 do
  begin
    FUCI_Variant.AddCaption(GameVariants[i].Name);
    if GameVariants[i].Name = 'chess' then
      FUCI_Variant.DefaultValue := i
    else if GameVariants[i].Name = 'chess960' then
      FindChess960 := i;
  end;
  FUCI_Variant.Value := FUCI_Variant.DefaultValue;
  FUCI_Variant.OnChange := OnVariantChange;
  ConsoleEngine.InternalEngine.Options.Add(FUCI_Variant);

  if FindChess960 >= 0 then
  begin
    FUCI_Chess960 := TSwitchArgument.Create;
    FUCI_Chess960.Shortcut := 'UCI_Chess960';
    FUCI_Chess960.Description := '';
    FUCI_Chess960.DefaultValue := False;
    FUCI_Chess960.Value := FUCI_Chess960.DefaultValue;
    FUCI_Chess960.OnChange := OnChess960Change;
    ConsoleEngine.InternalEngine.Options.Add(FUCI_Chess960);
  end;

  FUCI_AnalyseMode := TSwitchArgument.Create;
  FUCI_AnalyseMode.Shortcut := 'UCI_AnalyseMode';
  FUCI_AnalyseMode.Description := '';
  FUCI_AnalyseMode.DefaultValue := False;
  FUCI_AnalyseMode.Value := FUCI_AnalyseMode.DefaultValue;
  FUCI_AnalyseMode.OnChange := OnAnalyseModeChange;
  ConsoleEngine.InternalEngine.Options.Add(FUCI_AnalyseMode);

  FUCI_Opponent := TStringArgument.Create;
  FUCI_Opponent.Shortcut := 'UCI_Opponent';
  FUCI_Opponent.Description := '';
  FUCI_Opponent.DefaultValue := '';
  FUCI_Opponent.Value := FUCI_Opponent.DefaultValue;
  FUCI_Opponent.OnChange := OnOpponentChange;
  ConsoleEngine.InternalEngine.Options.Add(FUCI_Opponent);
end;

procedure TUCICommand.OnAnalyseModeChange(Sender: TObject);
begin
  ConsoleEngine.Protocol.AnalyzeMode := FUCI_AnalyseMode.Value;
end;

procedure TUCICommand.OnChess960Change(Sender: TObject);
begin
  if FUCI_Chess960.Value then
  begin
    FUCI_Variant.Value := FindChess960; // Call OnVariantChange
  end
  else
    FUCI_Variant.Value := 0; // Call OnVariantChange
end;

procedure TUCICommand.OnOpponentChange(Sender: TObject);
begin
  ConsoleEngine.InternalEngine.Opponent.ReadFromString(FUCI_Opponent.Value);
end;

procedure TUCICommand.OnVariantChange(Sender: TObject);
begin
  FUCI_Chess960.OnChange := nil;
  try
    FUCI_Chess960.Value := GameVariants[FUCI_Variant.Value].Name = 'chess960';
  finally
    FUCI_Chess960.OnChange := OnChess960Change;
  end;
  ConsoleEngine.InternalEngine.GameVariant := GameVariants[FUCI_Variant.Value];
end;

constructor TUCICommand.Create;
begin
  inherited;

  Description := 'Use the UCI (universal chess interface).';
end;

destructor TUCICommand.Destroy;
begin
  try
    FUCI_Variant.Free;
    FUCI_Chess960.Free;
    FUCI_AnalyseMode.Free;
    FUCI_Opponent.Free;
  finally
    inherited;
  end;
end;

procedure TUCICommand.ExecuteNoParam;
begin
  inherited;

  AddOptions;
  ConsoleEngine.InternalEngine.Output.Free; // TNoProtocolOutput.Free
  ConsoleEngine.InternalEngine.Output := TUCIEngineOutput.Create;

  TUCIEngineOutput(ConsoleEngine.InternalEngine.Output).AnalysisInfo := ConsoleEngine.InternalEngine.AnalysisInfo;
  ConsoleEngine.Protocol := TUCIProtocol.Create;
end;

end.
