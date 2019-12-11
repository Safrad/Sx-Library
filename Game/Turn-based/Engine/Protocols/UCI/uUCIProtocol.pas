// http://wbec-ridderkerk.nl/html/UCIProtocol.html
unit uUCIProtocol;

interface

uses
  uTypes,
  uCustomArgument,
  uComboArgument,

  uCommonProtocol,
  uNewGameCommand;

type
  TUCIProtocol = class(TCommonProtocol)
  private
    procedure CreateCommands;
    procedure WriteOptions;
    procedure WriteOption(const AArgument: TCustomArgument);
    procedure ShowCurrentMoveArgumentChanged(Sender: TObject);
    procedure ShowSelectiveDepthArgumentChanged(Sender: TObject);
  protected
    FShowCurrentMoveArgument: TComboArgument;
    FShowSelectiveDepthArgument: TComboArgument;
    InitializationReply: string;
    NewGameCommand: TNewGameCommand;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize; override;
  end;

implementation

uses
  SysUtils,

  uTextType,
  uStrings,
  uProjectInfo,

  uSwitchArgument,
  uStringArgument,
  uNumericalIntervalArgument,
  uButtonArgument,

  uUCIEngineOutput,

  uIsReadyCommand,
  uSetOptionCommand,
  uRegisterCommand,
  uPositionCommand,
  uGoCommand,
  uStopCommand,
  uPonderHitCommand;

{ TUCIProtocol }

function GetOptionType(const AArgument: TCustomArgument): string;
begin
  if AArgument is TSwitchArgument then
  begin
    Result := 'check';
  end
  else if AArgument is TNumericalIntervalArgument then
  begin
    Result := 'spin';
  end
  else if AArgument is TStringArgument then
  begin
    Result := 'string';
  end
  else if AArgument is TComboArgument then
  begin
    Result := 'combo';
  end
  else if AArgument is TCustomArgument then
  begin
    Result := 'button';
  end
  else
    raise ENotImplemented.Create('Argument.');
end;

procedure TUCIProtocol.WriteOption(const AArgument: TCustomArgument);
var
  i: SG;
  TextType: TTextType;
begin
	InternalEngine.Output.Write('option name ', ccKeyword);
	InternalEngine.Output.Write(AArgument.Shortcut, ccValue);
	InternalEngine.Output.Write(' type ', ccKeyword);
	InternalEngine.Output.Write(GetOptionType(AArgument), ccValue);
  if not (AArgument is TButtonArgument) then
  	InternalEngine.Output.Write(' default ', ccKeyword);

  if AArgument is TSwitchArgument then
  begin
    if TSwitchArgument(AArgument).DefaultValue then
      TextType := ccTrueValue
    else
      TextType := ccFalseValue;
    InternalEngine.Output.WriteLine(
      FalseTrue[SG(TSwitchArgument(AArgument).DefaultValue)], TextType);
  end
  else if AArgument is TNumericalIntervalArgument then
  begin
    InternalEngine.Output.Write(
      IntToStr(TNumericalIntervalArgument(AArgument).DefaultValue), ccValue);

  	InternalEngine.Output.Write(' min ', ccKeyword);
    InternalEngine.Output.Write(
      IntToStr(TNumericalIntervalArgument(AArgument).NumericalInterval.MinimalValue), ccValue);

	  InternalEngine.Output.Write(' max ', ccKeyword);
    InternalEngine.Output.WriteLine(
      IntToStr(TNumericalIntervalArgument(AArgument).NumericalInterval.MaximalValue), ccValue);
  end
  else if AArgument is TStringArgument then
  begin
    InternalEngine.Output.WriteLine(
      TStringArgument(AArgument).DefaultValue, ccValue);
  end
  else if AArgument is TComboArgument then
  begin
    InternalEngine.Output.Write(
      TComboArgument(AArgument).Captions[TComboArgument(AArgument).DefaultValue], ccValue);
    for i := 0 to Length(TComboArgument(AArgument).Captions) - 1 do
    begin
  	  InternalEngine.Output.Write(' var ', ccKeyword);
      InternalEngine.Output.Write(
        TComboArgument(AArgument).Captions[i], ccValue);
    end;
    InternalEngine.Output.WriteLine('', ccValue);
  end
  else if AArgument is TButtonArgument then
  begin
    InternalEngine.Output.WriteLine('', ccValue);
  end
  else
    raise ENotImplemented.Create('Argument.');
end;

procedure TUCIProtocol.WriteOptions;
var
  i: SG;
begin
  InternalEngine.Output.StartWrite;
  try
    InternalEngine.Output.Write('id name ', ccKeyword);
    InternalEngine.Output.WriteLine(GetProjectInfo(piProductName) + CharSpace + GetProjectInfo(piProductVersion), ccValue);
    InternalEngine.Output.Write('id author ', ccKeyword);
    InternalEngine.Output.WriteLine(GetProjectInfo(piAuthor), ccValue);

    for i := 0 to InternalEngine.Options.DefinedCount - 1 do
    begin
      WriteOption(InternalEngine.Options[i]);
    end;
    InternalEngine.Output.WriteLine(InitializationReply, ccKeyword);
  finally
    InternalEngine.Output.StopWrite;
  end;
end;

procedure TUCIProtocol.Initialize;
begin
  inherited;

  InternalEngine.Options.Add(FShowCurrentMoveArgument);
  TUCIEngineOutput(InternalEngine.Output).ShowCurrentMoveEverySecond := True;

  InternalEngine.Options.Add(FShowSelectiveDepthArgument);
  TUCIEngineOutput(InternalEngine.Output).ShowSelectiveDepthEverySecond := True;

  CreateCommands;

  WriteOptions;
end;

procedure TUCIProtocol.ShowCurrentMoveArgumentChanged(Sender: TObject);
begin
  TUCIEngineOutput(InternalEngine.Output).ShowCurrentMoveEverySecond := FShowCurrentMoveArgument.Value = 0;
  TUCIEngineOutput(InternalEngine.Output).ShowCurrentMoveOnChange := FShowCurrentMoveArgument.Value = 1;
end;

procedure TUCIProtocol.ShowSelectiveDepthArgumentChanged(Sender: TObject);
begin
  TUCIEngineOutput(InternalEngine.Output).ShowSelectiveDepthEverySecond := FShowSelectiveDepthArgument.Value = 0;
  TUCIEngineOutput(InternalEngine.Output).ShowSelectiveDepthOnChange := FShowSelectiveDepthArgument.Value = 1;
end;

constructor TUCIProtocol.Create;
begin
  inherited;

  InitializationReply := 'uciok';

  FShowCurrentMoveArgument := TComboArgument.Create;
  FShowCurrentMoveArgument.Shortcut := 'Show Current Move';
  FShowCurrentMoveArgument.AddCaption('Every second');
  FShowCurrentMoveArgument.AddCaption('On change');
  FShowCurrentMoveArgument.AddCaption('Disabled');
  FShowCurrentMoveArgument.DefaultValue := 0;
  FShowCurrentMoveArgument.Value := FShowCurrentMoveArgument.DefaultValue;
  FShowCurrentMoveArgument.OnChange := ShowCurrentMoveArgumentChanged;

  FShowSelectiveDepthArgument := TComboArgument.Create;
  FShowSelectiveDepthArgument.Shortcut := 'Show Selective Depth';
  FShowSelectiveDepthArgument.AddCaption('Every second');
  FShowSelectiveDepthArgument.AddCaption('On change');
  FShowSelectiveDepthArgument.AddCaption('Disabled');
  FShowSelectiveDepthArgument.DefaultValue := 0;
  FShowSelectiveDepthArgument.Value := FShowSelectiveDepthArgument.DefaultValue;
  FShowSelectiveDepthArgument.OnChange := ShowSelectiveDepthArgumentChanged;
end;

procedure TUCIProtocol.CreateCommands;
begin
  AddCommand(TIsReadyCommand.Create);
  AddCommand(TSetOptionCommand.Create);
  AddCommand(TRegisterCommand.Create);

  NewGameCommand := TNewGameCommand.Create;
  NewGameCommand.Shortcut := 'UciNewGame';
  AddCommand(NewGameCommand);

  AddCommand(TPositionCommand.Create);
  AddCommand(TGoCommand.Create);
  AddCommand(TStopCommand.Create);
  AddCommand(TPonderHitCommand.Create);
end;

destructor TUCIProtocol.Destroy;
begin
  try
    FShowCurrentMoveArgument.Free;
    FShowSelectiveDepthArgument.Free;
  finally
    inherited;
  end;
end;

end.
