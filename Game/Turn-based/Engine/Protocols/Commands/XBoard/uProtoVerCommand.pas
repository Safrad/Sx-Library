unit uProtoVerCommand;

interface

uses
  uEngineCommand,
  uXBoardProtocolCommand;

type
  TProtoVerCommand = class(TXBoardProtocolCommand)
  public
    function GetSyntax: string; override;
  public
    constructor Create;

    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,
  uTypes,
  uStrings,
  uProjectInfo,

  uCustomCommand,
  uUnsupportedCommand,
  uCustomArgument,
  uSwitchArgument,
  uComboArgument,
  uNumericalIntervalArgument,
  uStringArgument,
  uFileNameArgument,
  uDirectoryArgument,
  uButtonArgument,

  uGameVariants,
  uXBoardFeatures;

{ TProtoVerCommand }

constructor TProtoVerCommand.Create;
begin
  inherited;

  Description := 'Set protocol version and get engine features and options.';
end;

function OptionToStr(const Argument: TCustomArgument): string;
var
  i: SG;
begin
	Result :=
    Argument.Shortcut +
    ' -';
	if Argument is TSwitchArgument then
  begin
    Result := Result + 'check ' + IntToStr(SG(TSwitchArgument(Argument).DefaultValue));
  end
  else if Argument is TNumericalIntervalArgument then
	begin
		Result := Result + 'spin ' + // or slider
      IntToStr(TNumericalIntervalArgument(Argument).DefaultValue) + ' ' +
      IntToStr(TNumericalIntervalArgument(Argument).NumericalInterval.MinimalValue) + ' ' +
      IntToStr(TNumericalIntervalArgument(Argument).NumericalInterval.MaximalValue);
	end
  else if Argument is TDirectoryArgument then
    Result := Result + 'path ' + TDirectoryArgument(Argument).DefaultValue
  else if Argument is TFileNameArgument then
    Result := Result + 'file ' + TFileNameArgument(Argument).DefaultValue
  else if Argument is TStringArgument then
    Result := Result + 'string ' + TStringArgument(Argument).DefaultValue
  else if Argument is TButtonArgument then
    Result := Result + 'button'
  else if Argument is TComboArgument then
  begin
    Result := Result + 'combo ';
    for i := 0 to Length(TComboArgument(Argument).Captions) - 1 do
    begin
      Result := Result + TComboArgument(Argument).Captions[i] + ' ';
      if i = TComboArgument(Argument).DefaultValue then
        Result := Result + '///';
    end;
  end
  else
    raise ENotImplemented.Create('Argument.');
end;

function IsSupported(const ACommand: TCustomCommand): BG;
begin
  Assert(ACommand <> nil);
  Result := (ACommand <> nil) and (not (ACommand is TUnsupportedCommand));
end;

procedure TProtoVerCommand.Execute(const AParameters: string);
var
  InLineIndex: SG;
  i: SG;
  Features: TXBoardFeatures;
begin
  InLineIndex := 1;
  XBoardProtocol.ProtocolVersion := ReadSGFast(AParameters, InLineIndex);

  Features := TXBoardFeatures.Create;
  try
    // Prefix
    Features.Add('done', False);

    // Info
    Features.Add('myname', GetProjectInfo(piProductName) + CharSpace + GetProjectInfo(piProductVersion));

    // Compatibility
    Features.Add('debug', True); // # string
    Features.Add('sigint', False); // the interrupt signal, not supported in Windows
    Features.Add('sigterm', False); // the termination signal, True in engine hangs
    Features.Add('san', False); // do not use SAN notation, use coordinate notation for move
    Features.Add('reuse', True); // do not kill process before new game
    Features.Add('highlight', False); // GUI rules, Future implementation

    // Commands compatibility
    Features.Add('setboard', IsSupported(XBoardProtocol.SetBoardCommand));
    Features.Add('ping', IsSupported(XBoardProtocol.PingCommand));
    Features.Add('memory', IsSupported(XBoardProtocol.MemoryCommand));
    Features.Add('smp', IsSupported(XBoardProtocol.CoresCommand));
    Features.Add('usermove', IsSupported(XBoardProtocol.UserMoveCommand));
    Features.Add('draw', IsSupported(XBoardProtocol.DrawCommand));
    Features.Add('pause', IsSupported(XBoardProtocol.PauseCommand) and IsSupported(XBoardProtocol.ResumeCommand));
    Features.Add('nps', IsSupported(XBoardProtocol.NpsCommand));
    Features.Add('analyze', IsSupported(XBoardProtocol.AnalyzeCommand) and IsSupported(XBoardProtocol.ExitCommand));
    Features.Add('exclude',
      IsSupported(XBoardProtocol.ExcludeAllCommand) and
      IsSupported(XBoardProtocol.ExcludeCommand) and
      IsSupported(XBoardProtocol.IncludeAllCommand) and
      IsSupported(XBoardProtocol.IncludeCommand));
    Features.Add('setscore', IsSupported(XBoardProtocol.SetScoreCommand));
    Features.Add('playother', IsSupported(XBoardProtocol.PlayOtherCommand));
    Features.Add('ics', IsSupported(XBoardProtocol.IcsCommand));
    Features.Add('name', IsSupported(XBoardProtocol.NameCommand));
    Features.Add('colors', False); // Obsolete White / Black commands
    Features.Add('time', IsSupported(XBoardProtocol.TimeCommand) and IsSupported(XBoardProtocol.OTimCommand));

    Features.Add('variants', GameVariants.ToXBoardString);
    for i := 0 to InternalEngine.Options.DefinedCount - 1 do
    begin
      Features.Add('option', OptionToStr(InternalEngine.Options[i]));
    end;

    // Suffix
    Features.Add('done', True);

    Features.ExportToEngineOutput(InternalEngine.Output);
  finally
    Features.Free;
  end;
end;

function TProtoVerCommand.GetSyntax: string;
begin
  Result := '[Protocol Version]';
end;

end.
