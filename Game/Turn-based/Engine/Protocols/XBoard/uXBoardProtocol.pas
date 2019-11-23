// http://hgm.nubati.net/CECP.html
// http://home.hccnet.nl/h.g.muller/engine-intf.html
unit uXBoardProtocol;

interface

uses
  uTypes,
  uCommonProtocol,
  uCustomCommand;

type
  TXBoardProtocol = class(TCommonProtocol)
  private
    FProtocolVersion: SG;
    FAutoPlay: BG;
    FProtoVerCommand: TCustomCommand;
    FAnalyzeCommand: TCustomCommand;
    FGoCommand: TCustomCommand;
    FUserMoveCommand: TCustomCommand;
    FSetBoardCommand: TCustomCommand;
    FPingCommand: TCustomCommand;
    FMemoryCommand: TCustomCommand;
    FCoresCommand: TCustomCommand;
    FDrawCommand: TCustomCommand;
    FPauseCommand: TCustomCommand;
    FResumeCommand: TCustomCommand;
    FNpsCommand: TCustomCommand;
    FExitCommand: TCustomCommand;
    FSetScoreCommand: TCustomCommand;
    FPlayOtherCommand: TCustomCommand;
    FICSCommand: TCustomCommand;
    FNameCommand: TCustomCommand;
    FTimeCommand: TCustomCommand;
    FOTimCommand: TCustomCommand;
    FExcludeAllCommand: TCustomCommand;
    FExcludeCommand: TCustomCommand;
    FIncludeAllCommand: TCustomCommand;
    FIncludeCommand: TCustomCommand;

    procedure CreateCommands;
    procedure AddXBoardCommand(ACommand: TCustomCommand);
    procedure SetProtocolVersion(const Value: SG);
    procedure DoUserMoves(const AUserMoves: string);
    procedure SetAutoPlay(const Value: BG);
  protected
    procedure SetAnalyzeMode(const Value: BG); override;
  public
    constructor Create;

    procedure Initialize; override;

    property ProtocolVersion: SG read FProtocolVersion write SetProtocolVersion;
    property AutoPlay: BG read FAutoPlay write SetAutoPlay;
    function UnknownCommand(const AText: string): BG; override;

    property ProtoVerCommand: TCustomCommand read FProtoVerCommand;
    property AnalyzeCommand: TCustomCommand read FAnalyzeCommand;
    property XBoardGoCommand: TCustomCommand read FProtoVerCommand;
    property XBoardExitCommand: TCustomCommand read FProtoVerCommand;
    property UserMoveCommand: TCustomCommand read FProtoVerCommand;
    property SetBoardCommand: TCustomCommand read FSetBoardCommand;
    property PingCommand: TCustomCommand read FPingCommand;
    property MemoryCommand: TCustomCommand read FMemoryCommand;
    property CoresCommand: TCustomCommand read FCoresCommand;
    property DrawCommand: TCustomCommand read FDrawCommand;
    property PauseCommand: TCustomCommand read FPauseCommand;
    property ResumeCommand: TCustomCommand read FResumeCommand;
    property NpsCommand: TCustomCommand read FNpsCommand;
    property ExitCommand: TCustomCommand read FExitCommand;
    property SetScoreCommand: TCustomCommand read FSetScoreCommand;
    property PlayOtherCommand: TCustomCommand read FPlayOtherCommand;
    property ICSCommand: TCustomCommand read FICSCommand;
    property NameCommand: TCustomCommand read FNameCommand;
    property TimeCommand: TCustomCommand read FTimeCommand;
    property OTimCommand: TCustomCommand read FOTimCommand;
    property ExcludeAllCommand: TCustomCommand read FExcludeAllCommand;
    property ExcludeCommand: TCustomCommand read FExcludeCommand;
    property IncludeAllCommand: TCustomCommand read FIncludeAllCommand;
    property IncludeCommand: TCustomCommand read FIncludeCommand;
  end;

implementation

uses
  SysUtils,
  Classes,
  Contnrs,

  uStrings,
  uOptions,
  uInputFormat,

  uXBoardProtocolCommand,

  uProtoVerCommand,
  uAnalyzeCommand,
  uXBoardGoCommand,
  uXBoardExitCommand,

  uEngineCommand,

  uNewCommand,
  uAcceptedCommand,
  uRejectedCommand,
  uVariantCommand,
  uRandomCommand,
  uForceCommand,
  uPlayOtherCommand,
  uLevelCommand,
  uStCommand,
  uSdCommand,
  uNpsCommand,
  uTimeCommand,
  uOTimCommand,
  uUserMoveCommand,
  uExitCommand,
  uQuestionMarkCommand,
  uPingCommand,
  uDrawCommand,
  uResultCommand,
  uSetBoardCommand,
  uEditCommand,
  uHintCommand,
  uBkCommand,
  uUndoCommand,
  uRemoveCommand,
  uHardCommand,
  uEasyCommand,
  uPostCommand,
  uNoPostCommand,
  uNameCommand,
  uRatingCommand,
  uICSCommand,
  uComputerCommand,
  uPauseCommand,
  uResumeCommand,
  uMemoryCommand,
  uCoresCommand,
  uEGTPathCommand,
  uOptionCommand,
  uExcludeCommand,
  uIncludeCommand,
  uExcludeAllCommand,
  uIncludeAllCommand,
  uSetScoreCommand,
  uDotCommand;

{ TXBoardProtocol }

procedure TXBoardProtocol.AddXBoardCommand(ACommand: TCustomCommand);
begin
  Assert(ACommand is TXBoardProtocolCommand);
  TXBoardProtocolCommand(ACommand).XBoardProtocol := Self;
  AddCommand(TEngineCommand(ACommand));
end;

constructor TXBoardProtocol.Create;
begin
  inherited;

  FProtocolVersion := 1;
end;

procedure TXBoardProtocol.CreateCommands;
var
  NewCommand: TNewCommand;
  ForceCommand: TForceCommand;
  QuestionMarkCommand: TQuestionMarkCommand;
  DotCommand: TDotCommand;
begin
  FProtoVerCommand := TProtoVerCommand.Create;
  AddXBoardCommand(TProtoVerCommand(FProtoVerCommand));

  NewCommand := TNewCommand.Create;
  AddXBoardCommand(NewCommand);

  AddCommand(TAcceptedCommand.Create);
  AddCommand(TRejectedCommand.Create);
  AddCommand(TVariantCommand.Create);
  AddCommand(TRandomCommand.Create);

  ForceCommand := TForceCommand.Create;
  AddXBoardCommand(ForceCommand);

  FAnalyzeCommand := TAnalyzeCommand.Create;
  AddXBoardCommand(TAnalyzeCommand(FAnalyzeCommand));

  FGoCommand := TXBoardGoCommand.Create;
  FGoCommand.Shortcut := 'Go';
  AddXBoardCommand(TXBoardGoCommand(FGoCommand));

  FExitCommand := TXBoardExitCommand.Create;
  FExitCommand.Shortcut := 'Exit';
  FExitCommand.Enabled := False;
  AddXBoardCommand(TXBoardExitCommand(FExitCommand));

  FPlayOtherCommand := TPlayOtherCommand.Create;
  AddXBoardCommand(FPlayOtherCommand);
  AddCommand(TLevelCommand.Create);
  AddCommand(TStCommand.Create);
  AddCommand(TSdCommand.Create);
  FNpsCommand := TNpsCommand.Create;
  AddCommand(FNpsCommand);
  FTimeCommand := TTimeCommand.Create;
  AddCommand(FTimeCommand);
  FOTimCommand := TOTimCommand.Create;
  AddCommand(FOTimCommand);
  FUserMoveCommand := TUserMoveCommand.Create;
  AddXBoardCommand(FUserMoveCommand);

  QuestionMarkCommand := TQuestionMarkCommand.Create;
  QuestionMarkCommand.Shortcut := '?';
  AddCommand(QuestionMarkCommand);

  FPingCommand := TPingCommand.Create;
  AddCommand(FPingCommand);
  FDrawCommand := TDrawCommand.Create;
  AddCommand(FDrawCommand);
  AddCommand(TResultCommand.Create);
  FSetBoardCommand := TSetBoardCommand.Create;
  AddCommand(FSetBoardCommand);
  AddCommand(THintCommand.Create);
  AddCommand(TBkCommand.Create);
  AddCommand(TUndoCommand.Create);
  AddCommand(TRemoveCommand.Create);
  AddCommand(THardCommand.Create);
  AddCommand(TEasyCommand.Create);
  AddCommand(TPostCommand.Create);
  AddCommand(TNoPostCommand.Create);
  FNameCommand := TNameCommand.Create;
  AddCommand(FNameCommand);
  AddCommand(TRatingCommand.Create);
  FICSCommand := TICSCommand.Create;
  AddCommand(FIcsCommand);
  AddCommand(TComputerCommand.Create);
  FPauseCommand := TPauseCommand.Create;
  AddCommand(FPauseCommand);
  FResumeCommand := TResumeCommand.Create;
  AddCommand(FResumeCommand);
  AddCommand(TOptionCommand.Create);

  FExcludeCommand := TExcludeCommand.Create;
  AddCommand(FExcludeCommand);
  FIncludeCommand := TIncludeCommand.Create;
  AddCommand(FIncludeCommand);
  FExcludeAllCommand := TExcludeAllCommand.Create;
  AddCommand(FExcludeAllCommand);
  FIncludeAllCommand := TIncludeAllCommand.Create;
  AddCommand(FIncludeAllCommand);

  DotCommand := TDotCommand.Create;
  DotCommand.Shortcut := '.';
  AddCommand(DotCommand);

  // Unsupported
  FMemoryCommand := TMemoryCommand.Create;
  AddCommand(FMemoryCommand);
  FCoresCommand := TCoresCommand.Create;
  AddCommand(FCoresCommand);
  AddCommand(TEditCommand.Create);
  FSetScoreCommand := TSetScoreCommand.Create;
  AddCommand(FSetScoreCommand);
  AddCommand(TEGTPathCommand.Create);
// Unsupported: Bughouse commands, White, Black
end;

procedure TXBoardProtocol.DoUserMoves(const AUserMoves: string);
begin
  InternalEngine.DoMoves(AUserMoves);
end;

procedure TXBoardProtocol.Initialize;
begin
  inherited;

  VisualStyleStr[vsFilename] := 'File'; // replaces Filename
  CreateCommands;
end;

procedure TXBoardProtocol.SetAnalyzeMode(const Value: BG);
begin
  inherited;

  FAnalyzeCommand.Enabled := not AnalyzeMode;
  FGoCommand.Enabled := not AnalyzeMode;
  FExitCommand.Enabled := AnalyzeMode;
end;

procedure TXBoardProtocol.SetAutoPlay(const Value: BG);
begin
  FAutoPlay := Value;
end;

procedure TXBoardProtocol.SetProtocolVersion(const Value: SG);
begin
  FProtocolVersion := Value;
end;

function TXBoardProtocol.UnknownCommand(const AText: string): BG;
begin
  inherited;

  Result := True;
  try
    DoUserMoves(AText);
  except
    Result := False;
  end;
end;

end.
