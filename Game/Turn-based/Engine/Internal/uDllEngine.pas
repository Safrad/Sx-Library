unit uDllEngine;

interface

uses
  SysUtils,
  Classes,

  uTypes,
  uPlugin,
  uNumericalIntervalArgument,

  uInternalEngine,
  uEngineOutput,
  uDllInterface;

type
  TDllEngine = class(TInternalEngine)
  private
    FPlugin: TPlugin;

    // Dll methods
    FSetStartPos: TProcedureSetStartPos;
    FSetPositionFromString: TProcedureSetPositionFromString;
    FDoMove: TProcedureDoMove;
    FUndo: TProcedure;
    FAnalyze: TProcedureAnalyze;
    FBookMoves: TProcedure;
    FPerft: TPerftFunction;
    FWriteBoardToConsole: TProcedure;
    FEvalToConsole: TProcedure;
    FGetCurrLine: TGetCurrLine;
    FStop: TProcedureStop;
    FSetOptionOwnBook: TProcedureB1;
    FSetOptionHash: TProcedureSG;
    FSetOptionClearHash: TProcedure;

    procedure CreateOptions;

    // OnSetOption...
    procedure OnOptionOwnBookChange(Sender: TObject);
    procedure OnSetOptionAutomaticHashSizeChange(Sender: TObject);
    procedure OnSetOptionHashChange(Sender: TObject);
    procedure OnSetOptionClearHashChange(Sender: TObject);

    function GetDllName: TFileName;
    procedure SetDllName(const Value: TFileName);
    procedure CreateCallbacks;
    procedure GetDllMethods;
  protected
    procedure DoMove(const AMove: string); override;
    procedure SetPositionFromStringSpecific(const AFEN: string; out ASideToMove: SG); override;
    procedure SetOutput(const Value: TEngineOutput); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure SetStartPos; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Undo; override;
    procedure SetNumberOfHashEntries(const AHashEntries: U8); override;
    procedure BookMoves; override;
    procedure GetPerft(const ADepth: SG); override;
    procedure WriteBoardToConsole; override;
    procedure EvalToConsole; override;
    function GetCurrentLine: string; override;

    property DllName: TFileName read GetDllName write SetDllName;
  end;

implementation

uses
  Math,

  uFiles,
  uNewThread,
  uSystemMemory,
  uStrings,

  uProtocolEngineOutput,
  uSubtreeStatus,
  uScore,
  uStopCause;

var
  GOutput: TEngineOutput;
  GSelf: TDllEngine;

procedure CallbackNextDepth; cdecl;
begin
  GSelf.StopManager.UpdateStopDepth;
  if GSelf.StopManager.Cause <> scNone then
    GSelf.FStop
  else
  begin
    TProtocolEngineOutput(GOutput).AnalysisInfo.Depth := TProtocolEngineOutput(GOutput).AnalysisInfo.Depth + 1;
    GOutput.DrawDepth;
  end;
end;

procedure CallbackSetSelDepth(const ASelDepth: U4); cdecl;
begin
  TProtocolEngineOutput(GOutput).AnalysisInfo.SelDepth := ASelDepth;
  TProtocolEngineOutput(GOutput).DrawSelDepth;
end;

procedure CallbackNextMove(const AMoveIndex: U4; const AMove: PAnsiChar); cdecl;
begin
  TProtocolEngineOutput(GOutput).AnalysisInfo.SetCurrentMove(AMoveIndex, string(AMove));
  GOutput.DrawMove1;
  GSelf.StopManager.UpdateStopMove;
  if GSelf.StopManager.Cause <> scNone then
    GSelf.FStop;
end;

procedure CallbackNextNode; cdecl;
begin
  TProtocolEngineOutput(GOutput).AnalysisInfo.NextNode;
  GSelf.StopManager.UpdateStopNode;
  if GSelf.StopManager.Cause <> scNone then
    GSelf.FStop;
end;

procedure CallbackEndgameTableBasesHit; cdecl;
begin
  TProtocolEngineOutput(GOutput).AnalysisInfo.EndgameTableBasesHits :=
    TProtocolEngineOutput(GOutput).AnalysisInfo.EndgameTableBasesHits + 1;
end;

procedure CallbackNextBestMove(const AMoves: PAnsiChar; const AScore: S2; const AScoreBound: U1); cdecl;
var
  SubtreeStatus: TSubtreeStatus;
  Moves: array of string;
  InLineIndex: SG;
begin
  SubtreeStatus := Default(TSubtreeStatus);
  SubtreeStatus.Score := AScore;
  SubtreeStatus.ScoreBound := TScoreBound(AScoreBound);
  SubtreeStatus.MoveCount := 0;
  InLineIndex := 1;
  while InLineIndex < Length(AMoves) do
  begin
    SetLength(Moves, SubtreeStatus.MoveCount + 1);
    Moves[SubtreeStatus.MoveCount] := ReadToChar(string(AnsiString(AMoves)), InLineIndex, CharSpace);
    Inc(SubtreeStatus.MoveCount);
  end;
  TProtocolEngineOutput(GOutput).AnalysisInfo.AddAnalysis(SubtreeStatus, Moves);

  GOutput.DrawAMoves;
  GSelf.StopManager.UpdateStopAnalysis;
  if GSelf.StopManager.Cause <> scNone then
    GSelf.FStop;
end;

procedure CallbackOnStop; cdecl;
begin
  GSelf.DoBestMove;
end;

procedure CallbackTellGUIDebug(const AMessage: PAnsiChar); cdecl;
begin
  if Assigned(GOutput) and GOutput.DebugMode then
    GOutput.TellGUIDebug(string(AMessage));
end;

procedure CallbackTellGUIError(const AMessage: PAnsiChar); cdecl;
begin
  if Assigned(GOutput) then
    GOutput.TellGUIError(string(AMessage));
end;

procedure CallbackTellGUIInfo(const AMessage: PAnsiChar); cdecl;
begin
  if Assigned(GOutput) then
    GOutput.TellGUIInfo(string(AMessage));
end;

{ TDllEngine }

procedure TDllEngine.BookMoves;
begin
  inherited;

  if Assigned(FBookMoves) then
    FBookMoves;
end;

constructor TDllEngine.Create;
begin
  inherited;

  CreateOptions;

  FPlugin := TPlugin.Create;
  GSelf := Self;
end;

procedure TDllEngine.CreateOptions;
begin
  CommonOptions.OwnBook.OnChange := OnOptionOwnBookChange;
  CommonOptions.AutomaticHashSize.OnChange := OnSetOptionAutomaticHashSizeChange;
  CommonOptions.HashSizeInMB.OnChange := OnSetOptionHashChange;
  CommonOptions.ClearHash.OnChange := OnSetOptionClearHashChange;
end;

destructor TDllEngine.Destroy;
begin
  try
    Stop;
    FPlugin.Free;
  finally
    inherited;
  end;
end;

procedure TDllEngine.DoMove(const AMove: string);
var
  a: AnsiString;
begin
  inherited;

  a := AnsiString(AMove);
  FDoMove(PAnsiChar(a));
end;

procedure TDllEngine.EvalToConsole;
begin
  inherited;

  if Assigned(FEvalToConsole) then
    FEvalToConsole;
end;

function TDllEngine.GetCurrentLine: string;
begin
  if Assigned(FGetCurrLine) then
    Result := string(AnsiString(FGetCurrLine))
  else
    Result := '';
end;

procedure TDllEngine.GetDllMethods;
begin
  // Required
  FSetStartPos := FPlugin.GetAddress('SetStartPos');
  FSetPositionFromString := FPlugin.GetAddress('SetPositionFromString');
  FDoMove := FPlugin.GetAddress('DoMove');
  FUndo := FPlugin.GetAddress('Undo');
  FAnalyze := FPlugin.GetAddress('Analyze');
  FStop := FPlugin.GetAddress('Stop');

  // Optional
  FBookMoves := FPlugin.GetAddressOptional('BookMoves');
  FPerft := FPlugin.GetAddressOptional('Perft');
  FWriteBoardToConsole := FPlugin.GetAddressOptional('WriteBoardToConsole');
  FEvalToConsole := FPlugin.GetAddressOptional('WriteEvalToConsole');
  FGetCurrLine := FPlugin.GetAddressOptional('GetCurrLine');

  // Options - optional
  FSetOptionOwnBook := FPlugin.GetAddressOptional('SetOptionOwnBook');
  FSetOptionHash := FPlugin.GetAddressOptional('SetOptionHash');
  FSetOptionClearHash := FPlugin.GetAddressOptional('SetOptionClearHash');
end;

procedure TDllEngine.CreateCallbacks;
var
  Callbacks: TCallbacks;
  SetCallbacks: TProcedureSetCallbacks;
begin
  Callbacks.NextDepth := @CallbackNextDepth;
  Callbacks.SetSelDepth := @CallbackSetSelDepth;
  Callbacks.NextMove := @CallbackNextMove;
  Callbacks.NextNode := @CallbackNextNode;
  Callbacks.EndgameTableBasesHit := @CallbackEndgameTableBasesHit;
  Callbacks.NextBestMove := @CallbackNextBestMove;
  Callbacks.OnStop := @CallbackOnStop;
  Callbacks.TellGUIDebug := @CallbackTellGUIDebug;
  Callbacks.TellGUIError := @CallbackTellGUIError;
  Callbacks.TellGUIInfo := @CallbackTellGUIInfo;

  SetCallbacks := FPlugin.GetAddress('SetCallbacks');
  SetCallbacks(Callbacks);
end;

function TDllEngine.GetDllName: TFileName;
begin
  Result := FPlugin.FileName;
end;

var
  GDepth: SG;

procedure RunPerft(AInstance: TObject; ANewThread: TThread);
var
  NodesCount: U8;
begin
  NodesCount := TDllEngine(AInstance).FPerft(GDepth);
  TDllEngine(AInstance).Output.TellGUIInfo('Nodes Searched: ' + IntToStr(NodesCount));
end;

procedure TDllEngine.GetPerft(const ADepth: SG);
begin
  inherited;

  if Assigned(FPerft) then
  begin
    GDepth := ADepth;
    RunInNewThread(Self, RunPerft, tpNormal, True);
  end;
end;

procedure TDllEngine.OnOptionOwnBookChange(Sender: TObject);
begin
  FSetOptionOwnBook(CommonOptions.OwnBook.Value);
end;

procedure TDllEngine.OnSetOptionAutomaticHashSizeChange(Sender: TObject);
begin
  OnSetOptionHashChange(Sender);
end;

procedure TDllEngine.OnSetOptionClearHashChange(Sender: TObject);
begin
  FSetOptionClearHash();
end;

procedure TDllEngine.OnSetOptionHashChange(Sender: TObject);
begin
  FSetOptionHash(CommonOptions.HashSizeInMB.Value);
end;

procedure TDllEngine.SetDllName(const Value: TFileName);
begin
  if FPlugin.FileName <> Value then
  begin
    FPlugin.FileName := Value;
    GetDllMethods;
    CreateCallbacks;
  end;
end;

procedure TDllEngine.SetNumberOfHashEntries(const AHashEntries: U8);
const
  EntrySize = 8; // TODO : Dll specific
begin
  inherited;

  FSetOptionHash(Math.Min(CommonOptions.HashSizeInMB.Value, Round(EntrySize * AHashEntries / MB)));
end;

procedure TDllEngine.SetOutput(const Value: TEngineOutput);
begin
  inherited;

  GOutput := Output;
end;

procedure TDllEngine.SetPositionFromStringSpecific(const AFEN: string; out ASideToMove: SG);
var
  FEN: AnsiString;
  SideToMove: S4;
begin
  inherited;

  if Assigned(FSetPositionFromString) then
  begin
    FEN := AnsiString(AFEN);
    SideToMove := ASideToMove;
    FSetPositionFromString(PAnsiChar(FEN), SideToMove);
    ASideToMove := SideToMove;
  end;
end;

procedure TDllEngine.SetStartPos;
begin
  inherited;

  if Assigned(FSetStartPos) then
    FSetStartPos;
end;

procedure Run(AInstance: TObject; ANewThread: TThread);
begin
  TDllEngine(AInstance).FAnalyze(TDllEngine(AInstance).StopManager.LevelManager.InfiniteAnalysis);
end;

procedure TDllEngine.Start;
begin
  inherited;

  if Assigned(FAnalyze) then
    RunInNewThread(Self, Run, tpNormal, True);
end;

procedure TDllEngine.Stop;
begin
  inherited;

  if Assigned(FStop) then
    FStop;
end;

procedure TDllEngine.Undo;
begin
  inherited;

  if Assigned(FUndo) then
    FUndo;
end;

procedure TDllEngine.WriteBoardToConsole;
begin
  inherited;

  if Assigned(FWriteBoardToConsole) then
    FWriteBoardToConsole;
end;

end.
