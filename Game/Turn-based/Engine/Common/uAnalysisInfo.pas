unit uAnalysisInfo;

interface

uses
	SysUtils,
  Classes,

	uTypes,
  uTimeSpan,
  uStopwatch,

  uStopCause,
	uScore,
  uPosition,
  uAnalysis,
  uSubtreeStatus;

type
	TAnalysisInfo = class
	private
    FDepth: SG;

    FDepthElapsedTime, FLastDepthElapsedTime: TTimeSpan;
		FLastCallTime: TTimeSpan;

		DepthNodes, LastDepthNodes: U8;
    FLastCallNodes: U8;

    FLastDepthBestMoveScore: TScore;
    FBestMoveScore: TScore;

    FNodes: U8;
    FNodesPerSec: U8;
		FStopCause: TStopCause;
		FActualAnalysisIndex: SG;
		FElapsedTime: TStopwatch;
    FChangedMove: BG;
    FActualBestMoveIsBad: BG;
    FAnalysisCount: SG;
		FCurrentMoveIndex: SG;
		FCurrentMove: string;
    FMoveCount: SG;
    FBestMoveSince: TTimeSpan;
    FSelDepth: SG;
    FHashNew: U4;
    FHashPos: U4;
		FAnalysis: array of TAnalysis;
    FStartPosition: string;
    FBestMove: string;
    FOnAddAnalysis: TNotifyEvent;
    FOnBestMove: TNotifyEvent;
    FAverageMoveOverhead: TTimeSpan;
    FMaximalMoveOverhead: TTimeSpan;
    FEndgameTableBasesHits: U8;

		procedure NextDepth;
		function GetActualAnalysis: PAnalysis;
    procedure InternalSetBestMove(const ABestMove: string);
    procedure SetStopCause(const Value: TStopCause);
    procedure SetDepth(const Value: SG);
    function GetBestMoveFromAnalysis: string;
    procedure SetMoveCount(const Value: SG);
    procedure SetSelDepth(const Value: SG);
    procedure SetHashNew(const Value: U4);
    procedure SetHashPos(const Value: U4);
    procedure SetStartPosition(const Value: string);
    procedure SetNodes(const Value: U8);
    procedure SetBestMove(const Value: string);
    procedure SetOnAddAnalysis(const Value: TNotifyEvent);
    procedure SetOnBestMove(const Value: TNotifyEvent);
    procedure SetAverageMoveOverhead(const Value: TTimeSpan);
    procedure SetMaximalMoveOverhead(const Value: TTimeSpan);
    procedure SetEndgameTableBasesHits(const Value: U8);
	public
		constructor Create;
    destructor Destroy; override;

    // Setup data
		procedure Clear;
    procedure AddAnalysis(const ABestStatus: TSubtreeStatus; const AMoves: array of string); overload;
    procedure NextNode;

    // Setup and get data
    property StartPosition: string read FStartPosition write SetStartPosition;
		property Depth: SG read FDepth write SetDepth;
		property SelDepth: SG read FSelDepth write SetSelDepth;
    property Nodes: U8 read FNodes write SetNodes;

    property ChangedMove: BG read FChangedMove;
    property ActualBestMoveIsBad: BG read FActualBestMoveIsBad;

    property MoveCount: SG read FMoveCount write SetMoveCount;
    property BestMoveSince: TTimeSpan read FBestMoveSince;

		property HashNew: U4 read FHashNew write SetHashNew;
		property HashPos: U4 read FHashPos write SetHashPos;

    property EndgameTableBasesHits: U8 read FEndgameTableBasesHits write SetEndgameTableBasesHits;

    // Get data
		function GetAnalysis(const Value: SG): PAnalysis;
		function LastAnalysis: PAnalysis;

		function EstimateNextDepthElapsedTime: TTimeSpan;
		function EstimateNextDepthNodes: U8;
    function ActualAnalysisToString(const ANullMoveStr: string): string;
    procedure SetCurrentMove(const AMoveIndex: SG; const AMove: string);

		property StopCause: TStopCause read FStopCause write SetStopCause;
    property ElapsedTime: TStopwatch read FElapsedTime;
    function ElapsedTimeWithAverageMoveOverhead: TTimeSpan;
    property AverageMoveOverhead: TTimeSpan read FAverageMoveOverhead write SetAverageMoveOverhead;
    property MaximalMoveOverhead: TTimeSpan read FMaximalMoveOverhead write SetMaximalMoveOverhead;

		property ActualAnalysis: PAnalysis read GetActualAnalysis; // write SetActualAnalysis;

    property BestMove: string read FBestMove write SetBestMove;
		property AnalysisCount: SG read FAnalysisCount;
    property CurrentMoveIndex: SG read FCurrentMoveIndex;
    property CurrentMove: string read FCurrentMove;

    property OnAddAnalysis: TNotifyEvent read FOnAddAnalysis write SetOnAddAnalysis;
    property OnBestMove: TNotifyEvent read FOnBestMove write SetOnBestMove;
	end;

implementation

uses
	Math,

	uStrings,
	uMath,
  uFiles;

{ TAnalysisInfo }

function TAnalysisInfo.ActualAnalysisToString(const ANullMoveStr: string): string;
var
	i: SG;
  Analysis: PAnalysis;
begin
  Result := '';
  Analysis := GetActualAnalysis;
  for i := 0 to Analysis.Status.MoveCount - 1 do
  begin
    if i > 0 then
      Result := Result + ' ';
    Result := Result + Analysis.Moves[i];
  end;
end;

procedure TAnalysisInfo.AddAnalysis(const ABestStatus: TSubtreeStatus; const AMoves: array of string);
const
  BadMoveStep = 35; // [cp] Calibrate
var
	NewSize: SG;
	A: PAnalysis;
  i: SG;
  MoveCount: SG;
begin
	NewSize := FAnalysisCount + 1;
	if AllocByExp(Length(FAnalysis), NewSize) then
		SetLength(FAnalysis, NewSize);
	A := @FAnalysis[FAnalysisCount];
	A.Status := ABestStatus;
	A.ElapsedTime := FElapsedTime.Elapsed;
	A.Nodes := Nodes;
	A.Depth := Depth;
	A.SelDepth := SelDepth;
  MoveCount := Length(AMoves);
	A.ActMove := MoveCount;
	SetLength(A.Moves, MoveCount);
  for i := 0 to MoveCount - 1 do
    A.Moves[i] := AMoves[i];
	if FActualAnalysisIndex = FAnalysisCount - 1 then
	begin
		Inc(FActualAnalysisIndex);
	end;
	Inc(FAnalysisCount);

  InternalSetBestMove(GetBestMoveFromAnalysis);

  FBestMoveScore := ABestStatus.Score;
  if (FDepth >= 2) and (FBestMoveScore + BadMoveStep < FLastDepthBestMoveScore) then
    FActualBestMoveIsBad := True;

  if Assigned(FOnAddAnalysis) then
    FOnAddAnalysis(Self);
end;

procedure TAnalysisInfo.Clear;
begin
  Assert(FElapsedTime.IsRunning = False);
  if FElapsedTime.IsRunning then
  begin
    FElapsedTime.Stop;
    FElapsedTime.Reset;
  end;

  FDepth := 0;
  SelDepth := 0;

  FBestMove := '';
  FBestMoveScore := 0;
  FLastDepthBestMoveScore := 0;
  FBestMoveSince.Ticks := 0;

	FLastCallTime.Ticks := 0;
	DepthNodes := 0;
	LastDepthNodes := 0;
  FLastCallNodes := 0;

	FStopCause := scNone;

	FCurrentMoveIndex := -1;
  FCurrentMove := '';

	FHashNew := 0;
	FHashPos := 0;
  FEndgameTableBasesHits := 0;

	FActualAnalysisIndex := 0;
	FAnalysisCount := 0;
	SetLength(FAnalysis, 0);

  FNodes := 0;
  FNodesPerSec := 0;
end;

constructor TAnalysisInfo.Create;
begin
  FElapsedTime := TStopwatch.Create;
end;

destructor TAnalysisInfo.Destroy;
begin
  FElapsedTime.Free;

  inherited;
end;

function TAnalysisInfo.LastAnalysis: PAnalysis;
begin
	Result := nil;
	if (FAnalysisCount > 0) then
		Result := @FAnalysis[FAnalysisCount - 1];
end;

procedure TAnalysisInfo.SetDepth(const Value: SG);
begin
  if FDepth <> Value then
  begin
    NextDepth;
    FDepth := Value;
  end;
end;

procedure TAnalysisInfo.SetEndgameTableBasesHits(const Value: U8);
begin
  FEndgameTableBasesHits := Value;
end;

procedure TAnalysisInfo.SetHashNew(const Value: U4);
begin
  FHashNew := Value;
end;

procedure TAnalysisInfo.SetHashPos(const Value: U4);
begin
  FHashPos := Value;
end;

procedure TAnalysisInfo.SetMaximalMoveOverhead(const Value: TTimeSpan);
begin
  FMaximalMoveOverhead := Value;
end;

procedure TAnalysisInfo.SetMoveCount(const Value: SG);
begin
  FMoveCount := Value;
end;

procedure TAnalysisInfo.SetAverageMoveOverhead(const Value: TTimeSpan);
begin
  FAverageMoveOverhead := Value;
end;

procedure TAnalysisInfo.SetNodes(const Value: U8);
begin
  FNodes := Value;
end;

procedure TAnalysisInfo.SetOnAddAnalysis(const Value: TNotifyEvent);
begin
  FOnAddAnalysis := Value;
end;

procedure TAnalysisInfo.SetOnBestMove(const Value: TNotifyEvent);
begin
  FOnBestMove := Value;
end;

procedure TAnalysisInfo.SetSelDepth(const Value: SG);
begin
  FSelDepth := Value;
end;

procedure TAnalysisInfo.SetStartPosition(const Value: string);
begin
  FStartPosition := Value;
end;

procedure TAnalysisInfo.SetStopCause(const Value: TStopCause);
begin
  FStopCause := Value;
end;

const
	GrowFactor = 4; // Calibrate 2..n

function TAnalysisInfo.ElapsedTimeWithAverageMoveOverhead: TTimeSpan;
begin
  Result.Ticks := ElapsedTime.Elapsed.Ticks + FAverageMoveOverhead.Ticks;
end;

function TAnalysisInfo.EstimateNextDepthElapsedTime: TTimeSpan;
var
  ActualDepthElapsedTime, Lag: TTimeSpan;
begin
  Lag.Milliseconds := 10;
  ActualDepthElapsedTime.Ticks := Max(0, S8(FDepthElapsedTime.Ticks) - S8(Lag.Ticks));

  if FDepth = 0 then
    Result.Ticks := 0
	else if FDepth = 1 then
		Result.Ticks := GrowFactor * ActualDepthElapsedTime.Ticks
	else // FDepth >= 2
  begin
		Result.Ticks :=
      Max(
        ActualDepthElapsedTime.Ticks,
        RoundDivU8(ActualDepthElapsedTime.Ticks * ActualDepthElapsedTime.Ticks, FLastDepthElapsedTime.Ticks));
  end;
end;

procedure TAnalysisInfo.NextNode;
begin
  Inc(FNodes);
end;

function TAnalysisInfo.EstimateNextDepthNodes: U8;
begin
  if FDepth = 0 then
		Result := FMoveCount // Depth 1 Nodes
  else if FDepth = 1 then
		Result := GrowFactor * DepthNodes
	else
		Result := Max(DepthNodes, RoundDivU8(U8(DepthNodes) * DepthNodes, LastDepthNodes)) + Nodes;
end;

procedure TAnalysisInfo.NextDepth;
var
  Actual: TTimeSpan;
begin
  Actual := FElapsedTime.Elapsed;

  FLastDepthBestMoveScore := FBestMoveScore;

  FLastDepthElapsedTime := FDepthElapsedTime;
  FDepthElapsedTime.Ticks := Actual.Ticks - FLastCallTime.Ticks;
  FLastCallTime := Actual;

	LastDepthNodes := DepthNodes;

  if Nodes >= FLastCallNodes then
  begin
  	DepthNodes := Nodes - FLastCallNodes;
    FLastCallNodes := Nodes;
  end;

  SelDepth := 0;
  FChangedMove := False;
  FActualBestMoveIsBad := False;
end;

function TAnalysisInfo.GetActualAnalysis: PAnalysis;
begin
	Result := GetAnalysis(FActualAnalysisIndex);
end;

function TAnalysisInfo.GetAnalysis(const Value: SG): PAnalysis;
begin
	Result := nil;
	if (Value >= 0) and (Value < FAnalysisCount) then
		Result := @FAnalysis[Value];
end;

function TAnalysisInfo.GetBestMoveFromAnalysis: string;
var
  Analysis: PAnalysis;
begin
  Analysis := GetActualAnalysis;
  if (Analysis <> nil) and (Length(Analysis.Moves) > 0) then
    Result := Analysis.Moves[0]
  else
    Result := '';
end;

procedure TAnalysisInfo.InternalSetBestMove(const ABestMove: string);
begin
  if FBestMove <> ABestMove then
  begin
    FBestMove := ABestMove;
    FBestMoveSince := ElapsedTime.Elapsed;
    FChangedMove := True;
  end;
end;

procedure TAnalysisInfo.SetCurrentMove(const AMoveIndex: SG; const AMove: string);
begin
  FCurrentMoveIndex := AMoveIndex;
  FCurrentMove := AMove;
  if FBestMove = '' then
    FBestMove := FCurrentMove;
end;

procedure TAnalysisInfo.SetBestMove(const Value: string);
begin
  if FBestMove <> Value then
  begin
    InternalSetBestMove(Value);
    if Assigned(FOnBestMove) then
      FOnBestMove(Self);
  end;
end;

end.
