unit uGoCommand;

interface

uses
  uTypes,

  uCustomLevel,
  uEngineCommand;

type
  TGoCommand = class(TEngineCommand)
  private
    const
      Keywords: array[0..10] of string = ('searchmoves', 'infinite', 'depth', 'mate', 'movetime', 'nodes',
        'movestogo', 'wtime', 'btime', 'winc', 'binc');
    var
      MyLevel: TCustomLevel;
      OpponentLevel: TCustomLevel;
    procedure FreeLevel;
  protected
    function GetSyntax: string; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute(const AParameters: string); override;
  end;

implementation

uses
  SysUtils,

  uMath,

  uMaximalDepthLevel,
  uWinInLevel,
  uMoveTimeLevel,
  uNodesLevel,
  uTimeControlLevel,

  uEParseError,
  uStrings,
  uOutputFormat;

{ TGoCommand }

constructor TGoCommand.Create;
begin
  inherited;

  Description := 'Start calculating on the current position set up with the "position" command.';
end;

destructor TGoCommand.Destroy;
begin
  try
    FreeLevel;
  finally
    inherited;
  end;
end;

procedure TGoCommand.Execute(const AParameters: string);
var
  WhiteRemainTime, BlackRemainTime, WhiteIncrementalTime, BlackIncrementalTime: SG;
  MovesToGo: SG;
  InLineIndex: SG;
  Value: string;
begin
  inherited;
  if AParameters = '' then
  begin
    raise EParseError.Create(Keywords, '');
  end;

  FreeLevel;
  try
//    InternalEngine.Ponder := False;

    InternalEngine.RootMoves.Clear;
    WhiteRemainTime := 0;
    BlackRemainTime := 0;
    WhiteIncrementalTime := 0;
    BlackIncrementalTime := 0;
    MovesToGo := 0;

    InternalEngine.LevelManager.InfiniteAnalysis := False;

    InLineIndex := 1;
    while InLineIndex <= Length(AParameters) do
    begin
      Value := ReadToChar(AParameters, InLineIndex, CharSpace);
      if Value = '' then

      else if Value = 'searchmoves' then
      begin
        InternalEngine.RootMoves.ExcludeAll;
        while InLineIndex <= Length(AParameters) do
        begin
          Value := ReadToChar(AParameters, InLineIndex, CharSpace);
          InternalEngine.RootMoves.Include(Value);
        end;
      end
      else if Value = 'ponder' then
      begin
//        InternalEngine.Ponder := True;
      end
      else if Value = 'movestogo' then
      begin
        MovesToGo := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'wtime' then
      begin
        WhiteRemainTime := ReadSGFast(AParameters, InLineIndex);
        if WhiteRemainTime = MaxInt then // Lichess correspondence
          WhiteRemainTime := 15 * 60 * 1000; // 15 minutes
      end
      else if Value = 'btime' then
      begin
        BlackRemainTime := ReadSGFast(AParameters, InLineIndex);
        if BlackRemainTime = MaxInt then // Lichess correspondence
          BlackRemainTime := 15 * 60 * 1000; // 15 minutes
      end
      else if Value = 'winc' then
      begin
        WhiteIncrementalTime := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'binc' then
      begin
        BlackIncrementalTime := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'depth' then
      begin
        MyLevel := TMaximalDepthLevel.Create;
        TMaximalDepthLevel(MyLevel).Value := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'mate' then
      begin
        Assert(MyLevel = nil);
        MyLevel := TWinInLevel.Create;
        TWinInLevel(MyLevel).Value := 2 * ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'movetime' then
      begin
        Assert(MyLevel = nil);
        MyLevel := TMoveTimeLevel.Create;
        TMoveTimeLevel(MyLevel).ValueType := lvtEqual;
        TMoveTimeLevel(MyLevel).Value.Milliseconds := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'nodes' then
      begin
        Assert(MyLevel = nil);
        MyLevel := TNodesLevel.Create;
        TNodesLevel(MyLevel).ValueType := lvtEqual;
        TNodesLevel(MyLevel).Value := ReadSGFast(AParameters, InLineIndex);
      end
      else if Value = 'infinite' then
      begin
        Assert(MyLevel = nil);
        InternalEngine.LevelManager.InfiniteAnalysis := True;
      end
      else
      begin
        raise EParseError.Create(Keywords, Value);
      end;
    end;
    if MyLevel = nil then
    begin
      if InternalEngine.Output.DebugMode then
        InternalEngine.Output.TellGUIDebug('Side to move: ' + IntToStr(InternalEngine.SideToMove));
      if InternalEngine.SideToMove <> 0 then
      begin
        Exchange(WhiteRemainTime, BlackRemainTime);
        Exchange(WhiteIncrementalTime, BlackIncrementalTime);
      end;
      MyLevel := TTimeControlLevel.Create;
      TTimeControlLevel(MyLevel).TimeUsage := InternalEngine.CommonOptions.TimeUsage.Value;
      TTimeControlLevel(MyLevel).FixedMoveTime := InternalEngine.CommonOptions.FixedMoveTime.Value;
      TTimeControlLevel(MyLevel).MinimalTime.Milliseconds := InternalEngine.CommonOptions.MinimalMoveTime.Value;
      TTimeControlLevel(MyLevel).MaximalMoveOverhead.Milliseconds := InternalEngine.CommonOptions.MaximalMoveOverhead.Value;

      TTimeControlLevel(MyLevel).MoveCount := MovesToGo;
      TTimeControlLevel(MyLevel).IncrementTime.Milliseconds := WhiteRemainTime;
      TTimeControlLevel(MyLevel).MoveIncrementTime.Milliseconds := WhiteIncrementalTime;
      TTimeControlLevel(MyLevel).FreeTime.Ticks := 0;
      TTimeControlLevel(MyLevel).MoveIndex := 0; // Init time limit

      OpponentLevel := TTimeControlLevel.Create;
      TTimeControlLevel(OpponentLevel).MoveCount := MovesToGo;
      TTimeControlLevel(OpponentLevel).MaximalMoveOverhead.Milliseconds := InternalEngine.CommonOptions.MaximalMoveOverhead.Value;
      TTimeControlLevel(OpponentLevel).IncrementTime.Milliseconds := BlackRemainTime;
      TTimeControlLevel(OpponentLevel).MoveIncrementTime.Milliseconds := BlackIncrementalTime;
      TTimeControlLevel(OpponentLevel).FreeTime.Ticks := 0;
      TTimeControlLevel(OpponentLevel).MoveIndex := 0;
      TTimeControlLevel(OpponentLevel).TimeUsage := InternalEngine.CommonOptions.TimeUsage.Value;
      TTimeControlLevel(OpponentLevel).FixedMoveTime := InternalEngine.CommonOptions.FixedMoveTime.Value;
      TTimeControlLevel(OpponentLevel).MinimalTime.Milliseconds := InternalEngine.CommonOptions.MinimalMoveTime.Value;

      InternalEngine.LevelManager.MyRemainTime.Milliseconds := WhiteRemainTime;
      InternalEngine.LevelManager.OpponentRemainTime.Milliseconds := BlackRemainTime;
    end;
    TCustomLevel(MyLevel).AnalysisInfo := InternalEngine.AnalysisInfo;
    if OpponentLevel <> nil then
      TCustomLevel(OpponentLevel).AnalysisInfo := InternalEngine.AnalysisInfo;
  except
    MyLevel.Free;
    OpponentLevel.Free;
    raise;
  end;
  InternalEngine.LevelManager.MyLevel := MyLevel;
  InternalEngine.LevelManager.OpponentLevel := OpponentLevel;
  InternalEngine.Start;
end;

procedure TGoCommand.FreeLevel;
begin
  InternalEngine.LevelManager.MyLevel := nil;
  InternalEngine.LevelManager.OpponentLevel := nil;
  FreeAndNil(MyLevel);
  FreeAndNil(OpponentLevel);
end;

function StringArrayToString(const AValues: array of string; const ASeparator: string): string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to Length(AValues) - 1 do
  begin
    Result := Result + AValues[i];
    if i < Length(AValues) - 1 then
      Result := Result + ASeparator;
  end;
end;

function TGoCommand.GetSyntax: string;
begin
  Result := '[' + StringArrayToString(Keywords, ' | ') + ']';
end;

end.
