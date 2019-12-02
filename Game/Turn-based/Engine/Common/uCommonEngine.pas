// Ancestor for TInternalEngine in uInternalEngine and TExternalEngine in uExternalEngine

unit uCommonEngine;

interface

uses
  uTypes,
  uTimeSpan,
  uStopwatch,
  uAnalysisInfo,
  uArguments,
  uPowerRequest,

  uLevelManager,
  uRootMoves;

type
	TCommonEngine = class
	private
		FTitle: string;
    FAuthor: string;
    FOptions: TArguments;
    FAnalysisInfo: TAnalysisInfo;
    FSideToMove: SG;
    FRootMoves: TRootMoves;
    FIsReady: BG;
    FTerminated: BG;
    FLevelManager: TLevelManager;
    FPowerRequest: TPowerRequest;
    FInitializationTime: TTimeSpan;
    FInitializationTimeStopwatch: TStopwatch;
    FId: SG;

    // Property setters
    procedure SetSideToMove(const Value: SG);
    procedure SetTerminated(const Value: BG);
    procedure SetAuthor(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetIsReady(const Value: BG);
    procedure SetId(const Value: SG);
	protected
    procedure DoMove(const AMove: string); virtual;
    procedure SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG); virtual;
	public
		constructor Create;
		destructor Destroy; override;

    // Input
    property Id: SG read FId write SetId;
    property RootMoves: TRootMoves read FRootMoves;
    property AnalysisInfo: TAnalysisInfo read FAnalysisInfo;
    property LevelManager: TLevelManager read FLevelManager;

    // Process
		procedure Initialize; virtual;
    procedure WaitForReady; virtual;
		procedure Finalize; virtual;

    procedure NewGame; virtual;
    procedure DoMoves(const AMoves: string);
    procedure SetPositionFromString(const AString: string);
    procedure SetStartPos; virtual;
    procedure GetPerft(const ADepth: SG); virtual;

    property SideToMove: SG read FSideToMove write SetSideToMove;

    procedure WaitForCalculationDone(const ATimeOut: U8); virtual; abstract;

		procedure Start; virtual;
		procedure BookMoves; virtual;
		procedure Stop; virtual;
		procedure Pause; virtual;
		procedure Resume; virtual;

    procedure OnStop(Sender: TObject); virtual;

    // Output
		property Title: string read FTitle write SetTitle;
		property Author: string read FAuthor write SetAuthor;
    property InitializationTime: TTimeSpan read FInitializationTime;

 		property Options: TArguments read FOptions;

    property IsReady: BG read FIsReady write SetIsReady;
    property Terminated: BG read FTerminated write SetTerminated;
	end;

implementation

uses
  SysUtils,

  uStrings,
  uCustomPowerRequest;

{ TCommonEngine }

procedure TCommonEngine.BookMoves;
begin
  // No code
end;

constructor TCommonEngine.Create;
begin
	inherited Create;

  FInitializationTimeStopwatch := TStopwatch.Create;
  FOptions := TArguments.Create;
  FAnalysisInfo := TAnalysisInfo.Create;
  FRootMoves := TRootMoves.Create;
  FLevelManager := TLevelManager.Create;

  FPowerRequest := TPowerRequest.Create;
  FPowerRequest.Title := 'Analysis in progress';
  FPowerRequest.RequestType := PowerRequestExecutionRequired;
end;

destructor TCommonEngine.Destroy;
begin
  try
    FLevelManager.Free;
    FRootMoves.Free;
    FAnalysisInfo.Free;

    FOptions.Free;
    FPowerRequest.Free;
    FInitializationTimeStopwatch.Free;
  finally
  	inherited;
  end;
end;

procedure TCommonEngine.DoMove(const AMove: string);
begin
  FSideToMove := FSideToMove xor 1;
end;

procedure TCommonEngine.DoMoves(const AMoves: string);
var
	LineLength: SG;
	InLineIndex: SG;
  MoveIndex: SG;
  Move: string;
begin
  FRootMoves.Clear;

  MoveIndex:= 1;
	InLineIndex := 1;
	LineLength := Length(AMoves);
	while InLineIndex <= LineLength do
	begin
    try
      Move := ReadToChars(AMoves, InLineIndex, [CharSpace]);
  		DoMove(Move);
    except
      on E: Exception do
      begin
        E.Message := '(MoveIndex: ' + IntToStr(MoveIndex) + ', StringIndex: ' + IntToStr(InLineIndex) + '): ' + E.Message;
        raise;
      end;
    end;
  	SkipSpace(AMoves, InLineIndex);
    Inc(MoveIndex);
	end;
end;

procedure TCommonEngine.Stop;
begin
  // No code
end;

procedure TCommonEngine.Start;
begin
  if FAnalysisInfo.ElapsedTime.IsRunning then
    raise Exception.Create('Engine is running.');

  FPowerRequest.Increment;
  AnalysisInfo.Clear;
  AnalysisInfo.ElapsedTime.Restart;
end;

procedure TCommonEngine.Pause;
begin
// No code
end;

procedure TCommonEngine.Resume;
begin
  // No code
end;

procedure TCommonEngine.Finalize;
begin
  // No code
end;

procedure TCommonEngine.GetPerft(const ADepth: SG);
begin
  if ADepth <= 0 then
    raise EArgumentOutOfRangeException.Create('Perft requires positive number argument.');

  Stop; // Stops engine or previous perft calculation

  FPowerRequest.Increment;
end;

procedure TCommonEngine.Initialize;
begin
  // No code
  FInitializationTimeStopwatch.Start;
end;

procedure TCommonEngine.NewGame;
begin
  SetStartPos;
  RootMoves.Clear;
end;

procedure TCommonEngine.OnStop(Sender: TObject);
begin
  FPowerRequest.Decrement;
end;

procedure TCommonEngine.SetAuthor(const Value: string);
begin
  FAuthor := Value;
end;

procedure TCommonEngine.SetId(const Value: SG);
begin
  FId := Value;
end;

procedure TCommonEngine.SetIsReady(const Value: BG);
begin
  FIsReady := Value;
  if Value = True then
  begin
    FInitializationTime := FInitializationTimeStopwatch.Elapsed;
    FInitializationTimeStopwatch.Stop;
  end;
end;

procedure TCommonEngine.SetPositionFromString(const AString: string);
begin
  SetPositionFromStringSpecific(AString, FSideToMove);
end;

procedure TCommonEngine.SetPositionFromStringSpecific(const AString: string; out ASideToMove: SG);
begin
  FRootMoves.Clear;
end;

procedure TCommonEngine.SetSideToMove(const Value: SG);
begin
  FSideToMove := Value;
end;

procedure TCommonEngine.SetStartPos;
begin
  FSideToMove := 0;
end;

procedure TCommonEngine.SetTerminated(const Value: BG);
begin
  FTerminated := Value;
end;

procedure TCommonEngine.SetTitle(const Value: string);
begin
  FTitle := Value;
end;

procedure TCommonEngine.WaitForReady;
begin
  // No code
end;

end.
