unit uLongOperation;

interface

uses
  uTypes,
  uStopwatch,
  uPowerRequest;

type
  TLongOperation = class
  private
    FStopwatch: TStopwatch;
    FBackground: BG;
    FSound: BG;
    FPowerRequest: TPowerRequest;
    FTitle: string;
    procedure SetSound(const Value: BG);
    procedure SetBackground(const Value: BG);
    procedure SetTitle(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Title: string read FTitle write SetTitle;
    property Background: BG read FBackground write SetBackground;
    property Sound: BG read FSound write SetSound;
    property Stopwatch: TStopwatch read FStopwatch;
  end;

implementation

uses
  Controls,
  Forms,

  TaskBarAPI,

  uPlaySound,
  uMsg,
  uOutputFormat,
  uCustomPowerRequest;

{ TLongOperation }

constructor TLongOperation.Create;
begin
  inherited Create;

  FPowerRequest := TPowerRequest.Create;
  FPowerRequest.RequestType := PowerRequestExecutionRequired;

  FStopwatch := TStopwatch.Create;
  FBackground := False;
  FSound := True;
end;

procedure TLongOperation.Start;
begin
  FPowerRequest.Increment;

	if FBackground then
		Screen.Cursor := crAppStart
	else
		Screen.Cursor := crHourGlass;

  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);

  FStopwatch.Restart;
end;

procedure TLongOperation.Stop;
begin
  FPowerRequest.Decrement;
  FStopwatch.Stop;

	if FSound and (FStopwatch.Elapsed.Seconds >= 1) then
		PlayWinSound(wsAsterisk);

	Screen.Cursor := crDefault;

  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsNone);

  if IsDebug then
    Information('Total time: ' + MsToStr(FStopwatch.Elapsed.Milliseconds, TDisplay.diSD, 3));
end;

destructor TLongOperation.Destroy;
begin
  try
    if FStopwatch.IsRunning then
      Stop;
    FStopwatch.Free;
    FPowerRequest.Free;
  finally
    inherited;
  end;
end;

procedure TLongOperation.SetSound(const Value: BG);
begin
  FSound := Value;
end;

procedure TLongOperation.SetTitle(const Value: string);
begin
  FTitle := Value;
  FPowerRequest.Title := FTitle;
end;

procedure TLongOperation.SetBackground(const Value: BG);
begin
  FBackground := Value;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
  FinalizeTaskbarAPI;
{$ENDIF NoFinalization}

end.
