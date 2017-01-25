unit uLongOperation;

interface

uses
  uTypes,
  uStopwatch;

type
  TLongOperation = class
  private
    FStopwatch: TStopwatch;
    FBackground: BG;
    FSound: BG;
    procedure SetSound(const Value: BG);
    procedure SetBackground(const Value: BG);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Background: BG read FBackground write SetBackground;
    property Sound: BG read FSound write SetSound;
    property Stopwatch: TStopwatch read FStopwatch;
  end;

implementation

uses
  Controls,
  Forms,
  uWave,
  uMsg,
  uOutputFormat,
  TaskBarAPI;

{ TLongOperation }

constructor TLongOperation.Create;
begin
  inherited Create;

  FStopwatch := TStopwatch.Create;
  FBackground := False;
  FSound := True;
end;

procedure TLongOperation.Start;
begin
	if FBackground then
		Screen.Cursor := crAppStart
	else
		Screen.Cursor := crHourGlass;

  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsIndeterminate);

  FStopwatch.Start;
end;

procedure TLongOperation.Stop;
begin
  FStopwatch.Stop;

	if FSound and (FStopwatch.ElapsedMilliseconds >= Second) then
		PlayWinSound(wsAsterisk);

	Screen.Cursor := crDefault;

  InitializeTaskbarAPI;
  SetTaskbarProgressState(tbpsNone);

  if IsDebug then
    Information('Total time: ' + MsToStr(FStopwatch.ElapsedMilliseconds, diSD, 3));
end;

destructor TLongOperation.Destroy;
begin
  if FStopwatch.IsRunning then
    Stop;

  inherited;
end;

procedure TLongOperation.SetSound(const Value: BG);
begin
  FSound := Value;
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
