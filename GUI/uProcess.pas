unit uProcess;

interface

uses
  uTypes,
  uLongOperation,
  Forms;

type
  TProcessStatus = (psIdle, psRun{ning}, psPaused, psAborted);

  TProcess = class
  private
    FProcessStatus: TProcessStatus;
    FForm: TForm;
    FOneTick: U8;
    StartTime, PauseTime, LastInterruptTime: U8;
    FValue: S8;
    FMaximalValue: S8;
    FLongOperation: TLongOperation;
    function StatusToStr: string;
    function StatusToCaption: string;
    procedure StatusChanged(Value: TProcessStatus);
    procedure SetMaximalValue(const Value: S8);
    procedure SetValue(const Value: S8);
    procedure UpdateProgress;
  public
    constructor Create(Form: TForm);
    destructor Destroy; override;

    function Run: BG;
    function NotRun: BG;
    procedure Done;
    procedure Pause;
    procedure Abort;
    function Aborted: BG;
    function GetTime: U8;
    function Interrupt: BG;
    procedure ResetTime;
    property ProcessStatus: TProcessStatus read FProcessStatus write StatusChanged;
    property Value: S8 read FValue write SetValue;
    property MaximalValue: S8 read FMaximalValue write SetMaximalValue;
  end;

implementation

uses
  uStrings, uSystem, uDictionary, uProjectInfo, uMath, uMainTimer,

  TaskBarAPI, SysUtils;

{ TProcess }

function TProcess.Run: BG;
begin
  Result := FProcessStatus in [psRun, psPaused];
end;

function TProcess.NotRun: BG;
begin
  Result := FProcessStatus in [psIdle, psAborted];
  if Result then
  begin
    FLongOperation.Start;
    ProcessStatus := psRun
  end
  else if FProcessStatus = psPaused then
    ProcessStatus := psRun;
end;

procedure TProcess.Done;
begin
  if ProcessStatus <> psAborted then
    ProcessStatus := psIdle;
  FLongOperation.Stop;
end;

procedure TProcess.Pause;
begin
  case FProcessStatus of
    psRun:
      ProcessStatus := psPaused;
    psPaused:
      ProcessStatus := psRun;
  end;
end;

procedure TProcess.Abort;
begin
  case FProcessStatus of
    psRun, psPaused:
      ProcessStatus := psAborted;
  end;
end;

function TProcess.Aborted: BG;
begin
  if FProcessStatus = psAborted then
  begin
    Result := True;
    Exit;
  end;

  Application.ProcessMessages;
  PauseTime := MainTimer.Value.Ticks;
  while FProcessStatus = psPaused do
  begin
    Sleep(LoopSleepTime);
    Application.ProcessMessages;
  end;
  Inc(StartTime, MainTimer.IntervalFrom(PauseTime));
  Result := ProcessStatus <> psRun;
end;

function TProcess.StatusToStr: string;
begin
  case FProcessStatus of
    psIdle:
      Result := '';
    psRun:
      Result := Translate('[Running]');
    psAborted:
      Result := Translate('[Aborted]');
    psPaused:
      Result := Translate('[Paused]');
  end;
end;

procedure TProcess.StatusChanged(Value: TProcessStatus);
begin
  if FProcessStatus <> Value then
  begin
    FProcessStatus := Value;
    InitializeTaskbarAPI;
    case Value of
      psIdle:
        SetTaskbarProgressState(tbpsNone);
      psRun:
          SetTaskbarProgressState(tbpsIndeterminate);
      psPaused:
        SetTaskbarProgressState(tbpsPaused);
      psAborted:
        SetTaskbarProgressState(tbpsError);
    end;
    if Assigned(FForm) then
    begin
      FForm.Caption := StatusToCaption;
      Application.ProcessMessages;
    end;
  end;
end;

function TProcess.StatusToCaption: string;
begin
  Result := GetProjectInfo(piProductName) + CharSpace + StatusToStr;
end;

constructor TProcess.Create(Form: TForm);
begin
  FLongOperation := TLongOperation.Create;
  FLongOperation.Background := True;
  FValue := -1;

  FForm := Form;
  FOneTick := RoundDivU8(PerformanceFrequency, 1000 div LoopSleepTime);
  if Assigned(FForm) then
    FForm.Caption := StatusToCaption;
end;

function TProcess.GetTime: U8;
begin
  Result := IntervalFrom(StartTime);
end;

function TProcess.Interrupt: BG;
begin
  if (MainTimer.IntervalFrom(LastInterruptTime) > FOneTick) then
  begin
    Result := True;
    LastInterruptTime := MainTimer.Value.Ticks;
  end
  else
    Result := False;
end;

procedure TProcess.ResetTime;
begin
  StartTime := PerformanceCounter;
end;

procedure TProcess.SetMaximalValue(const Value: S8);
begin
  if FMaximalValue <> Value then
  begin
    FMaximalValue := Value;
    UpdateProgress;
  end;
end;

procedure TProcess.SetValue(const Value: S8);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    UpdateProgress;
  end;
end;

procedure TProcess.UpdateProgress;
begin
  InitializeTaskbarAPI;
  SetTaskbarProgressValue(FValue, FMaximalValue);
end;

destructor TProcess.Destroy;
begin
  FreeAndNil(FLongOperation);

  inherited;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}



finalization
{$IFNDEF NoFinalization}
  FinalizeTaskbarAPI;
{$ENDIF NoFinalization}

end.

