unit uProcess;

interface

uses
	uTypes,
	Forms;

type
	TProcessStatus = (psIdle, psRun{ning}, psPaused, psAborted);

	TProcess = class
	private
		FProcessStatus: TProcessStatus;
		FForm: TForm;
		FOneTick: U8;
		StartTime, PauseTime, LastInterruptTime: U8;
		function StatusToStr: string;
		function StatusToCaption: string;
		procedure StatusChanged(Value: TProcessStatus);
	public
		constructor Create(Form: TForm);
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
	end;

implementation

uses
	uStrings,
	uSystem,
	uDictionary,
	uProjectInfo,
	uMath,
	SysUtils;

{ TProcess }

function TProcess.Run: BG;
begin
	Result := FProcessStatus in [psRun, psPaused];
end;

function TProcess.NotRun: BG;
begin
	Result := FProcessStatus = psIdle;
	if Result then
	begin
		BeginLongOperation(True);
		ProcessStatus := psRun
	end
	else if FProcessStatus = psPaused then
		ProcessStatus := psRun;
end;

procedure TProcess.Done;
begin
	EndLongOperation(True);
	ProcessStatus := psIdle;
end;

procedure TProcess.Pause;
begin
	case FProcessStatus of
	psRun: ProcessStatus := psPaused;
	psPaused: ProcessStatus := psRun;
	end;
end;

procedure TProcess.Abort;
begin
	case FProcessStatus of
	psRun, psPaused: ProcessStatus := psAborted;
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
	PauseTime := PerformanceCounter;
	while FProcessStatus = psPaused do
	begin
		Sleep(LoopSleepTime);
		Application.ProcessMessages;
	end;
	Inc(StartTime, PerformanceCounter - PauseTime);
	Result := ProcessStatus <> psRun;
end;

function TProcess.StatusToStr: string;
begin
	case FProcessStatus of
	psIdle: Result := '';
	psRun: Result := Translate('[Running]');
	psAborted: Result := Translate('[Aborted]');
	psPaused: Result := Translate('[Paused]');
	end;
end;

procedure TProcess.StatusChanged(Value: TProcessStatus);
begin
	if FProcessStatus <> Value then
	begin
		FProcessStatus := Value;
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
	FForm := Form;
	FOneTick := RoundDivU8(PerformanceFrequency, 1000 div LoopSleepTime);
end;

function TProcess.GetTime: U8;
begin
	Result := PerformanceCounter - StartTime;
end;

function TProcess.Interrupt: BG;
var
	NTime: U8;
begin
	NTime := PerformanceCounter;
	if (NTime > LastInterruptTime + FOneTick) then
	begin
		Result := True;
		LastInterruptTime := NTime;
	end
	else
		Result := False;
end;

procedure TProcess.ResetTime;
begin
	StartTime := PerformanceCounter;
end;

end.

