unit uProcess;

interface

uses uTypes;

function ProcessNotRun: BG;
procedure ProcessDone;
procedure ProcessPause;
procedure ProcessAbort;
function ProcessAborted: BG;
function ProcessStatusToStr: string;
function ProcessStatusToCaption: string;

implementation

uses Forms, SysUtils;

type
	TProcessStatus = (psIdle, psRun{ning}, psPaused, psAborted);
var
	ProcessStatus: TProcessStatus;

function ProcessNotRun: BG;
begin
	Result := ProcessStatus = psIdle;
	if Result then
		ProcessStatus := psRun
	else
		if ProcessStatus = psPaused then ProcessStatus := psRun;
end;

procedure ProcessDone;
begin
	ProcessStatus := psIdle;
end;

procedure ProcessPause;
begin
	case ProcessStatus of
	psRun: ProcessStatus := psPaused;
	psPaused: ProcessStatus := psRun;
	end;
end;

procedure ProcessAbort;
begin
	case ProcessStatus of
	psRun, psPaused: ProcessStatus := psAborted;
	end;
end;

function ProcessAborted: BG;
begin
	if ProcessStatus = psAborted then
	begin
		Result := True;
		Exit;
	end;

	Application.ProcessMessages;
	while ProcessStatus = psPaused do
	begin
		Sleep(LoopSleepTime);
		Application.ProcessMessages;
	end;
	Result := ProcessStatus <> psRun;
end;

function ProcessStatusToStr: string;
begin
	case ProcessStatus of
	psIdle: Result := '';
	psRun: Result := '[Running]';
	psAborted: Result := '[Aborted]';
	psPaused: Result := '[Paused]';
	end;
end;

function ProcessStatusToCaption: string;
begin
	Result := Application.Title + ' ' + ProcessStatusToStr;
end;

end.

