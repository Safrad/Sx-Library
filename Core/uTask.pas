unit uTask;

interface

uses
	uSchedule, uData, uTypes, uTimeSpan, SysUtils;

type
	TTaskAction = (taNone, taSound, taNormal, taMore, taHibernate, taSuspend, taPowerOff, taReboot, taShutdown, taLogOff, taDownloadWebPage);
var
	ActionToStr: array[TTaskAction] of string;
type
	TTask = class(TSchedule)
	public
		FId: UG;
		// Clonable fields
		Name: string;
		WaveFileName: TFileName;
		ProgramFileName: TFileName;
		Params: string;
		Created: TDateTime;
		Modified: TDateTime;
		Action: TTaskAction;
		Enabled: BG;

		// Statistics
		LastRunCount: S4;
		LastRuns: array of TDateTime;
		RunCount: UG;
		LastSoundTime: U8;

		// Calculated
		FRunning: BG;
		Active: BG;
		Missed: BG;
		MissedCount: U4;
		// Temporary
		MarkAsDeleted: BG;
	private
		procedure SetRunning(const Value: BG);
		function GetLastRun: TDateTime;
		function GetLastRunLogFileName: TFileName;
	public
		constructor Create(const NewName: string = '');
		destructor Destroy; override;
		procedure Clone(const Task: TTask);
		property Running: BG read FRunning write SetRunning;
		property Id: UG read FId;
		property LastRun: TDateTime read GetLastRun;
		property LastRunLogFileName: TFileName read GetLastRunLogFileName;

		procedure ReadRunLogFromFile;
		procedure WriteRunLogToFile;
	end;
var
	Tasks: TData; // Array of TTask
	GTaskId: UG;

function GetRunningTaskCount: UG;
function FindTaskByName(const AName: string): TTask;

implementation

uses
	DateUtils,
  Math,

	uInputFormat,
  uOutputFormat,
  uStrings,
  uMath,
  uDictionary,
  uFiles,
  uRawFile,
  uTextFile;

var
	MessagesLogDir: string;
	RunnedTaskCount: UG;

function GetRunningTaskCount: UG;
begin
	Result := RunnedTaskCount;
end;

function FindTaskByName(const AName: string): TTask;
var
  i: SG;
begin
 Result := nil;
 for i := 0 to Tasks.Count - 1 do
 begin
   if TTask(Tasks[i]).Name = AName then
   begin
     Result := TTask(Tasks[i]);
     Exit;
   end;
 end;
end;

{ TTask}

procedure TTask.Clone(const Task: TTask);
begin
	Name := Task.Name;
	LastRuns := Task.LastRuns;
	LastRunCount := Task.LastRunCount;
	WaveFileName := Task.WaveFileName;
	ProgramFileName := Task.ProgramFileName;
	Params := Task.Params;
	ScheduleType := Task.ScheduleType;
	Created := Task.Created;
	Modified := Task.Modified;
	StartDT := Task.StartDT;
	EndDT := Task.EndDT;
	Duration := Task.Duration;
	EveryXDay := Task.EveryXDay;
	EveryXWeek := Task.EveryXWeek;
	EveryXMonth := Task.EveryXMonth;
	EveryXYear := Task.EveryXYear;
	EveryXIdle := Task.EveryXIdle;
	EveryXOverload := Task.EveryXOverload;
	Action := Task.Action;
	WeekDays := Task.WeekDays;
	Months := Task.Months;
	Enabled := Task.Enabled;
end;

constructor TTask.Create(const NewName: string = '');
var i: SG;
begin
  inherited Create;

	FId := GTaskId;

	Enabled := True;
	Name := Name;

	Created := StartDT;
	Modified := Created;
	RunCount := 0;

	StartDT := Created;
	EndDT := 0;
	Duration := Hour;
	Action := taNormal;
	WaveFileName := 'Sounds' + PathDelim + 'Reminder.wav';
	EveryXDay := 1;
	EveryXWeek := 1;
	EveryXMonth := 1;
	EveryXYear := 1;
	EveryXIdle.Minutes := 10;
	EveryXOverload.Minutes := 1;
	for i := 0 to DaysInWeek - 1 do
		WeekDays[i] := False;
	for i := 0 to MonthsInYear - 1 do
		Months[i] := False;
	Running := False;
	Active := False;
end;

destructor TTask.Destroy;
begin
	Running := False;

  inherited;
end;

procedure TTask.ReadRunLogFromFile;
var
	Line: string;
	F: TTextFile;
begin
	F := TTextFile.Create;
	try
		LastRunCount := 0;
		SetLength(LastRuns, 0);
    F.FileName := GetLastRunLogFileName;
    F.FileMode := fmReadOnly;
		F.Open;
    while not F.Eof do
    begin
      F.ReadLine(Line);

      SetLength(LastRuns, LastRunCount + 1);
      LastRuns[LastRunCount] := SToDateTime(Line, ifIO);
      Inc(LastRunCount);
    end;
    F.Close();
	finally
		F.Free;
	end;
end;

procedure TTask.WriteRunLogToFile;
var
	i: SG;
	s: string;
begin
	for i := 0 to LastRunCount - 1 do
		s := s + DateTimeToS(LastRuns[i], -3, ofIO) + FileSep;
	WriteStringToFile(GetLastRunLogFileName, s, False);
end;

procedure TTask.SetRunning(const Value: BG);
begin
	if FRunning <> Value then
	begin
		FRunning := Value;
		if FRunning then Inc(RunnedTaskCount) else Dec(RunnedTaskCount);
	end;
end;

function TTask.GetLastRun: TDateTime;
begin
	if LastRunCount = 0 then
		Result := 0
	else
		Result := LastRuns[LastRunCount - 1];
end;

function TTask.GetLastRunLogFileName: TFileName;
begin
	Result := MessagesLogDir + IntToStr(Id) + '.log';
end;

initialization
	MessagesLogDir := LocalAppDataDir + 'MessagesLog' + PathDelim;
	CreateDirEx(MessagesLogDir);
end.
