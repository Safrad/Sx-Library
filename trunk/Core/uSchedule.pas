unit uSchedule;

interface

uses
	uTypes, uDIniFile, SysUtils;

type
	TScheduleType = (scNever, scOnce, scInterval, scDaily, scWeekly, scMonthly, scYearly,
		scWhenIdle, scWhenOverload, scLag, scWindowsStartUp, scUserLogOn, scUserLogOff, scUserLock, scUserUnlock, scProgramStartUp, scProgramFinish, scBeforeHibernation, scAfterHibernation);
type
	TSchedule = class(TObject)
  private
		FEnabled: BG;
    procedure SetEnabled(const Value: Boolean);

	public // protected
		ScheduleType: TScheduleType;
		StartDT: TDateTime; // scOnce, scInterval, scDaily ; Weekly from date ; Monthly from date ; yearly from date
		EndDT: TDateTime; // Expiration
		Duration: U4;
		// scIdle, scOverload for Time
		EveryXDay: U4; // scDaily (Every x day)
		EveryXWeek: U4; // scWeekly (Every x day of week)
		EveryXMonth: U4; // scMonthly (Every x week of month)
		EveryXYear: U4; // scYearly (Every month of x year)
		EveryXIdle: U4; // scIdle
		EveryXOverload: U4; // scOverload
		WeekDays: array[0..DaysInWeek - 1] of BG; // scWeekly
		Months: array[0..MonthsInYear - 1] of BG; // scMonthly

		// Calculated
		NextRun: TDateTime;
	public
		constructor Create;
		destructor Destroy; override;

		procedure UpdateNextRun;
		function NextRunToStr: string;
    function IsActive: BG;
		function ToString: string;
    procedure Clone(const Source: TSchedule);

    procedure RWIni(const IniFile: TDIniFile; const Section: string; const Save: BG);

    property Enabled: Boolean read FEnabled write SetEnabled;
	end;

implementation

uses
	uInputFormat, uOutputFormat, uStrings, uMath, uDictionary, uFiles, uFile,
	Windows, DateUtils, Math;

{ TSchedule }

constructor TSchedule.Create;
var
  i: SG;
begin
	inherited;

	ScheduleType := scNever;

	StartDT := Now;
	EndDT := 0;
	Duration := Hour;
	EveryXDay := 1;
	EveryXWeek := 1;
	EveryXMonth := 1;
	EveryXYear := 1;
	EveryXIdle := 10 * Minute;
	EveryXOverload := Minute;
	for i := 0 to DaysInWeek - 1 do
		WeekDays[i] := False;
	for i := 0 to MonthsInYear - 1 do
		Months[i] := False;
end;

destructor TSchedule.Destroy;
begin
	inherited;
end;

procedure TSchedule.UpdateNextRun;
var
	StartTime: TSystemTime;
	SystemTime: TSystemTime;
	FNow, FNow2: TDateTime;
	Offset: FA;
	Ofs: SG;
	Week, Month: UG;
	i: SG;
begin
	if FEnabled = False then
	begin
		NextRun := 0;
	end;
	GetLocalTime(SystemTime);
	FNow := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay) +
		EncodeTime(SystemTime.wHour, SystemTime.wMinute, SystemTime.wSecond, SystemTime.wMilliseconds);

	DecodeDate(StartDT, StartTime.wYear, StartTime.wMonth, StartTime.wDay);
	DecodeTime(StartDT, StartTime.wHour, StartTime.wMinute, StartTime.wSecond, StartTime.wMilliseconds);

	NextRun := 0;
	if (EndDT = 0) or (EndDT < FNow) then
	case ScheduleType of
	scOnce:
	begin
		if StartDT + Duration / MSecsPerDay > FNow then
			NextRun := StartDT;
	end;
	scInterval:
	begin
		if StartDT + Duration / MSecsPerDay > FNow then
			NextRun := StartDT
		else
			NextRun := FNow + Duration / MSecsPerDay;
	end;
	scDaily:
	begin
		if StartDT + Duration / MSecsPerDay > FNow then
		begin
			NextRun := StartDT;
		end
		else
		begin
			if EveryXDay <= 1 then
			begin
{					if Frac(M.StartDT) < Frac(FNow) then w := 1 else w := 0;
				M.NextRun := DateOf(FNow) + TimeOf(M.StartDT) + w}
				NextRun := FNow + 1 - Frac(FNow - StartDT);
			end
			else
			begin
				Offset := FNow - StartDT;
				Offset := EveryXDay - ModE(Offset, EveryXDay);
				NextRun := FNow + Offset;
			end;
		end;
	end;
	scWeekly:
	begin
		Week := WeekOf(StartDT);

		FNow2 := DateOf(Max(FNow, StartDT));
		for i := 0 to DaysInWeek - 1 do
		begin
			if WeekDays[DayOfWeek(FNow2) - 1] then
			begin
				Break;
			end;
			// Add one day.
			FNow2 := FNow2 + 1;
		end;

		if EveryXWeek > 1 then
		begin
			Ofs := UnsignedMod(Week - WeekOf(FNow2), EveryXWeek);
			FNow2 := FNow2 + Ofs * DaysInWeek;
		end;

		NextRun := FNow2 + TimeOf(StartDT);
	end;
	scMonthly:
	begin
		FNow2 := DateOf(Max(EncodeDate(SystemTime.wYear, SystemTime.wMonth, StartTime.wDay), StartDT));
		if FNow2 < FNow then
		begin
			// Add one month.
			FNow2 := FNow2 + DaysInMonth(FNow2);
		end;

		Month := YearOf(StartDT);
		for i := 0 to MonthsInYear - 1 do
		begin
			if Months[MonthOf(FNow2) - 1] then
			begin
				Break;
			end;
			// Add one month.
			FNow2 := FNow2 + DaysInMonth(FNow2);
		end;

		if EveryXMonth > 1 then
		begin
			Ofs := UnsignedMod(Month - YearOf(FNow2), EveryXMonth);
			for i := 1 to Ofs do
			begin
				// Add one year.
				FNow2 := FNow2 + DaysInYear(FNow2);
			end;
		end;
//		FNow2 := StartDT;
{		if EveryXMonth > 1 then
		begin}
(*			while FNow2 + Duration / MSecsPerDay < FNow do
			begin
				for i := 0 to EveryXMonth - 1 do
				begin
					// Add one month.
					FNow2 := FNow2 + DaysInMonth(FNow2);
				end;
			end;
//		end;

		// Setup correct (enabled) month of the year.
		for i := 0 to MonthsInYear - 1 do
		begin
			if Months[MonthOf(FNow2) - 1] then
			begin
				Break;
			end;
			// Add one month.
			FNow2 := FNow2 + DaysInMonth(FNow2);
		end; *)

		NextRun := FNow2 + TimeOf(StartDT);
	end;
	scYearly:
	begin
		if EveryXYear <= 1 then
		begin
			StartTime.wYear := SystemTime.wYear;
			NextRun := EncodeDate(StartTime.wYear, StartTime.wMonth, StartTime.wDay) +
				EncodeTime(StartTime.wHour, StartTime.wMinute, StartTime.wSecond, StartTime.wMilliseconds);

			if NextRun + Duration / MSecsPerDay < FNow then
			begin
				StartTime.wYear := SystemTime.wYear + 1;
				NextRun := EncodeDate(StartTime.wYear, StartTime.wMonth, StartTime.wDay) +
					EncodeTime(StartTime.wHour, StartTime.wMinute, StartTime.wSecond, StartTime.wMilliseconds);
			end;
		end
		else
		begin
			NextRun := StartDT;

			while NextRun + Duration / MSecsPerDay < FNow do
			begin
				StartTime.wYear := StartTime.wYear + EveryXYear;
				NextRun := EncodeDate(StartTime.wYear, StartTime.wMonth, StartTime.wDay) +
					EncodeTime(StartTime.wHour, StartTime.wMinute, StartTime.wSecond, StartTime.wMilliseconds);
			end;
		end;
	end;
	end;
end;

function TSchedule.ToString: string;
var
	i: SG;
	Count: UG;
	NextOne: BG;
begin
  if FEnabled = False then
  begin
    Result := '';
    Exit;
  end;

	case ScheduleType of
	scOnce:
	begin
		Result := 'At ' + TimeToS(StartDT, 0, ofDisplay) + ' on ' + DateToS(StartDT, ofDisplay);
	end;
	scInterval:
	begin
		Result := 'At ' + DateTimeToS(StartDT, 0, ofDisplay) + ' every ' + MsToStr(Duration, diDHMSD, 0, False);
	end;
	scDaily:
	begin
		Result := 'At ' + TimeToS(StartDT, 0, ofDisplay) + ' every ' + NToS(EveryXDay) + ' day' + Plural(EveryXDay) + ', starting ' + DateToS(StartDT, ofDisplay);
	end;
	scWeekly:
	begin
		Count := 0;
		for i := 0 to DaysInWeek - 1 do
			if WeekDays[i] then Inc(Count);
		Result := 'At ' + TimeToS(StartDT, 0, ofDisplay) + ' ';
		NextOne := False;
		if Count = 0 then
			Result := Result + 'no day'
		else if Count = DaysInWeek then
			Result := Result + 'every day'
		else
		begin
			Result := Result + 'every ';
			for i := 0 to DaysInWeek - 1 do
				if WeekDays[i] then
				begin
					if NextOne then
						Result := Result + ', '
					else
						NextOne := True;
					Result := Result + {$if CompilerVersion >= 23}FormatSettings.{$ifend}ShortDayNames[i + 1];
				end;
		end;
		Result := Result + ' of every ' + NToS(EveryXWeek) + ' week' + Plural(EveryXWeek);
		Result := Result + ', starting ' + DateToS(StartDT, ofDisplay);
	end;
	scMonthly:
	begin
		Result := 'At ' + TimeToS(StartDT, 0, ofDisplay) + ' on day ' + NToS(DayOf(StartDT)) + ' of ';
		Count := 0;
		for i := 0 to MonthsInYear - 1 do
			if Months[i] then Inc(Count);
		if Count = 0 then
			Result := Result + 'no year'
		else if Count = MonthsInYear then
			Result := Result + 'every year'
		else
		begin
			Result := Result + 'every ';
			NextOne := False;
			for i := 0 to MonthsInYear - 1 do
				if Months[i] then
				begin
					if NextOne then
						Result := Result + ', '
					else
						NextOne := True;
					Result := Result + {$if CompilerVersion >= 23}FormatSettings.{$ifend}ShortMonthNames[i + 1];
				end;
		end;
		Result := Result + ' of every ' + NToS(EveryXMonth) + ' year' + Plural(EveryXWeek);
		Result := Result + ', starting ' + DateToS(StartDT, ofDisplay);
	end;
	scYearly:
	begin
		Result := 'At ' + TimeToS(StartDT, 0, ofDisplay) + ' every ' + NToS(EveryXYear) + ' year' + Plural(EveryXYear);
		Result := Result + ', starting ' + DateToS(StartDT, ofDisplay);
	end;
	scWhenIdle:
	begin
		Result := 'Run when idle for ' + MsToStr(EveryXIdle, diDHMSD, 0, False);
	end;
	scWhenOverload:
	begin
		Result := 'Run when overload for ' + MsToStr(EveryXOverload, diDHMSD, 0, False);
	end;
	scLag:
	begin
		Result := 'Lag longer that ' + MsToStr(EveryXOverload, diDHMSD, 0, False);
	end;
	scWindowsStartup:
		Result :='Run at Windows startup';
	scUserLogOn:
		Result :='Run at User Log On';
	scUserLogOff:
		Result :='Run at User Log Off';
	scUserLock:
		Result :='Run at User Lock';
	scUserUnlock:
		Result :='Run at User Unlock';
	scProgramStartup:
		Result :='Run at Program startup';
	scBeforeHibernation:
		Result := 'Run before hibernation';
	scAfterHibernation:
		Result := 'Run after hibernation restored';
	end;
	if EndDT <> 0 then Result := Result + ' and ending ' + DateToS(EndDT, ofDisplay);
	Result := Translate(Result);
end;

function TSchedule.NextRunToStr: string;
begin
	if NextRun = 0 then
	begin
		case ScheduleType of
		scWhenIdle, scWhenOverload, scWindowsStartup, scUserLogOn, scUserLogOff, scUserLock, scUserUnlock, scProgramStartup, scBeforeHibernation, scAfterHibernation:
			Result := 'Unknown'
		else
			Result := 'Never';
		end;
	end
	else
		Result := DateTimeToS(NextRun, 0, ofDisplay);
end;

procedure TSchedule.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    FEnabled := Value;
    UpdateNextRun;
  end;
end;

procedure TSchedule.RWIni(const IniFile: TDIniFile; const Section: string;
  const Save: BG);
var i: SG;
begin
  IniFile.RWEnum(Section, TypeInfo(TScheduleType), U1(ScheduleType), Save);
  if Save = False then
    FEnabled := True;
  IniFile.RWBool(Section, 'Enabled', FEnabled, Save);
  IniFile.RWDateTime(Section, 'StartDT', StartDT, Save);
  IniFile.RWDateTime(Section, 'EndDT', EndDT, Save);
  IniFile.RWNum(Section, 'Duration', Duration, Save);

  IniFile.RWNum(Section, 'EveryXDay', EveryXDay, Save);
  IniFile.RWNum(Section, 'EveryXWeek', EveryXWeek, Save);
  IniFile.RWNum(Section, 'EveryXMonth', EveryXMonth, Save);
  IniFile.RWNum(Section, 'EveryXYear', EveryXYear, Save);
  IniFile.RWNum(Section, 'EveryXIdle', EveryXIdle, Save);
  IniFile.RWNum(Section, 'EveryXOverload', EveryXOverload, Save);

  for i := 0 to DaysInWeek - 1 do
    IniFile.RWBool(Section, 'Day' + IntToStr(i), WeekDays[i], Save);
  for i := 0 to MonthsInYear - 1 do
    IniFile.RWBool(Section, 'Month' + IntToStr(i), Months[i], Save);
  if Save = False then
    UpdateNextRun;
end;

function TSchedule.IsActive: BG;
begin
  Result := FEnabled and (Now >= NextRun) and (NextRun > 0);
end;

procedure TSchedule.Clone(const Source: TSchedule);
begin
	ScheduleType := Source.ScheduleType;
	StartDT := Source.StartDT;
	EndDT := Source.EndDT;
	Duration := Source.Duration;
	EveryXDay := Source.EveryXDay;
	EveryXWeek := Source.EveryXWeek;
	EveryXMonth := Source.EveryXMonth;
	EveryXYear := Source.EveryXYear;
	EveryXIdle := Source.EveryXIdle;
	EveryXOverload := Source.EveryXOverload;
	WeekDays := Source.WeekDays;
	Months := Source.Months;
	FEnabled := Source.Enabled;
  UpdateNextRun;
end;

end.
