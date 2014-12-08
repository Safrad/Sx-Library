unit ufSchedule;

interface

uses
	uTypes, uDForm, uSchedule,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	StdCtrls, ExtCtrls, uDButton, Grids, Calendar, ComCtrls,
	uDLabel, uDEdit, Dialogs, uSxCheckBox;

type
	TfSchedule = class(TDForm)
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		LabelScheduleTask: TLabel;
		ComboBoxSchedule: TComboBox;
		Calendar: TCalendar;
		TimePicker: TDateTimePicker;
		ComboBoxM: TComboBox;
		ComboBoxY: TComboBox;
		Bevel1: TBevel;
		BevelDT: TBevel;
		EditNextRun: TDEdit;
		EditWofY: TDEdit;
		EditPrename: TDEdit;
		LabelNextRun: TLabel;
		DatePicker: TDateTimePicker;
		LabelEveryText: TLabel;
		LabelEvery: TLabel;
		ComboBoxEvery: TComboBox;
		EditSchedule: TDEdit;
		LabelStartTime: TLabel;
		EditDofY: TDEdit;
		LabelWofY: TLabel;
		LabelDofY: TLabel;
		LabelDuration: TLabel;
		TimePickerDuration: TDateTimePicker;
		ComboBoxDuration: TComboBox;
		LabelDurationDays: TLabel;
		ButtonNow: TDButton;
    CheckBoxEnabled: TSxCheckBox;
    Bevel2: TBevel;
    Bevel3: TBevel;
		procedure FormCreate(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure ComboBoxScheduleChange(Sender: TObject);
		procedure TimePickerChange(Sender: TObject);
		procedure CalendarChange(Sender: TObject);
		procedure ComboBoxYChange(Sender: TObject);
		procedure ComboBoxMChange(Sender: TObject);
		procedure ComboBoxEveryChange(Sender: TObject);
		procedure DatePickerChange(Sender: TObject);
		procedure TimePickerDurationChange(Sender: TObject);
		procedure ButtonNowClick(Sender: TObject);
		procedure FormDestroy(Sender: TObject);
    procedure CheckBoxEnabledClick(Sender: TObject);
	private
		{ Private declarations }
    // Temporary store form data (Copy to Original Schedule if button OK is pressed)
		FormSchedule: TSchedule;

    OriginalSchedule: TSchedule;

		DButtonMonth: array[0..11] of TDButton;
		DButtonWeek: array[0..6] of TDButton;
		procedure ButtonClick(Sender: TObject);
		procedure InitNextRun;
		procedure Init;
		procedure InitDate;
		procedure SetCalendarDate(const NewDate: TDate);
	public
		{ Public declarations }
		ActItem: SG;
	end;

function SetSchedule(const Schedule: TSchedule): BG;

implementation

{$R *.dfm}
uses
	uMath, uGetTime, uInputFormat, uOutputFormat, uStrings, uFiles, uWave, uDictionary,
	DateUtils, TypInfo;

function SetSchedule(const Schedule: TSchedule): BG;
var
	fSchedule: TfSchedule;
begin
  fSchedule := TfSchedule.Create(nil);
  try
  	fSchedule.OriginalSchedule := Schedule;
  	Result := fSchedule.ShowModal = mrOk;
  finally
  	fSchedule.Free;
  end;
end;

{ TfSchedule }

procedure TfSchedule.ButtonClick(Sender: TObject);
var i: SG;
begin
	for i := 0 to 6 do
		FormSchedule.WeekDays[i] := DButtonWeek[i].Down;
	for i := 0 to 11 do
		FormSchedule.Months[i] := DButtonMonth[i].Down;
	InitNextRun;
end;

procedure TfSchedule.FormCreate(Sender: TObject);
var
	i: SG;
	x, y: SG;
begin
	Background := baGradient;

	ComboBoxSchedule.Items.BeginUpdate;
	try
		for i := 0 to SG(High(TScheduleType)) do
		begin
			ComboBoxSchedule.Items.Add(AddSpace(Copy(GetEnumName(TypeInfo(TScheduleType), i), 3, MaxInt)));
		end;
	finally
		ComboBoxSchedule.Items.EndUpdate;
	end;

	ComboBoxM.Items.BeginUpdate;
	try
		for i := 1 to 12 do
			ComboBoxM.Items.Add({$if CompilerVersion >= 23}FormatSettings.{$ifend}LongMonthNames[i] + ' (' + NToS(i) + ')');
	finally
		ComboBoxM.Items.EndUpdate;
	end;

	y := CurrentYear;
	ComboBoxY.Items.BeginUpdate;
	try
		ComboBoxY.Items.Add(NToS(y - 1));
		ComboBoxY.Items.Add(NToS(y));
		ComboBoxY.Items.Add(NToS(y + 1));
	finally
		ComboBoxY.Items.EndUpdate;
	end;

	ComboBoxDuration.Items.BeginUpdate;
	try
		for i := 0 to 7 do
			ComboBoxDuration.Items.Add(NToS(i));
	finally
		ComboBoxDuration.Items.EndUpdate;
	end;

	for i := 0 to 11 do
	begin
		DButtonMonth[i] := TDButton.Create(Self);
		DButtonMonth[i].Name := 'ButtonMonth' + IntToStr(i);
		DButtonMonth[i].Caption := {$if CompilerVersion >= 23}FormatSettings.{$ifend}LongMonthNames[i + 1];
		x := BevelDT.Left + BevelDT.Width + 12;
		y := 19;
		DButtonMonth[i].SetBounds(x, LabelEveryText.Top + y * i, ClientWidth - x - 16, y);
		DButtonMonth[i].AutoChange := True;
		DButtonMonth[i].OnClick := ButtonClick;
		InsertControl(DButtonMonth[i]);
	end;
	for i := 0 to 6 do
	begin
		DButtonWeek[i] := TDButton.Create(Self);
		DButtonWeek[i].Name := 'ButtonWeek' + IntToStr(i);
		DButtonWeek[i].Caption := {$if CompilerVersion >= 23}FormatSettings.{$ifend}LongDayNames[i + 1];
		x := BevelDT.Left + BevelDT.Width + 12;
		y := 19;
		DButtonWeek[i].SetBounds(x, LabelEveryText.Top +  (12 * y div 7) * i, ClientWidth - x - 16, y);
		DButtonWeek[i].AutoChange := True;
		DButtonWeek[i].OnClick := ButtonClick;
		InsertControl(DButtonWeek[i]);
	end;

end;

procedure TfSchedule.FormDestroy(Sender: TObject);
begin
	FreeAndNil(FormSchedule);
end;

procedure TfSchedule.ButtonCancelClick(Sender: TObject);
begin
	//
end;

procedure TfSchedule.ButtonOkClick(Sender: TObject);
begin
  OriginalSchedule.Clone(FormSchedule);
end;

procedure TfSchedule.FormShow(Sender: TObject);
var
	i: SG;
begin
	FormSchedule.Free;
	FormSchedule := TSchedule.Create;
  FormSchedule.Clone(OriginalSchedule);
	CheckBoxEnabled.Checked := FormSchedule.Enabled;

	for i := 0 to 6 do
		DButtonWeek[i].Down := FormSchedule.WeekDays[i];
	for i := 0 to 11 do
		DButtonMonth[i].Down := FormSchedule.Months[i];

	ComboBoxSchedule.ItemIndex := SG(FormSchedule.ScheduleType);
	TimePicker.DateTime := 1 + TimeOf(FormSchedule.StartDT);
	DatePicker.DateTime := 0.5 + DateOf(FormSchedule.StartDT);
	TimePickerDuration.DateTime := 1 + TimeOf(FormSchedule.Duration / MSecsPerDay);
	ComboBoxDuration.Text := NToS(FormSchedule.Duration div MSecsPerDay);
	DatePicker.OnChange(Sender);

	Init;
end;

procedure TfSchedule.InitNextRun;
begin
	EditSchedule.Text := FormSchedule.ToString;
	FormSchedule.UpdateNextRun;
	EditNextRun.Text := FormSchedule.NextRunToStr;
end;

procedure TfSchedule.Init;
var
	E0, E1, E2, E3: BG;
	i: SG;
begin
	E0 := True;
	E1 := False;
	E2 := False;
	E3 := True;
	case FormSchedule.ScheduleType of
	scOnce:
	begin
	end;
	scInterval:
	begin
	end;
	scDaily:
	begin
	end;
	scWeekly:
	begin
		E1 := True;
	end;
	scMonthly:
	begin
		E2 := True;
	end;
	scYearly:
	begin
	end
	else
	begin
		E0 := False;
		E3 := False;
	end;
	end;
	ComboBoxY.Visible := E0;
	ComboBoxM.Visible := E0;
	Calendar.Visible := E0;
	DatePicker.Visible := E0;
	EditWofY.Visible := E0;
	EditDofY.Visible := E0;
	LabelWofY.Visible := E0;
	LabelDofY.Visible := E0;
	EditPrename.Visible := E0;
	BevelDT.Visible := E0;

{	TimePickerDuration.Visible := E0;
	LabelDuration.Visible := E0;
	ComboBoxDuration.Visible := E0;
	LabelDurationDays.Visible := E0;}

	for i := 0 to 6 do
		DButtonWeek[i].Visible := E1;
	for i := 0 to 11 do
		DButtonMonth[i].Visible := E2;

	LabelStartTime.Visible := E3;
	TimePicker.Visible := E3;
	ButtonNow.Visible := E3;

	E0 := FormSchedule.ScheduleType in [scInterval, scDaily, scWeekly, scMonthly, scYearly, scWhenIdle, scWhenOverload, scLag];
	LabelEvery.Visible := E0;
	ComboBoxEvery.Visible := E0;
	LabelEveryText.Visible := E0;

	if E0 then
	begin
		ComboBoxEvery.Items.BeginUpdate;
		try
			ComboBoxEvery.Items.Clear;
			case FormSchedule.ScheduleType of
			scInterval:
			begin

			end;
			scDaily:
			begin
				LabelEveryText.Caption := 'day(s)';
				for i := 1 to 31 do
					ComboBoxEvery.Items.Add(NToS(i));
				ComboBoxEvery.Text := NToS(FormSchedule.EveryXDay);
			end;
			scWeekly:
			begin
				LabelEveryText.Caption := 'week(s) on this day(s):';
				for i := 1 to 7 do
					ComboBoxEvery.Items.Add(NToS(i));
				ComboBoxEvery.Text := NToS(FormSchedule.EveryXWeek);
			end;
			scMonthly:
			begin
				LabelEveryText.Caption := 'year(s) in this month(s):';
				for i := 1 to 12 do
					ComboBoxEvery.Items.Add(NToS(i));
				ComboBoxEvery.Text := NToS(FormSchedule.EveryXMonth);
			end;
			scYearly:
			begin
				LabelEveryText.Caption := 'year(s)';
				for i := 1 to 10 do
					ComboBoxEvery.Items.Add(NToS(i));
				ComboBoxEvery.Text := NToS(FormSchedule.EveryXYear);
			end;
			scWhenIdle:
			begin
				LabelEveryText.Caption := ' is idle';
				ComboBoxEvery.Items.Add(MsToStr(Second, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Minute, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Hour, diMSD, 0, False));
				ComboBoxEvery.Text := MsToStr(FormSchedule.EveryXIdle, diMSD, -3, False);
			end;
			scWhenOverload:
			begin
				LabelEveryText.Caption := ' is overload';
				ComboBoxEvery.Items.Add(MsToStr(Second, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Minute, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Hour,diMSD, 0, False));
				ComboBoxEvery.Text := MsToStr(FormSchedule.EveryXOverload, diMSD, -3, False);
			end;
			scLag:
			begin
				LabelEveryText.Caption := ' long lag';
				ComboBoxEvery.Items.Add(MsToStr(Second, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Minute, diMSD, 0, False));
				ComboBoxEvery.Items.Add(MsToStr(Hour, diMSD, 0, False));
				ComboBoxEvery.Text := MsToStr(FormSchedule.EveryXOverload, diMSD, -3, False);
			end;
			end;
		finally
			ComboBoxEvery.Items.EndUpdate;
		end;
	end;
end;

procedure TfSchedule.ComboBoxScheduleChange(Sender: TObject);
begin
	FormSchedule.ScheduleType := TScheduleType(ComboBoxSchedule.ItemIndex);
	Init;
	InitNextRun;
end;

procedure TfSchedule.TimePickerChange(Sender: TObject);
begin
	FormSchedule.StartDT := DateOf(FormSchedule.StartDT) + TimeOf(TimePicker.DateTime);
	InitNextRun;
end;

procedure TfSchedule.InitDate;
var Y, M, D, DOW: U2;
begin
	DecodeDateFully(DatePicker.Date, Y, M, D, DOW);
	EditWofY.Text := NToS(WeekOfTheYear(DatePicker.Date));
	EditDofY.Text := NToS(DayOfTheYear(DatePicker.Date));
end;

procedure TfSchedule.CalendarChange(Sender: TObject);
begin
	DatePicker.Date := EncodeDate(Calendar.Year, Calendar.Month, Calendar.Day);
	FormSchedule.StartDT := DateOf(DatePicker.Date) + TimeOf(FormSchedule.StartDT);
	InitNextRun;
	InitDate;
end;

procedure TfSchedule.CheckBoxEnabledClick(Sender: TObject);
begin
  FormSchedule.Enabled := CheckBoxEnabled.Checked;
	InitNextRun;
end;

procedure TfSchedule.ComboBoxYChange(Sender: TObject);
begin
	Calendar.Year := StrToValI(ComboBoxY.Text, True, 0, 2003, SG(9999), 1);
	DatePicker.Date := EncodeDate(Calendar.Year, Calendar.Month, Calendar.Day);
	FormSchedule.StartDT := DateOf(DatePicker.Date) + TimeOf(FormSchedule.StartDT);
	InitNextRun;
	InitDate;
end;

procedure TfSchedule.ComboBoxMChange(Sender: TObject);
begin
	Calendar.Month := ComboBoxM.ItemIndex + 1;
	DatePicker.Date := EncodeDate(Calendar.Year, Calendar.Month, Calendar.Day);
	FormSchedule.StartDT := DateOf(DatePicker.Date) + TimeOf(FormSchedule.StartDT);
	InitNextRun;
	InitDate;
end;

procedure TfSchedule.ComboBoxEveryChange(Sender: TObject);
begin
	try
		case FormSchedule.ScheduleType of
		scDaily: FormSchedule.EveryXDay := StrToValI(ComboBoxEvery.Text, True, 1, 1, High(FormSchedule.EveryXDay), 1);
		scWeekly: FormSchedule.EveryXWeek := StrToValI(ComboBoxEvery.Text, True, 1, 1, High(FormSchedule.EveryXWeek), 1);
		scMonthly: FormSchedule.EveryXMonth := StrToValI(ComboBoxEvery.Text, True, 1, 1, UG(31), 1);
		scYearly: FormSchedule.EveryXYear := StrToValI(ComboBoxEvery.Text, True, 1, 1, High(FormSchedule.EveryXYear), 1);
		scWhenIdle: FormSchedule.EveryXIdle := StrToMs(ComboBoxEvery.Text, 0, Minute, MaxInt, True);
		scWhenOverload: FormSchedule.EveryXOverload := StrToMs(ComboBoxEvery.Text, 0, Minute, MaxInt, True);
		end;
	finally
		InitNextRun;
	end;
end;

procedure TfSchedule.SetCalendarDate(const NewDate: TDate);
var Y, M, D, DOW: U2;
begin
	DecodeDateFully(NewDate, Y, M, D, DOW);

	Calendar.OnChange := nil;
	Calendar.CalendarDate := NewDate;
	Calendar.OnChange := CalendarChange;

	ComboBoxY.OnChange := nil;
	ComboBoxY.Text := NToS(Y);
	ComboBoxY.OnChange := ComboBoxYChange;

	ComboBoxM.OnChange := nil;
	ComboBoxM.ItemIndex := M - 1;
	ComboBoxM.OnChange := ComboBoxMChange;

	InitNextRun;
	InitDate;
end;

procedure TfSchedule.DatePickerChange(Sender: TObject);
begin
	FormSchedule.StartDT := DateOf(DatePicker.Date) + TimeOf(FormSchedule.StartDT);
	SetCalendarDate(DatePicker.Date);
end;

procedure TfSchedule.TimePickerDurationChange(Sender: TObject);
begin
	FormSchedule.Duration :=
		RoundN(MSecsPerDay * StrToValE(ComboBoxDuration.Text, True, 0, 0, High(FormSchedule.Duration) div MSecsPerDay)) +
		RoundN(MSecsPerDay * TimeOf(TimePickerDuration.DateTime));
	InitNextRun;
end;

procedure TfSchedule.ButtonNowClick(Sender: TObject);
begin
	FormSchedule.StartDT := Now;
	TimePicker.DateTime := 1 + TimeOf(FormSchedule.StartDT);
	DatePicker.DateTime := FormSchedule.StartDT;
	SetCalendarDate(DatePicker.Date);
end;

end.
