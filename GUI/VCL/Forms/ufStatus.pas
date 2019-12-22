unit ufStatus;

interface

uses
	uTypes, uDForm, uThreadPool,
	Menus,
	SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, uDButton, uDWinControl, uDImage, uDGauge;

type
	TfStatus = class(TDForm)
		DGauge: TDGauge;
		ButtonStop: TButton;
		ButtonPause: TButton;
		EditElapsedTime: TLabeledEdit;
		EditRemainTime: TLabeledEdit;
		ButtonResume: TButton;
		EditTotalTime: TLabeledEdit;
		procedure FormCreate(Sender: TObject);
		procedure ButtonPauseClick(Sender: TObject);
		procedure ButtonStopClick(Sender: TObject);
		procedure ButtonResumeClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormShow(Sender: TObject);
	private
		{ Private declarations }
		FThreadPool: TThreadPool;
		FMenuItem: TMenuItem;
		StartTime: U8; // TODO Move to Project
		ElapsedTime, RemainTime: U8;
		PauseTime: U8;
		procedure Init;
	public
		{ Public declarations }
	end;

var
	Cancel: BG;

procedure ShowStatusWindow(const ThreadPool: TThreadPool; const MenuItem: TMenuItem; const Caption: string);
procedure UpdateStatus(const Actual: SG);
procedure UpdateMaximum(const Value: SG);
procedure HideStatusWindow;

implementation

{$R *.dfm}

uses
	uOutputFormat, uMath, uStrings, uDictionary, uMainTimer;

var
	fStatus: TfStatus;

procedure ShowStatusWindow(const ThreadPool: TThreadPool; const MenuItem: TMenuItem; const Caption: string);
begin
	Cancel := False;
	if not Assigned(fStatus) then
	begin
		fStatus := TfStatus.Create(nil);
		fStatus.FThreadPool := ThreadPool;
		fStatus.FMenuItem := MenuItem;
    fStatus.Caption := Caption;
	end;
	fStatus.Show;
end;

procedure HideStatusWindow;
begin
	if Assigned(fStatus) then
	begin
		fStatus.Close;
	end;
end;

procedure UpdateStatus(const Actual: SG);
begin
	if Assigned(fStatus) then
	begin
		if Actual = 0 then
		begin
			fStatus.StartTime := MainTimer.Value.Ticks;
			fStatus.ElapsedTime := 0;
			fStatus.RemainTime := 0;
			fStatus.ButtonResume.Enabled := False;
			fStatus.ButtonPause.Enabled := fStatus.FThreadPool <> nil;
		end
		else
		begin
			fStatus.ElapsedTime := MainTimer.IntervalFrom(fStatus.StartTime);
			if (fStatus.DGauge.Max > 0) then
				fStatus.RemainTime := RoundDivS8((fStatus.DGauge.Max - Actual) * S8(fStatus.ElapsedTime), Actual);
		end;
		fStatus.DGauge.Position := Actual;
		fStatus.Init;
	end;
end;

procedure UpdateMaximum(const Value: SG);
begin
	if Assigned(fStatus) then
	begin
		fStatus.DGauge.Position := 0;
		fStatus.DGauge.Max := Value;
		fStatus.Init;
	end;
end;

{ TfStatus }

procedure TfStatus.FormCreate(Sender: TObject);
begin
	Background := baStandard;
end;

procedure TfStatus.ButtonPauseClick(Sender: TObject);
begin
	if Assigned(FThreadPool) then
	begin
		FThreadPool.Pause;
	end;
	PauseTime := MainTimer.Value.Ticks;
	ButtonPause.Enabled := False;
	ButtonResume.Enabled := True;
end;

procedure TfStatus.ButtonStopClick(Sender: TObject);
begin
	if Assigned(FThreadPool) then
	begin
		FThreadPool.ClearTasks;
	end
	else
		Cancel := True;
	Close;
end;

procedure TfStatus.Init;
begin
	EditElapsedTime.Text := MsToStr(ElapsedTime, TDisplay.diMSD, 0);
	if (DGauge.Max > 0) and (DGauge.Position > 0) then
	begin
		EditRemainTime.Text := MsToStr(RemainTime, TDisplay.diMSD, 0);
		EditTotalTime.Text := MsToStr(ElapsedTime + RemainTime, TDisplay.diSD, 0);
	end
	else
	begin
		EditRemainTime.Text := NAStr;
		EditTotalTime.Text := NAStr;
	end;
	EditElapsedTime.Refresh;
	EditRemainTime.Refresh;
	EditTotalTime.Refresh;
end;

procedure TfStatus.ButtonResumeClick(Sender: TObject);
begin
	if Assigned(FThreadPool) then
	begin
		FThreadPool.Resume;
	end;
	Inc(StartTime, MainTimer.IntervalFrom(PauseTime));
	ButtonPause.Enabled := True;
	ButtonResume.Enabled := False;
end;

procedure TfStatus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if Assigned(FMenuItem) then
		FMenuItem.Checked := False;
end;

procedure TfStatus.FormShow(Sender: TObject);
begin
	if Assigned(FMenuItem) then
		FMenuItem.Checked := True;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
	FreeAndNil(fStatus);
{$ENDIF NoFinalization}
end.
