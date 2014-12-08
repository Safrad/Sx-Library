unit ufTextStatus;

interface

uses
	uTypes, uDForm,
	Menus,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, uDButton, uDWinControl, uDImage, uDGauge;

type
	TfTextStatus = class(TDForm)
		DGauge: TDGauge;
		EditElapsedTime: TLabeledEdit;
		EditRemainTime: TLabeledEdit;
		EditTotalTime: TLabeledEdit;
		ButtonStop: TDButton;
		ButtonPause: TDButton;
		ButtonResume: TDButton;
		edtAction: TLabeledEdit;
		Timer1: TTimer;
		procedure FormCreate(Sender: TObject);
		procedure ButtonPauseClick(Sender: TObject);
		procedure ButtonStopClick(Sender: TObject);
		procedure ButtonResumeClick(Sender: TObject);
		procedure Timer1Timer(Sender: TObject);
	private
		{ Private declarations }
		StartTime: U8; // TODO Move to Project
		ElapsedTime, RemainTime: U8;
		PauseTime: U8;
		procedure Init;
	public
		{ Public declarations }
	end;

var
	Cancel: BG;

procedure ShowStatusWindow(const Text: string);
procedure UpdateStatus(const Actual: SG; const Text: string);
procedure UpdateMaximum(const Value: SG);
procedure HideStatusWindow;

implementation

{$R *.dfm}

uses
	uSimulation, uOutputFormat, uMath, uStrings, uDictionary;

var
	fTextStatus: TfTextStatus;

procedure ShowStatusWindow(const Text: string);
begin
	Cancel := False;
	if not Assigned(fTextStatus) then
	begin
		fTextStatus := TfTextStatus.Create(nil);
		fTextStatus.edtAction.Text := Text;
	end;
	fTextStatus.Show;
end;

procedure HideStatusWindow;
begin
	if Assigned(fTextStatus) then
	begin
		fTextStatus.Close;
	end;
end;

procedure UpdateStatus(const Actual: SG; const Text: string);
begin
	if Assigned(fTextStatus) then
	begin
		if Actual = 0 then
		begin
			fTextStatus.StartTime := GetTickCount;
			fTextStatus.ElapsedTime := 0;
			fTextStatus.RemainTime := 0;
			fTextStatus.ButtonResume.Enabled := False;
			fTextStatus.ButtonPause.Enabled := False;
		end
		else
		begin
			fTextStatus.ElapsedTime := TimeDifference(GetTickCount, fTextStatus.StartTime);
			if (fTextStatus.DGauge.Max > 0) then
				fTextStatus.RemainTime := RoundDivS8((fTextStatus.DGauge.Max - Actual) * fTextStatus.ElapsedTime, Actual);
		end;
		fTextStatus.DGauge.Position := Actual;
		fTextStatus.Init;
	end;
end;

procedure UpdateMaximum(const Value: SG);
begin
	if Assigned(fTextStatus) then
	begin
		fTextStatus.DGauge.Position := 0;
		fTextStatus.DGauge.Max := Value;
		fTextStatus.Init;
	end;
end;

{ TfTextStatus }

procedure TfTextStatus.FormCreate(Sender: TObject);
begin
	Background := baStandard;
end;

procedure TfTextStatus.ButtonPauseClick(Sender: TObject);
begin
	PauseTime := GetTickCount;
	ButtonPause.Enabled := False;
	ButtonResume.Enabled := True;
end;

procedure TfTextStatus.ButtonStopClick(Sender: TObject);
begin
	ButtonStop.Enabled := False;
	Cancel := True;
end;

procedure TfTextStatus.Init;
begin
	EditElapsedTime.Text := MsToStr(ElapsedTime, diSD);
	if (DGauge.Max > 0) and (DGauge.Position > 0) then
	begin
		EditRemainTime.Text := MsToStr(RemainTime, diSD);
		EditTotalTime.Text := MsToStr(ElapsedTime + RemainTime, diSD);
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

procedure TfTextStatus.ButtonResumeClick(Sender: TObject);
begin
	Inc(StartTime, TimeDifference(GetTickCount, PauseTime));
	ButtonPause.Enabled := True;
	ButtonResume.Enabled := False;
end;

procedure TfTextStatus.Timer1Timer(Sender: TObject);
begin
	UpdateStatus(DGauge.Position, '');
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}
finalization
{$IFNDEF NoFinalization}
	FreeAndNil(fTextStatus);
{$ENDIF NoFinalization}
end.
