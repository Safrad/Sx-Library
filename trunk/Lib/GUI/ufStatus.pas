// * File:     Lib\GUI\ufStatus.pas
// * Created:  2009-12-29
// * Modified: 2009-12-29
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit ufStatus;

interface

uses
	uTypes, uDForm, uThreadPool,
	Menus,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, uDButton, uDWinControl, uDImage, uDGauge;

type
	TfStatus = class(TDForm)
		DGauge: TDGauge;
		ButtonStop: TDButton;
		ButtonPause: TDButton;
		EditElapsedTime: TLabeledEdit;
		EditRemainTime: TLabeledEdit;
		ButtonResume: TDButton;
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

procedure ShowStatusWindow(const ThreadPool: TThreadPool; const MenuItem: TMenuItem);
procedure UpdateStatus(const Actual: SG);
procedure UpdateMaximum(const Value: SG);
procedure HideStatusWindow;

implementation

{$R *.dfm}

uses
	uSimulation, uOutputFormat, uMath, uStrings, uDictionary;

var
	fStatus: TfStatus;

procedure ShowStatusWindow(const ThreadPool: TThreadPool; const MenuItem: TMenuItem);
begin
	if not Assigned(fStatus) then
	begin
		fStatus := TfStatus.Create(nil);
		fStatus.FThreadPool := ThreadPool;
		fStatus.FMenuItem := MenuItem;
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
			fStatus.StartTime := GetTickCount;
			fStatus.ElapsedTime := 0;
			fStatus.RemainTime := 0;
			fStatus.ButtonResume.Enabled := False;
			fStatus.ButtonPause.Enabled := True;
		end
		else
		begin
			fStatus.ElapsedTime := TimeDifference(GetTickCount, fStatus.StartTime);
			if (fStatus.DGauge.Max > 0) then
				fStatus.RemainTime := RoundDivS8((fStatus.DGauge.Max - Actual) * fStatus.ElapsedTime, Actual);
		end;
		fStatus.DGauge.Position := Actual;
		fStatus.Init;
	end;
end;

procedure UpdateMaximum(const Value: SG);
begin
	if Assigned(fStatus) then
	begin
		fStatus.DGauge.Max := Value;
		fStatus.Init;
	end;
end;

{ TfStatus }

procedure TfStatus.FormCreate(Sender: TObject);
begin
	Background := baStandard;
	Dictionary.TranslateForm(Self);
end;

procedure TfStatus.ButtonPauseClick(Sender: TObject);
begin
	FThreadPool.Pause;
	PauseTime := GetTickCount;
	ButtonPause.Enabled := False;
	ButtonResume.Enabled := True;
end;

procedure TfStatus.ButtonStopClick(Sender: TObject);
begin
	FThreadPool.Clear;
	FThreadPool.Resume;
	Close;
end;

procedure TfStatus.Init;
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

procedure TfStatus.ButtonResumeClick(Sender: TObject);
begin
	FThreadPool.Resume;
	Inc(StartTime, TimeDifference(GetTickCount, PauseTime));
	ButtonPause.Enabled := True;
	ButtonResume.Enabled := False;
end;

procedure TfStatus.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	FMenuItem.Checked := False;
end;

procedure TfStatus.FormShow(Sender: TObject);
begin
	FMenuItem.Checked := True;
end;

initialization
finalization
	FreeAndNil(fStatus);
end.
