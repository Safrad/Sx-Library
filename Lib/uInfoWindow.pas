//* File:     Lib\uInfoWindow.pas
//* Created:  2006-12-21
//* Modified: 2006-12-21
//* Version:  1.0.35.0
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uInfoWindow;

interface

uses
	uDForm, uTypes,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, StdCtrls, ExtCtrls, uDLabel, uDButton;

type
	TfInfoWindow = class(TDForm)
		Timer1: TTimer;
    LabelText: TLabel;
		Bevel1: TBevel;
		procedure FormCreate(Sender: TObject);
		procedure FormShow(Sender: TObject);
		procedure LabelTextMouseDown(Sender: TObject; Button: TMouseButton;
			Shift: TShiftState; X, Y: Integer);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure Timer1Timer(Sender: TObject);
	private
		{ Private declarations }
		StartTickCount: U4;
		StartHeight: SG;
	public
		{ Public declarations }
	end;

var
	fInfoWindow: TfInfoWindow;

procedure ShowInfoWindow(const Text: string);

implementation

{$R *.dfm}
uses
	uScreen, uMath, uStrings,
	Math;

procedure ShowInfoWindow(const Text: string);
begin
	if not Assigned(fInfoWindow) then
		fInfoWindow := TfInfoWindow.Create(nil);

	fInfoWindow.Height := fInfoWindow.StartHeight;
	fInfoWindow.StartTickCount := GetTickCount;
	fInfoWindow.LabelText.Caption := ReplaceF(Text, LineSep, LineSep + LineSep);
	fInfoWindow.SendToBack;
	fInfoWindow.Visible := True;
end;

procedure TfInfoWindow.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	StartHeight := Height;
end;

procedure TfInfoWindow.FormShow(Sender: TObject);
var R: TRect;
begin
	GetDesktopRect(R);
	fInfoWindow.Left := R.Right - fInfoWindow.Width;
	fInfoWindow.Top := R.Bottom - fInfoWindow.Height;
	Timer1.Enabled := True;
	Timer1Timer(Sender);
end;

procedure TfInfoWindow.LabelTextMouseDown(Sender: TObject;
	Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	Close;
end;

procedure TfInfoWindow.FormCloseQuery(Sender: TObject;
	var CanClose: Boolean);
begin
	Timer1.Enabled := False;
end;

procedure TfInfoWindow.Timer1Timer(Sender: TObject);
var
	R: TRect;
	H: SG;
begin
	GetGTime;
	if TimeDifference(GTime, StartTickCount) <= 2 * Second then
	begin
		GetDesktopRect(R);
		H := RoundDivS8(Min(1000, TimeDifference(GTime, StartTickCount)) * StartHeight, 1000);
		Self.SetBounds(
			Left,
			R.Bottom - H,
			Width,
			H);
	end
	else if TimeDifference(GTime, StartTickCount) > 4 * Second then
	begin
		GetDesktopRect(R);
		H := StartHeight - RoundDivS8(TimeDifference(GTime, StartTickCount) - 4 * Second) * StartHeight, 1000);
		if H <= 0 then
			Close
		else
			Self.SetBounds(
				Left,
				R.Bottom - H,
				Width,
				H);
	end;
end;

end.
