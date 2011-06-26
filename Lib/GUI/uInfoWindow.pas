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
	uScreen, uMath, uStrings, uSimulation,
	Math;

procedure ShowInfoWindow(const Text: string);
begin
	if not Assigned(fInfoWindow) then
		fInfoWindow := TfInfoWindow.Create(nil);

	fInfoWindow.Height := fInfoWindow.StartHeight;
	GetGTime;
	fInfoWindow.StartTickCount := GTime;
	fInfoWindow.LabelText.Caption := ReplaceF(Text, LineSep, LineSep + LineSep);
//	fInfoWindow.SendToBack;
	fInfoWindow.Visible := True;
end;

procedure TfInfoWindow.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	StartHeight := Height;
	Caption := Application.Title;
end;

procedure TfInfoWindow.FormShow(Sender: TObject);
var R: TRect;
begin
	R := Screen.MonitorFromWindow(Handle).WorkareaRect;
	fInfoWindow.SetBounds(
		R.Right - fInfoWindow.Width,
		R.Bottom - fInfoWindow.Height,
		fInfoWindow.Width,
		fInfoWindow.Height);

	Timer1Timer(Sender);
	Timer1.Enabled := True;
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
		R := Screen.MonitorFromWindow(Handle).WorkareaRect;
		H := RoundDivS8(Min(1000, TimeDifference(GTime, StartTickCount)) * StartHeight, 1000);
		Self.SetBounds(
			Left,
			R.Bottom - H,
			Width,
			H);
	end
	else if TimeDifference(GTime, StartTickCount) > 4 * Second then
	begin
		R := Screen.MonitorFromWindow(Handle).WorkareaRect;
		H := StartHeight - RoundDivS8((S8(TimeDifference(GTime, StartTickCount)) - 4 * Second) * StartHeight, 1000);
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
