// Build: 07/1998-09/1999 Author: Safranek David

unit uGetInt;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls, Spin, uDBitBtn, ExtCtrls, uDLabel;

type
	TfGetInt = class(TForm)
		EditInput: TEdit;
		ButtonOk: TDBitBtn;
		ButtonCancel: TDBitBtn;
		TrackBar: TTrackBar;
		ButtonMin: TDBitBtn;
		ButtonCur: TDBitBtn;
		ButtonMax: TDBitBtn;
		SpinButton1: TSpinButton;
		LabelMin: TDLabel;
		LabelMax: TDLabel;
		LabelNow: TDLabel;
		ImageBackground: TImage;
		ButtonDef: TDBitBtn;
		DLabelError: TDLabel;
		procedure EditInputChange(Sender: TObject);
		procedure ButtonMinClick(Sender: TObject);
		procedure ButtonCurClick(Sender: TObject);
		procedure ButtonMaxClick(Sender: TObject);
		procedure TrackBarChange(Sender: TObject);
		procedure SpinButton1DownClick(Sender: TObject);
		procedure SpinButton1UpClick(Sender: TObject);
		procedure ButtonDefClick(Sender: TObject);
	private
		{ private declarations }
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: Integer;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ public declarations }
		function Execute(const prompt: string;
			var CurVal: Integer; const DefVal, MinVal, MaxVal: Integer): Boolean;
	end;

function GetInt(const prompt: string;
	var CurVal: Integer; const DefVal, MinVal, MaxVal: Integer): Boolean;

implementation

{$R *.DFM}
uses uTexture, uAdd;
var
	fGetInt: TfGetInt;

function GetInt(const prompt: string;
	var CurVal: Integer; const DefVal, MinVal, MaxVal: Integer): Boolean;
begin
	if not Assigned(fGetInt) then
	begin
		fGetInt := TfGetInt.Create(Application.MainForm);
		FormImage(fGetInt.ImageBackground);
	end;
	CorrectPos(fGetInt);
	Result := fGetInt.Execute(prompt, CurVal, DefVal, MinVal, MaxVal);
end;

procedure TfGetInt.InitButtons;
begin
	ButtonMin.Enabled := NowVal <> TMinVal;
	ButtonCur.Enabled := NowVal <> TCurVal;
	ButtonDef.Enabled := NowVal <> TDefVal;
	ButtonMax.Enabled := NowVal <> TMaxVal;
	LabelNow.Caption := IntToStr(NowVal);
end;

procedure TfGetInt.InitEdit;
begin
	EditInput.Text := IntToStr(NowVal);
	EditInput.SelectAll;
end;

procedure TfGetInt.InitTrackBar;
begin
	TrackBar.OnChange := nil;
	TrackBar.Position := NowVal;
	TrackBar.OnChange := TrackBarChange;
end;

function TfGetInt.Execute;
begin
	TMinVal := MinVal;
	TCurVal := CurVal;
	TDefVal := DefVal;
	TMaxVal := MaxVal;
	if TMaxVal < TMinVal then TMaxVal := TMinVal;
	if TCurVal < TMinVal then
		TCurVal := TMinVal
	else if TCurVal > TMaxVal then
		TCurVal := TMaxVal;
	NowVal := TCurVal;
	Caption := prompt;
	LabelMin.Caption := IntToStr(TMinVal);
	LabelMax.Caption := IntToStr(TMaxVal);
	LabelNow.Caption := IntToStr(NowVal);

	TrackBar.OnChange := nil;
	TrackBar.Frequency := (TMaxVal - TMinVal + 19) div 20;
	TrackBar.PageSize := (TMaxVal - TMinVal + 19) div 20;
	if TMaxVal < TrackBar.Min then
	begin
		TrackBar.Min := TMinVal;
		TrackBar.Max := TMaxVal;
	end
	else
	begin
		TrackBar.Max := TMaxVal;
		TrackBar.Min := TMinVal;
	end;
	TrackBar.SelStart := TCurVal;
	TrackBar.SelEnd := TCurVal;
	TrackBar.OnChange := TrackBarChange;

{ if MaxVal-MinVal > 112 then
		TrackBar.TickStyle := tsNone
	else
		TrackBar.TickStyle := tsAuto;}
	InitTrackBar;
	InitButtons;
	InitEdit;
	if ActiveControl <> EditInput then ActiveControl := EditInput;
	if ShowModal = mrOK then
	begin
		CurVal := NowVal;
		Result := True;
	end
	else
	begin
		Result := False;
	end;
end;

procedure TfGetInt.EditInputChange(Sender: TObject);
var ErrorMsg: string;
begin
	EditInput.OnChange := nil;
	NowVal := StrToValI(EditInput.Text, TMinVal, NowVal, TMaxVal, 1, ErrorMsg);
	if ErrorMsg <> '' then NowVal := TDefVal;
	DLabelError.Caption := ErrorMsg;
	InitButtons;
	InitTrackBar;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetInt.TrackBarChange(Sender: TObject);
begin
	NowVal := TrackBar.Position;
	InitButtons;
	InitEdit;
end;

procedure TfGetInt.ButtonMinClick(Sender: TObject);
begin
	NowVal := TMinVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetInt.ButtonCurClick(Sender: TObject);
begin
	NowVal := TCurVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetInt.ButtonDefClick(Sender: TObject);
begin
	NowVal := TDefVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetInt.ButtonMaxClick(Sender: TObject);
begin
	NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetInt.SpinButton1DownClick(Sender: TObject);
begin
	if NowVal > TMinVal then Dec(NowVal) else Exit;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetInt.SpinButton1UpClick(Sender: TObject);
begin
	if NowVal < TMaxVal then Inc(NowVal) else Exit;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

end.
