// Build: 07/1998-09/1999 Author: Safranek David

unit uGetInt;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls, Spin, uDButton, ExtCtrls, uDLabel;

type
	TOnApplyInt = procedure(Value: Integer);

	TfGetInt = class(TForm)
		EditInput: TEdit;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		TrackBar: TTrackBar;
		ButtonMin: TDButton;
		ButtonCur: TDButton;
		ButtonMax: TDButton;
		SpinButton1: TSpinButton;
		LabelMin: TDLabel;
		LabelMax: TDLabel;
		LabelNow: TDLabel;
		ImageBackground: TImage;
		ButtonDef: TDButton;
		DLabelError: TDLabel;
		ButtonApply: TDButton;
		procedure EditInputChange(Sender: TObject);
		procedure ButtonMinClick(Sender: TObject);
		procedure ButtonCurClick(Sender: TObject);
		procedure ButtonMaxClick(Sender: TObject);
		procedure TrackBarChange(Sender: TObject);
		procedure SpinButton1DownClick(Sender: TObject);
		procedure SpinButton1UpClick(Sender: TObject);
		procedure ButtonDefClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
	private
		{ private declarations }
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: Integer;
		OnApply: TOnApplyInt;
		procedure ChangeInt;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ Public declarations }
	end;

function GetInt(const prompt: string;
	var CurVal: Integer; const MinVal, DefVal, MaxVal: Integer; OnApplyInt: TOnApplyInt): Boolean;

implementation

{$R *.DFM}
uses uTexture, uAdd;
var
	fGetInt: TfGetInt;

function GetInt(const prompt: string;
	var CurVal: Integer; const MinVal, DefVal, MaxVal: Integer; OnApplyInt: TOnApplyInt): Boolean;
begin
	if not Assigned(fGetInt) then
	begin
		fGetInt := TfGetInt.Create(Application.MainForm);
		FormImage(fGetInt.ImageBackground);
	end;
	fGetInt.ButtonApply.Enabled := Assigned(OnApplyInt);
	fGetInt.OnApply := OnApplyInt;
	CorrectFormPos(fGetInt);
	fGetInt.TMinVal := MinVal;
	fGetInt.TCurVal := CurVal;
	fGetInt.TDefVal := DefVal;
	fGetInt.TMaxVal := MaxVal;
	if fGetInt.TMaxVal < fGetInt.TMinVal then fGetInt.TMaxVal := fGetInt.TMinVal;
	if fGetInt.TCurVal < fGetInt.TMinVal then
		fGetInt.TCurVal := fGetInt.TMinVal
	else if fGetInt.TCurVal > fGetInt.TMaxVal then
		fGetInt.TCurVal := fGetInt.TMaxVal;
	fGetInt.NowVal := fGetInt.TCurVal;
	fGetInt.Caption := prompt;
	fGetInt.LabelMin.Caption := IntToStr(fGetInt.TMinVal);
	fGetInt.LabelMax.Caption := IntToStr(fGetInt.TMaxVal);
	fGetInt.LabelNow.Caption := IntToStr(fGetInt.NowVal);

	fGetInt.TrackBar.OnChange := nil;
	fGetInt.TrackBar.Frequency := (fGetInt.TMaxVal - fGetInt.TMinVal + 19) div 20;
	fGetInt.TrackBar.PageSize := (fGetInt.TMaxVal - fGetInt.TMinVal + 19) div 20;
	if fGetInt.TMaxVal < fGetInt.TrackBar.Min then
	begin
		fGetInt.TrackBar.Min := fGetInt.TMinVal;
		fGetInt.TrackBar.Max := fGetInt.TMaxVal;
	end
	else
	begin
		fGetInt.TrackBar.Max := fGetInt.TMaxVal;
		fGetInt.TrackBar.Min := fGetInt.TMinVal;
	end;
	fGetInt.TrackBar.SelStart := fGetInt.TCurVal;
	fGetInt.TrackBar.SelEnd := fGetInt.TCurVal;
	fGetInt.TrackBar.OnChange := fGetInt.TrackBarChange;

{ if MaxVal-MinVal > 112 then
		TrackBar.TickStyle := tsNone
	else
		TrackBar.TickStyle := tsAuto;}
	fGetInt.InitTrackBar;
	fGetInt.InitButtons;
	fGetInt.InitEdit;
	if fGetInt.ActiveControl <> fGetInt.EditInput then fGetInt.ActiveControl := fGetInt.EditInput;
	if Assigned(fGetInt.OnApply) then
	begin
		fGetInt.FormStyle := fsStayOnTop;
		fGetInt.Show;
		Result := True;
	end
	else
	begin
		fGetInt.FormStyle := fsNormal;
		if fGetInt.ShowModal = mrOK then
		begin
			CurVal := fGetInt.NowVal;
			Result := True;
		end
		else
		begin
			Result := False;
		end;
	end;
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

procedure TfGetInt.ChangeInt;
begin
	if Assigned(OnApply) then OnApply(NowVal);
end;

procedure TfGetInt.EditInputChange(Sender: TObject);
var ErrorMsg: string;
begin
	EditInput.OnChange := nil;
	NowVal := StrToValI(EditInput.Text, TMinVal, NowVal, TMaxVal, 1, ErrorMsg);
	if ErrorMsg <> '' then
	begin
		NowVal := TDefVal;
		ChangeInt;
	end;
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
	ChangeInt;
end;

procedure TfGetInt.ButtonMinClick(Sender: TObject);
begin
	NowVal := TMinVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.ButtonCurClick(Sender: TObject);
begin
	NowVal := TCurVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.ButtonDefClick(Sender: TObject);
begin
	NowVal := TDefVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.ButtonMaxClick(Sender: TObject);
begin
	NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.SpinButton1DownClick(Sender: TObject);
begin
	if NowVal > TMinVal then Dec(NowVal) else Exit;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.SpinButton1UpClick(Sender: TObject);
begin
	if NowVal < TMaxVal then Inc(NowVal) else Exit;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		if NowVal <> TCurVal then OnApply(TCurVal);
		Close;
	end;
end;

procedure TfGetInt.ButtonOkClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
//    OnApply(NowVal);
		Close;
	end;
end;

end.
