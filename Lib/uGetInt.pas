//* File:     Lib\uGetInt.pas
//* Created:  1998-07-01
//* Modified: 2004-09-26
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uGetInt;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ComCtrls, uDButton, ExtCtrls, uDLabel, uDForm;

type
	TOnApplyInt = procedure(Value: S8);

	TfGetInt = class(TDForm)
		EditInput: TLabeledEdit;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		TrackBar: TTrackBar;
		ButtonMin: TDButton;
		ButtonCur: TDButton;
		ButtonMax: TDButton;
		LabelMin: TDLabel;
		LabelMax: TDLabel;
		LabelNow: TDLabel;
		ButtonDef: TDButton;
		ButtonApply: TDButton;
    EditError: TMemo;
    UpDown: TUpDown;
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
		procedure FormCreate(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
	private
		{ Private declarations }
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: Integer;
		OnApply: TOnApplyInt;
		procedure ChangeInt;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ Public declarations }
	end;

function GetU4(Prompt: string;
	var CurVal: U4; const MinVal, DefVal, MaxVal: U4; OnApplyInt: TOnApplyInt): Boolean;
function GetInt(Prompt: string;
	var CurVal: S4; const MinVal, DefVal, MaxVal: S4; OnApplyInt: TOnApplyInt): Boolean;
function GetS8(Prompt: string;
	var CurVal: S8; const MinVal, DefVal, MaxVal: S8; OnApplyInt: TOnApplyInt): Boolean;

implementation

{$R *.DFM}
uses uStrings, uInput, uError, uParser;

var
	fGetInt: TfGetInt;

function GetU4(Prompt: string;
	var CurVal: U4; const MinVal, DefVal, MaxVal: U4; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetS8(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetInt(Prompt: string;
	var CurVal: S4; const MinVal, DefVal, MaxVal: S4; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetS8(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetS8(Prompt: string;
	var CurVal: S8; const MinVal, DefVal, MaxVal: S8; OnApplyInt: TOnApplyInt): Boolean;
begin
	{$ifopt d+}
	if (MinVal > MaxVal) or (DefVal < MinVal) or (DefVal > MaxVal)
		or (CurVal < MinVal) or (CurVal > MaxVal) then IE(0);
	{$endif}
	if not Assigned(fGetInt) then
	begin
		fGetInt := TfGetInt.Create(Application.MainForm);
	end;
	fGetInt.ButtonApply.Enabled := Assigned(OnApplyInt);
	fGetInt.OnApply := OnApplyInt;
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
	fGetInt.Caption := DelCharsF(Prompt, '&');
	fGetInt.LabelMin.Caption := IntToStr(fGetInt.TMinVal);
	fGetInt.LabelMax.Caption := IntToStr(fGetInt.TMaxVal);
	fGetInt.LabelNow.Caption := IntToStr(fGetInt.NowVal);

	fGetInt.TrackBar.OnChange := nil;
	fGetInt.TrackBar.Frequency := (UG(fGetInt.TMaxVal - fGetInt.TMinVal) + 19) div 20;
	fGetInt.TrackBar.PageSize := fGetInt.TrackBar.Frequency;
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
	LabelNow.Repaint;
end;

procedure TfGetInt.InitEdit;
begin
	EditInput.OnChange := nil;
	EditInput.Text := IntToStr(NowVal);
	EditInput.SelectAll;
	EditInput.Repaint;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetInt.InitTrackBar;
begin
	TrackBar.OnChange := nil;
	TrackBar.Position := NowVal;
	TrackBar.OnChange := TrackBarChange;
	TrackBar.Repaint;
end;

procedure TfGetInt.EditInputChange(Sender: TObject);
begin
	EditInput.OnChange := nil;
	NowVal := StrToValI(EditInput.Text, True, TMinVal, NowVal, TMaxVal, 1);
	MesToMemo(EditError);
	ClearErrors;

	InitButtons;
	InitTrackBar;
	ChangeInt;
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

procedure TfGetInt.ChangeInt;
begin
	if Assigned(OnApply) then OnApply(NowVal);
end;

procedure TfGetInt.ButtonOkClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		Close;
	end;
end;

procedure TfGetInt.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		if NowVal <> TCurVal then OnApply(TCurVal);
		Close;
	end;
end;

procedure TfGetInt.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

procedure TfGetInt.UpDownChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: Smallint;
	Direction: TUpDownDirection);
begin
	AllowChange := True; //(NewValue >= TMinVal) and (NewValue <= TMaxVal);
	//	if NowVal > TMinVal then Dec(NowVal) else Exit;

	if AllowChange then
	begin
		if Direction = updUp then
		begin
			if NowVal < TMaxVal then Inc(NowVal) else Exit;
		end
		else
		begin
			if NowVal > TMinVal then Dec(NowVal) else Exit;
		end;
		InitTrackBar;
		InitEdit;
		InitButtons;
		ChangeInt;
	end;
end;

end.
