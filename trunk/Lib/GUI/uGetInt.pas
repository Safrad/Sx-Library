//* File:     Lib\GUI\uGetInt.pas
//* Created:  1998-07-01
//* Modified: 2007-05-20
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uGetInt;

interface

uses
	uTypes, uParserMsg,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
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
		LabelMin: TLabel;
		LabelMax: TLabel;
		LabelNow: TLabel;
		ButtonDef: TDButton;
		ButtonApply: TDButton;
		EditError: TMemo;
		UpDown: TUpDown;
		Bevel1: TBevel;
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
		Messages: TParserMessages;
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: S8;
		OnApply: TOnApplyInt;
		procedure ChangeInt;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

function GetNumber(Prompt: string;
	var CurVal: S8; const MinVal, DefVal, MaxVal: S8; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: U4; const MinVal, DefVal, MaxVal: U4; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: S4; const MinVal, DefVal, MaxVal: S4; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: U2; const MinVal, DefVal, MaxVal: U2; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: S2; const MinVal, DefVal, MaxVal: S2; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: U1; const MinVal, DefVal, MaxVal: U1; OnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var CurVal: S1; const MinVal, DefVal, MaxVal: S1; OnApplyInt: TOnApplyInt): Boolean; overload;

implementation

{$R *.DFM}
uses
	Math,
	uStrings, uInputFormat, uDParser;

var
	fGetInt: TfGetInt;

function GetNumber(Prompt: string;
	var CurVal: S8; const MinVal, DefVal, MaxVal: S8; OnApplyInt: TOnApplyInt): Boolean;
begin
	Assert(not ((MinVal > MaxVal) or (DefVal < MinVal) or (DefVal > MaxVal)
		or (CurVal < MinVal) or (CurVal > MaxVal)));

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
	fGetInt.TrackBar.Frequency := (UG(Min(High(SG) div 2, fGetInt.TMaxVal - fGetInt.TMinVal)) + 19) div 20;
	fGetInt.TrackBar.PageSize := fGetInt.TrackBar.Frequency;
	if fGetInt.TMaxVal < fGetInt.TrackBar.Min then
	begin
		fGetInt.TrackBar.Min := fGetInt.TMinVal;
		fGetInt.TrackBar.Max := fGetInt.TMaxVal;
	end
	else
	begin
		fGetInt.TrackBar.Max := Min(High(SG), fGetInt.TMaxVal);
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
	fGetInt.EditInputChange(nil);
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
	LabelNow.Update;
end;

procedure TfGetInt.InitEdit;
begin
	EditInput.OnChange := nil;
	EditInput.Text := IntToStr(NowVal);
	EditInput.SelectAll;
	EditInput.Update;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetInt.InitTrackBar;
begin
	TrackBar.OnChange := nil;
	TrackBar.Position := Min(High(SG), NowVal);
	TrackBar.Update;
	TrackBar.OnChange := TrackBarChange;
end;

procedure TfGetInt.EditInputChange(Sender: TObject);
begin
	EditInput.OnChange := nil;
	NowVal := StrToValS8(EditInput.Text, True, TMinVal, NowVal, TMaxVal, 1, Messages);
	Messages.ToStrings(EditError.Lines);
	Messages.Clear;

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

function GetNumber(Prompt: string;
	var CurVal: U4; const MinVal, DefVal, MaxVal: U4; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetNumber(Prompt: string;
	var CurVal: S4; const MinVal, DefVal, MaxVal: S4; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetNumber(Prompt: string;
	var CurVal: S2; const MinVal, DefVal, MaxVal: S2; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetNumber(Prompt: string;
	var CurVal: U2; const MinVal, DefVal, MaxVal: U2; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetNumber(Prompt: string;
	var CurVal: S1; const MinVal, DefVal, MaxVal: S1; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

function GetNumber(Prompt: string;
	var CurVal: U1; const MinVal, DefVal, MaxVal: U1; OnApplyInt: TOnApplyInt): Boolean;
var C: S8;
begin
	C := CurVal;
	Result := GetNumber(Prompt, C, MinVal, DefVal, MaxVal, OnApplyInt);
	CurVal := C;
end;

constructor TfGetInt.Create(AOwner: TComponent);
begin
	inherited;
	Messages := TParserMessages.Create;
end;

destructor TfGetInt.Destroy;
begin
	FreeAndNil(Messages);
	inherited;
end;

end.
