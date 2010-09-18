// Build: 05/1999-07/1999 Author: Safranek David

unit uGetTime;

interface

uses
	Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
	ExtCtrls, ComCtrls, Spin, uDButton, uDLabel;

type
	TfGetTime = class(TForm)
		TrackBarH: TTrackBar;
		TrackBarM: TTrackBar;
		TrackBarS: TTrackBar;
		Label1: TDLabel;
		Label2: TDLabel;
		Label3: TDLabel;
		TrackBarD: TTrackBar;
		Label4: TDLabel;
		LabelH: TDLabel;
		LabelM: TDLabel;
		LabelS: TDLabel;
		LabelD: TDLabel;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		EditInput: TEdit;
		ButtonMin: TDButton;
		ButtonCur: TDButton;
		ButtonMax: TDButton;
		SpinButtonH: TSpinButton;
		SpinButtonS: TSpinButton;
		SpinButtonM: TSpinButton;
		SpinButtonD: TSpinButton;
		ImageBackground: TImage;
		ButtonDef: TDButton;
		procedure EditInputChange(Sender: TObject);
		procedure TrackBarHMSDChange(Sender: TObject);
		procedure ButtonMinClick(Sender: TObject);
		procedure ButtonCurClick(Sender: TObject);
		procedure ButtonMaxClick(Sender: TObject);
		procedure SpinButtonDownClick(Sender: TObject);
		procedure SpinButtonUpClick(Sender: TObject);
		procedure ButtonDefClick(Sender: TObject);
	private
		{ private declarations }
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: LongWord;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ public declarations }
		function Execute(const prompt: string;
			var CurVal: LongWord; const DefVal, MinVal, MaxVal: LongWord): Boolean;
	end;

function GetTime(const prompt: string;
	var CurVal: LongWord; const DefVal, MinVal, MaxVal: LongWord): Boolean;


implementation

{$R *.DFM}
uses uAdd, uTexture, uStrings;
var
	fGetTime: TfGetTime;

function GetTime(const prompt: string;
	var CurVal: LongWord; const DefVal, MinVal, MaxVal: LongWord): Boolean;
begin
	if not Assigned(fGetTime) then
	begin
		fGetTime := TfGetTime.Create(Application.MainForm);
		FormImage(fGetTime.ImageBackground);
	end;
	CorrectFormPos(fGetTime);
	Result := fGetTime.Execute(prompt, CurVal, DefVal, MinVal, MaxVal);
end;

function StrToShortTime(const Str: string): LongInt;
var
	V: LongInt;
	Mul: LongInt;
	W: Byte;
	F: Byte;
	DP: Byte;
begin
	V := 0;
	if Length(Str) > 0 then
	begin
		F := 0;
		for W := Length(Str) - 1 downto 1 do
		begin
			if Str[W] = '.' then
			begin
				F := W;
				Break;
			end;
		end;
		Mul := 1000 div 10;
		if F > 0 then
		for W := F + 1 to Length(Str) do
		begin
			case Str[W] of
			'0'..'9':
			begin
				V := V + Mul * (Ord(Str[W]) - 48);
				Mul := Mul div 10;
			end;
			end;
		end;
		Mul := 1000;
		DP := 0;
		if F = 0 then F := Length(Str) + 1;
		for W := F - 1 downto 1 do
		begin
			case Str[W] of
			'0'..'9':
			begin
				V := V + Mul * (Ord(Str[W]) - 48);
				if V > 100000000 then
				begin
					Result := V;
					Exit;
				end;
				if Mul < 100000000 then Mul := Mul * 10;
			end;
			':':
			begin
				case DP of
				0: Mul := 60 * 1000;
				1: Mul := 60 * 60 * 1000;
				2: Mul := 60 * 60 * 1000;
				end;
				Inc(DP);
			end;
			end;
		end;
	end;
	Result := V;
end;

function ShortTimeToStr(const T: LongInt): string;
begin
	Result := DelCharsF(msToStr(T, diMSD, -3), ' ');
end;

procedure TfGetTime.InitButtons;
var
	H, M, S, D: LongWord;
begin
	ButtonMin.Enabled := NowVal <> TMinVal;
	ButtonCur.Enabled := NowVal <> TCurVal;
	ButtonDef.Enabled := NowVal <> TDefVal;
	ButtonMax.Enabled := NowVal <> TMaxVal;
	msToHMSD(NowVal, H, M, S, D);
	LabelH.Caption := IntToStr(H);
	LabelM.Caption := IntToStr(M);
	LabelS.Caption := IntToStr(S);
	LabelD.Caption := IntToStr(D);
end;

procedure TfGetTime.InitEdit;
begin
	EditInput.OnChange := nil;
	EditInput.Text := ShortTimeToStr(NowVal);
	EditInput.SelectAll;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetTime.InitTrackBar;
var
	H, M, S, D: LongWord;
begin
	msToHMSD(NowVal, H, M, S, D);
	TrackBarH.OnChange := nil;
	TrackBarM.OnChange := nil;
	TrackBarS.OnChange := nil;
	TrackBarD.OnChange := nil;
	TrackBarH.Position := H;
	TrackBarM.Position := M;
	TrackBarS.Position := S;
	TrackBarD.Position := D;
	TrackBarH.OnChange := TrackBarHMSDChange;
	TrackBarM.OnChange := TrackBarHMSDChange;
	TrackBarS.OnChange := TrackBarHMSDChange;
	TrackBarD.OnChange := TrackBarHMSDChange;
end;

function TfGetTime.Execute;
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
// H
	TrackBarH.OnChange := nil;
	TrackBarH.Min := TMinVal div (60 * 60 * 1000);
	TrackBarH.Max := TMaxVal div (60 * 60 * 1000);
	if TrackBarH.Max - TrackBarH.Min > 112 then
		TrackBarH.TickStyle := tsNone
	else
		TrackBarH.TickStyle := tsAuto;
	TrackBarH.OnChange := TrackBarHMSDChange;
// M
	TrackBarM.OnChange := nil;
	if TMaxVal < 60 * 60 * 1000 then
	begin
		TrackBarM.Min := TMinVal div (60 * 1000);
		TrackBarM.Max := TMaxVal div (60 * 1000);
	end
	else
	begin
		TrackBarM.Min := 0;
		TrackBarM.Max := 59;
	end;
	TrackBarM.OnChange := TrackBarHMSDChange;
// S
	TrackBarS.OnChange := nil;
	if TMaxVal < 60 * 1000 then
	begin
		TrackBarS.Min := TMinVal div (1000);
		TrackBarS.Max := TMaxVal div (1000);
	end
	else
	begin
		TrackBarS.Min := 0;
		TrackBarS.Max := 59;
	end;
	TrackBarS.OnChange := TrackBarHMSDChange;
// D
	TrackBarD.OnChange := nil;
	if TMaxVal < 1000 then
	begin
		TrackBarD.Min := TMinVal;
		TrackBarD.Max := TMaxVal;
	end
	else
	begin
		TrackBarD.Min := 0;
		TrackBarD.Max := 1000;
	end;
	TrackBarD.OnChange := TrackBarHMSDChange;

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

procedure TfGetTime.EditInputChange(Sender: TObject);
begin
	NowVal := StrToShortTime(EditInput.Text);
	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;
	InitButtons;
	InitTrackBar;
end;

procedure TfGetTime.TrackBarHMSDChange(Sender: TObject);
begin
	NowVal :=
		TrackBarH.Position * 60 * 60 * 1000 +
		TrackBarM.Position * 60 * 1000 +
		TrackBarS.Position * 1000 +
		TrackBarD.Position;
	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.ButtonMinClick(Sender: TObject);
begin
	NowVal := TMinVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.ButtonCurClick(Sender: TObject);
begin
	NowVal := TCurVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.ButtonDefClick(Sender: TObject);
begin
	NowVal := TDefVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.ButtonMaxClick(Sender: TObject);
begin
	NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.SpinButtonDownClick(Sender: TObject);
var L: LongWord;
begin
	L := TSpinButton(Sender).Tag;
	if NowVal >= L then Dec(NowVal, L) else NowVal := 0;
	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

procedure TfGetTime.SpinButtonUpClick(Sender: TObject);
begin
	Inc(NowVal, TSpinButton(Sender).Tag);
	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
end;

end.
