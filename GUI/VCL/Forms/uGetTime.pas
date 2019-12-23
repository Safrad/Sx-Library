unit uGetTime;

interface

uses
  SysUtils,
  Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
	Vcl.ExtCtrls,
  Vcl.ComCtrls,

	uTypes,
  uParserMsg,
  uTimeSpan,
  uDButton, uDLabel, uDForm, uDEdit, uDMemo;

type
	TOnApplyTime = procedure(Value: TTimeSpan);

	TfGetTime = class(TDForm)
		TrackBarH: TTrackBar;
		TrackBarM: TTrackBar;
		TrackBarS: TTrackBar;
		Label1: TLabel;
		Label2: TLabel;
		Label3: TLabel;
		TrackBarD: TTrackBar;
		Label4: TLabel;
		LabelH: TLabel;
		LabelM: TLabel;
		LabelS: TLabel;
		LabelD: TLabel;
		ButtonOk: TDButton;
		ButtonCancel: TDButton;
		EditInput: TDEdit;
		ButtonMin: TDButton;
		ButtonCur: TDButton;
		ButtonMax: TDButton;
		SpinButtonH: TUpDown;
		SpinButtonS: TUpDown;
		SpinButtonM: TUpDown;
		SpinButtonD: TUpDown;
		ButtonDef: TDButton;
		ButtonApply: TDButton;
		EditError: TDMemo;
		procedure EditInputChange(Sender: TObject);
		procedure TrackBarHMSDChange(Sender: TObject);
		procedure ButtonMinClick(Sender: TObject);
		procedure ButtonCurClick(Sender: TObject);
		procedure ButtonMaxClick(Sender: TObject);
		procedure ButtonDefClick(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure ButtonOkClick(Sender: TObject);
		procedure ButtonCancelClick(Sender: TObject);
		procedure SpinButtonHMSDChangingEx(Sender: TObject;
			var AllowChange: Boolean; NewValue: Integer;
			Direction: TUpDownDirection);
	private
		{ Private declarations }
		Messages: TParserMessages;
		TMinVal, TCurVal, TDefVal, TMaxVal, NowVal: TTimeSpan;
		OnApply: TOnApplyTime;
		procedure ChangeTime;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

function GetTime(const Prompt: string;
	var CurVal: TTimeSpan; const MinVal, DefVal, MaxVal: TTimeSpan; OnApplyTime: TOnApplyTime): Boolean;

implementation

{$R *.DFM}
uses
	Math,
	uStrings, uInputFormat, uOutputFormat, uLayout, uDictionary;

var
	fGetTime: TfGetTime;

function GetTime(const Prompt: string;
	var CurVal: TTimeSpan; const MinVal, DefVal, MaxVal: TTimeSpan; OnApplyTime: TOnApplyTime): Boolean;
begin
{	Assert(not ((MinVal > MaxVal) or (DefVal < MinVal) or (DefVal > MaxVal)
		or (CurVal < MinVal) or (CurVal > MaxVal)));}

	if not Assigned(fGetTime) then
	begin
		fGetTime := TfGetTime.Create(Application.MainForm);
	end;

	with fGetTime do
	begin
		ButtonApply.Enabled := Assigned(OnApplyTime);
		OnApply := OnApplyTime;
		TMinVal := MinVal;
		TCurVal := CurVal;
		TDefVal := DefVal;
		TMaxVal := MaxVal;
		if TMaxVal.Ticks < TMinVal.Ticks then
      TMaxVal.Ticks := TMinVal.Ticks;
		if TCurVal.Ticks < TMinVal.Ticks then
			TCurVal.Ticks := TMinVal.Ticks
		else if TCurVal.Ticks > TMaxVal.Ticks then
			TCurVal.Ticks := TMaxVal.Ticks;
		NowVal := TCurVal;
		Caption := Translate(RemoveSingleAmp(Prompt));
	// H
		TrackBarH.OnChange := nil;
		TrackBarH.Min := TMinVal.Hours;
		TrackBarH.Max := TMaxVal.Hours;
		if TrackBarH.Max - TrackBarH.Min > 112 then
			TrackBarH.TickStyle := tsNone
		else
			TrackBarH.TickStyle := tsAuto;
		TrackBarH.OnChange := TrackBarHMSDChange;
	// M
		TrackBarM.OnChange := nil;
		if TMaxVal.Hours < 1 then
		begin
			TrackBarM.Min := TMinVal.Minutes;
			TrackBarM.Max := TMaxVal.Minutes;
		end
		else
		begin
			TrackBarM.Min := 0;
			TrackBarM.Max := 59;
		end;
		TrackBarM.OnChange := TrackBarHMSDChange;
	// S
		TrackBarS.OnChange := nil;
		if TMaxVal.Minutes < 1 then
		begin
			TrackBarS.Min := TMinVal.Seconds;
			TrackBarS.Max := TMaxVal.Seconds;
		end
		else
		begin
			TrackBarS.Min := 0;
			TrackBarS.Max := 59;
		end;
		TrackBarS.OnChange := TrackBarHMSDChange;
	// D
		TrackBarD.OnChange := nil;
		if TMaxVal.Seconds < 1 then
		begin
			TrackBarD.Min := TMinVal.Milliseconds;
			TrackBarD.Max := TMaxVal.Milliseconds;
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
		if Assigned(OnApply) then
		begin
			FormStyle := fsStayOnTop;
			Show;
			Result := True;
		end
		else
		begin
			FormStyle := fsNormal;
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
	end;
end;

procedure TfGetTime.InitButtons;
begin
	ButtonMin.Enabled := NowVal <> TMinVal;
	ButtonCur.Enabled := NowVal <> TCurVal;
	ButtonDef.Enabled := NowVal <> TDefVal;
	ButtonMax.Enabled := NowVal <> TMaxVal;
	LabelH.Caption := IntToStr(NowVal.Hours);
	LabelM.Caption := IntToStr(NowVal.Minutes);
	LabelS.Caption := IntToStr(NowVal.Seconds);
	LabelD.Caption := IntToStr(NowVal.Milliseconds);
end;

procedure TfGetTime.InitEdit;
begin
	EditInput.OnChange := nil;
	if TMaxVal.Hours >= 1 then
		EditInput.Text := MsToStr(NowVal.Milliseconds, TDisplay.diMSD, -3, False)
	else
		EditInput.Text := MsToStr(NowVal.Milliseconds, TDisplay.diHMSD, -3, False);
	EditInput.SelectAll;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetTime.InitTrackBar;
begin
	TrackBarH.OnChange := nil;
	TrackBarM.OnChange := nil;
	TrackBarS.OnChange := nil;
	TrackBarD.OnChange := nil;
	TrackBarH.Position := NowVal.Hours;
	TrackBarM.Position := NowVal.Minutes;
	TrackBarS.Position := NowVal.Seconds;
	TrackBarD.Position := NowVal.Milliseconds;
	TrackBarH.OnChange := TrackBarHMSDChange;
	TrackBarM.OnChange := TrackBarHMSDChange;
	TrackBarS.OnChange := TrackBarHMSDChange;
	TrackBarD.OnChange := TrackBarHMSDChange;
end;

procedure TfGetTime.EditInputChange(Sender: TObject);
begin
	EditInput.OnChange := nil;
	NowVal := StrToMs(EditInput.Text, TMinVal, TDefVal, TMaxVal, True, Messages);
  EditError.Lines.Text := Messages.ToString;
	Messages.Clear;

	InitButtons;
	InitTrackBar;
	ChangeTime;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetTime.TrackBarHMSDChange(Sender: TObject);
begin
	NowVal.Milliseconds :=
		S8(TrackBarH.Position) * Hour +
		TrackBarM.Position * Minute +
		TrackBarS.Position * Second +
		TrackBarD.Position;
	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

procedure TfGetTime.ButtonMinClick(Sender: TObject);
begin
	NowVal := TMinVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

procedure TfGetTime.ButtonCurClick(Sender: TObject);
begin
	NowVal := TCurVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

procedure TfGetTime.ButtonDefClick(Sender: TObject);
begin
	NowVal := TDefVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

procedure TfGetTime.ButtonMaxClick(Sender: TObject);
begin
	NowVal := TMaxVal;
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

procedure TfGetTime.ChangeTime;
begin
	if Assigned(OnApply) then
    OnApply(NowVal);
end;

procedure TfGetTime.ButtonOkClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		Close;
	end;
end;

procedure TfGetTime.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(OnApply) then
	begin
		if NowVal <> TCurVal then OnApply(TCurVal);
		Close;
	end;
end;

procedure TfGetTime.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	LayoutControls([ButtonOk, ButtonCancel, ButtonApply], ClientWidth, ClientHeight);
end;

procedure TfGetTime.SpinButtonHMSDChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: Integer;
	Direction: TUpDownDirection);
var
  L: UG;
begin
	L := TUpDown(Sender).Tag;
	if Direction = updUp then
	begin
		if NowVal.Milliseconds + L <= TMaxVal.Milliseconds then
      NowVal.Milliseconds := NowVal.Milliseconds + L
    else
      NowVal := TMaxVal;
	end
	else
	begin
		if NowVal.Milliseconds >= L + TMinVal.Milliseconds then
      NowVal.Milliseconds := NowVal.Milliseconds - L
    else
      NowVal := TMinVal;
	end;
{	if NowVal < TMinVal then
		NowVal := TMinVal
	else if NowVal > TMaxVal then
		NowVal := TMaxVal;}
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeTime;
end;

constructor TfGetTime.Create(AOwner: TComponent);
begin
	inherited;
	Messages := TParserMessages.Create;
end;

destructor TfGetTime.Destroy;
begin
	FreeAndNil(Messages);
	inherited;
end;

end.
