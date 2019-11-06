unit uGetInt;

interface

uses
  Velthuis.BigDecimals,

	uTypes, uParserMsg,

  SysUtils, Classes,
  Graphics, Controls, Forms,
	StdCtrls, ComCtrls, uDButton, ExtCtrls, uDLabel, uDForm, uDMemo;

type
	TOnApplyInt = procedure(Value: BigDecimal);

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
		EditError: TDMemo;
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
			NewValue: Integer; Direction: TUpDownDirection);
	private
		{ Private declarations }
		FMessages: TParserMessages;
		FMinVal, FCurVal, FDefVal, FMaxVal, FNowVal: BigDecimal;
		FOnApply: TOnApplyInt;
    FTrackBarMultiplier: S8;
    procedure InitTrackBarMultiplier;
		procedure ChangeInt;
		procedure InitButtons;
		procedure InitEdit;
		procedure InitTrackBar;
    procedure InitAll;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	end;

function GetNumber(APrompt: string;
	var ACurVal: BigDecimal; const AMinVal, ADefVal, AMaxVal: BigDecimal; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: S8; const AMinVal, ADefVal, AMaxVal: S8; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: U4; const AMinVal, ADefVal, AMaxVal: U4; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var ACurVal: S4; const AMinVal, ADefVal, AMaxVal: S4; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(Prompt: string;
	var ACurVal: U2; const AMinVal, ADefVal, AMaxVal: U2; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: S2; const AMinVal, ADefVal, AMaxVal: S2; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: U1; const AMinVal, ADefVal, AMaxVal: U1; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: S1; const AMinVal, ADefVal, AMaxVal: S1; AOnApplyInt: TOnApplyInt): Boolean; overload;
{$ifdef CPUX64}
function GetNumber(APrompt: string;
	var ACurVal: U8; const AMinVal, ADefVal, AMaxVal: U8; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: SG; const AMinVal, ADefVal, AMaxVal: SG; AOnApplyInt: TOnApplyInt): Boolean; overload;
function GetNumber(APrompt: string;
	var ACurVal: UG; const AMinVal, ADefVal, AMaxVal: UG; AOnApplyInt: TOnApplyInt): Boolean; overload;
{$endif}

implementation

{$R *.DFM}
uses
	Math,
	uDictionary,
  uOutputFormat,
	uStrings, uInputFormat, uMathExpressionParser, uLayout;

var
	fGetInt: TfGetInt;

function GetNumber(APrompt: string;
	var ACurVal: BigDecimal; const AMinVal, ADefVal, AMaxVal: BigDecimal; AOnApplyInt: TOnApplyInt): Boolean;
begin
	Assert(not ((AMinVal > AMaxVal) or (ADefVal < AMinVal) or (ADefVal > AMaxVal)
		or (ACurVal < AMinVal) or (ACurVal > AMaxVal)));

	if not Assigned(fGetInt) then
	begin
		fGetInt := TfGetInt.Create(Application.MainForm);
	end;
	fGetInt.ButtonApply.Enabled := Assigned(AOnApplyInt);
	fGetInt.FOnApply := AOnApplyInt;

	fGetInt.FMinVal := AMinVal;
	fGetInt.FCurVal := ACurVal;
	fGetInt.FDefVal := ADefVal;
	fGetInt.FMaxVal := AMaxVal;
	if fGetInt.FMaxVal < fGetInt.FMinVal then fGetInt.FMaxVal := fGetInt.FMinVal;
	if fGetInt.FCurVal < fGetInt.FMinVal then
		fGetInt.FCurVal := fGetInt.FMinVal
	else if fGetInt.FCurVal > fGetInt.FMaxVal then
		fGetInt.FCurVal := fGetInt.FMaxVal;
	fGetInt.FNowVal := fGetInt.FCurVal;
	fGetInt.Caption := Translate(RemoveSingleAmp(APrompt));
	fGetInt.LabelMin.Caption := fGetInt.FMinVal.ToString;
	fGetInt.LabelMax.Caption := fGetInt.FMaxVal.ToString;
	fGetInt.LabelNow.Caption := fGetInt.FNowVal.ToString;

  fGetInt.InitTrackBarMultiplier;

	fGetInt.TrackBar.OnChange := nil;
	fGetInt.TrackBar.PageSize := 10;
	fGetInt.TrackBar.Frequency := fGetInt.TrackBar.PageSize div 2;
	if fGetInt.FMaxVal < fGetInt.TrackBar.Min then
	begin
		fGetInt.TrackBar.Min := 0;
		fGetInt.TrackBar.Max := ((fGetInt.FMaxVal - fGetInt.FMinVal) div fGetInt.FTrackBarMultiplier).Trunc;
	end
	else
	begin
		fGetInt.TrackBar.Max := ((fGetInt.FMaxVal - fGetInt.FMinVal) div fGetInt.FTrackBarMultiplier).Trunc;
		fGetInt.TrackBar.Min := 0;
	end;
	fGetInt.TrackBar.SelStart := SG((fGetInt.FCurVal - fGetInt.FMinVal) div fGetInt.FTrackBarMultiplier) - 1;
	fGetInt.TrackBar.SelEnd := SG((fGetInt.FCurVal - fGetInt.FMinVal) div fGetInt.FTrackBarMultiplier) + 1;
	fGetInt.TrackBar.OnChange := fGetInt.TrackBarChange;

	fGetInt.InitTrackBar;
	fGetInt.InitButtons;
	fGetInt.InitEdit;
	fGetInt.EditInputChange(nil);
	if fGetInt.ActiveControl <> fGetInt.EditInput then
     fGetInt.ActiveControl := fGetInt.EditInput;
	if Assigned(fGetInt.FOnApply) then
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
			ACurVal := fGetInt.FNowVal;
			Result := True;
		end
		else
		begin
			Result := False;
		end;
	end;
end;

procedure TfGetInt.InitAll;
begin
	InitTrackBar;
	InitEdit;
	InitButtons;
	ChangeInt;
end;

procedure TfGetInt.InitButtons;
begin
	ButtonMin.Enabled := FNowVal <> FMinVal;
	ButtonCur.Enabled := FNowVal <> FCurVal;
	ButtonDef.Enabled := FNowVal <> FDefVal;
	ButtonMax.Enabled := FNowVal <> FMaxVal;
	LabelNow.Caption := FNowVal.ToString;
	LabelNow.Update;
end;

procedure TfGetInt.InitEdit;
begin
	EditInput.OnChange := nil;
	EditInput.Text := FNowVal.ToString;
	EditInput.SelectAll;
	EditInput.Update;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetInt.InitTrackBar;
begin
	TrackBar.OnChange := nil;
	TrackBar.Position := ((FNowVal - FMinVal) div FTrackBarMultiplier).Trunc;
	TrackBar.Update;
	TrackBar.OnChange := TrackBarChange;
end;

procedure TfGetInt.InitTrackBarMultiplier;
const
  MaxTrackBarTicks = 500;
begin
  FTrackBarMultiplier := 1;
  while (FMaxVal - FMinVal) div FTrackBarMultiplier > MaxTrackBarTicks do
  begin
    FTrackBarMultiplier := FTrackBarMultiplier * 10;
  end;
end;

procedure TfGetInt.EditInputChange(Sender: TObject);
begin
	EditInput.OnChange := nil;
 	FNowVal := StrToValBD(EditInput.Text, True, FMinVal, FNowVal, FMaxVal, FMessages);
	EditError.Text := FMessages.ToString;
	FMessages.Clear;

	InitButtons;
	InitTrackBar;
	ChangeInt;
	EditInput.OnChange := EditInputChange;
end;

procedure TfGetInt.TrackBarChange(Sender: TObject);
begin
  if TrackBar.Position = TrackBar.Min then
    FNowVal := FMinVal
  else if TrackBar.Position = TrackBar.Max then
    FNowVal := FMaxVal
  else
  begin
  	FNowVal := TrackBar.Position;
    FNowVal := FNowVal * FTrackBarMultiplier + FMinVal;
  end;
	InitButtons;
	InitEdit;
	ChangeInt;
end;

procedure TfGetInt.ButtonMinClick(Sender: TObject);
begin
	FNowVal := FMinVal;
  InitAll;
end;

procedure TfGetInt.ButtonCurClick(Sender: TObject);
begin
	FNowVal := FCurVal;
  InitAll;
end;

procedure TfGetInt.ButtonDefClick(Sender: TObject);
begin
	FNowVal := FDefVal;
  InitAll;
end;

procedure TfGetInt.ButtonMaxClick(Sender: TObject);
begin
	FNowVal := FMaxVal;
  InitAll;
end;

procedure TfGetInt.SpinButton1DownClick(Sender: TObject);
begin
	if FNowVal > FMinVal then
  begin
    Dec(FNowVal);
    InitAll;
  end;
end;

procedure TfGetInt.SpinButton1UpClick(Sender: TObject);
begin
	if FNowVal < FMaxVal then
  begin
    Inc(FNowVal);
    InitAll;
  end;
end;

procedure TfGetInt.ChangeInt;
begin
	if Assigned(FOnApply) then
    FOnApply(FNowVal);
end;

procedure TfGetInt.ButtonOkClick(Sender: TObject);
begin
	if Assigned(FOnApply) then
	begin
		Close;
	end;
end;

procedure TfGetInt.ButtonCancelClick(Sender: TObject);
begin
	if Assigned(FOnApply) then
	begin
		if FNowVal <> FCurVal then
      FOnApply(FCurVal);
		Close;
	end;
end;

procedure TfGetInt.FormCreate(Sender: TObject);
begin
	Background := baGradient;
	LayoutControls([ButtonOk, ButtonCancel, ButtonApply], ClientWidth, ClientHeight);
end;

procedure TfGetInt.UpDownChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: Integer;
	Direction: TUpDownDirection);
begin
	AllowChange := True;

	if AllowChange then
	begin
		if Direction = updUp then
		begin
			if FNowVal < FMaxVal then
      begin
        Inc(FNowVal);
        InitAll;
      end;
		end
		else if Direction = updDown then
		begin
			if FNowVal > FMinVal then
      begin
        Dec(FNowVal);
        InitAll;
      end;
		end;
	end;
end;

function GetNumber(APrompt: string;
	var ACurVal: S8; const AMinVal, ADefVal, AMaxVal: S8; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: U4; const AMinVal, ADefVal, AMaxVal: U4; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(Prompt: string;
	var ACurVal: S4; const AMinVal, ADefVal, AMaxVal: S4; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(Prompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: S2; const AMinVal, ADefVal, AMaxVal: S2; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(Prompt: string;
	var ACurVal: U2; const AMinVal, ADefVal, AMaxVal: U2; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(Prompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: S1; const AMinVal, ADefVal, AMaxVal: S1; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: U1; const AMinVal, ADefVal, AMaxVal: U1; AOnApplyInt: TOnApplyInt): Boolean;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

{$ifdef CPUX64}
function GetNumber(APrompt: string;
	var ACurVal: U8; const AMinVal, ADefVal, AMaxVal: U8; AOnApplyInt: TOnApplyInt): Boolean; overload;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: SG; const AMinVal, ADefVal, AMaxVal: SG; AOnApplyInt: TOnApplyInt): Boolean; overload;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;

function GetNumber(APrompt: string;
	var ACurVal: UG; const AMinVal, ADefVal, AMaxVal: UG; AOnApplyInt: TOnApplyInt): Boolean; overload;
var C: BigDecimal;
begin
	C := ACurVal;
	Result := GetNumber(APrompt, C, AMinVal, ADefVal, AMaxVal, AOnApplyInt);
	ACurVal := C.Trunc;
end;
{$endif}

constructor TfGetInt.Create(AOwner: TComponent);
begin
	inherited;

	FMessages := TParserMessages.Create;
end;

destructor TfGetInt.Destroy;
begin
  try
  	FreeAndNil(FMessages);
  finally
  	inherited;
  end;
end;

end.
