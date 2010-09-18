unit uDialog;

interface

uses
	uDButton, uAdd, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, uDPanel, StdCtrls, uDLabel, uDTimer, uDForm;

type
	TDlgBtn = (
		mbOK, mbYes, mbYesToAll,
		mbRetry, mbIgnore, mbAbort,
		mbDelete, mbDeleteAll,
		mbNo, mbNoToAll, mbCancel);
	TDlgButtons = set of TDlgBtn;


	TfDialog = class(TDForm)
		Memo: TMemo;
		Image: TImage;
		LabelTimeLeft: TDLabel;
		PanelTimeLeft: TDPanel;
    Timer1: TDTimer;
		CheckBoxA: TCheckBox;
		procedure Timer1Timer(Sender: TObject);
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
		ModResult: SG;
		TimeLeft: SG;
		FButtons: array of TDButton;
		procedure DrawTimeLeft;
		procedure BClick(Sender: TObject);
	public
		{ Public declarations }
	end;

const
	DlgBtnNames: array[TDlgBtn] of string = (
		'OK', 'Yes', 'YesToAll',
		'Retry', 'Ignore', 'Abort',
		'Delete', 'DeleteAll',
		'No', 'NoToAll', 'Cancel');

	DlgNoTime = 0;
	DlgWait = 15;

function MessageD(Msg: string; DlgType: TMsgDlgType;
	Buttons: TDlgButtons): TDlgBtn;
function MessageDEx(Msg: string; DlgType: TMsgDlgType;
	Buttons: array of ShortString; TimeLeft: SG; Owener: TComponent): SG;

implementation

{$R *.DFM}
uses
	Consts, Math,
	uWave, uStrings;
var
	fDialog: TfDialog;


	IgnoreCount: SG;
	Ignores: array of record
		Msg: string;
		Res: SG;
	end;

procedure TfDialog.DrawTimeLeft;
begin
	PanelTimeLeft.Caption := msToStr(1000 * TimeLeft, diMSD, 0);
end;

procedure TfDialog.BClick(Sender: TObject);
begin
	ModResult := TDButton(Sender).Tag;
	Close;
end;

var
	Captions: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError,
		SMsgDlgInformation, SMsgDlgConfirm, '');
	IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
		IDI_ASTERISK, IDI_QUESTION, nil);

function MessageDEx(Msg: string; DlgType: TMsgDlgType;
	Buttons: array of ShortString; TimeLeft: SG; Owener: TComponent): SG;
const
	BWidth = 75;
	BHeight = 23;
	BSpace = 8;
var
	B: SG;
	ButtonWidth, BTop: SG;
	IconID: PChar;
	Wid, MaxWid, LastLine, LineCount, i, Hei, ButtonCount: SG;
begin
	Result := SG(mbCancel);
	PlayWinSound(wsQuestion);
	DelChars(Msg, '&');
	for i := 0 to IgnoreCount - 1 do
		if Ignores[i].Msg = Msg then
		begin
			Result := Ignores[i].Res;
			Exit;
		end;
	if not Assigned(fDialog) then fDialog := TfDialog.Create(Owener);
	if fDialog.Visible then Exit;
	fDialog.TimeLeft := TimeLeft;
	fDialog.DrawTimeLeft;
	if DlgType <> mtCustom then
		fDialog.Caption := string(Captions[DlgType])
	else
		fDialog.Caption := Application.Title;

	IconID := IconIDs[DlgType];
	if IconID <> nil then
	begin
		fDialog.Image.Name := 'Image';
		fDialog.Image.Picture.Icon.Handle := LoadIcon(0, IconID);
	end;

	LineCount := 1;
	MaxWid := 0;
	LastLine := 1;
	for i := 1 to Length(Msg) + 1 do
		if (i = Length(Msg) + 1) or (Msg[i] = #10) then
		begin
			Inc(LineCount);
			Wid := fDialog.Canvas.TextWidth(Copy(Msg, LastLine, i - LastLine));
			if Wid > MaxWid then MaxWid := Wid;
			LastLine := i + 1;
		end;
	Inc(MaxWid, 2 * BSpace + fDialog.Image.Width);

	ButtonWidth := BSpace;
	ButtonCount := 0;
	for B := 0 to Length(Buttons) - 1 do
	begin
//		if B in Buttons then
		begin
			Inc(ButtonWidth, BWidth + BSpace);
			Inc(ButtonCount);
		end;
	end;

	MaxWid := Max(MaxWid, ButtonWidth);
	if MaxWid < 128 then
		MaxWid := 128
	else if MaxWid > Screen.Width then
		MaxWid := Screen.Width;

	fDialog.Memo.Clear;
	fDialog.Memo.Width := MaxWid - fDialog.Memo.Left - BSpace;
	Hei := LineCount * fDialog.Canvas.TextHeight(Msg);
	fDialog.Memo.Height := Hei;
	if Hei < 40 then Hei := 40;
	fDialog.Memo.Lines.Add(Msg);
	fDialog.ClientWidth := fDialog.Memo.Left + fDialog.Memo.Width + 8;

	fDialog.LabelTimeLeft.Top := Hei + 20;
	fDialog.PanelTimeLeft.Top := fDialog.LabelTimeLeft.Top;

	BTop := fDialog.PanelTimeLeft.Top + fDialog.PanelTimeLeft.Height + BSpace;
	i := 0;
	if Length(Buttons) > Length(fDialog.FButtons) then SetLength(fDialog.FButtons, Length(Buttons));
	for B := 0 to Length(Buttons) - 1 do
	begin
//		if B in Buttons then
		begin
			if not Assigned(fDialog.FButtons[B]) then
			begin
				fDialog.FButtons[B] := TDButton.Create(fDialog);
				fDialog.InsertControl(fDialog.FButtons[B]);
			end;

			fDialog.FButtons[B].Name := 'Button' + Buttons[B] + IntToStr(B);
			fDialog.FButtons[B].Caption := '/';//ButtonNames[B];
			fDialog.FButtons[B].Visible := True;
			fDialog.FButtons[B].Left := ((BWidth + BSpace) * (2 * i - ButtonCount) + BSpace + MaxWid) div 2;
			fDialog.FButtons[B].Top := BTop;
			fDialog.FButtons[B].Width := BWidth;
			fDialog.FButtons[B].Height := BHeight;
			fDialog.FButtons[B].OnClick := fDialog.BClick;
			fDialog.FButtons[B].Default := i = 0;
			fDialog.FButtons[B].TabOrder := i + 2;
			fDialog.FButtons[B].TabStop := True;
			fDialog.FButtons[B].Tag := B;
			if i = 0 then
				fDialog.ActiveControl := fDialog.FButtons[B];
			fDialog.FButtons[B].Cancel := i = ButtonCount - 1;

			Inc(i);
		end;
{		else
			fDialog.FButtons[B].Visible := False;}
	end;

	fDialog.CheckBoxA.Width := fDialog.ClientWidth - 2 * fDialog.CheckBoxA.Left;
	fDialog.CheckBoxA.Top := BTop + BHeight + BSpace;
	fDialog.CheckBoxA.Checked := False;
	fDialog.ClientHeight := fDialog.CheckBoxA.Top + fDialog.CheckBoxA.Height + BSpace;

	fDialog.ModResult := -1;
	fDialog.ShowModal;
	Result := fDialog.ModResult;
	for B := 0 to Length(Buttons) - 1 do
	begin
		fDialog.RemoveControl(fDialog.FButtons[B]);
		fDialog.FButtons[B].Free; fDialog.FButtons[B] := nil;
	end;
	if fDialog.CheckBoxA.Checked then
	begin
		SetLength(Ignores, IgnoreCount + 1);
		Ignores[IgnoreCount].Msg := Msg;
		Ignores[IgnoreCount].Res := Result;
		Inc(IgnoreCount);
	end;
end;

function MessageD(Msg: string; DlgType: TMsgDlgType;
	Buttons: TDlgButtons): TDlgBtn;

var
	B: TDlgBtn;
	Res, i: SG;
	But: array of ShortString;
	procedure AddS(s: string);
	begin
		SetLength(But, Length(But) + 1);
		But[Length(But) - 1] := s;
	end;
begin
	for B := Low(B) to High(B) do
		if B in Buttons then AddS(DlgBtnNames[B]);
	Res := MessageDEx(Msg, DlgType, But, DlgWait, nil);
	i := 0;
	Result := mbCancel;
	for B := Low(B) to High(B) do
		if B in Buttons then
		begin
			if i = Res then
			begin
				Result := B;
				Break;
			end;
			Inc(i);
		end;
end;

procedure TfDialog.Timer1Timer(Sender: TObject);
begin
	Dec(TimeLeft);
	DrawTimeLeft;
	if TimeLeft = 0 then Close;
end;

procedure TfDialog.FormCreate(Sender: TObject);
{var
	B: TMsgDlgBtn;}
begin
//	Background := baGradient;
	Background := baStandard;
{	for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
	begin
		FButtons[B] := TDButton.Create(Self);
			InsertControl(FButtons[B]);
	end;}
end;

end.
