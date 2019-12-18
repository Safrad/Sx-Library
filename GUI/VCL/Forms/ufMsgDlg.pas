unit ufMsgDlg;

interface

uses
	SysUtils,
  Classes,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Consts,
	Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ComCtrls,

  uIgnore,
  uDButton, uDLabel,
	uDForm, uDTimer, uDEdit,
  uOutputInfo, uTypes, uDWinControl, uDMemo;

type
	TfMsgDlg = class(TDForm)
		ButtonExit: TDButton;
		OpenDialogFile: TOpenDialog;
		MemoMsg: TRichEdit;
		LabelX: TLabel;
		PanelCount: TDEdit;
		Timer1: TDTimer;
		Image: TImage;
		LabelTimeLeft: TDLabel;
		PanelTimeLeft: TDLabel;
		LabelCreated: TDLabel;
		PanelCreated: TDLabel;
		LabelMessage: TDLabel;
		ButtonAll: TDButton;
		Bevel1: TBevel;
		ButtonLeft: TDButton;
		EditIndex: TDEdit;
		ButtonRight: TDButton;
		procedure ButtonExitClick(Sender: TObject);
		procedure FormKeyDown(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure FormKeyUp(Sender: TObject; var Key: Word;
			Shift: TShiftState);
		procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
			Y: Integer);
		procedure Timer1Timer(Sender: TObject);
		procedure LabelTimeLeftClick(Sender: TObject);
		procedure ButtonRetryClick(Sender: TObject);
		procedure ButtonIgnoreClick(Sender: TObject);
		procedure ButtonLeftClick(Sender: TObject);
		procedure EditIndexChange(Sender: TObject);
		procedure FormHide(Sender: TObject);
		procedure FormClick(Sender: TObject);
		procedure FormShow(Sender: TObject);
	strict private
		ShiftDown: BG;

		FButtons: array of TDButton;

		procedure DrawTimeLeft;
		procedure BClick(Sender: TObject);
		procedure TryClose;
		procedure ShowMes;
		procedure FillMemo(const Ignore: PIgnore);
	public
		ActItem: SG;
		procedure ShowForm;
	end;

const
	DlgNoTime = 0;
	DlgWait = 15; // Default
var
	fMsgDlg: TfMsgDlg;

procedure ShowDlg;

implementation

{$R *.DFM}
uses
  uVisualOptions,
  SynTaskDialog,
  uPlaySound,
  uMsg,
	uFiles, uColor, uDictionary,
	uStrings, uChar, uGraph, uDBitmap, uData, uInputFormat, uOutputFormat, uMath,
	Winapi.Windows, Math, UITypes;

var
	IgnoreAll: TIgnoreAll;

	Ignores: TData;
	TickCount, StartTickCount: U8;

procedure ShowDlg;
begin
	if not Assigned(fMsgDlg) then
	begin
		fMsgDlg := TfMsgDlg.Create(nil{ActiveForm - fSplash destroys this form});
		fMsgDlg.Background := baGradient;
	end;
	fMsgDlg.Center;
	fMsgDlg.ShowForm;
end;

{ TfMsgDlg }

procedure TfMsgDlg.DrawTimeLeft;
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	if Ignore.TimeLeft = 0 then
		PanelTimeLeft.Caption := ''
	else
		PanelTimeLeft.Caption := MsToStr(Second * S8(Ignore.TimeLeft) - 1 - S8(TimeDifference(TickCount, StartTickCount)), diMSD, 0, False);
end;

const
	IconIDs: array[TMessageLevel] of PChar = (IDI_QUESTION, IDI_WINLOGO, IDI_INFORMATION, IDI_WARNING, IDI_ERROR,
		IDI_APPLICATION, '');
  IconID2s: array[TMessageLevel] of SG = (102, 100, 104, 101, 103, 105, 0);

procedure TfMsgDlg.FillMemo(const Ignore: PIgnore);
var
	i, LastPos, Id: SG;
	Input, Output, p: string;
	ParamStart: array of SG;
begin
//	MemoMsg.Lines.BeginUpdate;
	try
		SetLength(ParamStart, Length(Ignore.Param));
		i := 1;
		if StartStr(ErrorCodeStr, Ignore.Text) then
			Input := Ignore.Text
		else
			Input := Translate(Ignore.Text);
		while i <= Length(Input) do
		begin
			LastPos := i;
			i := PosEx('%', Input, i);
			if i = 0 then
				i := MaxInt;
			Output := Output + Copy(Input, LastPos, i - LastPos);// ReplaceF(Msg, CharLF, CharCR + CharLF);
{			MemoMsg.SelStart := LastPos;
			MemoMsg.SelLength := i - LastPos;
			MemoMsg.SelAttributes.Color := clWindowText;}

			if i = MaxInt then Break;

			Inc(i); // Accept %
			Id := uStrings.ReadSGFast(Input, i);
			if (Id > 0) and (Id <= Length(Ignore.Param)) then
			begin
				ParamStart[Id - 1] := Length(Output) + 1;
				p := '''' + Ignore.Param[Id - 1] + '''';
				// Inc(i, Length(p));
				Output := Output + p;
			end
			else
				Output := Output + '%';
		end;
		MemoMsg.Text := Output;
		for i := 0 to Length(Ignore.Param) - 1 do
		begin
			if ParamStart[i] <> 0 then
			begin
				MemoMsg.SelStart := ParamStart[i];
				MemoMsg.SelLength := Length(Ignore.Param[i]);
				MemoMsg.SelAttributes.Color := MixColors(clWindowText, SpectrumColor(128 * i mod (MaxSpectrum + 1)));
				MemoMsg.SelAttributes.Style := [fsBold];
			end;
		end;

	finally
//		MemoMsg.Lines.EndUpdate;
	end;
//	MemoMsg.Update;
end;

procedure TfMsgDlg.ShowMes;
var
	IconID: PChar;
	i, LastLine: SG;
	Wid, Hei, BWid, MaxWid, LineCount: SG;
	R: TRect;
	Ignore: PIgnore;
	Msg: string;

	BMinWidth: SG;
	BHeight: SG;
begin
  BMinWidth := LgToPx(81);
  BHeight := LgToPx(23);

	Ignore :=Ignores.Get(ActItem);

	// Captions
	EditIndex.OnChange := nil;
	EditIndex.Text:= NToS(ActItem + 1);
	EditIndex.OnChange := EditIndexChange;
	PanelCreated.Caption := DateTimeToS(Ignore.DateTime, 0, ofDisplay);
	Caption := MessageLevelStr[Ignore.MsgType];

	IconID := IconIDs[Ignore.MsgType];

	// Image
	if IconID <> nil then
	begin
		Image.Name := 'Image';
//		Image.Picture.Icon.Handle := LoadIcon(0, IconID);
    Image.Picture.Icon.Handle := LoadImage(GetModuleHandle('user32'), MAKEINTRESOURCE(IconID2s[Ignore.MsgType]), IMAGE_ICON, Image.Width, Image.Height, LR_DEFAULTCOLOR);
                                                                             
//    Image.Picture.Bitmap.

//    Image.Picture.
	end;

	MaxWid := PanelCreated.Left + PanelCreated.Width + FormBorder;

	// Message Text
	Msg := ReplaceParam(Ignore.Text, Ignore.Param);
	LineCount := 1;
	LastLine := 1;
	for i := 1 to Length(Msg) + 1 do
		if (i = Length(Msg) + 1) or (Msg[i] = CharLF) then
		begin
			Inc(LineCount);
			Wid := LgToPx(Canvas.TextWidth(Copy(Msg, LastLine, i - LastLine))) + 4 + FormBorder + MemoMsg.Left;
			if Wid > MaxWid then
        MaxWid := Wid;
			LastLine := i + 1;
		end;

	// Buttons
	BWid := 0;
	if Length(Ignore.Buttons) > Length(FButtons) then SetLength(FButtons, Length(Ignore.Buttons));
	for i := 0 to Length(FButtons) - 1 do
	begin
		if i < Length(Ignore.Buttons) then
		begin
			if not Assigned(FButtons[i]) then
			begin
				FButtons[i] := TDButton.Create(Self);
				FButtons[i].OnKeyDown := FormKeyDown;
				FButtons[i].OnKeyUp := FormKeyUp;
				FButtons[i].OnMouseMove := FormMouseMove;
				FButtons[i].Visible := False;

				InsertControl(FButtons[i]);
			end
			else
				FButtons[i].Visible := False;

			FButtons[i].Name := 'Button' + IntToStr(i);
			FButtons[i].Caption := Ignore.Buttons[i];
//			(ButtonAll.Left + ButtonAll.Width + BSpace + (BWidth + BSpace) * (2 * i - ButtonCount) + BSpace + MaxWid) div 2;
			FButtons[i].SetBounds(0, 0, Max(BMinWidth, Canvas.TextWidth(FButtons[i].Caption) + 2 * 5), BHeight);
			Inc(BWid, FButtons[i].Width + FormBorder);
			FButtons[i].OnClick := BClick;
			FButtons[i].Default :=  i = 0;
			FButtons[i].TabOrder := i + 4;
			FButtons[i].TabStop := True;
			FButtons[i].Tag := i;
			FButtons[i].Down := Ignore.Res = i;
			FButtons[i].Enabled := Ignore.Res = -1;
			FButtons[i].Cancel := i = Length(Ignore.Buttons) - 1;
		end
		else if Assigned(FButtons[i]) then
			FButtons[i].Visible := False;

	end;

	Wid := BWid + MemoMsg.Left;
	MaxWid := Max(MaxWid, Wid);

	R := Screen.MonitorFromWindow(Handle).WorkareaRect;

	Hei := Max(LineCount, 3) * Canvas.TextHeight(Msg) + 6;
	i := Canvas.TextHeight('W');
	if i = 0 then Exit;
	Wid := Canvas.TextHeight('W') * ((R.Bottom - R.Top - 128{TaskBar} - 2 * (Height - ClientHeight)) div i) + 6;
	if Hei > Wid then
	begin
		Hei := Wid;
		MemoMsg.ScrollBars := ssVertical;
		Inc(MaxWid, 20); // ScrollBarWidth
	end
	else
		MemoMsg.ScrollBars := ssNone;
	MaxWid := Min(MaxWid, R.Right - R.Left - 2 * (Width - ClientWidth));
	MemoMsg.SetBounds(MemoMsg.Left, MemoMsg.Top, MaxWid - MemoMsg.Left - FormBorder + 6, Hei);

	FillMemo(Ignore);
//	Hei := Max(Hei, ButtonAll.Top + ButtonAll.Height + 6);
	Inc(Hei, MemoMsg.Top + FormBorder);
	ClientWidth := MaxWid;
	ClientHeight := Hei + BHeight + FormBorder;//ButtonA.Top + ButtonA.Height + BSpace;

	ButtonAll.Down := Ignore.Ignore <> iaNone;
	ButtonAll.Enabled := Ignore.Res = -1;
	ButtonAll.Top := Hei;

	Wid := MaxWid - BWid;
	for i := 0 to Length(Ignore.Buttons) - 1 do
	begin
		FButtons[i].SetBounds(Wid, Hei, FButtons[i].Width, FButtons[i].Height);
		Inc(Wid, FButtons[i].Width + FormBorder);
		FButtons[i].Visible := True;
	end;
end;
{
procedure TfIOError.UpDown1ChangingEx(Sender: TObject;
	var AllowChange: BG; NewValue: SmallInt;
	Direction: TUpDownDirection);
begin
	if NewValue < 0 then
	begin
		AllowChange := False;
		Exit;
	end;
	if NewValue >= SG(Ignores.Count) then
	begin
		AllowChange := False;
		Exit;
	end;
	EditIndex.Text := NToS(NewValue + 1);
	ShowMes(NewValue);
end;}

procedure TfMsgDlg.ButtonExitClick(Sender: TObject);
begin
	if Assigned(Application.MainForm) and (ShiftDown = False){(GetAsyncKeyState(VK_SHIFT) = 0)} then
		Application.MainForm.Close
	else
	begin
		TerminateProcess(GetCurrentProcess, 1); // Immediate
//		Halt; // Call finalize units
	end;
end;

procedure TfMsgDlg.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = False) and (Key = VK_SHIFT) then
	begin
		ShiftDown := True;
		ButtonAll.Caption := Translate('Never Show');
		ButtonExit.Caption := Translate('Terminate');
	end;
end;

procedure TfMsgDlg.FormKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = True) and (Key = VK_SHIFT) then
	begin
		ShiftDown := False;
		ButtonAll.Caption := Translate('Use Answer for All');
		ButtonExit.Caption := Translate('Close Program');
	end;
end;

procedure TfMsgDlg.FormMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
begin
	StartTickCount := TickCount;
	DrawTimeLeft;
end;

procedure TfMsgDlg.Timer1Timer(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	TickCount := GetTickCount;
	DrawTimeLeft;
	if (Ignore.TimeLeft > 0) and (TickCount > U8(Ignore.TimeLeft) * 1000 + StartTickCount) then Close;
end;

procedure TfMsgDlg.ShowForm;
var Ignore: PIgnore;
begin
	PanelCount.Text := NToS(Ignores.Count);

	if Visible = False then
	begin
		TickCount := GetTickCount;
		StartTickCount := TickCount;
{			UpDown1.OnChangingEx := nil;
		UpDown1.Position := Ignores.Count - 1;
		UpDown1.OnChangingEx := UpDown1ChangingEx;}
		ActItem := Ignores.Count - 1;
		EditIndex.OnChange := nil;
		EditIndex.Text := NToS(ActItem);
		EditIndex.OnChange := EditIndexChange;
		ShowMes;
		DrawTimeLeft;

		ModalResult := mrNone;
{		if Application.Terminated then
		begin
			FormStyle := fsStayOnTop;
			Timer1.Enabled := True;
//				ShowModal;
			Show;
			repeat
				Application.HandleMessage;
			until ModalResult <> mrNone;
			Hide;
		end
		else
		begin}
			FormStyle := fsNormal;
			Timer1.Enabled := True;
			ShowModal;
{		end;}
	end
	else
	begin
		Ignore := Ignores.Get(ActItem);
		Ignore.Res := -1;
		Exit;
	end;
end;

procedure TfMsgDlg.LabelTimeLeftClick(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

procedure TfMsgDlg.TryClose;
var
	i: SG;
	MCount, Last: SG;
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	if ButtonAll.Down = False then
		Ignore.Ignore := iaNone
	else
	begin
		if fMsgDlg.ShiftDown then
		begin
			Ignore.Ignore := iaAll;
			IgnoreAll := iaAll;
		end
		else
		begin
			Ignore.Ignore := iaSame;
			IgnoreAll := iaSame;
		end;
	end;

	Ignore.TimeLeft := DlgNoTime;

	MCount := 0;
	Last := -1;
	for i := 0 to Ignores.Count - 1 do
	begin
		if TIgnore(Ignores.Get(i)^).Res = -1 then
		begin
			if Last = -1 then Last := i;
			Inc(MCount);
		end;
	end;
	if MCount = 0 then
	begin
		Close;
		ModalResult := mrCancel;
	end
	else
	begin
		ActItem := Last;
		ShowMes;
	end;
end;

// Button Clicks

procedure TfMsgDlg.BClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.Res := TDButton(Sender).Tag;
	TryClose;
end;

procedure TfMsgDlg.ButtonRetryClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.Res := 2;
	TryClose;
end;

procedure TfMsgDlg.ButtonIgnoreClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.Res := 1;
	TryClose;
end;

procedure TfMsgDlg.ButtonLeftClick(Sender: TObject);
begin
	if TDButton(Sender).Tag = 0 then
	begin
		if ActItem > 0 then
			Dec(ActItem);
	end
	else
	begin
		if ActItem + 1 < Ignores.Count then
			Inc(ActItem);
	end;
	ShowMes;
end;

procedure TfMsgDlg.EditIndexChange(Sender: TObject);
begin
	ActItem := StrToValI(EditIndex.Text, True, 1, ActItem + 1, Ignores.Count, 1) - 1;
	ShowMes;
end;

procedure TfMsgDlg.FormHide(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

procedure TfMsgDlg.FormClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.TimeLeft := DlgNoTime;
end;

procedure TfMsgDlg.FormShow(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	if (Length(Ignore.Buttons) > 0) and (FButtons[0].Enabled) then
		ActiveControl := FButtons[0];
end;

procedure FreeIgnores;
var i: SG;
begin
	if Ignores <> nil then
	begin
		for i := 0 to Ignores.Count - 1 do
		begin
			Finalize(TIgnore(Ignores.Get(i)^));
		end;
		FreeAndNil(Ignores);
	end;
end;

initialization
{$IFNDEF NoInitialization}
{$ENDIF NoInitialization}

finalization
{$IFNDEF NoFinalization}
	FreeIgnores;
  FreeAndNil(fMsgDlg);
{$ENDIF NoFinalization}
end.
