unit uMsgDlg;

interface

uses
	Dialogs, SysUtils, Classes, Graphics, Controls, Forms, Consts,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDLabel,
	uDForm, uDTimer, uDEdit,
  uOutputInfo, uTypes, uDWinControl, uDMemo;

type
	TIgnoreAll = (iaNone, iaSame, iaAll);

	PIgnore = ^TIgnore;
	TIgnore = packed record // 32
		MsgType: TMessageLevel; // 1
		Retry: B1; // 1
		Ignore: TIgnoreAll; // 1
		Reserved: U1;

		Text: string; // 4
		Param: array of string; // 4
//		ErrorFileName: TFileName; // 4
		Res: S4; // 4

		Buttons: array of string; // 4

		DateTime: TDateTime; // 8
		TimeLeft: U2; // 4
	end;

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
	private
		{ Private declarations }
		ActItem: SG;
		ShiftDown: BG;

		FButtons: array of TDButton;

		procedure DrawTimeLeft;
		procedure BClick(Sender: TObject);
		procedure ShowMes;
		procedure TryClose;
		procedure ShowForm;
		procedure FillMemo(const Ignore: PIgnore);
	public
		{ Public declarations }
	end;

const
	DlgNoTime = 0;
	DlgWait = 15; // Default
var
  DisplayDialogs: Boolean = True;

procedure ShowMessages;
function MsgDlg(
	const Text: string;
	const Param: array of string;
	const Retry: BG;
	const MsgType: TMessageLevel;
	const Buttons: array of string; const TimeLeft: UG): SG;

function MessageD(const Text: string; const MsgType: TMessageLevel;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn; overload;

function MessageD(const Text: string; const Param: array of string; const MsgType: TMessageLevel;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn; overload;

function DeleteFileDialog(const FileName: TFileName): Boolean;

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

	fMsgDlg: TfMsgDlg;
	Ignores: TData;
	TickCount, StartTickCount: U8;


// TfMsgDlg

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

function MessageLevelToMsgDlgType(const AMessageLevel: TMessageLevel): TMsgDlgType;
begin
  case AMessageLevel of
  mlConfirmation: Result := mtConfirmation;
  mlDebug: Result := mtInformation;
  mlInformation: Result := mtInformation;
  mlWarning: Result := mtWarning;
  mlError: Result := mtError;
  mlFatalError: Result := mtError;
  mlNone: Result := mtCustom;
  else Result := mtCustom;
  end;
end;

procedure PlaySoundForMsgType(const MsgType: TMessageLevel);
begin
  case MsgType of
  mlConfirmation:
    PlayWinSound(wsQuestion);
  mlDebug,
  mlInformation,
  mlWarning:
    PlayWinSound(wsExclamation);
  mlError,
  mlFatalError:
    PlayWinSound(wsCriticalStop);
  end;
end;

const
  MessageLevelToTaskDialog: array[TMessageLevel] of TTaskDialogIcon = (tiQuestion, tiShield, tiInformation, tiWarning, tiError, tiError, tiBlank);

function StringsToString(const AStrings: array of string; const Sepearator: string): string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to Length(AStrings) - 1 do
  begin
    Result := Result + AStrings[i] + Sepearator;
  end;
end;

function MsgDlg(
	const Text: string;
	const Param: array of string;
	const Retry: BG;
	const MsgType: TMessageLevel;
	const Buttons: array of string; const TimeLeft: UG): SG;
var
	i: SG;
	FoundSame: BG;
	Ignore: PIgnore;
	B: SG;
  TaskDialog: TTaskDialog;
begin
	Result := -1; // If Window X is pressed (None of button pressed), then result is unknown.
  if not DisplayDialogs then Exit;

  case VisualOptions.DialogVisualStyle of
  dsWindowsVista:
  begin
    PlaySoundForMsgType(MsgType);
    TaskDialog.Inst := '';
    TaskDialog.Content := ReplaceParam(Text, Param);
    TaskDialog.Buttons := StringsToString(Buttons, FullSep);
    Result := TaskDialog.Execute([], mrOk, [tdfAllowDialogCancellation, tdfUseCommandLinks], MessageLevelToTaskDialog[MsgType]) - 100;
    if Result < -1 then
      Result := -1;
    Exit;
  end;
  end;

	Ignore := nil;
(*
	Assert(Length(Text) > 0);
	s := LastChar(DelEndSpaceF(Text));
	case DlgType of
	mtDebug,
	mtInformation,
	mtWarning,
	mtError,
	mtInternal, mtIO:
	begin
		if not (FirstChar(s) in ['.', '!']) then
			Text := Text + '.';
	end;
	mtConfirmation:
	begin
		if FirstChar(s) <> '?' then
			Text := Text + '?';
	end;
	end;
*)

	if Ignores = nil then
	begin
		Ignores := TData.Create(True);
		Ignores.ItemSize := SizeOf(TIgnore);
	end;

	FoundSame := False;
	if IgnoreAll = iaSame then
	begin
		Ignore := Ignores.GetLast;
		for i := SG(Ignores.Count) - 1 downto 0 do
		begin
			if (Ignore.Ignore <> iaNone) then
			if (Ignore.Retry = Retry) and (Ignore.MsgType = MsgType) then
			if (Ignore.Text {ReadToChar(Ignore.Msg, LineIndex, LineSep)} = Text) then
			begin
				FoundSame := True;
				Result := Ignore.Res;
{				if (IgnoreAll = iaAll) then
				begin
					Ignore.Res := Result;
					Exit;
				end;}
				Break;
			end;
			Dec(PByte(Ignore), Ignores.ItemMemSize);
		end;
	end;

	if FoundSame = False then
//		if Text <> '' then
		begin
	{		if Ignores.Count = 100 then
				Ignores.DeleteFirst; // can not be actual!}
			Ignore := Ignores.Add;
			Ignore.MsgType := MsgType;
			Ignore.Retry := Retry;
			Ignore.Text := Text;
			SetLength(Ignore.Param, Length(Param));
			for i := 0 to Length(Param) - 1 do
				Ignore.Param[i] := Param[i];
(*			if Param <> '' then
				Ignore.Msg := ReplaceParam(Copy(Text, 1, 65536){Memory limit}, Param)
			else
				Ignore.Msg := Copy(Text, 1, 65536){Memory limit};*)
			Ignore.Buttons := nil;
			SetLength(Ignore.Buttons, Length(Buttons));
			for B := 0 to Length(Buttons) - 1 do
			begin
				Ignore.Buttons[B] := Buttons[B];
			end;
			Ignore.Res := -1;
			Ignore.Ignore := iaNone;
			Ignore.DateTime := Now;
			Ignore.TimeLeft := TimeLeft;
		end
{		else
			Ignore := Ignores.GetLast};

	if (IgnoreAll <> iaAll) and (FoundSame = False) and (FormDraw(fMsgDlg) = False) then
	begin
    PlaySoundForMsgType(MsgType);

		ShowDlg;
		Ignore := Ignores.Get(fMsgDlg.ActItem);
//		if ModalResult = mrNone then ModalResult := mrCancel;
		if Ignore.Res = -1 then
		begin
			// X win button pressed
			Ignore.Res := Result
		end
		else
			Result := Ignore.Res;
//		if Ignore.ErrorFileName <> FName then FName := Ignore.ErrorFileName;

//		if IsMultiThread then
		FreeAndNil(fMsgDlg); // Needed for MultiSaver crash in finalize units!
	end
	else
		Ignore.Res := Result;
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

procedure ShowMessages;
begin
	if Assigned(Ignores) and (Ignores.Count > 0) then
	begin
		ShowDlg;
	end
	else
		MessageD('No message found.', mlInformation, [mbOk]);
end;

function MsgDlgBtnToDlgBtn(const AButton: TMsgDlgBtn): TDlgBtn;
const
  MsgDlgBtnToDlgBtnA: array[TMsgDlgBtn] of TDlgBtn = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
    mbAll, mbNoToAll, mbYesToAll, mbHelp{$if CompilerVersion >= 21}, mbClose{$ifend});
begin
  Result := MsgDlgBtnToDlgBtnA[AButton];
end;

function ModalResultToDlgBtn(const AModalResult: TModalResult): TDlgBtn;
const
  ModalResultToDlgBtnA: array[0..{$if CompilerVersion >= 21}12{$else}11{$ifend}] of TDlgBtn = (
    mbHelp{None}, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore, mbYes, mbNo, mbAll, mbNoToAll, mbYesToAll, mbHelp{$if CompilerVersion >= 21}, mbClose{$ifend});
begin
  Result := ModalResultToDlgBtnA[AModalResult];
end;

function DlgButtonsToMsgDlgButtons(const AButtons: TDlgButtons) :TMsgDlgButtons;
var
  B: TMsgDlgBtn;
begin
  Result := [];
  for B := Low(B) to High(B) do
    if MsgDlgBtnToDlgBtn(B) in AButtons then
      Result := Result + [B];
end;

function DlgButtonsToCommonButtons(const Buttons: TDlgButtons): TCommonButtons;
begin
  Result := [];
(*  if mbOK in Buttons then
    Result := Result + [cbOK];
  if mbYes in Buttons then
    Result := Result + [cbYes];
  if mbRetry in Buttons then
    Result := Result + [cbRetry];
  if mbNo in Buttons then
    Result := Result + [cbNo];
  if mbCancel in Buttons then
    Result := Result + [cbCancel];
  if mbClose in Buttons then
    Result := Result + [cbClose];*)
end;

type
  TArrayOfDlgBtn = array of TDlgBtn;

function DlgButtonsToArray(const Buttons: TDlgButtons): TArrayOfDlgBtn;
var
  i: SG;
  Index: SG;
  Count: SG;
begin
	Count := 0;
  for i := 0 to Length(DlgBtnNames) - 1 do
  begin
    if TDlgBtn(i) in Buttons then
    begin
      Inc(Count);
    end;
  end;

	SetLength(Result, Count);

  Index := 0;
  for i := 0 to Length(DlgBtnNames) - 1 do
  begin
    if TDlgBtn(i) in Buttons then
    begin
      Result[Index] := TDlgBtn(i);
      Inc(Index);
    end;
  end;
end;

function IndexToDlgBtn(const Index: SG; const Buttons: TArrayOfDlgBtn): TDlgBtn;
begin
  if Index >= 0 then
    Result := Buttons[Index]
  else
    Result := mbCancel;
end;

function SynDialogIndexToDlgBtn(const Index: SG; const Buttons: TArrayOfDlgBtn): TDlgBtn;
const
  SynDialogButtonOffset = 100;
begin
  if Index >= SynDialogButtonOffset then
    Result := IndexToDlgBtn(Index - SynDialogButtonOffset, Buttons)
  else
    Result := mbCancel;
end;

function DlgButtonsToButtonNames(const Buttons: TArrayOfDlgBtn): TArrayOfString;
var
  i: SG;
begin
	SetLength(Result, Length(Buttons));
  for i := 0 to Length(Buttons) - 1 do
  begin
    Result[i] := DlgBtnNames[Buttons[i]];
  end;
end;

function MessageD(const Text: string; const Param: array of string; const MsgType: TMessageLevel;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn; overload;
var
	ButtonNames: TArrayOfString;
  DlgButtons: TArrayOfDlgBtn;
  TaskDialog: TTaskDialogEx;
begin
	Result := mbCancel;
  ButtonNames := nil;
  DlgButtons := nil;
	if not DisplayDialogs then Exit;

  case VisualOptions.DialogVisualStyle of
  dsWindowsXP:
  begin
    Result := ModalResultToDlgBtn(Dialogs.MessageDlg(ReplaceParam(Text, Param), MessageLevelToMsgDlgType(MsgType), DlgButtonsToMsgDlgButtons(Buttons), 0));
    Exit;
  end;
  end;

  DlgButtons := DlgButtonsToArray(Buttons);
  ButtonNames := DlgButtonsToButtonNames(DlgButtons);

  case VisualOptions.DialogVisualStyle of
  dsWindowsVista:
  begin
    PlaySoundForMsgType(MsgType);
    TaskDialog.Init;
    TaskDialog.Base.Inst := '';
    TaskDialog.Base.Content := ReplaceParam(Text, Param);
    TaskDialog.Base.Buttons := StringsToString(ButtonNames, FullSep);
    TaskDialog.CommonButtons := DlgButtonsToCommonButtons(Buttons);
    TaskDialog.ButtonDef := mrOk;
    TaskDialog.Flags := [tdfAllowDialogCancellation, tdfUseCommandLinks];
    TaskDialog.DialogIcon := MessageLevelToTaskDialog[MsgType];

    Result := SynDialogIndexToDlgBtn(TaskDialog.Execute(Application.Handle), DlgButtons);
  end;
  dsSxLibrary:
  begin
    Result := IndexToDlgBtn(MsgDlg(Text, Param, False, MsgType, ButtonNames, DlgWait), DlgButtons);
  end;
  end;
end;

function MessageD(const Text: string; const MsgType: TMessageLevel;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn; overload;
begin
	Result := MessageD(Text, [], MsgType, Buttons, TimeLeft);
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

function DeleteFileDialog(const FileName: TFileName): Boolean;
begin
	Result := False;
	if MessageD('Delete file %1.', [FileName], mlConfirmation, [mbYes, mbNo]) = mbYes then
		Result := DeleteFileEx(FileName);
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
