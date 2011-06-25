//* File:     Lib\GUI\uMsgDlg.pas
//* Created:  1999-12-01
//* Modified: 2008-02-16
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uMsgDlg;

interface

uses
	Dialogs, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Consts,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDLabel, uWave,
	uDForm, uDTimer, uDEdit, uMsg, uTypes, uDWinControl;

type
	TfMsgDlg = class(TDForm)
		ButtonExit: TDButton;
		OpenDialogFile: TOpenDialog;
		MemoMsg: TMemo;
		LabelX: TLabel;
		PanelCount: TDLabel;
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
	public
		{ Public declarations }
	end;

const
	DlgNoTime = 0;
	DlgWait = 15;

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
	uFiles,
	uStrings, uGraph, uDBitmap, uData, uInputFormat, uOutputFormat, uSimulation,
	Registry, MMSystem, Math;

type
	TIgnoreAll = (iaNone, iaSame, iaAll);
var
	IgnoreAll: TIgnoreAll;
type
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

		Buttons: array of ShortString; // 4

		DateTime: TDateTime; // 8
		TimeLeft: U2; // 4
	end;

var
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
		PanelTimeLeft.Caption := MsToStr(Second * S8(Ignore.TimeLeft) - 1 - TimeDifference(TickCount, StartTickCount), diMSD, 0, False);
end;

const
	IconIDs: array[TMessageLevel] of PChar = (IDI_QUESTION, IDI_WINLOGO, IDI_INFORMATION, IDI_WARNING, IDI_ERROR,
		IDI_APPLICATION, '');

procedure TfMsgDlg.ShowMes;
var
	IconID: PChar;
	i, LastLine: SG;
	Wid, Hei, BWid, MaxWid, LineCount: SG;
	R: TRect;
	Ignore: PIgnore;
	Msg: string;
const
	BMinWidth = 81;
	BHeight = 23;
begin
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
		Image.Picture.Icon.Handle := LoadIcon(0, IconID);
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
			Wid := Canvas.TextWidth(Copy(Msg, LastLine, i - LastLine)) + 4 + FormBorder + MemoMsg.Left;
			if Wid > MaxWid then MaxWid := Wid;
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

	GetDesktopRect(R);

	Hei := Max(LineCount, 3) * Canvas.TextHeight(Msg) + 6;
	Wid := Canvas.TextHeight('W') * ((R.Bottom - R.Top - 128{TaskBar} - 2 * (Height - ClientHeight)) div Canvas.TextHeight('W')) + 6;
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

	MemoMsg.Lines.BeginUpdate;
//	MemoMsg.Lines.Clear;
//	MemoMsg.Lines.Insert(0, ReplaceF(Ignore.Msg, CharLF, CharCR + CharLF));
	MemoMsg.Text := ReplaceF(Msg, CharLF, CharCR + CharLF);
	MemoMsg.Lines.EndUpdate;
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
		TerminateProcess(GetCurrentProcess, 1);
//		Halt;
	end;
end;

procedure TfMsgDlg.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = False) and (Key = VK_SHIFT) then
	begin
		ShiftDown := True;
		ButtonAll.Caption := 'Never Show';
		ButtonExit.Caption := 'Terimante';
	end;
end;

procedure TfMsgDlg.FormKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = True) and (Key = VK_SHIFT) then
	begin
		ShiftDown := False;
		ButtonAll.Caption := 'Use Answer for All';
		ButtonExit.Caption := 'Close Program';
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
	PanelCount.Caption := NToS(Ignores.Count);

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
		if Application.Terminated then
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
		begin
			FormStyle := fsNormal;
			Timer1.Enabled := True;
			ShowModal;
		end;
	end
	else
	begin
		Ignore :=Ignores.Get(ActItem);
		Ignore.Res := -1;
		Exit;
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
begin
	Ignore := nil;
(*	{$ifopt d+}
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
			Text := Text + {$ifopt d+}'.%'{$else}'.'{$endif};
	end;
	mtConfirmation:
	begin
		if FirstChar(s) <> '?' then
			Text := Text + {$ifopt d+}'?%'{$else}'?'{$endif};
	end;
	end;
	{$endif} *)


	Result := -1; // If Window X is pressed (None of button pressed), then result is unknown.

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
			Dec(SG(Ignore), Ignores.ItemMemSize);
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

		if not Assigned(fMsgDlg) then
		begin
			fMsgDlg := TfMsgDlg.Create(Application.MainForm);
			fMsgDlg.Background := baGradient;
		end;
		fMsgDlg.ShowForm;
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

		if IsMultiThread then
			FreeAndNil(fMsgDlg);
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
		fMsgDlg.ShowForm;
	end
	else
		MessageD('No message found.', mlInformation, [mbOk]);
end;

function MessageD(const Text: string; const Param: array of string; const MsgType: TMessageLevel;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn; overload;
var
	B: TDlgBtn;
	But: array of string;
	Res, i: SG;
begin
	i := 0;
	for B := Low(B) to High(B) do
		if B in Buttons then Inc(i);
	SetLength(But, i);
	i := 0;
	for B := Low(B) to High(B) do
		if B in Buttons then
		begin
			But[i] := DlgBtnNames[B];
			Inc(i);
		end;

	Res := MsgDlg(Text, Param, False, MsgType, But, DlgWait);

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

initialization

finalization
	FreeAndNil(Ignores);
end.
