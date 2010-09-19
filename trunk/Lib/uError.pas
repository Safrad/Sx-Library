//* File:     Lib\uError.pas
//* Created:  1999-12-01
//* Modified: 2005-09-25
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uError;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Consts,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDLabel, uWave, uTypes,
	uDForm, uDTimer, Dialogs, uDEdit;

type
	TDlgType = (
		mtDebug,
		mtInformation,
		mtWarning,
		mtError,
		mtConfirmation,
		mtInternal,
		mtIO);

	TDlgBtn = (
		mbOK, mbYes, mbYesToAll,
		mbRetry, mbIgnore, mbAbort,
		mbDelete, mbDeleteAll,
		mbNo, mbNoToAll, mbCancel);
	TDlgButtons = set of TDlgBtn;

	TfIOError = class(TDForm)
		ButtonRetry: TDButton;
		ButtonIgnore: TDButton;
		ButtonIgnoreAll: TDButton;
		ButtonExit: TDButton;
		ButtonOpen: TDButton;
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
		procedure ButtonOpenClick(Sender: TObject);
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
	DlgBtnNames: array[TDlgBtn] of string = (
		SMsgDlgOK, SMsgDlgYes, SMsgDlgYesToAll,
		SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAbort,
		'&Delete', 'Delete All',
		SMsgDlgNo, SMsgDlgNoToAll, SMsgDlgCancel);

	DlgNoTime = 0;
	DlgWait = 15;

procedure ShowMessages;
function DoForm(
	var FName: TFileName; // Used when DlgType is mtIO
	ErrorMsg: string; const Retry: BG;
	const DlgType: TDlgType;
	const Buttons: array of string; const TimeLeft: UG): SG;
function MessageD(const Msg: string; const DlgType: TDlgType;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn;
function MessageDEx(const Msg: string; const DlgType: TDlgType;
	const Buttons: array of string; const TimeLeft: UG = DlgWait): SG;

function DeleteFileDialog(const FileName: TFileName): Boolean;

implementation

{$R *.DFM}
uses
	uFiles,
	uStrings, uGraph, uDBitmap, uData, uInput, uFormat, uSimulation,
	Registry, MMSystem, Math;

type
	TIgnoreAll = (iaNone, iaSame, iaAll);
var
	IgnoreAll: TIgnoreAll;
type
	PIgnore = ^TIgnore;
	TIgnore = packed record // 32
		DlgType: TDlgType; // 1
		Retry: B1; // 1
		Ignore: TIgnoreAll; // 1
		Reserved: U1;

		Msg: string; // 4
		ErrorFileName: TFileName; // 4
		Res: S4; // 4

		Buttons: array of ShortString; // 4

		DateTime: TDateTime; // 8
		TimeLeft: U2; // 4
	end;

var
	fIOError: TfIOError;
	Ignores: TData;
	TickCount, StartTickCount: U8;


// TfIOError

procedure TfIOError.DrawTimeLeft;
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	if Ignore.TimeLeft = 0 then
		PanelTimeLeft.Caption := ''
	else
		PanelTimeLeft.Caption := MsToStr(Second * S8(Ignore.TimeLeft) - 1 - TimeDifference(TickCount, StartTickCount), diMSD, 0, False);
end;

var
	Captions: array[TDlgType] of string = ('Debug', SMsgDlgInformation, SMsgDlgWarning, SMsgDlgError, SMsgDlgConfirm,
		'Internal', 'I/O');
	IconIDs: array[TDlgType] of PChar = (IDI_WINLOGO, IDI_ASTERISK, IDI_EXCLAMATION, IDI_HAND, IDI_QUESTION,
		IDI_APPLICATION, IDI_WINLOGO);

procedure TfIOError.ButtonOpenClick(Sender: TObject);
var
	FExt: string;
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	FExt := ExtractFileExt(Ignore.ErrorFileName);
	if Length(FExt) > 0 then
		OpenDialogFile.Filter := '(*' + FExt + ')|*' + FExt + '|Any file (*.*)|*.*'
	else
		OpenDialogFile.Filter := 'Any file (*.*)|*.*';
	if ExecuteDialog(OpenDialogFile, Ignore.ErrorFileName) then
	begin
		Ignore.Res := 3;
		TryClose;
	end;
end;

procedure TfIOError.ShowMes;
var
	IconID: PChar;
	Bo: BG;
	i, LastLine: SG;
	Wid, Hei, BWid, MaxWid, LineCount: SG;
	R: TRect;
	Ignore: PIgnore;
const
	BMinWidth = 81;
	BHeight = 23;
begin
	Ignore :=Ignores.Get(ActItem);

	// Captions
	EditIndex.OnChange := nil;
	EditIndex.Text:= NToS(ActItem + 1);
	EditIndex.OnChange := EditIndexChange;
	PanelCreated.Caption := DTToStr(Ignore.DateTime);
	Caption := Captions[Ignore.DlgType];

	IconID := IconIDs[Ignore.DlgType];

	// Image
	if IconID <> nil then
	begin
		Image.Name := 'Image';
		Image.Picture.Icon.Handle := LoadIcon(0, IconID);
	end;

	MaxWid := PanelCreated.Left + PanelCreated.Width + FormBorder;

	// Message Text
	LineCount := 1;
	LastLine := 1;
	for i := 1 to Length(Ignore.Msg) + 1 do
		if (i = Length(Ignore.Msg) + 1) or (Ignore.Msg[i] = CharLF) then
		begin
			Inc(LineCount);
			Wid := Canvas.TextWidth(Copy(Ignore.Msg, LastLine, i - LastLine)) + 4 + FormBorder + MemoMsg.Left;
			if Wid > MaxWid then MaxWid := Wid;
			LastLine := i + 1;
		end;

	// Buttons
	Bo := Ignore.DlgType in [mtIO];
	ButtonRetry.Visible := Bo;
	ButtonIgnore.Visible := Bo;
//	ButtonIgnoreAll.Visible := B;
	ButtonOpen.Visible := Bo;

//	ButtonExit.Visible := Bo;

	BWid := 0;
//	if Bo = False then
	begin
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
	end;
	if Bo then
	begin
		ButtonOpen.Enabled := (Ignore.Res = -1) and Ignore.Retry;
		ButtonRetry.Enabled := (Ignore.Res = -1) and Ignore.Retry;
		ButtonRetry.Default := Ignore.Retry;
		ButtonIgnore.Enabled := Ignore.Res = -1;
		if ButtonRetry.Enabled then
			ActiveControl := ButtonRetry
		else if ButtonIgnore.Enabled then
			ActiveControl := ButtonIgnore;


		ButtonIgnore.Default := not Ignore.Retry;

		ButtonIgnore.Down := Ignore.Res = 1;
		ButtonRetry.Down := (Ignore.Res = 2);
		ButtonOpen.Down := (Ignore.Res = 3);

		Wid := ButtonOpen.Left + ButtonOpen.Width + FormBorder;
	end;

	MaxWid := Max(MaxWid, Wid);

	GetDesktopRect(R);

	Hei := Max(LineCount, 3) * Canvas.TextHeight(Ignore.Msg) + 6;
	Wid := Canvas.TextHeight(Ignore.Msg) * ((R.Bottom - R.Top - 128{TaskBar} - 2 * (Height - ClientHeight)) div Canvas.TextHeight(Ignore.Msg)) + 6;
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
	MemoMsg.Text := ReplaceF(Ignore.Msg, CharLF, CharCR + CharLF);
	MemoMsg.Lines.EndUpdate;
//	Hei := Max(Hei, ButtonAll.Top + ButtonAll.Height + 6);
	Inc(Hei, MemoMsg.Top + FormBorder);
	ClientWidth := MaxWid;
	ClientHeight := Hei + BHeight + FormBorder;//ButtonA.Top + ButtonA.Height + BSpace;

	ButtonAll.Down := Ignore.Ignore <> iaNone;
	ButtonAll.Enabled := Ignore.Res = -1;
	ButtonAll.Top := Hei;

	if Bo = False then
	begin
		Wid := MaxWid - BWid;
		for i := 0 to Length(Ignore.Buttons) - 1 do
		begin
			FButtons[i].SetBounds(Wid, Hei, FButtons[i].Width, FButtons[i].Height);
			Inc(Wid, FButtons[i].Width + FormBorder);
			FButtons[i].Visible := True;
		end;
	end
	else
	begin
		ButtonRetry.Top := Hei;
		ButtonIgnore.Top := Hei;
		ButtonOpen.Top := Hei;
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

procedure TfIOError.ButtonExitClick(Sender: TObject);
begin
	if Assigned(Application.MainForm) and (ShiftDown = False){(GetAsyncKeyState(VK_SHIFT) = 0)} then
		Application.MainForm.Close
	else
	begin
		TerminateProcess(GetCurrentProcess, 1);
//		Halt;
	end;
end;

procedure TfIOError.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = False) and (Key = VK_SHIFT) then
	begin
		ShiftDown := True;
		ButtonAll.Caption := 'Never Show';
		ButtonExit.Caption := 'Terimante';
	end;
end;

procedure TfIOError.FormKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = True) and (Key = VK_SHIFT) then
	begin
		ShiftDown := False;
		ButtonAll.Caption := 'Use Answer for All';
		ButtonExit.Caption := 'Close Program';
	end;
end;

procedure TfIOError.FormMouseMove(Sender: TObject; Shift: TShiftState;
	X, Y: Integer);
begin
	StartTickCount := TickCount;
	DrawTimeLeft;
end;

procedure TfIOError.Timer1Timer(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	TickCount := GetTickCount;
	DrawTimeLeft;
	if (Ignore.TimeLeft > 0) and (TickCount > U8(Ignore.TimeLeft) * 1000 + StartTickCount) then Close;
end;

procedure TfIOError.ShowForm;
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

function DoForm(
	var FName: TFileName; // Used as FileName when DlgType is mtIO
	ErrorMsg: string; const Retry: BG;
	const DlgType: TDlgType;
	const Buttons: array of string; const TimeLeft: UG): SG;
var
	s: string;
	i: SG;
	FoundSame: BG;
	Ignore: PIgnore;
	LineIndex: SG;
	B: SG;
begin
	{$ifopt d+}
	Assert(Length(ErrorMsg) > 0);
	s := LastChar(DelEndSpaceF(ErrorMsg));
	case DlgType of
	mtDebug,
	mtInformation,
	mtWarning,
	mtError,
	mtInternal, mtIO:
	begin
		if not (s[1] in ['.', '!']) then
			ErrorMsg := ErrorMsg + '%.%';
	end;
	mtConfirmation:
	begin
		if s[1] <> '?' then
			ErrorMsg := ErrorMsg + '%?%';
	end;
	end;
	{$endif}

	// Win X pressed
	case DlgType of
	mtIO:
		Result := 0; // Ignore
	else
		Result := -1; // None of Button
	end;

	if Ignores = nil then
	begin
		Ignores := TData.Create(True);
		Ignores.ItemSize := SizeOf(TIgnore);
	end;

	LineIndex := 1;
	s := ReadToChar(ErrorMsg, LineIndex, LineSep);

	FoundSame := False;
	if IgnoreAll = iaSame then
	begin
		Ignore := Ignores.GetLast;
		for i := SG(Ignores.Count) - 1 downto 0 do
		begin
			LineIndex := 1;
			if (Ignore.Ignore <> iaNone) then
			if (Ignore.Retry = Retry) and (Ignore.DlgType = DlgType) then
			if (ReadToChar(Ignore.Msg, LineIndex, LineSep) = s) then
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
		if ErrorMsg <> '' then
		begin
	{		if Ignores.Count = 100 then
				Ignores.DeleteFirst; // can not be actual!}
			Ignore := Ignores.Add;
			Ignore.DlgType := DlgType;
			Ignore.Retry := Retry;
			if FName <> '' then
				Ignore.Msg := FName + LineSep;
			Ignore.Msg := Ignore.Msg + Copy(ErrorMsg, 1, 65536);
			Ignore.ErrorFileName := FName;
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
		else
			Ignore := Ignores.GetLast;

	if (IgnoreAll <> iaAll) and (FoundSame = False) and (FormDraw(fIOError) = False) then
	begin
		case DlgType of
		mtWarning:
			PlayWinSound(wsExclamation);
		mtError:
			PlayWinSound(wsCriticalStop);
		else
			PlayWinSound(wsQuestion);
		end;

		if not Assigned(fIOError) then
		begin
			fIOError := TfIOError.Create(Application.MainForm);
			fIOError.Background := baGradient;
		end;
		fIOError.ShowForm;
		Ignore :=Ignores.Get(fIOError.ActItem);
//		if ModalResult = mrNone then ModalResult := mrCancel;
		if Ignore.Res = -1 then
		begin
			// X win button pressed
			Ignore.Res := Result
		end
		else
			Result := Ignore.Res;
		if Ignore.ErrorFileName <> FName then FName := Ignore.ErrorFileName;

		if IsMultiThread then
			FreeAndNil(fIOError);
	end
	else
		Ignore.Res := Result;
end;

procedure TfIOError.LabelTimeLeftClick(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

procedure TfIOError.TryClose;
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
		if fIOError.ShiftDown then
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

procedure TfIOError.BClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.Res := TDButton(Sender).Tag;
	TryClose;
end;

procedure TfIOError.ButtonRetryClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.Res := 2;
	TryClose;
end;

procedure TfIOError.ButtonIgnoreClick(Sender: TObject);
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
		fIOError.ShowForm;
	end
	else
		MessageD('No message found.', mtInformation, [mbOk]);
end;

function MessageDEx(const Msg: string; const DlgType: TDlgType;
	const Buttons: array of string; const TimeLeft: UG = DlgWait): SG;
var
	FileName: TFileName;
begin
	FileName := '';
	Result := DoForm(FileName, Msg, False, DlgType, Buttons, TimeLeft);
end;

function MessageD(const Msg: string; const DlgType: TDlgType;
	const Buttons: TDlgButtons; const TimeLeft: UG = DlgWait): TDlgBtn;
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

	Res := MessageDEx(Msg, DlgType, But, DlgWait);

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

procedure TfIOError.ButtonLeftClick(Sender: TObject);
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

procedure TfIOError.EditIndexChange(Sender: TObject);
begin
	ActItem := StrToValI(EditIndex.Text, True, 1, ActItem + 1, Ignores.Count, 1) - 1;
	ShowMes;
end;

procedure TfIOError.FormHide(Sender: TObject);
begin
	Timer1.Enabled := False;
end;

procedure TfIOError.FormClick(Sender: TObject);
var
	Ignore: PIgnore;
begin
	Ignore := Ignores.Get(ActItem);
	Ignore.TimeLeft := DlgNoTime;
end;

function DeleteFileDialog(const FileName: TFileName): Boolean;
begin
	Result := False;
	if MessageD('Delete file' + LineSep + FileName, mtConfirmation, [mbYes, mbNo]) = mbYes then
		Result := DeleteFileEx(FileName);
end;

procedure TfIOError.FormShow(Sender: TObject);
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
