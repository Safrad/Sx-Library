//* File:     Lib\uError.pas
//* Created:  1999-12-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uError;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDLabel, uWave, uAdd,
	uDForm, uDTimer, uDEdit;

type
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
		Label1: TDLabel;
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
    ButtonDown: TDButton;
		EditIndex: TDEdit;
		ButtonUp: TDButton;
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
		procedure ButtonIgnoreAllClick(Sender: TObject);
		procedure ButtonDownClick(Sender: TObject);
		procedure EditIndexChange(Sender: TObject);
	private
		{ Private declarations }
		ActItem: UG;
		ShiftDown: BG;

		FButtons: array of TDButton;

		procedure DrawTimeLeft;
		procedure BClick(Sender: TObject);
		procedure ShowMes;
		procedure TryClose;
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

type
	TIgnoreAll = (iaNone, iaSame, iaAll);
var
	IgnoreAll: TIgnoreAll;

procedure IE; overload;
procedure IE(ErrorCode: SG); overload;
procedure CreateException;
function ErrorMes(const ErrorCode: U4): string;

procedure ShowMessages;
// Normal Dialog
function MessageD(Msg: string; DlgType: TMsgDlgType;
	Buttons: TDlgButtons): TDlgBtn;
function MessageDEx(Msg: string; DlgType: TMsgDlgType;
	Buttons: array of ShortString; TimeLeft: SG; Owener: TComponent): SG;
// IO Error
procedure IOError(FName: TFileName; const ErrorCode: U4);
function IOErrorRetry(var FName: TFileName; const ErrorCode: U4): Boolean;
// File Error
procedure IOErrorMessage(FName: TFileName; const ErrorMsg: string);
function IOErrorMessageRetry(var FName: TFileName; const ErrorMsg: string): Boolean;
// Internal Error
procedure ErrorMessage(ErrorMsg: string);
function ErrorMessageRetry(ErrorMsg: string): Boolean;

implementation

{$R *.DFM}
uses
	uStrings, uGraph, uDBitmap, uData,
	Registry, MMSystem, Consts, Math;

type
	TStyle = (stNormal, stInternal, stIO, stFile);

	PIgnore = ^TIgnore;
	TIgnore = packed record // 32
		Style: TStyle; // 1
		Retry: B1; // 1
		Ignore: TIgnoreAll; // 1
		DlgType: TMsgDlgType; // 1

		Msg: string; // 4
		ErrorFileName: TFileName; // 4
		Res: S4; // 4

		Buttons: array of ShortString; // 4

		DateTime: TDateTime; // 4
		TimeLeft: U4; // 4
//		Reserved: array[0..3] of U1; // 4
	end;

var
	fIOError: TfIOError;
	Ignores: TData;
	Ignore: PIgnore;
	TickCount, StartTickCount: U8;

procedure IE;
begin
	IE(0);
end;

procedure IE(ErrorCode: SG);
var S: string;
begin
	{$ifopt d+}
	S := 'Internal Error ' + IntToStr(ErrorCode);
	ErrorMessage(S);
	{$else}
	PlayWinSound(wsCriticalStop);
	{$endif}
end;

procedure CreateException;
begin
	asm
	mov eax, $ffffffff
	call eax
	end;
end;

function ErrorMes(const ErrorCode: U4): string;
var
	NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := FormatMessage(
		{FORMAT_MESSAGE_ALLOCATE_BUFFER or}
		FORMAT_MESSAGE_FROM_SYSTEM or
		FORMAT_MESSAGE_IGNORE_INSERTS,
		nil,
		ErrorCode,
		LANG_NEUTRAL or SUBLANG_DEFAULT shl 10,
		PChar(Result),
		MAX_PATH,
		nil);
	SetLength(Result, NewLength);
	Replace(Result, CharCR + CharLF, ' ');
	DelBESpace(Result);
	Result := Result + ' (' + NToS(ErrorCode) + ')';
end;

// TfIOError

procedure TfIOError.DrawTimeLeft;
begin
//	if StartTickCount > 0 then
	PanelTimeLeft.Caption := msToStr(1000 * UG(Ignore.TimeLeft) + StartTickCount - TickCount, diMSD, 0, False);
end;

var
	Captions: array[TMsgDlgType] of string = (SMsgDlgWarning, SMsgDlgError,
		SMsgDlgInformation, SMsgDlgConfirm, '');
	IconIDs: array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND,
		IDI_ASTERISK, IDI_QUESTION, nil);

procedure TfIOError.ButtonOpenClick(Sender: TObject);
var FExt: string;
begin
	FExt := ExtractFileExt(Ignore.ErrorFileName);
	if Length(FExt) > 0 then
		OpenDialogFile.Filter := '(*' + FExt + ')|*' + FExt + '|Any file (*.*)|*.*'
	else
		OpenDialogFile.Filter := 'Any file (*.*)|*.*';
	OpenDialogFile.FileName := Ignore.ErrorFileName;
	if OpenDialogFile.Execute then
	begin
		Ignore.ErrorFileName := OpenDialogFile.FileName;
		Ignore.Res := 3;
		TryClose;
	end;
end;

procedure TfIOError.ShowMes;
var
	IconID: PChar;
	B: SG;
	Bo: BG;
	ButtonWidth: SG;
	Wid, MaxWid, LastLine, LineCount, i, Hei, ButtonCount, x: SG;
const
	BWidth = 81;
	BHeight = 23;
	BSpace = 8;
begin
	Ignore := PIgnore(Ignores.Get(ActItem));
	IconID := nil;

	case Ignore.Style of
	stNormal:
	begin
		if Ignore.DlgType <> mtCustom then
			fIOError.Caption := string(Captions[Ignore.DlgType])
		else
			fIOError.Caption := Application.Title;

		IconID := IconIDs[Ignore.DlgType];
	end;
	stInternal:
	begin
		fIOError.Caption := 'Internal Error';
		IconID := IDI_APPLICATION;
	end;
	stIO:
	begin
		fIOError.Caption := 'I/O Error';
		IconID := IDI_WINLOGO;
	end;
	stFile:
	begin
		fIOError.Caption := 'File Error';
		IconID := IDI_WINLOGO;
	end;
	end;

	if IconID <> nil then
	begin
		fIOError.Image.Name := 'Image';
		fIOError.Image.Picture.Icon.Handle := LoadIcon(0, IconID);
	end;

	fIOError.EditIndex.Text:= NToS(ActItem + 1);
	fIOError.PanelCreated.Caption := DateTimeToStr(Ignore.DateTime);

	LineCount := 1;
	MaxWid := 0;
	LastLine := 1;
	for i := 1 to Length(Ignore.Msg) + 1 do
		if (i = Length(Ignore.Msg) + 1) or (Ignore.Msg[i] = CharLF) then
		begin
			Inc(LineCount);
			Wid := fIOError.Canvas.TextWidth(Copy(Ignore.Msg, LastLine, i - LastLine)) + 4;
			if Wid > MaxWid then MaxWid := Wid;
			LastLine := i + 1;
		end;
	Inc(MaxWid, BSpace + fIOError.MemoMsg.Left);

	x := fIOError.ButtonAll.Left + fIOError.ButtonAll.Width + BSpace;
	if (Ignore.Style = stNormal) or (Ignore.Style = stInternal) then
	begin
		ButtonWidth := x - BSpace;
		ButtonCount := 0;
		for B := 0 to Length(Ignore.Buttons) - 1 do
		begin
	//		if B in Buttons then
			begin
				Inc(ButtonWidth, BWidth + BSpace);
				Inc(ButtonCount);
			end;
		end;
	end
	else
	begin
		ButtonWidth := ButtonOpen.Left + ButtonOpen.Width + BSpace;
		ButtonCount := 0;
	end;

	MaxWid := Max(MaxWid, fIOError.PanelCreated.Left + fIOError.PanelCreated.Width + BSpace);

	if ButtonWidth < MaxWid then
		x := MaxWid - Length(Ignore.Buttons) * (BWidth + BSpace) + BSpace
	else
		MaxWid := Max(MaxWid, ButtonWidth);

(*	if MaxWid < fIOError.ButtonA.Left + fIOError.ButtonA.Width + 8 then
		MaxWid := fIOError.ButtonA.Left + fIOError.ButtonA.Width + 8*)
	if MaxWid > Screen.Width - 2 * (fIOError.Width - fIOError.ClientWidth) then
		MaxWid := Screen.Width - 2 * (fIOError.Width - fIOError.ClientWidth);

	fIOError.MemoMsg.Width := MaxWid - fIOError.MemoMsg.Left - BSpace + 6;
	Hei := Max(LineCount, 3) * fIOError.Canvas.TextHeight(Ignore.Msg) + 6;

	fIOError.MemoMsg.Height := Hei;

	fIOError.MemoMsg.Lines.Clear;
	fIOError.MemoMsg.Lines.Insert(0, ReplaceF(Ignore.Msg, CharLF, CharCR + CharLF));
//	Hei := Max(Hei, ButtonAll.Top + ButtonAll.Height + 6);
	Inc(Hei, fIOError.MemoMsg.Top + BSpace);
	fIOError.ClientWidth := fIOError.MemoMsg.Left + fIOError.MemoMsg.Width + 8;
	fIOError.ClientHeight := Hei + BHeight + BSpace;//fIOError.ButtonA.Top + fIOError.ButtonA.Height + BSpace;

(*	fIOError.LabelTimeLeft.Top := Hei + 20;
	fIOError.PanelTimeLeft.Top := fIOError.LabelTimeLeft.Top;
	fIOError.ButtonA.Top := fIOError.LabelTimeLeft.Top;*)
	fIOError.ButtonAll.Down := Ignore.Ignore <> iaNone;
	fIOError.ButtonAll.Enabled := Ignore.Res = -1;
	fIOError.ButtonAll.Top := Hei;

	// Buttons
	Bo := Ignore.Style >= stIO;
	fIOError.ButtonRetry.Visible := Bo;
	fIOError.ButtonIgnore.Visible := Bo;
//	fIOError.ButtonIgnoreAll.Visible := B;
	fIOError.ButtonOpen.Visible := Bo;

//	fIOError.ButtonExit.Visible := Bo;

	if Bo then
	begin
		fIOError.ButtonOpen.Enabled := (Ignore.Res = -1) and Ignore.Retry and ((Ignore.Style = stIO) or (Ignore.Style = stFile));
		fIOError.ButtonRetry.Enabled := (Ignore.Res = -1) and Ignore.Retry;
		fIOError.ButtonRetry.Default := Ignore.Retry;
		fIOError.ButtonIgnore.Enabled := Ignore.Res = -1;
		if fIOError.ButtonRetry.Enabled then
			fIOError.ActiveControl := fIOError.ButtonRetry
		else if fIOError.ButtonIgnore.Enabled then
			fIOError.ActiveControl := fIOError.ButtonIgnore;


		fIOError.ButtonIgnore.Default := not Ignore.Retry;

		fIOError.ButtonRetry.Top := Hei;
		fIOError.ButtonIgnore.Top := Hei;
		fIOError.ButtonOpen.Top := Hei;

		fIOError.ButtonIgnore.Down := Ignore.Res = 1;
		fIOError.ButtonRetry.Down := (Ignore.Res = 2);
		fIOError.ButtonOpen.Down := (Ignore.Res = 3);

	end;

	i := 0;
	if Length(Ignore.Buttons) > Length(fIOError.FButtons) then SetLength(fIOError.FButtons, Length(Ignore.Buttons));
	for B := 0 to Length(FButtons) - 1 do
	begin
		if B < Length(Ignore.Buttons) then
		begin
			if not Assigned(fIOError.FButtons[B]) then
			begin
				fIOError.FButtons[B] := TDButton.Create(fIOError);
				fIOError.FButtons[B].OnKeyDown := FormKeyDown;
				fIOError.FButtons[B].OnKeyUp := FormKeyUp;
				fIOError.FButtons[B].OnMouseMove := FormMouseMove;

				fIOError.InsertControl(fIOError.FButtons[B]);
			end;

			fIOError.FButtons[B].Name := 'Button' + Ignore.Buttons[B] + IntToStr(B);
			fIOError.FButtons[B].Caption := '/';//ButtonNames[B];
			fIOError.FButtons[B].Visible := True;
//			(fIOError.ButtonAll.Left + fIOError.ButtonAll.Width + BSpace + (BWidth + BSpace) * (2 * i - ButtonCount) + BSpace + MaxWid) div 2;
			fIOError.FButtons[B].Left := x;
			Inc(x, BWidth + BSpace);
			fIOError.FButtons[B].Top := Hei;
			fIOError.FButtons[B].Width := BWidth;
			fIOError.FButtons[B].Height := BHeight;
			fIOError.FButtons[B].OnClick := fIOError.BClick;
			fIOError.FButtons[B].Default := i = 0;
			fIOError.FButtons[B].TabOrder := i + 4;
			fIOError.FButtons[B].TabStop := True;
			fIOError.FButtons[B].Tag := B;
			fIOError.FButtons[B].Down := Ignore.Res = B;
			fIOError.FButtons[B].Enabled := Ignore.Res = -1;
			if (B = 0) and (fIOError.FButtons[B].Enabled) then
				fIOError.ActiveControl := fIOError.FButtons[B];
			fIOError.FButtons[B].Cancel := i = ButtonCount - 1;

			Inc(i);
		end
		else
			fIOError.FButtons[B].Visible := False;

	end;

//	fIOError.ButtonA.Width := fIOError.ClientWidth - 2 * fIOError.ButtonA.Left;
//	fIOError.ButtonA.Top := BTop + BHeight + BSpace;
end;
{
procedure TfIOError.UpDown1ChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: SmallInt;
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
	Timer1Timer(Sender);
end;

procedure TfIOError.Timer1Timer(Sender: TObject);
begin
	TickCount := GetTickCount;
	DrawTimeLeft;
	if TickCount > Ignore.TimeLeft * 1000 + StartTickCount then Close;
end;

function DoForm(
	const Style: TStyle; var FName: TFileName; const ErrorCode: U4;
	ErrorMsg: string; const Retry: Boolean;
	DlgType: TMsgDlgType;
	Buttons: array of ShortString; TimeLeft: SG; Owener: TComponent): SG;

var
	s: string;
	i: SG;
	FoundSame: BG;
	Ignore: PIgnore;
	LineIndex: SG;
	B: SG;
begin
	// Win X pressed
	case Style of
	stNormal, stInternal:
		Result := -1; // None of Button
	else // stIO, stFile:
		Result := 2; // Ignore
	end;

	if Ignores = nil then Exit;

	if Style = stIO then
		ErrorMsg := ErrorMes(ErrorCode);
{	else
		s := ErrorMsg;}
	DelChars(ErrorMsg, '&');
	LineIndex := 1;
	s := ReadToChar(ErrorMsg, LineIndex, LineSep);

	FoundSame := False;
	if IgnoreAll = iaSame then
	begin
		for i := SG(Ignores.Count) - 1 downto 0 do
		begin
			Ignore := Ignores.Get(i);
			LineIndex := 1;
			if (Ignore.Ignore <> iaNone) then
			if (Ignore.Style = Style) and (Ignore.Retry = Retry) and (Ignore.DlgType = DlgType) then
			if (ReadToChar(Ignore.Msg, LineIndex, LineSep) = s) then
			begin
				FoundSame := True;
				Result := Ignore.Res;
				Break;
			end;
		end;
	end;
	if FName <> '' then ErrorMsg := ErrorMsg + LineSep + FName;

	Ignore := Ignores.Add;
	FillChar(Ignore^, SizeOf(Ignore^), 0);
	Ignore.Style := Style;
	Ignore.Retry := Retry;
	Ignore.DlgType := DlgType;
	Ignore.Msg := ErrorMsg;
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

	if (IgnoreAll <> iaAll) and (FoundSame = False) then
	begin
		if Style = stNormal then
			PlayWinSound(wsQuestion)
		else
			PlayWinSound(wsCriticalStop);

		if not Assigned(fIOError) then
		begin
			fIOError := TfIOError.Create(Owener);
			fIOError.Background := baGradient;
		end;

		if Assigned(fIOError) then
		begin
//			fIOError.UpDown1.OnChangingEx := nil;
//			fIOError.UpDown1.Max := Ignores.Count - 1;
//			fIOError.UpDown1.OnChangingEx := fIOError.UpDown1ChangingEx;
			fIOError.PanelCount.Caption := NToS(Ignores.Count);
		end;

		if fIOError.Visible = False then
		begin
			TickCount := GetTickCount;
			StartTickCount := TickCount;
{			fIOError.UpDown1.OnChangingEx := nil;
			fIOError.UpDown1.Position := Ignores.Count - 1;
			fIOError.UpDown1.OnChangingEx := fIOError.UpDown1ChangingEx;}
			fIOError.ActItem := Ignores.Count - 1;
			fIOError.EditIndex.Text := NToS(fIOError.ActItem);
			fIOError.ShowMes;
			fIOError.DrawTimeLeft;

			fIOError.ModalResult := mrNone;
			if Application.Terminated then
			begin
				fIOError.FormStyle := fsStayOnTop;
				fIOError.Timer1.Enabled := True;
				fIOError.Show;
				repeat
					Application.HandleMessage;
				until fIOError.ModalResult <> mrNone;
				fIOError.Hide;
			end
			else
			begin
				fIOError.FormStyle := fsNormal;
//				if fIOError.Visible = True then Exit;
				fIOError.Timer1.Enabled := True;
				fIOError.ShowModal;
			end;
		end
		else
		begin
			while Ignore.Res = -1 do
			begin
//				Application.HandleMessage;
				Application.ProcessMessages;
//				uDTimer.TryTimer;
				Sleep(20);
			end;
		end;

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
		begin
			fIOError.Free;
			fIOError := nil;
		end;
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
begin
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
		Close
	else
	begin
		ActItem := Last;
		ShowMes;
	end;
end;

// Button Clicks

procedure TfIOError.BClick(Sender: TObject);
begin
	Ignore.Res := TDButton(Sender).Tag;
	TryClose;
end;

procedure TfIOError.ButtonRetryClick(Sender: TObject);
begin
	Ignore.Res := 2;
	TryClose;
end;

procedure TfIOError.ButtonIgnoreClick(Sender: TObject);
begin
	Ignore.Res := 1;
	TryClose;
end;

procedure TfIOError.ButtonIgnoreAllClick(Sender: TObject);
begin
{	if fIOError.ShiftDown then
		IgnoreAll := iaAll
	else
		IgnoreAll := iaSame;
	Ignore.Res := ;
	TryClose;}
end;

// Public

procedure ShowMessages;
begin
	if Assigned(fIOError) then
	begin
		fIOError.ShowMes;
		fIOError.ShowModal;
	end
	else
		MessageD('No message found', mtInformation, [mbOk]);
end;

function MessageDEx(Msg: string; DlgType: TMsgDlgType;
	Buttons: array of ShortString; TimeLeft: SG; Owener: TComponent): SG;
var
	FileName: TFileName;
begin
	FileName := '';
	Result := DoForm(stNormal, FileName, 0, Msg, False, DlgType, Buttons, TimeLeft, Owener);
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

procedure IOError(FName: TFileName; const ErrorCode: U4);
begin
	DoForm(stIO, FName, ErrorCode, '', False, mtCustom, [], DlgWait, Application.MainForm);
end;

function IOErrorRetry(var FName: TFileName; const ErrorCode: U4): Boolean;
begin
	Result := DoForm(stIO, FName, ErrorCode, '', True, mtCustom, [], DlgWait, Application.MainForm) > 1;
end;

procedure IOErrorMessage(FName: TFileName; const ErrorMsg: string);
begin
	DoForm(stFile, FName, 0, ErrorMsg, False, mtCustom, [], DlgWait, Application.MainForm);
end;

function IOErrorMessageRetry(var FName: TFileName; const ErrorMsg: string): Boolean;
begin
	Result := DoForm(stFile, FName, 0, ErrorMsg, True, mtCustom, [], DlgWait, Application.MainForm) > 1;
end;

procedure ErrorMessage(ErrorMsg: string);
var FName: TFileName;
begin
	DoForm(stInternal, FName, 0, ErrorMsg, False, mtCustom, ['OK'], DlgWait, Application.MainForm);
end;

function ErrorMessageRetry(ErrorMsg: string): Boolean;
var FName: TFileName;
begin
	Result := DoForm(stInternal, FName, 0, ErrorMsg, True, mtCustom, ['OK'], DlgWait, Application.MainForm) > 1;
end;

procedure TfIOError.ButtonDownClick(Sender: TObject);
begin
	if TDButton(Sender).Tag = 0 then
	begin
		if ActItem > 0 then
			Dec(ActItem);
	end
	else
	begin
		if ActItem < Ignores.Count - 1 then
			Inc(ActItem);
	end;
	ShowMes;
end;

procedure TfIOError.EditIndexChange(Sender: TObject);
begin
	ActItem := StrToValI(EditIndex.Text, True, 1, ActItem + 1, Ignores.Count, 1) - 1;
	ShowMes;
end;

initialization
	Ignores := TData.Create;
	Ignores.ItemSize := SizeOf(TIgnore);
finalization
	Ignores.Free; Ignores := nil;
end.
