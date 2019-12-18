unit uMsgDlg;

interface

uses
  uTypes,
  uOutputInfo;

const
	DlgNoTime = 0;

  /// [seconds] Default
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

implementation

uses
  SysUtils,

  uVisualOptions,
{$ifdef MSWINDOWS}
  uPlaySound,

  // XP
  Vcl.Dialogs,

  // Vista
  SynTaskDialog,
  Vcl.Forms,

   // SxLibrary
  ufMsgDlg,
  uDForm,
{$else}
  FMX.Forms,
{$endif}
  uIgnore,
	uStrings,
  uData,
	UITypes;

var
  IgnoreAll: TIgnoreAll;
  Ignores: TData;

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

{$ifdef MSWINDOWS}
const
  MessageLevelToTaskDialog: array[TMessageLevel] of SynTaskDialog.TTaskDialogIcon = (
    SynTaskDialog.TTaskDialogIcon.tiQuestion,
    SynTaskDialog.TTaskDialogIcon.tiShield,
    SynTaskDialog.TTaskDialogIcon.tiInformation,
    SynTaskDialog.TTaskDialogIcon.tiWarning,
    SynTaskDialog.TTaskDialogIcon.tiError,
    SynTaskDialog.TTaskDialogIcon.tiError,
    SynTaskDialog.TTaskDialogIcon.tiBlank);

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
  TaskDialog: SynTaskDialog.TTaskDialog;
begin
	Result := -1; // If Window X is pressed (None of button pressed), then result is unknown.
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
{$endif}

procedure ShowMessages;
begin
{$ifdef MSWINDOWS}
	if Assigned(Ignores) and (Ignores.Count > 0) then
	begin
		ShowDlg;
	end
	else
		MessageD('No message found.', mlInformation, [TDlgBtn.mbOk]);
{$endif}
end;

function MsgDlgBtnToDlgBtn(const AButton: TMsgDlgBtn): TDlgBtn;
const
  MsgDlgBtnToDlgBtnA: array[TMsgDlgBtn] of TDlgBtn = (
    TDlgBtn.mbYes,
    TDlgBtn.mbNo,
    TDlgBtn.mbOK,
    TDlgBtn.mbCancel,
    TDlgBtn.mbAbort,
    TDlgBtn.mbRetry,
    TDlgBtn.mbIgnore,
    TDlgBtn.mbAll,
    TDlgBtn.mbNoToAll,
    TDlgBtn.mbYesToAll,
    TDlgBtn.mbHelp,
    TDlgBtn.mbClose);
begin
  Result := MsgDlgBtnToDlgBtnA[AButton];
end;

function ModalResultToDlgBtn(const AModalResult: TModalResult): TDlgBtn;
const
  ModalResultToDlgBtnA: array[0..12] of TDlgBtn = (
    TDlgBtn.mbHelp{None},
    TDlgBtn.mbOK,
    TDlgBtn.mbCancel,
    TDlgBtn.mbAbort,
    TDlgBtn.mbRetry,
    TDlgBtn.mbIgnore,
    TDlgBtn.mbYes,
    TDlgBtn.mbNo,
    TDlgBtn.mbAll,
    TDlgBtn.mbNoToAll,
    TDlgBtn.mbYesToAll,
    TDlgBtn.mbHelp,
    TDlgBtn.mbClose);
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
  if TDlgBtn.mbOK in Buttons then
    Result := Result + [cbOK];
  if TDlgBtn.mbYes in Buttons then
    Result := Result + [cbYes];
  if TDlgBtn.mbRetry in Buttons then
    Result := Result + [cbRetry];
  if TDlgBtn.mbNo in Buttons then
    Result := Result + [cbNo];
  if TDlgBtn.mbCancel in Buttons then
    Result := Result + [cbCancel];
  if TDlgBtn.mbClose in Buttons then
    Result := Result + [cbClose];
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
    Result := TDlgBtn.mbCancel;
end;

function SynDialogIndexToDlgBtn(const Index: SG; const Buttons: TArrayOfDlgBtn): TDlgBtn;
const
  SynDialogButtonOffset = 100;
begin
  if Index >= SynDialogButtonOffset then
    Result := IndexToDlgBtn(Index - SynDialogButtonOffset, Buttons)
  else
    Result := TDlgBtn.mbCancel;
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
	Result := TDlgBtn.mbCancel;
  ButtonNames := nil;
  DlgButtons := nil;

  case VisualOptions.DialogVisualStyle of
  dsWindowsXP:
  begin
    Result := ModalResultToDlgBtn(Vcl.Dialogs.MessageDlg(ReplaceParam(Text, Param), MessageLevelToMsgDlgType(MsgType), DlgButtonsToMsgDlgButtons(Buttons), 0));
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

end.
