// Build: 12/1999-04/2000 Author: Safranek David

unit uError;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDLabel, uWave, uAdd,
	uDForm;

type
	TfIOError = class(TDForm)
    ButtonRetry: TDButton;
    ButtonIgnore: TDButton;
		ButtonIgnoreAll: TDButton;
		ButtonExit: TDButton;
		Image1: TImage;
		ButtonOpen: TDButton;
		OpenDialogFile: TOpenDialog;
		MemoMsg: TMemo;
		UpDown1: TUpDown;
		Label1: TDLabel;
    PanelIndex: TDLabel;
    PanelCount: TDLabel;
		procedure ButtonOpenClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
			NewValue: SmallInt; Direction: TUpDownDirection);
		procedure ButtonExitClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
	private
		{ Private declarations }
		ErrorFileName: TFileName;
		ShiftDown: BG;
	public
		{ Public declarations }
	end;

type
	TIgnoreAll = (iaNone, iaSame, iaAll);
var
	IgnoreAll: TIgnoreAll;
	SndError: PWave;

procedure IE; overload;
procedure IE(ErrorCode: SG); overload;
procedure CreateException;
function ErrorMes(const ErrorCode: U4): string;

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
	uStrings, uGraph, uDBitmap,
	Registry, MMSystem;
var
	fIOError: TfIOError;
	IOErrorMessages: TStrings;
type
	TStyle = (stIO, stFile, stInternal);

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
	Result := Replace(Result, #$0D + #$0A, ' ');
	DelBESpace(Result);
	Result := Result + ' (' + NToS(ErrorCode) + ')';
end;

function DoForm(const Style: TStyle; var FName: TFileName; const ErrorCode: U4;
	const ErrorMsg: string; const Retry: Boolean): Boolean;
var
	s: string;
	i: SG;
	FoundSame: BG;
begin
	Result := False;
	if IOErrorMessages = nil then IOErrorMessages := TStringList.Create;
	if Style = stIO then
		s := ErrorMes(ErrorCode)
	else
		s := ErrorMsg;
	if FName <> '' then s := s + #13 + #10 + FName;

	FoundSame := False;
	if IgnoreAll = iaSame then
	begin
		for i := 0 to IOErrorMessages.Count - 1 do
		begin
			if s = IOErrorMessages[i] then
			begin
				FoundSame := True;
				Break;
			end;
		end;
	end;
	IOErrorMessages.Add(s);

	if (IgnoreAll <> iaAll) and (FoundSame = False) then
	begin
		PlayWinSound(wsCriticalStop);

		if not Assigned(fIOError) then
		begin
			fIOError := TfIOError.Create(Application.MainForm);
			fIOError.Background := baGradient;
		end;
		fIOError.ErrorFileName := FName;
		case Style of
		stIO: fIOError.Caption := 'IO Error';
		stFile: fIOError.Caption := 'File Error';
		stInternal: fIOError.Caption := 'Internal Error';
		end;

		if Style = stIO then
		begin
			fIOError.Image1.Width := fIOError.Image1.Picture.Width;
			fIOError.Image1.Height := fIOError.Image1.Picture.Height;
		end
		else
		begin
			fIOError.Image1.Width := 32;
			fIOError.Image1.Height := 32;
		end;

		fIOError.MemoMsg.Lines.Clear;
		fIOError.MemoMsg.Lines.Add(s);

		fIOError.ButtonRetry.Enabled := Retry;
		fIOError.ButtonOpen.Enabled := Retry and ((Style = stIO) or (Style = stFile));
		fIOError.ButtonIgnore.Default := not Retry;
		fIOError.UpDown1.OnChangingEx := nil;
		fIOError.UpDown1.Max := IOErrorMessages.Count - 1;
		fIOError.UpDown1.Position := IOErrorMessages.Count - 1;
		fIOError.UpDown1.OnChangingEx := fIOError.UpDown1ChangingEx;
		fIOError.PanelIndex.Caption := NToS(IOErrorMessages.Count);
		fIOError.PanelCount.Caption := NToS(IOErrorMessages.Count);
		fIOError.ModalResult := mrNone;

		if Application.Terminated then
		begin
			fIOError.FormStyle := fsStayOnTop;
			fIOError.Show;
			repeat
				Application.HandleMessage;
			until fIOError.ModalResult <> mrNone;
			fIOError.Hide;
		end
		else
		begin
			fIOError.FormStyle := fsNormal;
			if fIOError.Visible = True then Exit;
			fIOError.ShowModal;
		end;

		case fIOError.ModalResult of
		mrAll:
		begin
			if fIOError.ShiftDown then
				IgnoreAll := iaAll
			else
				IgnoreAll := iaSame;
		end;
		mrRetry:
		begin
			if fIOError.ErrorFileName <> '' then FName := fIOError.ErrorFileName;
			Result := True;
		end;
		end;
		if IsMultiThread then
		begin
			fIOError.Free;
			fIOError := nil;
		end;
	end;
end;

procedure IOError(FName: TFileName; const ErrorCode: U4);
begin
	DoForm(stIO, FName, ErrorCode, '', False);
end;

function IOErrorRetry(var FName: TFileName; const ErrorCode: U4): Boolean;
begin
	Result := DoForm(stIO, FName, ErrorCode, '', True);
end;

procedure IOErrorMessage(FName: TFileName; const ErrorMsg: string);
begin
	DoForm(stFile, FName, 0, ErrorMsg, False);
end;

function IOErrorMessageRetry(var FName: TFileName; const ErrorMsg: string): Boolean;
begin
	Result := DoForm(stFile, FName, 0, ErrorMsg, True);
end;

procedure ErrorMessage(ErrorMsg: string);
var FName: TFileName;
begin
	DoForm(stInternal, FName, 0, ErrorMsg, False);
end;

function ErrorMessageRetry(ErrorMsg: string): Boolean;
var FName: TFileName;
begin
	Result := DoForm(stInternal, FName, 0, ErrorMsg, True);
end;

procedure TfIOError.ButtonOpenClick(Sender: TObject);
var FExt: string;
begin
	FExt := ExtractFileExt(ErrorFileName);
	if Length(FExt) > 0 then
		OpenDialogFile.Filter := '(*' + FExt + ')|*' + FExt + '|Any file (*.*)|*.*'
	else
		OpenDialogFile.Filter := 'Any file (*.*)|*.*';
	OpenDialogFile.FileName := ErrorFileName;
	if OpenDialogFile.Execute then
	begin
		ErrorFileName := OpenDialogFile.FileName;
		ModalResult := mrRetry;
	end;
end;

procedure TfIOError.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	if ModalResult = mrNone then ModalResult := mrCancel;
end;

procedure TfIOError.UpDown1ChangingEx(Sender: TObject;
	var AllowChange: Boolean; NewValue: SmallInt;
	Direction: TUpDownDirection);
begin
	if NewValue < 0 then
	begin
		AllowChange := False;
		Exit;
	end;
	if NewValue >= IOErrorMessages.Count then
	begin
		AllowChange := False;
		Exit;
	end;
	PanelIndex.Caption := NToS(NewValue + 1);
	MemoMsg.Lines.Clear;
	MemoMsg.Lines.Insert(0, IOErrorMessages[NewValue]);
end;

procedure TfIOError.ButtonExitClick(Sender: TObject);
begin
	if Assigned(Application.MainForm) and (ShiftDown = False){(GetAsyncKeyState(VK_SHIFT) = 0)} then
		Application.MainForm.Close
	else
		TerminateProcess(GetCurrentProcess, 1); //Halt;
end;

procedure TfIOError.FormKeyDown(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = False) and (Key = VK_SHIFT) then
	begin
		ShiftDown := True;
		ButtonIgnoreAll.Caption := 'Ignore Realy All';
		ButtonExit.Caption := 'Terimante';
	end;
end;

procedure TfIOError.FormKeyUp(Sender: TObject; var Key: Word;
	Shift: TShiftState);
begin
	if (ShiftDown = True) and (Key = VK_SHIFT) then
	begin
		ShiftDown := False;
		ButtonIgnoreAll.Caption := 'Ignore All';
		ButtonExit.Caption := 'Close';
	end;
end;

end.
