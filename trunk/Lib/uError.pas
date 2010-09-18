// Build: 12/1999-04/2000 Author: Safranek David

unit uError;

interface

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDButton, ComCtrls, uDPanel, uDLabel, uWave, uAdd,
	uDForm;

type
	TfIOError = class(TDForm)
		ButtonIRetry: TDButton;
		ButtonIIgnore: TDButton;
		ButtonIIgnoreAll: TDButton;
		ButtonExit: TDButton;
		Image1: TImage;
		ButtonOpen: TDButton;
		OpenDialogFile: TOpenDialog;
		MemoMsg: TMemo;
		UpDown1: TUpDown;
		Label1: TDLabel;
		PanelIndex: TDPanel;
		PanelCount: TDPanel;
		procedure ButtonOpenClick(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
			NewValue: SmallInt; Direction: TUpDownDirection);
		procedure ButtonExitClick(Sender: TObject);
	private
		{ Private declarations }
		ErrorFileName: TFileName;
	public
		{ Public declarations }
	end;

var
	IgnoreAll: Boolean;
	SndError: PWave;

procedure CreateException;
function ErrorMes(const ErrorCode: U32): string;

// IO Error
procedure IOError(FName: TFileName; const ErrorCode: U32);
function IOErrorRetry(var FName: TFileName; const ErrorCode: U32): Boolean;
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

procedure CreateException;
begin
	asm
	mov eax, $ffffffff
	call eax
	end;
end;

function ErrorMes(const ErrorCode: U32): string;
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
	Result := Result + ' (' + IntToStr(ErrorCode) + ')';
end;

function DoForm(const Style: TStyle; var FName: TFileName; const ErrorCode: U32; const ErrorMsg: string; const Retry: Boolean): Boolean;
var s: string;
begin
	Result := False;
	if IOErrorMessages = nil then IOErrorMessages := TStringList.Create;
	if Style = stIO then
		s := ErrorMes(ErrorCode)
	else
		s := ErrorMsg;
	if FName <> '' then s := s + #13 + #10 + FName;
	IOErrorMessages.Add(s);

	if IgnoreAll = False then
	begin
//		PlayWinSound(wsProgramError);
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

		fIOError.ButtonIRetry.Enabled := Retry;
		fIOError.ButtonOpen.Enabled := Retry and ((Style = stIO) or (Style = stFile));
		fIOError.ButtonIIgnore.Default := not Retry;
		fIOError.UpDown1.OnChangingEx := nil;
		fIOError.UpDown1.Max := IOErrorMessages.Count - 1;
		fIOError.UpDown1.Position := IOErrorMessages.Count - 1;
		fIOError.UpDown1.OnChangingEx := fIOError.UpDown1ChangingEx;
		fIOError.PanelIndex.Caption := IntToStr(IOErrorMessages.Count);
		fIOError.PanelCount.Caption := IntToStr(IOErrorMessages.Count);
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
		mrAll: IgnoreAll := True;
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

procedure IOError(FName: TFileName; const ErrorCode: U32);
begin
	DoForm(stIO, FName, ErrorCode, '', False);
end;

function IOErrorRetry(var FName: TFileName; const ErrorCode: U32): Boolean;
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
	PanelIndex.Caption := IntToStr(NewValue + 1);
	MemoMsg.Lines.Clear;
	MemoMsg.Lines.Insert(0, IOErrorMessages[NewValue]);
end;

procedure TfIOError.ButtonExitClick(Sender: TObject);
begin
	if Assigned(Application.MainForm) and (GetAsyncKeyState(VK_SHIFT) = 0) then
		Application.MainForm.Close
	else
		TerminateProcess(GetCurrentProcess, 1); //Halt;
end;

end.
