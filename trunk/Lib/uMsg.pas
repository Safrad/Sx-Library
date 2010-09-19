unit uMsg;

interface

uses
	SysUtils,
	uTypes{$ifndef Console}, uError{$endif};

procedure Information(const Text: string);
procedure ErrorMsg(const Text: string); overload;
procedure ErrorMsg(const ErrorCode: SG); overload;
function ErrorRetry(const Text: string): BG;
function ErrorCodeToStr(const ErrorCode: U4): string;
procedure Warning(const Text: string; const Param: string = '');
{$ifopt d+}
procedure Debug(const Text: string);
procedure IE(const Text: string);
{$endif}
{$ifndef Console}function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;{$endif}
procedure IOError(const FileName: TFileName; const ErrorCode: U4);
function IOErrorRetry(var FileName: TFileName; const ErrorCode: U4): BG;
procedure IOErrorMessage(FileName: TFileName; const ErrorMsg: string);
function IOErrorMessageRetry(var FileName: TFileName; const ErrorMsg: string): BG;

implementation

uses
	Windows,
	uStrings, uLog;

procedure Information(const Text: string);
begin
//	Log.Add(Text, ltInformation);
	{$ifndef Console}
	MessageD(Text, mtInformation, [mbOk]);
//	MessageDlg(Text, mtInformation, [mbOk], 0);
	{$else}
	Writeln('Information: ' + Text);
	{$endif}
end;

procedure ErrorMsg(const Text: string);
begin
	MainLogAdd(Text, ltError);
	{$ifndef Console}
	MessageD(Text, mtError, [mbAbort]);
//	MessageDlg(Text, mtInformation, [mbAbort], 0);
	{$else}
	Writeln('Error: ' + Text);
	{$endif}
end;

procedure ErrorMsg(const ErrorCode: SG);
begin
	ErrorMsg(ErrorCodeToStr(ErrorCode));
end;

function ErrorRetry(const Text: string): BG;
begin
	MainLogAdd(Text, ltError);
	{$ifndef Console}
	Result := MessageD(Text, mtError, [mbRetry, mbIgnore]) <> mbIgnore;
	{$else}
	Result := False;
	Writeln('Error: ' + Text);
	{$endif}
end;

function ErrorCodeToStr(const ErrorCode: U4): string;
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
	DelBESpace(Result);
	Result := 'ErrorCode ' + IntToStr(ErrorCode) + ' - ' + Result;
end;

procedure Warning(const Text: string; const Param: string = ''); // TODO : Params for all and MessageD
var
	ExpandedText: string;
begin
	ExpandedText := ReplaceF(Text, '%1', Param);
	MainLogAdd(ExpandedText, ltWarn);
	{$ifndef Console}
	MessageD(ExpandedText, mtWarning, [mbOk]);
//	MessageDlg(Text, mtWarning, [mbOk], 0);
	{$else}
	Writeln('Warning: ' + ExpandedText);
	{$endif}
end;

procedure Debug(const Text: string);
begin
	MainLogAdd(Text, ltDebug);
	{$ifndef Console}
	MessageD(Text, mtDebug, [mbOk]);
//	MessageDlg(Text, mtDebug, [mbOk], 0);
	{$else}
	Writeln('Debug: ' + OneLine(Text));
	{$endif}
end;

{$ifopt d+}
procedure IE(const Text: string);
begin
	MainLogAdd(Text, ltDebug);
	{$ifndef Console}
	MessageD(Text, mtError, [mbOk]);
//	MessageDlg(Text, mtDebug, [mbOk], 0);
	{$else}
	Writeln('Debug: ' + OneLine(Text));
	{$endif}
end;
{$endif}

{$ifndef Console}
function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
begin
	Result := MessageD(Text, mtConfirmation, Buttons);
end;
{$endif}

procedure IOError(const FileName: TFileName; const ErrorCode: U4);
begin
	IOErrorMessage(FileName, ErrorCodeToStr(ErrorCode));
end;

function IOErrorRetry(var FileName: TFileName; const ErrorCode: U4): BG;
begin
	Result := IOErrorMessageRetry(FileName, ErrorCodeToStr(ErrorCode));
end;

procedure IOErrorMessage(FileName: TFileName; const ErrorMsg: string);
var Text: string;
begin
	Text := ErrorMsg + ': ' + FileName;
	MainLogAdd(Text, ltError);
	{$ifndef Console}
	DoForm(FileName, ErrorMsg, False, mtIO, [], DlgWait);
	{$else}
	Writeln('I/O Error: ' + OneLine(Text));
	{$endif}
end;

function IOErrorMessageRetry(var FileName: TFileName; const ErrorMsg: string): BG;
var
	Text: string;
	{$ifdef Console}
	s: string;
	{$endif}
begin
	Text := ErrorMsg + ': ' + FileName;
	MainLogAdd(Text, ltError);
	{$ifndef Console}
	Result := DoForm(FileName, ErrorMsg, True, mtIO, [], DlgWait) > 1;
	{$else}
	Writeln('I/O Error: ' + OneLine(Text));
	Writeln('Press [R]etry or [I]gnore.');
	Readln(s);
	Result := StartStr('R', UpperCase(s));
	{$endif}
end;

end.
