unit uMsg;

interface

uses
	SysUtils,
	Consts,
	uTypes;

function ReplaceParam(const Text: string; const Param: array of string): string; overload;
// function ReplaceParam(const Text: string; const Param: string): string; overload;

var
	MessageLevelStr: array [TMessageLevel] of string;

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;
procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string;
	const Param: array of string); overload;
{$IFOPT d+}
procedure Debug(const Text: string); overload;
procedure Debug(const Text: string; const Param: array of string); overload;
procedure IE(const Text: string); // Internal Error
{$ENDIF}
procedure Information(const Text: string); overload;
procedure Information(const Text: string; const Param: array of string); overload;
procedure Warning(const Text: string); overload;
procedure Warning(const Text: string; const Param: array of string); overload;
procedure ErrorMsg(const Text: string); overload;
procedure ErrorMsg(const Text: string; const Param: array of string); overload;
procedure ErrorMsg(const ErrorCode: SG); overload;
procedure Fatal(const E: Exception; const C: TObject);

function ErrorRetry(const Text: string): BG;
function ErrorCodeToStr(const ErrorCode: U4): string;

const
	ErrorCodeStr = 'Error Code';

{$IFNDEF Console}
type
	TDlgBtn = (mbOK, mbYes, mbYesToAll, mbRetry, mbIgnore, mbAbort, mbDelete, mbDeleteAll, mbNo,
		mbNoToAll, mbCancel);
	TDlgButtons = set of TDlgBtn;

const
	DlgBtnNames: array [TDlgBtn] of string = (SMsgDlgOK, SMsgDlgYes, SMsgDlgYesToAll, SMsgDlgRetry,
		SMsgDlgIgnore, SMsgDlgAbort, '&Delete', 'Delete All', SMsgDlgNo, SMsgDlgNoToAll, SMsgDlgCancel);

function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
{$ENDIF}
procedure IOError(const FileName: TFileName; const ErrorCode: U4);
function IOErrorRetry(var FileName: TFileName; const ErrorCode: U4): BG;
procedure IOErrorMessage(FileName: TFileName; const ErrorMsg: string);
function IOErrorMessageRetry(var FileName: TFileName; const ErrorMsg: string): BG;

implementation

uses
	Windows,
	uStrings, uLog {$IFNDEF Console}, uMsgDlg, Dialogs {$ENDIF};

const
	MsgTypeNames: array [TMessageLevel] of string = (SMsgDlgConfirm, 'Debug', SMsgDlgInformation,
		SMsgDlgWarning, SMsgDlgError, 'Fatal Error', '');

function ReplaceParam(const Text: string; const Param: array of string): string; overload;
var
	i: SG;
begin
	Result := Text;
	for i := 1 to Length(Param) do
	begin
{$IFOPT d+}
		if Pos('%' + IntToStr(i), Text) = 0 then
			Result := Result + LineSep + Param[i - 1]
		else
{$ENDIF}
			Replace(Result, '%' + IntToStr(i), '''' + Param[i - 1] + '''');
	end;
end;

{
	function ReplaceParam(const Text: string; const Param: string): string; overload;
	begin
	if Param <> '' then
	begin
	if Pos('%1', Text) = 0 then
	Result := Text + LineSep + Param
	else
	Result := ReplaceF(Text, '%1', '''' + Param + '''')
	end
	else
	Result := Text;
	end; }

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;
begin
	MainLogAdd(ExpandedText, MessageLevel);
{$IFNDEF Console}
	MessageD(ExpandedText, [], MessageLevel, [mbOK]);
{$ELSE}
	Writeln(MsgTypeNames[MessageLevel] + ': ' + ExpandedText);
{$ENDIF}
end;

procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string;
	const Param: array of string); overload;
var
	ExpandedText: string;
begin
	ExpandedText := ReplaceParam(Text, Param);
	MainLogAdd(ExpandedText, MessageLevel);
{$IFNDEF Console}
	MessageD(Text, Param, MessageLevel, [mbOK]);
{$ELSE}
	Writeln(MsgTypeNames[MessageLevel] + ': ' + ExpandedText);
{$ENDIF}
end;
{$IFOPT d+}

procedure Debug(const Text: string);
begin
	ShowMessage(mlDebug, Text, []);
end;

procedure Debug(const Text: string; const Param: array of string);
begin
	ShowMessage(mlDebug, Text, Param);
end;

procedure IE(const Text: string);
begin
	ShowMessage(mlFatalError, 'Internal Error: ' + Text);
end;
{$ENDIF}

procedure Information(const Text: string); overload;
begin
	ShowMessage(mlInformation, Text, []);
end;

procedure Information(const Text: string; const Param: array of string); overload;
begin
	ShowMessage(mlInformation, Text, Param);
end;

procedure Warning(const Text: string);
begin
	ShowMessage(mlWarning, Text, []);
end;

procedure Warning(const Text: string; const Param: array of string);
begin
	ShowMessage(mlWarning, Text, Param);
end;

procedure ErrorMsg(const Text: string);
begin
	ShowMessage(mlError, Text, []);
end;

procedure ErrorMsg(const Text: string; const Param: array of string);
begin
	ShowMessage(mlError, Text, Param);
end;

procedure ErrorMsg(const ErrorCode: SG);
begin
	if ErrorCode <> 0 then
		ErrorMsg(ErrorCodeToStr(ErrorCode));
end;

procedure Fatal(const E: Exception; const C: TObject);
var
	ExpandedText: string;
begin
	if C = nil then
		ExpandedText := E.Message
	else
		ExpandedText := ReplaceParam(E.Message + ' in class %1', [C.ClassName]);
	MainLogAdd(ExpandedText, mlFatalError);
end;

function ErrorRetry(const Text: string): BG;
begin
	MainLogAdd(Text, mlError);
{$IFNDEF Console}
	// Result := MessageDlg(Text, mtError, [Dialogs.mbRetry, Dialogs.mbIgnore], 0) <> 1;
	Result := MessageD(Text, mlError, [mbRetry, mbIgnore]) <> mbIgnore;
{$ELSE}
	Result := False;
	Writeln('Error: ' + Text);
{$ENDIF}
end;

function ErrorCodeToStr(const ErrorCode: U4): string;
var
	NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := FormatMessage(
		{ FORMAT_MESSAGE_ALLOCATE_BUFFER or }
		FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, ErrorCode,
		0{LANG_USER_DEFAULT} {LANG_NEUTRAL or SUBLANG_DEFAULT shl 10}, PChar(Result), MAX_PATH, nil);
	SetLength(Result, NewLength);
	DelBESpace(Result);
	Result := ErrorCodeStr + CharSpace + IntToStr(ErrorCode) + ' - ' + Result;
end;
{$IFNDEF Console}

function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
begin
	Result := MessageD(Text, mlConfirmation, Buttons);
end;
{$ENDIF}

procedure IOError(const FileName: TFileName; const ErrorCode: U4);
begin
	IOErrorMessage(FileName, ErrorCodeToStr(ErrorCode));
end;

function IOErrorRetry(var FileName: TFileName; const ErrorCode: U4): BG;
begin
	Result := IOErrorMessageRetry(FileName, ErrorCodeToStr(ErrorCode));
end;

procedure IOErrorMessage(FileName: TFileName; const ErrorMsg: string);
var
	Text: string;
begin
	Text := ErrorMsg + ': ' + FileName;
	MainLogAdd(Text, mlError);
{$IFNDEF Console}
	MsgDlg(ErrorMsg + LineSep + '%1', [FileName], False, mlError, [SMsgDlgOK], DlgWait);
{$ELSE}
	Writeln('I/O Error: ' + OneLine(Text));
{$ENDIF}
end;

function IOErrorMessageRetry(var FileName: TFileName; const ErrorMsg: string): BG;
var
	Text: string;
{$IFDEF Console}
	s: string;
{$ENDIF}
begin
	Text := ErrorMsg + ': ' + FileName;
	MainLogAdd(Text, mlError);
{$IFNDEF Console}
	Result := MsgDlg(ErrorMsg + LineSep + '%1', [FileName], True, mlError,
		[SMsgDlgRetry, SMsgDlgIgnore], DlgWait) = 0;
{$ELSE}
	Writeln('I/O Error: ' + OneLine(Text));
	Writeln('Press [R]etry or [I]gnore.');
	Readln(s);
	Result := StartStr('R', UpperCase(s));
{$ENDIF}
end;

initialization

EnumToStr(TypeInfo(TMessageLevel), MessageLevelStr);

end.
