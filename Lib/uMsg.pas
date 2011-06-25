//* File:     Lib\uMsg.pas
//* Created:  2000-08-01
//* Modified: 2008-03-17
//* Version:  1.1.41.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uMsg;

interface

uses
	SysUtils,
	Consts,
	uTypes;

function ReplaceParam(const Text: string; const Param: array of string): string; overload;
function ReplaceParam(const Text: string; const Param: string): string; overload;

var
	MessageLevelStr: array[TMessageLevel] of string;

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;
procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string; const Param: array of string); overload;

{$ifopt d+}
procedure Debug(const Text: string); overload;
procedure Debug(const Text: string; const Param: array of string); overload;
procedure IE(const Text: string); // Internal Error
{$endif}
procedure Information(const Text: string); overload;
procedure Information(const Text: string; const Param: array of string); overload;
procedure Warning(const Text: string);overload;
procedure Warning(const Text: string; const Param: array of string); overload;
procedure ErrorMsg(const Text: string); overload;
procedure ErrorMsg(const Text: string; const Param: array of string); overload;
procedure ErrorMsg(const ErrorCode: SG); overload;
procedure Fatal(const E: Exception; const C: TObject);

function ErrorRetry(const Text: string): BG;
function ErrorCodeToStr(const ErrorCode: U4): string;
{$ifndef Console}
type
	TDlgBtn = (
		mbOK, mbYes, mbYesToAll,
		mbRetry, mbIgnore, mbAbort,
		mbDelete, mbDeleteAll,
		mbNo, mbNoToAll, mbCancel);
	TDlgButtons = set of TDlgBtn;
const
	DlgBtnNames: array[TDlgBtn] of string = (
		SMsgDlgOK, SMsgDlgYes, SMsgDlgYesToAll,
		SMsgDlgRetry, SMsgDlgIgnore, SMsgDlgAbort,
		'&Delete', 'Delete All',
		SMsgDlgNo, SMsgDlgNoToAll, SMsgDlgCancel);

function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
{$endif}
procedure IOError(const FileName: TFileName; const ErrorCode: U4);
function IOErrorRetry(var FileName: TFileName; const ErrorCode: U4): BG;
procedure IOErrorMessage(FileName: TFileName; const ErrorMsg: string);
function IOErrorMessageRetry(var FileName: TFileName; const ErrorMsg: string): BG;

implementation

uses
	Windows,
	uStrings, uLog{$ifndef Console}, uMsgDlg{$endif};

const
	MsgTypeNames: array[TMessageLevel] of string = (
		SMsgDlgConfirm,
		'Debug',
		SMsgDlgInformation,
		SMsgDlgWarning,
		SMsgDlgError,
		'Fatal Error',
		'');

function ReplaceParam(const Text: string; const Param: array of string): string; overload;
var i: SG;
begin
	Result := Text;
	for i := 1 to Length(Param) do
	begin
		{$ifopt d+}
		if Pos('%' + IntToStr(i), Text) = 0 then
			Result := Result + LineSep + Param[i - 1]
		else
		{$endif}
		Replace(Result, '%' + IntToStr(i), '''' + Param[i - 1] + '''');
	end;
end;

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
end;

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;
begin
	MainLogAdd(ExpandedText, MessageLevel);
	{$ifndef Console}
	MessageD(ExpandedText, [], MessageLevel, [mbOk]);
	{$else}
	Writeln(MsgTypeNames[MessageLevel] + ': ' + ExpandedText);
	{$endif}
end;

procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string; const Param: array of string); overload;
var
	ExpandedText: string;
begin
	ExpandedText := ReplaceParam(Text, Param);
	MainLogAdd(ExpandedText, MessageLevel);
	{$ifndef Console}
	MessageD(Text, Param, MessageLevel, [mbOk]);
	{$else}
	Writeln(MsgTypeNames[MessageLevel] + ': ' + ExpandedText);
	{$endif}
end;

{$ifopt d+}
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
{$endif}

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
	ErrorMsg(ErrorCodeToStr(ErrorCode));
end;

procedure Fatal(const E: Exception; const C: TObject);
var
	ExpandedText: string;
begin
	if C = nil then
		ExpandedText := E.Message
	else
		ExpandedText := ReplaceParam(E.Message + ' in class %1', C.ClassName);
	MainLogAdd(ExpandedText, mlFatalError);
end;

function ErrorRetry(const Text: string): BG;
begin
	MainLogAdd(Text, mlError);
	{$ifndef Console}
	Result := MessageD(Text, mlError, [mbRetry, mbIgnore]) <> mbIgnore;
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

{$ifndef Console}
function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
begin
	Result := MessageD(Text, mlConfirmation, Buttons);
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
	MainLogAdd(Text, mlError);
	{$ifndef Console}
	MsgDlg(ErrorMsg + LineSep + '%1', [FileName], False, mlError, [SMsgDlgOK], DlgWait);
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
	MainLogAdd(Text, mlError);
	{$ifndef Console}
	Result := MsgDlg(ErrorMsg + LineSep + '%1', [FileName], True, mlError, [SMsgDlgRetry, SMsgDlgIgnore], DlgWait) = 0;
	{$else}
	Writeln('I/O Error: ' + OneLine(Text));
	Writeln('Press [R]etry or [I]gnore.');
	Readln(s);
	Result := StartStr('R', UpperCase(s));
	{$endif}
end;

initialization
	EnumToStr(TypeInfo(TMessageLevel), MessageLevelStr);
end.
