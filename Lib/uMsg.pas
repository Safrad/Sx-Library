//* File:     Lib\uMsg.pas
//* Created:  2000-08-01
//* Modified: 2007-10-22
//* Version:  1.1.39.8
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
	MsgTypeStr: array[TMsgType] of string;

procedure ShowMessage(const MsgType: TMsgType; const ExpandedText: string); overload;
procedure ShowMessage(const MsgType: TMsgType; const Text: string; const Param: array of string); overload;

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
	MsgTypeNames: array[TMsgType] of string = (
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

procedure ShowMessage(const MsgType: TMsgType; const ExpandedText: string); overload;
begin
	MainLogAdd(ExpandedText, MsgType);
	{$ifndef Console}
	MessageD(ExpandedText, [], MsgType, [mbOk]);
	{$else}
	Writeln(MsgTypeNames[MsgType] + ': ' + OneLine(ExpandedText));
	{$endif}
end;

procedure ShowMessage(const MsgType: TMsgType; const Text: string; const Param: array of string); overload;
var
	ExpandedText: string;
begin
	ExpandedText := ReplaceParam(Text, Param);
	MainLogAdd(ExpandedText, MsgType);
	{$ifndef Console}
	MessageD(Text, Param, MsgType, [mbOk]);
	{$else}
	Writeln(MsgTypeNames[MsgType] + ': ' + OneLine(ExpandedText));
	{$endif}
end;

{$ifopt d+}
procedure Debug(const Text: string);
begin
	ShowMessage(mtDebug, Text, []);
end;

procedure Debug(const Text: string; const Param: array of string);
begin
	ShowMessage(mtDebug, Text, Param);
end;

procedure IE(const Text: string);
begin
	ShowMessage(mtFatalError, Text);
end;
{$endif}

procedure Information(const Text: string); overload;
begin
	ShowMessage(mtInformation, Text, []);
end;

procedure Information(const Text: string; const Param: array of string); overload;
begin
	ShowMessage(mtInformation, Text, Param);
end;

procedure Warning(const Text: string);
begin
	ShowMessage(mtWarning, Text, []);
end;

procedure Warning(const Text: string; const Param: array of string);
begin
	ShowMessage(mtWarning, Text, Param);
end;

procedure ErrorMsg(const Text: string);
begin
	ShowMessage(mtError, Text, []);
end;

procedure ErrorMsg(const Text: string; const Param: array of string);
begin
	ShowMessage(mtError, Text, Param);
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
	MainLogAdd(ExpandedText, mtFatalError);
end;

function ErrorRetry(const Text: string): BG;
begin
	MainLogAdd(Text, mtError);
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
	MainLogAdd(Text, mtError);
	{$ifndef Console}
	MsgDlg(ErrorMsg + LineSep + '%1', [FileName], False, mtError, [SMsgDlgOK], DlgWait);
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
	MainLogAdd(Text, mtError);
	{$ifndef Console}
	Result := MsgDlg(ErrorMsg + LineSep + '%1', [FileName], True, mtError, [SMsgDlgRetry, SMsgDlgIgnore], DlgWait) = 0;
	{$else}
	Writeln('I/O Error: ' + OneLine(Text));
	Writeln('Press [R]etry or [I]gnore.');
	Readln(s);
	Result := StartStr('R', UpperCase(s));
	{$endif}
end;

initialization
	EnumToStr(TypeInfo(TMsgType), MsgTypeStr);
end.
