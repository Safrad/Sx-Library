unit uMsg;

interface

uses
  SysUtils,

  uTypes,
  uOutputInfo;

const
  ErrorCodeStr = 'I/O: ';

var
  MessageLevelStr: array[TMessageLevel] of string;

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;

procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string; const Param: array of string); overload;

procedure Debug(const Text: string); overload;

procedure Debug(const Text: string; const Param: array of string); overload;

procedure IE(const Text: string); // Internal Error

procedure Information(const Text: string); overload;

procedure Information(const Text: string; const Param: array of string); overload;

procedure Warning(const Text: string); overload;

procedure Warning(const Text: string; const Param: array of string); overload;

procedure ErrorMsg(const Text: string); overload;

procedure ErrorMsg(const Text: string; const Param: array of string); overload;

{$ifdef MSWINDOWS}
procedure ErrorMsg(const ErrorCode: SG); overload;
{$endif}

procedure Fatal(const AException: Exception; const C: TObject = nil);

function ErrorRetry(const Text: string): BG;

{$ifdef MSWINDOWS}
function ErrorCodeToStr(const ErrorCode: U4): string;
{$endif}

function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn; overload;

function Confirmation(const Text: string; const Buttons: TDlgButtons; const Param: array of string): TDlgBtn; overload;

{$ifdef MSWINDOWS}
procedure IOError(const FileName: TFileName; const ErrorCode: U4);

function IOErrorRetry(const FileName: TFileName; const ErrorCode: U4): BG;
{$endif}

procedure IOErrorMessage(const FileName: TFileName; const ErrorMsg: string);

function IOErrorMessageRetry(const FileName: TFileName; const ErrorMsg: string): BG;

implementation

uses
{$ifdef MSWINDOWS}
  Winapi.Windows,
{$endif}

  uStrings,
  uLog,
  uCommonOutput,
  uChar;

resourcestring
  rsRetry = 'Retry?';

procedure ShowMessage(const MessageLevel: TMessageLevel; const ExpandedText: string); overload;
begin
  if MainLogWrite(MessageLevel) then
    MainLogAdd(ExpandedText, MessageLevel);

  if Assigned(CommonOutput) then
    CommonOutput.AddMessage(ExpandedText, MessageLevel);
end;

procedure ShowMessage(const MessageLevel: TMessageLevel; const Text: string; const Param: array of string); overload;
var
  ExpandedText: string;
begin
  ExpandedText := ReplaceParam(Text, Param);
  if MainLogWrite(MessageLevel) then
    MainLogAdd(ExpandedText, MessageLevel);

  if Assigned(CommonOutput) then
    CommonOutput.AddMessage(ExpandedText, MessageLevel);
end;

procedure Debug(const Text: string);
begin
  if IsDebug then
    ShowMessage(mlDebug, Text, []);
end;

procedure Debug(const Text: string; const Param: array of string);
begin
  if IsDebug then
    ShowMessage(mlDebug, Text, Param);
end;

procedure IE(const Text: string);
begin
  if IsDebug then
    ShowMessage(mlFatalError, 'Internal' + CharSpace + CharEnDash + CharSpace + Text);
end;

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

{$IFDEF MSWINDOWS}
procedure ErrorMsg(const ErrorCode: SG);
begin
  if ErrorCode <> 0 then
    ErrorMsg(ErrorCodeToStr(ErrorCode));
end;
{$ENDIF}

procedure Fatal(const AException: Exception; const C: TObject = nil);
var
  ExpandedText: string;
  Ex: Exception;
begin
  if AException is EAbort then
    Exit; // EAbort is the exception class for errors that should not display an error message dialog box.

  ExpandedText := '';
  Ex := AException;
  repeat
    ExpandedText := ExpandedText + Ex.Message;
    if IsDebug then
      ExpandedText := ExpandedText + ' (' + Ex.ClassName + ')';
    ExpandedText := ExpandedText + LineSep;
    {$if CompilerVersion >= 20}
    Ex := Ex.InnerException;
    {$else}
    Ex := nil;
    {$ifend}
  until Ex = nil;

  if IsDebug and (C <> nil) then
  begin
    ExpandedText := ExpandedText + '(in class ' + C.ClassName + ')';
  end
  else
    ExpandedText := DelLastChar(ExpandedText);

  ShowMessage(mlFatalError, ExpandedText);
end;

function ErrorRetry(const Text: string): BG;
begin
  if LogError then
    MainLogAdd(Text, mlError);

  if Assigned(CommonOutput) then
  begin
    CommonOutput.AddError(Text);
    Result := CommonOutput.ConfirmationYesNo(rsRetry);
  end
  else
    Result := False;
end;

{$IFDEF MSWINDOWS}
function ErrorCodeToStr(const ErrorCode: U4): string;
var
  NewLength: SG;
begin
  SetLength(Result, MAX_PATH);
  NewLength := FormatMessage(
		{ FORMAT_MESSAGE_ALLOCATE_BUFFER or }
    FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_IGNORE_INSERTS, nil, ErrorCode, 0{LANG_USER_DEFAULT}
    {LANG_NEUTRAL or SUBLANG_DEFAULT shl 10}, PChar(Result), MAX_PATH, nil);
  SetLength(Result, NewLength);
  DelBESpace(Result);
  Result := ErrorCodeStr + CharSpace + IntToStr(ErrorCode) + ' ' + CharEnDash + ' ' + Result;
end;
{$ENDIF}

function Confirmation(const Text: string; const Buttons: TDlgButtons): TDlgBtn;
begin
  if LogConfirmation then
    MainLogAdd(Text, mlConfirmation);
  if Assigned(CommonOutput) then
    Result := CommonOutput.Confirmation(Text, Buttons)
  else
    Result := mbCancel;
end;

function Confirmation(const Text: string; const Buttons: TDlgButtons; const Param: array of string): TDlgBtn;
var
  ExpandedText: string;
begin
  ExpandedText := ReplaceParam(Text, Param);
  if LogConfirmation then
    MainLogAdd(ExpandedText, mlConfirmation);
  if Assigned(CommonOutput) then
    Result := CommonOutput.Confirmation(ExpandedText, Buttons)
  else
    Result := mbCancel;
end;

{$ifdef MSWINDOWS}
procedure IOError(const FileName: TFileName; const ErrorCode: U4);
begin
  IOErrorMessage(FileName, ErrorCodeToStr(ErrorCode));
end;

function IOErrorRetry(const FileName: TFileName; const ErrorCode: U4): BG;
begin
  Result := IOErrorMessageRetry(FileName, ErrorCodeToStr(ErrorCode));
end;
{$endif}

procedure IOErrorMessage(const FileName: TFileName; const ErrorMsg: string);
var
  Text: string;
begin
  Text := ErrorMsg + ': ' + FileName;
  if LogError then
    MainLogAdd(Text, mlError);

  if Assigned(CommonOutput) then
    CommonOutput.AddError(ErrorCodeStr + Text);
end;

function IOErrorMessageRetry(const FileName: TFileName; const ErrorMsg: string): BG;
begin
  Result := ErrorRetry(ErrorMsg + ': ' + FileName);
end;

initialization

{$IFNDEF NoInitialization}
  EnumToStr(TypeInfo(TMessageLevel), MessageLevelStr);
{$ENDIF NoInitialization}

end.

