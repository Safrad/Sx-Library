unit uErrorCodeToStr;

interface

uses
  uTypes;

function ErrorCodeToStr(const ErrorCode: U4): string;

implementation

uses
  SysUtils,
  Winapi.Windows,

  uStrings,
  uChar;

function ErrorCodeToStr(const ErrorCode: U4): string;
const
  ErrorCodeStr = 'I/O: ';
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

end.
