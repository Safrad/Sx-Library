unit uCharset;

{$WARN SYMBOL_DEPRECATED OFF}

interface

function ConvertUTF8ToUnicode(const s: AnsiString): UnicodeString;
function ConvertUnicodeToUTF8(const s: UnicodeString): AnsiString;

function ConvertToAscii(const AInput: AnsiString): AnsiString; overload;
function ConvertToAscii(const AInput: UnicodeString): AnsiString; overload;

function ConvertAnsiToOem(const s: string): string;

function RemoveUnicode(const AInput: string): string;

implementation

uses
  uTypes,
  uChar,
  uStrings,
  SysUtils, Windows;

function ConvertUTF8ToUnicode(const s: AnsiString): UnicodeString;
begin
	SetLength(Result, 2 * Length(s));
	SetLength(Result, Utf8ToUnicode(PWideChar(Result), PAnsiChar(s), 2 * Length(s)) - 1);
end;

function ConvertUnicodeToUTF8(const s: UnicodeString): AnsiString;
var
	l: SG;
begin
//	Result := StringTo UTF8ToString(s;
	SetLength(Result, 2 * Length(s) + 1);
	l := UnicodeToUtf8(PAnsiChar(Result), Length(Result), PWideChar(s), Length(s));
	SetLength(Result, l - 1);
end;

function ConvertToAscii(const AInput: AnsiString): AnsiString;
begin
  Result := ConvertToAscii(UnicodeString(AInput));
end;

function ConvertToAscii(const AInput: UnicodeString): AnsiString;
const
  CodePage = 20127; // us-ascii
var
  WS: WideString;
begin
  WS := WideString(AInput);
  SetLength(Result, WideCharToMultiByte(CodePage, 0, PWideChar(WS),
    Length(WS), nil, 0, nil, nil));
  WideCharToMultiByte(CodePage, 0, PWideChar(WS), Length(WS),
    PAnsiChar(Result), Length(Result), nil, nil);
end;

function ConvertAnsiToOem(const s: string): string;
var
  sBuffer: AnsiString;
begin
  if s = '' then
  begin
    Result := '';
    Exit;
  end;

  SetLength(sBuffer, Length(s));
  if CharToOem(PChar(s), PAnsiChar(sBuffer)) then
    Result := string(sBuffer)
  else
    Result := s;
end;

function RemoveUnicode(const AInput: string): string;
begin
  Result := ReplaceF(AInput, CharCopyright, '(c)');
end;

end.
