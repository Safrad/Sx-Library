unit uCharset;

{$WARN SYMBOL_DEPRECATED OFF}

interface

function ConvertUTF8ToUnicode(const s: RawByteString): UnicodeString;
function ConvertUnicodeToUTF8(const s: UnicodeString): RawByteString;

function ConvertToAscii(const AInput: AnsiString): AnsiString; overload;
function ConvertToAscii(const AInput: UnicodeString): AnsiString; overload;

function ConvertAnsiToOem(const s: string): string;

function RemoveUnicode(const AInput: string): string;

implementation

uses
  uTypes,
  uChar,
  uStrings,
{$IF defined(MSWINDOWS)}
  Winapi.Windows,
{$ENDIF}
  SysUtils;

function ConvertUTF8ToUnicode(const s: RawByteString): UnicodeString;
begin
	SetLength(Result, 2 * Length(s));
	SetLength(Result, Utf8ToUnicode(PWideChar(Result), PAnsiChar(s), 2 * Length(s)) - 1);
end;

function ConvertUnicodeToUTF8(const s: UnicodeString): RawByteString;
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
  SetLength(Result, LocaleCharsFromUnicode(CodePage, 0, PWideChar(WS),
    Length(WS), nil, 0, nil, nil));
  LocaleCharsFromUnicode(CodePage, 0, PWideChar(WS), Length(WS),
    PAnsiChar(Result), Length(Result), nil, nil);
end;

function ConvertAnsiToOem(const s: string): string;
{$IF defined(MSWINDOWS)}
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
{$ELSE}
begin
  Result := s;
{$ENDIF}
end;

function RemoveUnicode(const AInput: string): string;
begin
  Result := ReplaceF(AInput, CharCopyright, '(c)');
end;

end.
