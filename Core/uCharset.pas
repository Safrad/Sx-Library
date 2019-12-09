unit uCharset;

{$WARN SYMBOL_DEPRECATED OFF}

interface

function ConvertUTF8ToUnicode(const s: RawByteString): UnicodeString;
function ConvertUnicodeToUTF8(const s: UnicodeString): RawByteString;

{$ifdef MSWINDOWS}
function ConvertToAscii(const AInput: AnsiString): AnsiString; overload;
function ConvertToAscii(const AInput: UnicodeString): AnsiString; overload;

function ConvertAnsiToOem(const s: string): string;
{$endif}

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
  Result := Utf8Decode(s);
end;

function ConvertUnicodeToUTF8(const s: UnicodeString): RawByteString;
begin
  Result := UTF8Encode(s);
end;

{$IF defined(MSWINDOWS)}

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
{$ENDIF}

function RemoveUnicode(const AInput: string): string;
begin
  Result := ReplaceF(AInput, CharCopyright, '(c)');
end;

end.
