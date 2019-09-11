unit uEParseError;

interface

uses
  SysUtils;

type
  EParseError = class(Exception)
  private
    function CharsToString(const AChars: array of Char): string;
    function TokensToString(const ATokens: array of string): string;
  public
    constructor Create(const AExpectedChars: array of Char; const AActualChar: Char); overload;
    constructor Create(const AExpectedTokens: array of string; const AActualToken: string); overload;
    constructor Create(const AMessage: string; const AExpectedChars: array of Char; const AActualChar: Char); overload;
    constructor Create(const AMessage: string; const AExpectedTokens: array of string; const AActualToken: string); overload;
  end;

implementation

uses
  uTypes,
  uStrings;

{ EParseError }

constructor EParseError.Create(const AExpectedTokens: array of string; const AActualToken: string);
begin
  Create('Parse:', AExpectedTokens, AActualToken);
end;

constructor EParseError.Create(const AMessage: string; const AExpectedTokens: array of string;
  const AActualToken: string);
begin
  Message := AMessage + ' ' + TokensToString(AExpectedTokens) + ' expected but token ' + AddSingleQuoteF(AActualToken) + ' found.';
end;

constructor EParseError.Create(const AExpectedChars: array of Char; const AActualChar: Char);
begin
  Create('Parse:', AExpectedChars, AActualChar);
end;

constructor EParseError.Create(const AMessage: string; const AExpectedChars: array of Char; const AActualChar: Char);
begin
  Message := AMessage + ' ' + CharsToString(AExpectedChars) + ' expected but token ' + AddSingleQuoteF(AActualChar) + ' found.';
end;

function EParseError.TokensToString(const ATokens: array of string): string;
var
  i: SG;
begin
  if Length(ATokens) = 0 then
  begin
    Result := 'nothing';
    Exit;
  end;
  Result := '';
  for i := 0 to Length(ATokens) - 1 do
  begin
    Result := Result + AddSingleQuoteF(ATokens[i]);
    if i = Length(ATokens) - 2 then
      Result := Result + ' or '
    else if i < Length(ATokens) - 1 then
      Result := Result + ', ';
  end;
end;

function EParseError.CharsToString(const AChars: array of Char): string;
var
  i: SG;
begin
  if Length(AChars) = 0 then
  begin
    Result := 'nothing';
    Exit;
  end;
  Result := '';
  for i := 0 to Length(AChars) - 1 do
  begin
    Result := Result + AddSingleQuoteF(AChars[i]);
    if i = Length(AChars) - 2 then
      Result := Result + ' or '
    else if i < Length(AChars) - 1 then
      Result := Result + ', ';
  end;
end;

end.
