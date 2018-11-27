unit uEParseError;

interface

uses
  SysUtils;

type
  EParseError = class(Exception)
  private
    function TokensToString(const ATokens: array of string): string;
  public
    constructor Create(const AExpectedTokens: array of string; const AActualToken: string); overload;
    constructor Create(const AMessage: string; const AExpectedTokens: array of string; const AActualToken: string); overload;
  end;

implementation

uses
  uTypes,
  uStrings;

{ EParseError }

constructor EParseError.Create(const AExpectedTokens: array of string; const AActualToken: string);
begin
  Create('Parse error:', AExpectedTokens, AActualToken);
end;

constructor EParseError.Create(const AMessage: string; const AExpectedTokens: array of string;
  const AActualToken: string);
begin
  Message := AMessage + ' ' + TokensToString(AExpectedTokens) + ' expected but token ' + AddSingleQuoteF(AActualToken) + ' found.';
end;

function EParseError.TokensToString(const ATokens: array of string): string;
var
  i: SG;
begin
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

end.
