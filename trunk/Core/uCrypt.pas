unit uCrypt;

interface

function DecryptString(const Data: AnsiString; const Password: AnsiString): AnsiString; overload;
function EncryptString(const Data: AnsiString; const Password: AnsiString): AnsiString; overload;
{$ifdef UNICODE}
function DecryptString(const Data: UnicodeString; const Password: UnicodeString): UnicodeString; overload;
function EncryptString(const Data: UnicodeString; const Password: UnicodeString): UnicodeString; overload;
{$endif}

implementation

uses
  SysUtils,
  DCPrijndael,
  DCPsha256;

var
  Crypt: TDCP_rijndael;

function DecryptString(const Data: AnsiString; const Password: AnsiString): AnsiString;
begin
  if Crypt = nil then
    Crypt := TDCP_rijndael.Create(nil);

  Crypt.InitStr(Password, TDCP_sha256);
  Result := Crypt.DecryptString(Data);
  Crypt.Burn;
end;

function EncryptString(const Data: AnsiString; const Password: AnsiString): AnsiString;
begin
  if Crypt = nil then
    Crypt := TDCP_rijndael.Create(nil);

  Crypt.InitStr(Password, TDCP_sha256);
  Result := Crypt.EncryptString(Data);
  Crypt.Burn;
end;

{$ifdef UNICODE}
function DecryptString(const Data: UnicodeString; const Password: UnicodeString): UnicodeString;
begin
  if Crypt = nil then
    Crypt := TDCP_rijndael.Create(nil);

  Crypt.InitStr(Password, TDCP_sha256);
  Result := Crypt.DecryptString(Data);
  Crypt.Burn;
end;

function EncryptString(const Data: UnicodeString; const Password: UnicodeString): UnicodeString;
begin
  if Crypt = nil then
    Crypt := TDCP_rijndael.Create(nil);

  Crypt.InitStr(Password, TDCP_sha256);
  Result := Crypt.EncryptString(Data);
  Crypt.Burn;
end;
{$endif}

initialization

finalization
{$IFNDEF NoFinalization}
	FreeAndNil(Crypt);
{$ENDIF NoFinalization}
end.
