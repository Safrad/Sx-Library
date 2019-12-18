unit uEIOException;

interface

uses
  SysUtils,
  uTypes;

type
  EIOException = class(EInOutError)
  public
    constructor Create(const APath: string; const AErrorCode: U4);
  end;

implementation

{$ifdef MSWINDOWS}
uses
  uErrorCodeToStr;
{$endif}

{ EIOException }

constructor EIOException.Create(const APath: string; const AErrorCode: U4);
begin
{$ifdef MSWINDOWS}
  Message := ErrorCodeToStr(AErrorCode) + ': ' + APath;
{$else}
  Message := IntToStr(AErrorCode) + ': ' + APath;
{$endif}
end;

end.

