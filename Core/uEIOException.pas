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

uses
  uMsg;

{ EIOException }

constructor EIOException.Create(const APath: string; const AErrorCode: U4);
begin
  Message := ErrorCodeToStr(AErrorCode) + ': ' + APath;
end;

end.
