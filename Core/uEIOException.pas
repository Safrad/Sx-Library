unit uEIOException;

interface

uses
  SysUtils,
  uTypes;

type
  EIOException = class(EInOutError)
  public
    constructor Create(const AErrorCode: U4; const APath: string);
  end;

implementation

uses
  uMsg;

{ EIOException }

constructor EIOException.Create(const AErrorCode: U4; const APath: string);
begin
  Message := ErrorCodeToStr(AErrorCode) + ': ' + APath;
end;

end.
