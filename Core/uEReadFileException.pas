unit uEReadFileException;

interface

uses
  SysUtils,
  uTypes;

type
  EReadFileException = class(Exception)
  public
    constructor Create(const APath: string; const AErrorMessage: string); overload;
  end;

implementation

uses
  uMsg;

{ EReadFileException }

constructor EReadFileException.Create(const APath, AErrorMessage: string);
begin
  Message := AErrorMessage + ': ' + APath;
end;

end.
