unit uEExternalApplication;

interface

uses
  SysUtils,
  uTypes;

type
  EExternalApplication = class(Exception)
  public
    constructor Create(const APath: string; const AExitCode: U4; const AOuput: string);
  end;

implementation

uses
  uMsg;

{ EExternalException }

constructor EExternalApplication.Create(const APath: string; const AExitCode: U4; const AOuput: string);
begin
  Message := '"' + APath + '" exit code is ' + IntToStr(AExitCode) + ', Output: ' + AOuput;
end;

end.
