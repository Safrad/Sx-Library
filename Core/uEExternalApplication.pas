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
  uOutputFormat;

{ EExternalException }

constructor EExternalApplication.Create(const APath: string; const AExitCode: U4; const AOuput: string);
begin
  Message := '"' + APath + '" exit code is ' + ExitCodeToString(AExitCode, ofIO) + ', Output: ' + AOuput;
end;

end.
