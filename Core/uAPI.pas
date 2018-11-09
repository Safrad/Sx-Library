unit uAPI;

interface

uses
	SysUtils;

procedure APIOpen(const AFileName: TFileName; const AParameters: string = '');

implementation

uses
  uExternalApplicationThread;

procedure APIOpen(const AFileName: TFileName; const AParameters: string = '');
var
  ExternalApplicationThread: TExternalApplicationThread;
begin
  ExternalApplicationThread := TExternalApplicationThread.Create;
  ExternalApplicationThread.ExternalApplication.FileName := AFileName;
  ExternalApplicationThread.ExternalApplication.Parameters := AParameters;
  ExternalApplicationThread.Start;
end;

end.
