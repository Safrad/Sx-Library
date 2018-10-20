unit uAPI;

interface

uses
	SysUtils;

procedure APIOpen(AFileName: TFileName; const AParameters: string = '');

implementation

uses
  uExternalApplicationThread;

procedure APIOpen(AFileName: TFileName; const AParameters: string = '');
var
  ExternalApplicationThread: TExternalApplicationThread;
begin
  ExternalApplicationThread := TExternalApplicationThread.Create;
  try
    ExternalApplicationThread.ExternalApplication.FileName := AFileName;
    ExternalApplicationThread.ExternalApplication.Parameters := AParameters;
  finally
    ExternalApplicationThread.Free;
  end;
  ExternalApplicationThread.Start;
end;

end.
