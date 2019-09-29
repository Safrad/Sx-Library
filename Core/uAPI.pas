unit uAPI;

interface

uses
	SysUtils;

procedure APIOpen(const AFileName: TFileName; const AParameters: string = '');

implementation

uses
  uShellApplicationThread;

procedure APIOpen(const AFileName: TFileName; const AParameters: string = '');
var
  ShellApplicationlThread: TShellApplicationThread;
begin
  ShellApplicationlThread := TShellApplicationThread.Create;
  ShellApplicationlThread.ShellApplication.FileName := AFileName;
  ShellApplicationlThread.ShellApplication.Parameters := AParameters;
  ShellApplicationlThread.ShellApplication.CurrentDirectory := ExtractFileDir(AFileName);
  ShellApplicationlThread.Start;
end;

end.
