program Echo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

  uTypes,
  uMainLog;

var
  Line, Command: string;
begin
  try
    InitializeMainLog;
    while True do
    begin
      Readln(Line);
	  if MainLog.IsLoggerFor(mlDebug) then
        MainLog.Add('Readed ' + IntToStr(Length(Line)) + ' chars: ' + Line, mlDebug);

      Command := UpperCase(Line);
      if (Command = 'QUIT') or (Command = 'EXIT') then
        Break;
      Writeln(Line);
      Flush(Output);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
