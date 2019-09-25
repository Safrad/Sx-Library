program Echo;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Winapi.Windows,

  uTypes,
  uLog;

var
  Line, Command: string;
begin
  try
    InitializeLog;
    while True do
    begin
      Readln(Line);
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
