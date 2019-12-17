unit uPipedExternalApplicationTest;

interface

uses
  SysUtils,

  uPipedExternalApplication,

  TestFrameWork;

type
  TPipedExternalApplicationTest = class(TTestCase)
  private
    FReadedText: string;
    procedure OnRead(const AText: string);
    procedure OnReadLine(const AText: string);
    procedure TestRepeatRunInternal(const AExternalApplication: TPipedExternalApplication);
    procedure SetDeaultApplication(const AExternalApplication: TPipedExternalApplication);
  published
    procedure TestReadAll;
    procedure TestWrite;
    procedure TestReadAndWrite;
    procedure TestReapatRun;
    procedure TestThroughput;
  end;

implementation

uses
  uTypes,
  uStartupWindowState,
  uStrings,
  uSystemPaths,
  uEIOException,
  uMainTimer,
  uTimeSpan,
  uOutputFormat;

{ TExternalApplicationTest }

procedure TPipedExternalApplicationTest.OnRead(const AText: string);
begin
  FReadedText := AText;
end;

procedure TPipedExternalApplicationTest.OnReadLine(const AText: string);
begin
  CheckEquals('test', AText);
end;

procedure TPipedExternalApplicationTest.SetDeaultApplication(const AExternalApplication: TPipedExternalApplication);
var
  StartupWindowState: TStartupWindowState;
begin
  FReadedText := '';
  AExternalApplication.FileName := 'wmic';
  AExternalApplication.Parameters := 'cpu get name';
  AExternalApplication.CurrentDirectory := SystemPaths.LocalAppDataDir;
  StartupWindowState.WindowState := hwsHidden;
  StartupWindowState.Active := False;
  AExternalApplication.StartupWindowState := StartupWindowState;
end;

procedure TPipedExternalApplicationTest.TestReadAll;
var
  ExternalApplication: TPipedExternalApplication;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.RequireOutputText := True;

    ExternalApplication.Execute;

    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckTrue(ExternalApplication.OutputText <> '', 'Application output is empty.');
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TPipedExternalApplicationTest.TestReadAndWrite;
var
  ExternalApplication: TPipedExternalApplication;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'echo.exe';
    ExternalApplication.OnRead := OnRead;

    ExternalApplication.Execute;

    CheckTrue(ExternalApplication.Running, 'Running application expected.');
    ExternalApplication.WriteLine('test');
    ExternalApplication.WriteLine('quit');
    ExternalApplication.WaitFor;
    CheckEquals('test' + FullSep, FReadedText);
    CheckFalse(ExternalApplication.Running);
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TPipedExternalApplicationTest.TestReapatRun;
var
  ExternalApplication: TPipedExternalApplication;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.RequireOutputText := False;

    TestRepeatRunInternal(ExternalApplication);
    TestRepeatRunInternal(ExternalApplication);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TPipedExternalApplicationTest.TestRepeatRunInternal(const AExternalApplication: TPipedExternalApplication);
begin
  AExternalApplication.Execute;
  CheckTrue(AExternalApplication.Running);
  AExternalApplication.WaitFor;
  CheckFalse(AExternalApplication.Running);
  CheckEquals(0, AExternalApplication.ExitCode);
end;

procedure TPipedExternalApplicationTest.TestThroughput;
var
  ExternalApplication: TPipedExternalApplication;
  StartTime: TTimeSpan;
  BytesSend: U8;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'echo.exe';
    ExternalApplication.OnReadLine := OnReadLine;
    ExternalApplication.InputBufferSize := 4 * KB; // Smaller size improve throughput and latency
    ExternalApplication.OutputBufferSize := 64 * KB; // Improve throughtput
    ExternalApplication.ErrorBufferSize := 64 * KB;

    ExternalApplication.Execute;

    CheckTrue(ExternalApplication.Running, 'Running application expected.');
    StartTime := MainTimer.Value;
    BytesSend := 0;
    while MainTimer.IntervalFrom(StartTime).SecondsAsF < 1 do
    begin
      ExternalApplication.WriteLine('test');
      Inc(BytesSend, 6);
    end;
    Status('Bytes Send:' + BToStr(BytesSend));
    ExternalApplication.WriteLine('quit');
    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TPipedExternalApplicationTest.TestWrite;
var
  ExternalApplication: TPipedExternalApplication;
begin
  ExternalApplication := TPipedExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'echo.exe';
    ExternalApplication.RequireOutputText := True;

    ExternalApplication.Execute;

    CheckTrue(ExternalApplication.Running, 'Running application expected.');
    ExternalApplication.WriteLine('test');
    ExternalApplication.WriteLine('quit');
    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

initialization
  RegisterTest('Piped External Application Test', TPipedExternalApplicationTest.Suite);
end.
