unit uExternalApplicationTest;

interface

uses
  SysUtils,

  uExternalApplication,

  TestFrameWork;

type
  TExternalApplicationTest = class(TTestCase)
  private
    procedure TestRepeatRunInternal(const AExternalApplication: TExternalApplication);
    procedure SetDeaultApplication(const AExternalApplication: TExternalApplication);
  published
    procedure TestReapatRun;
    procedure TestProperties;
    procedure TestMissingDirectory;
    procedure TestNotExists;
    procedure TestNotLoaded;
    procedure TestLoad;
    procedure TestExitCode;
    procedure TestClose;
    procedure TestTerminate;
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

procedure TExternalApplicationTest.SetDeaultApplication(const AExternalApplication: TExternalApplication);
var
  StartupWindowState: TStartupWindowState;
begin
  AExternalApplication.FileName := 'wmic';
  AExternalApplication.Parameters := 'cpu get name';
  AExternalApplication.CurrentDirectory := SystemPaths.LocalAppDataDir;
  StartupWindowState.WindowState := hwsHidden;
  StartupWindowState.Active := False;
  AExternalApplication.StartupWindowState := StartupWindowState;
end;

procedure TExternalApplicationTest.TestExitCode;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);

    ExternalApplication.Execute;

    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestLoad;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    CheckTrue(ExternalApplication.AllocatedMemory = 0);
    CheckTrue(ExternalApplication.AllocatedMemoryPeak = 0);
    SetDeaultApplication(ExternalApplication);

    ExternalApplication.Execute;

    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckTrue(ExternalApplication.AllocatedMemory > 0);
    CheckTrue(ExternalApplication.AllocatedMemoryPeak > 0);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestMissingDirectory;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    ExternalApplication.FileName := 'notepad.exe';

    StartExpectingException(EArgumentException);
    ExternalApplication.Execute;
    StopExpectingException;

    CheckFalse(ExternalApplication.Running);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestNotExists;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'NotExisting.exe';

    ExternalApplication.Execute;

    StartExpectingException(EIOException);
    try
      ExternalApplication.CheckErrorCode;
    except
      CheckEquals(2, ExternalApplication.ErrorCode);
      CheckFalse(ExternalApplication.Running);
      raise;
    end;
    StopExpectingException;

  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestNotLoaded;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'Uncomplete.exe'; // i. e. a required DLL is missing TODO : must exists

    ExternalApplication.Execute;

    StartExpectingException(EIOException);
    try
      ExternalApplication.CheckErrorCode;
    except
      CheckEquals(2, ExternalApplication.ErrorCode);
      CheckFalse(ExternalApplication.Running);
      raise;
    end;
    StopExpectingException;

    CheckFalse(ExternalApplication.Running);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestProperties;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.StartupType := stConsoleApplication;
    ExternalApplication.ProcessPriority := ppLow;

    ExternalApplication.Execute;

    ExternalApplication.WaitFor;
    CheckFalse(ExternalApplication.Running);
    CheckEquals(0, ExternalApplication.ExitCode);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestReapatRun;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);

    TestRepeatRunInternal(ExternalApplication);
    TestRepeatRunInternal(ExternalApplication);
    TestRepeatRunInternal(ExternalApplication);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestRepeatRunInternal(const AExternalApplication: TExternalApplication);
begin
  AExternalApplication.Execute;
  CheckTrue(AExternalApplication.Running);
  AExternalApplication.WaitFor;
  CheckFalse(AExternalApplication.Running);
  CheckEquals(0, AExternalApplication.ExitCode);
end;

procedure TExternalApplicationTest.TestClose;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'echo.exe';
    ExternalApplication.Parameters := '';

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;
    Sleep(100);
    CheckTrue(ExternalApplication.Running = True);
    ExternalApplication.Close;
    ExternalApplication.WaitFor;
    CheckTrue(ExternalApplication.Running = False);
  finally
    ExternalApplication.Free;
  end;
end;

procedure TExternalApplicationTest.TestTerminate;
var
  ExternalApplication: TExternalApplication;
begin
  ExternalApplication := TExternalApplication.Create;
  try
    SetDeaultApplication(ExternalApplication);
    ExternalApplication.FileName := 'echo.exe';
    ExternalApplication.Parameters := '';

    ExternalApplication.Execute;
    ExternalApplication.CheckErrorCode;
    Sleep(100);
    CheckTrue(ExternalApplication.Running = True);
    ExternalApplication.Terminate;
    ExternalApplication.WaitFor;
    CheckTrue(ExternalApplication.Running = False);
  finally
    ExternalApplication.Free;
  end;
end;

initialization
  RegisterTest('External Application Test', TExternalApplicationTest.Suite);
end.
