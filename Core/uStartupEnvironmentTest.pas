unit uStartupEnvironmentTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TStartupEnvironmentTest = class(TTestCase)
  published
    procedure TestStartupEnvironmentVariables;
    procedure TestRemoveVariablesFromText;
  end;

implementation

uses
  SysUtils,
  Windows,

  uFiles,
  uFileCharset,
  uOperatingSystem,

  uStartupEnvironment;

{ TStartupEnvironmentTest }

procedure TStartupEnvironmentTest.TestStartupEnvironmentVariables;
const
  StartupEnvironmentVariables: array[0..31] of string = (
    // Windows XP
    'ALLUSERSPROFILE',
    'APPDATA',
    'CommonProgramFiles',
    'COMPUTERNAME',
    'ComSpec',
//    'FP_NO_HOST_CHECK',
    'HOMEDRIVE',
    'HOMEPATH',
    'LOGONSERVER',
    'NUMBER_OF_PROCESSORS',
    'OS',
    'Path',
    'PATHEXT',
    'PROCESSOR_ARCHITECTURE',
    'PROCESSOR_IDENTIFIER',
    'PROCESSOR_LEVEL',
    'PROCESSOR_REVISION',
    'ProgramFiles',
//    'PROMPT',
    'SESSIONNAME',
    'SystemDrive',
    'SystemRoot',
    'TEMP',
    'TMP',
    'USERDOMAIN',
    'USERNAME',
    'USERPROFILE',
    'windir',

    // Windows Vista
//    'DriverData',
    'LOCALAPPDATA',
    'PUBLIC',

    // 64 bit
    'CommonProgramFiles(x86)',
    'CommonProgramW6432',
    'ProgramFiles(x86)',
    'ProgramW6432'
  );

var
  i: SG;
  StartupEnvironment: TStartupEnvironment;
begin
  StartupEnvironment := TStartupEnvironment.Create;
  try
    CheckEquals('', StartupEnvironment.FindValue('unknown'));
    CheckEquals('C:\Windows', StartupEnvironment.FindValue('SystemRoot'));

    for i := 0 to Length(StartupEnvironmentVariables) - 1 do
    begin
      CheckTrue(StartupEnvironment.VariableExists(StartupEnvironmentVariables[i]), StartupEnvironmentVariables[i]);
      CheckTrue(StartupEnvironment.FindValue(StartupEnvironmentVariables[i]) <> '', StartupEnvironmentVariables[i]);
    end;

    CheckEquals('%SystemRoot%\Temp', StartupEnvironment.InsertVariablesFromStart('C:\Windows\Temp'));
  finally
    StartupEnvironment.Free;
  end;
end;

procedure TStartupEnvironmentTest.TestRemoveVariablesFromText;
var
  Charset: TFileCharset;
  DataA: AnsiString;
  Data: string;
  SourceFileName, TargetFileName, ReferentialFileName: TFileName;
begin
  SetEnvironmentVariable('testVariable2', 'test2');
  StartupEnvironment.ReloadVariables;
  StartupEnvironment.Add('testVariable', 'test');

  SourceFileName := DataDir + 'StartupEnvironment\Test.txt';

  Charset := ReadStringFromFileEx(SourceFileName, DataA);
  Data := StartupEnvironment.RemoveVariables(string(DataA));
  TargetFileName := OperatingSystem.TemporaryDirectory.ProcessTempDir + 'Test.txt';
  if FileExists(TargetFileName) then
    DeleteFileEx(TargetFileName);
  CheckTrue(WriteStringToFile(TargetFileName, Data, False, Charset));

  ReferentialFileName := DataDir + 'StartupEnvironment\Reference.txt';
  Check(SameFilesNoPrefix(TargetFileName, ReferentialFileName) = True, 'Files are different!');
end;

initialization
	RegisterTest('Startup Environment Test', TStartupEnvironmentTest.Suite);
end.
