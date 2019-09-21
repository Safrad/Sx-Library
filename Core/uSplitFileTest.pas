unit uSplitFileTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TSplitFileTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uSplitFile,
  uFiles,
  uTemporaryDirectory,
  SysUtils;

{ TSplitFileTest }

procedure TSplitFileTest.Test;
const
  MaximalFileSize = 16;
var
  TargetPath: string;
begin
  TargetPath := TemporaryDirectory.ProcessTempDir;
  SplitFile(DataDir + 'Text3.txt', TargetPath, MaximalFileSize, True);
  CheckTrue(not FileExists(TargetPath + 'Text3.000'));
  CheckTrue(FileExists(TargetPath + 'Text3.001'));
  CheckTrue(FileExists(TargetPath + 'Text3.002'));
  CheckTrue(FileExists(TargetPath + 'Text3.003'));
  CheckTrue(FileExists(TargetPath + 'Text3.004'));
  CheckTrue(FileExists(TargetPath + 'Text3.005'));
  CheckTrue(not FileExists(TargetPath + 'Text3.006'));
end;

initialization
	RegisterTest('Split File Test', TSplitFileTest.Suite);
end.
