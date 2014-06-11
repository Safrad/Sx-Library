unit uProjectOptionsTest;

interface

uses TestFrameWork;

type
  TProjectOptionsTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
	uTypes, uFiles, uProjectOptions;

{ TProjectOptionsTest }

procedure TProjectOptionsTest.Test;
var
  ProjectOptions: TProjectOptions;
begin
  ProjectOptions := TProjectOptions.Create;
  try
    ProjectOptions.RWBDSProj(DataDir + 'Test.bdsproj', False);
  finally
    ProjectOptions.Free;
  end;
end;

initialization
	RegisterTest('Project Options Test', TProjectOptionsTest.Suite);
end.
