unit uAsyncTaskForProjectOptionsTest;

interface

uses
  uAsyncTask;

type
  TAsyncTaskForProjectOptionsTest = class(TAsyncTask)
  public
    procedure Execute; override;
  end;

implementation

uses
  uTypes,
  uSystemPaths,
  uProjectOptions;

{ TAsyncTaskForProjectOptionsTest }

procedure TAsyncTaskForProjectOptionsTest.Execute;
var
  ProjectOptions: TProjectOptions;
  i: SG;
begin
  inherited;

  for i := 0 to 99 do
  begin
    ProjectOptions := TProjectOptions.Create;
    try
      ProjectOptions.RWBDSProj(SystemPaths.DataDir + 'Test.bdsproj', False);
    finally
      ProjectOptions.Free;
    end;
  end;
end;

end.
