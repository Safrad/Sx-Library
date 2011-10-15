unit RegistryUnitTest;

interface

uses RegistryUnit, TestFramework, Registry;

type
  TTestRegistry = class(TTestCase)
  private
    FInstallPath: string;
    FModData: string;
    FReg: TRegistry;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestRegistrySample;
    procedure TestGetInstallPath;
  end;

  function Suite: ITestSuite;

implementation

function Suite: ITestSuite;
begin
  Suite := TTestSuite.Create(TTestRegistry);
end;

{ TTestRegistry }

procedure TTestRegistry.Setup;
begin
  FInstallPath := 'C:\Program Files\SampleApp';
  FModData := 'SomeData';
  FReg := TRegistry.Create;
  FReg.OpenKey(ABaseKey, true);
  FReg.WriteString('InstallPath', FInstallPath);
  FReg.OpenKey('ModuleAData', true);
  FReg.WriteString('Data', FModData);
  FReg.CloseKey;
end;

procedure TTestRegistry.TearDown;
begin
  FReg.DeleteKey(ABaseKey);
  FReg.Free;
end;

procedure TTestRegistry.TestGetInstallPath;
begin
  check(RegistryUnit.GetRegInstallPath = FInstallPath);
end;

procedure TTestRegistry.TestRegistrySample;
var
  InstallPath: string;
  ModData: string;
begin
  RegistryUnit.GetRegData(InstallPath, ModData);
  check(InstallPath = FInstallPath);
  check(ModData = FModData);
end;

end.
