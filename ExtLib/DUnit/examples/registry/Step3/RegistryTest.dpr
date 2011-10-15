program RegistryTest;

uses
  TestFramework,
  GUITestRunner,
  RegistryUnitTest;

{$R *.RES}

function MasterTestSuite: ITestSuite;
begin
  Result := TTestSuite.Create;
  Result.AddTest(RegistryUnitTest.Suite);
end;

begin
  GUITestRunner.RunTest(MasterTestSuite);

end.
