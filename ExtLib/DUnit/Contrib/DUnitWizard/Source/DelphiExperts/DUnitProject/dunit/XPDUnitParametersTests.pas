unit XPDUnitParametersTests;

interface

uses
  TestFrameWork,
  XPDUnitCommon;

type

  TXPDUnitParametersTests = class(TTestCase)
  private

    FParameters: IXPDUnitParameters;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Add test methods here...
    procedure EnumerationLimits;
    procedure Descriptions;
    procedure Identifiers;
    procedure DefaultTemplates;
  end;

implementation

uses
  XPDUnitParameters,
  SysUtils;       // DeleteFile()

{ TXPDUnitParametersTests }

procedure TXPDUnitParametersTests.SetUp;
begin
  inherited;
  FParameters := XPDUnitParameters.CreateXPDUnitParameters;
end;

procedure TXPDUnitParametersTests.TearDown;
begin
  FParameters := nil;
  inherited;
end;

procedure TXPDUnitParametersTests.EnumerationLimits;
begin
  Check(System.Low(TXPDUnitParameter) = dpTestedUnitName);
  Check(System.High(TXPDUnitParameter) = dpProjectPath);
  CheckEquals(7, integer(dpProjectPath));
end;

procedure TXPDUnitParametersTests.Descriptions;
begin
  CheckEquals('TestModule unit name.', FParameters.Descriptions(dpUnitName));
end;

procedure TXPDUnitParametersTests.DefaultTemplates;
begin
  SysUtils.DeleteFile(XPDUnitSetupFile);
  CheckEquals('$FILESTEM($CURRENTUNIT)', FParameters.Templates(dpTestedUnitName));
  CheckEquals('$FILESTEM($CURRENTUNIT)Tests', FParameters.Templates(dpUnitName));
  CheckEquals('$FILEPATH($CURRENTUNIT)dunit\', FParameters.Templates(dpUnitPath));
  CheckEquals('$TESTEDCLASSNAMETests', FParameters.Templates(dpClassName));
  CheckEquals('$FILESTEM($CURRENTPROJECT)Tests', FParameters.Templates(dpProjectName));
  CheckEquals('$FILEPATH($CURRENTPROJECT)dunit\', FParameters.Templates(dpProjectPath));
end;

procedure TXPDUnitParametersTests.Identifiers;
begin
  CheckEquals('UNITNAME', FParameters.Identifiers(dpUnitName));
end;

initialization

  TestFramework.RegisterTest('XPDUnitParametersTests Suite',
    TXPDUnitParametersTests.Suite);

end.

