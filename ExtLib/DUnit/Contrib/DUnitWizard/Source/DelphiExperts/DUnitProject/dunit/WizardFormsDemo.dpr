program WizardFormsDemo;

uses
  Forms,
  XPDUnitSetup in '..\XPDUnitSetup.pas' {XPDUnitSetupForm},
  WizardFormsDemoMain in 'WizardFormsDemoMain.pas' {Form1},
  XPDUnitTestModule in '..\XPDUnitTestModule.pas' {XPDUnitTestCaseForm},
  XPDUnitProject in '..\XPDUnitProject.pas' {XPDUnitProjectForm},
  XPTestedUnitParser in '..\XPTestedUnitParser.pas',
  TestedUnitStream in 'TestedUnitStream.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
