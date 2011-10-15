// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program EPCDUnitWizardTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  XPTemplateParser in '..\XPTemplateParser.pas',
  XPDUnitMacrosTests in 'XPDUnitMacrosTests.pas',
  XPDUnitMacros in '..\XPDUnitMacros.pas',
  XPDUnitParametersTests in 'XPDUnitParametersTests.pas',
  XPDUnitParameters in '..\XPDUnitParameters.pas',
  XPDUnitCommon in '..\XPDUnitCommon.pas',
  XPTestedUnitUtils in '..\XPTestedUnitUtils.pas',
  XPTestedUnitUtilsTests in 'XPTestedUnitUtilsTests.pas',
  XPTemplateParserTests in 'XPTemplateParserTests.pas',
  XPTestedUnitParser in '..\XPTestedUnitParser.pas',
  XPTestedUnitParserTests in 'XPTestedUnitParserTests.pas',
  XPParserFilters in '..\XPParserFilters.pas';

{$R *.RES}

begin
  Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.

