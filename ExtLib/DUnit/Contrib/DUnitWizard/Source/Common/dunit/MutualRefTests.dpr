// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program MutualRefTests;

uses
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  XPObserver in '..\XPObserver.pas',
  XPInterfacedObjectTests in 'XPInterfacedObjectTests.pas',
  XPInterfacedObject in '..\XPInterfacedObject.pas',
  XPObserverTests in 'XPObserverTests.pas',
  XPTempReleaseTests in 'XPTempReleaseTests.pas';

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

