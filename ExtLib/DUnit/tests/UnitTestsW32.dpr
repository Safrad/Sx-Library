{ $Id: UnitTestsW32.dpr 23 2008-08-26 04:42:20Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 23 $
}
(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2004.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

program UnitTestsW32;

uses
{$IFDEF FASTMM}
  {$IFNDEF CLR}
    FastMM4,
  {$ENDIF}
{$ENDIF}
  SysUtils,
  Forms,
  UnitTestFramework in 'UnitTestFramework.pas',
  UnitTestExtensions in 'UnitTestExtensions.pas',
  UnitTestGUITesting in 'UnitTestGUITesting.pas' {TestForm},
  UnitTestGUITestRunner in 'UnitTestGUITestRunner.pas',
  TestFramework in '..\src\TestFramework.pas',
  TestExtensions in '..\src\TestExtensions.pas',
  GUITestRunner in '..\src\GUITestRunner.pas',
  TextTestRunner in '..\src\TextTestRunner.pas',
  GUITesting in '..\src\GUITesting.pas';

{$R *.res}

(* NOTE:
  This program uses the test registration system.
  Units containing test cases register their test suites calling one of:
    TestFramework.RegisterTest
    TestFramework.RegisterTests
    TestFramework.RegisterTestSuites
*)

begin
  if FindCmdLineSwitch('text-mode', ['-','/'], true) then
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures)
  else
  begin
    Application.Initialize;
    Application.Title := 'DUnit Tests';
    // RunRegisteredTests class methods are recommended
    TGUITestRunner.RunRegisteredTests;
  end;
end.

