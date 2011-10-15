{#(@)$Id: UnitTests4Net.dpr 7 2008-04-24 11:59:47Z judc $ }
{  DUnit: An XTreme testing framework for Delphi programs. }
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
 * Brett Shearer <BrettShearer@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{ Note. This .NET test suite only runs the non-GUI tests as the GUI tests use pointers. }

program UnitTests4Net;

{%DotNetAssemblyCompiler '$(SystemRoot)\Microsoft.NET\Framework\v1.1.4322\System.Drawing.dll'}

uses
  Forms,
  SysUtils,
  UnitTestFramework in 'UnitTestFramework.pas',
  UnitTestExtensions in 'UnitTestExtensions.pas',
  TestFramework in '..\src\TestFramework.pas',
  TestExtensions in '..\src\TestExtensions.pas',
  NGUITestRunner in '..\src\NGUITestRunner.pas',
  TextTestRunner in '..\src\TextTestRunner.pas';

[STAThread]
begin
  if FindCmdLineSwitch('text-mode', ['-','/'], true) then
    TextTestRunner.RunRegisteredTests(rxbHaltOnFailures)
  else
    TGUITestRunner.RunRegisteredTests();
end.
