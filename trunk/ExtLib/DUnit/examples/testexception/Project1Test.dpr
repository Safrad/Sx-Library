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
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Juanco Añez <juanco@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{$IFDEF LINUX}
{$DEFINE DUNIT_CLX}
{$ENDIF}

{ Define DUNIT_CLX for project to use D6+ CLX }

program Project1Test;
uses
{$IFDEF DUNIT_CLX}
  QGUITestRunner,
{$ELSE}
  GUITestRunner,
{$ENDIF}
  TestFramework,
  Unit1Test,
  Unit2Test;

{$R *.res}

function MasterTestSuite: ITestSuite;
begin
  Result := TTestSuite.Create('Test App');
  Result.AddTest(Unit1Test.Suite);
  Result.AddTest(Unit2Test.Suite);
end;

begin
  TGUITestRunner.RunTest(MasterTestSuite);
end.

