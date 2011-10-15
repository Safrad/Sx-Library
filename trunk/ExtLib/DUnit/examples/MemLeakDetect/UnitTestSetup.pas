{ $Id: UnitTestSetup.pas 7 2008-04-24 11:59:47Z judc $ }
{: DUnit: An XTreme testing framework for Delphi programs.
   @author  The DUnit Group.
   @version $Revision: 7 $
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
 * Portions created by The DUnit Group are Copyright (C) 2000.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

unit UnitTestSetup;
interface
uses
  TestFramework,
  TestExtensions;

// Implement a test decorator to initialize and clean up variable space for
// the demonstration of leak detection testcases.
type
  TSetUpClient = class(TTestSetUp)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation
uses
  FastMMMonitorTest,
  UnitTestLeak;

{ TSetUpClient }
procedure TSetUpClient.SetUp;
begin
  inherited;
  ClearVars;
end;

procedure TSetUpClient.TearDown;
begin
  inherited;
  ClearVars;
end;

initialization

  TestFramework.RegisterTest(TSetUpClient.Create(
    TTestSuite.Create('Demo Object Leak Tests', [TTestLeak.Suite,
                                                 TTestSetUpLeaks.Suite,
                                                 TTestTearDownLeaks.Suite,
                                                 TTestLeakOfSizeAllowed.Suite,
                                                 TTestLeakOfDifferentSizeAllowed.Suite])));
  TestFramework.RegisterTest(TSetUpClient.Create(
    TTestSuite.Create('Demo String Leak Tests', [TMemMonitorStringLeakHandling.Suite])));

  TestFramework.RegisterTest(TSetUpClient.Create(
    TTestSuite.Create('Demo Object Leak Tests', [TMemMonitorObjectLeakHandling.Suite])));

  TestFramework.RegisterTest(TSetUpClient.Create(
    TTestSuite.Create('Demo Object Leak Tests', [TMemMonitorMemAllocLeakHandling.Suite])));

  TestFramework.RegisterTest(TSetUpClient.Create(
    TTestSuite.Create('Demo Object Leak Tests', [TMemMonitorExceptLeakHandling.Suite])));

end.
