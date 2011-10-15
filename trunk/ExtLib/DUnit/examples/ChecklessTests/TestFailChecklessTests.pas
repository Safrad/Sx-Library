{ $Id: TestFailChecklessTests.pas 7 2008-04-24 11:59:47Z judc $ }
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

unit TestFailChecklessTests;

interface

uses
  TestFrameWork;

type
  TTestEmptyTestProcs = class(TTestCase)
  published
    procedure TestWithActiveCallToCheck;
    procedure EmptyTestOptimizationOn;
    procedure EmptyTestOptimizationOff;
    procedure TestWithNoCallToCheck;
    procedure TestWithNoCallToCheck_FailSuppressed;
  end;

implementation
uses
  windows;

{$IFOPT O+}
{$DEFINE OPTIMIZED}
{$OPTIMIZATION OFF}
{$ENDIF}
procedure TTestEmptyTestProcs.EmptyTestOptimizationOff;
begin
// Test left intentionally empty
end;
{$IFDEF OPTIMIZED}
{$OPTIMIZATION ON}
{$ENDIF}

{$IFOPT O-}
{$DEFINE UNOPTIMIZED}
{$OPTIMIZATION ON}
{$ENDIF}
procedure TTestEmptyTestProcs.EmptyTestOptimizationOn;
begin
// Test left intentionally empty
end;

procedure TTestEmptyTestProcs.TestWithNoCallToCheck;
begin
  // Just sufficient code to thwart optimisation
  Sleep(10);
end;

procedure TTestEmptyTestProcs.TestWithActiveCallToCheck;
begin
  CheckTrue(True, 'Failed simple "True" check');
end;

procedure TTestEmptyTestProcs.TestWithNoCallToCheck_FailSuppressed;
begin
  FailsOnNoChecksExecuted := False;
end;

{$IFDEF UNOPTIMIZED}
{$OPTIMIZATION OFF}
{$ENDIF}

initialization
  TestFramework.RegisterTest(TTestEmptyTestProcs.Suite);

end.
