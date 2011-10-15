{ $Id: UnitTestGUITestRunner.pas 7 2008-04-24 11:59:47Z judc $ }
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
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Uberto Barbini <uberto@usa.net>
 *
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{$IFDEF LINUX}
{$DEFINE DUNIT_CLX}
{$ENDIF}

unit UnitTestGUITestRunner;

interface

uses
  TestFramework,
  TestExtensions,
{$IFDEF DUNIT_CLX}
  QGUITestRunner,
  QComCtrls,
{$ELSE}
  GUITestRunner,
  ComCtrls,
{$ENDIF}
  SysUtils;

type
  TSampleTest = class(TTestCase)
  published
    procedure Succeed;
  end;

  TSampleTestSetup = class(TTestSetup)
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  Crack_TGUITestRunner = class(TGUITestRunner);

  TTestGUITestRunner = class(TTestCase)
  private
    FGUIRunner: Crack_TGUITestRunner;
  protected
    function SampleSuite: ITestSuite;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestDisplayOfDecorator;
{$IFNDEF DUNIT_CLX}
  {$IFDEF CONSOLE}
     protected
  {$ENDIF}
    procedure Test_CopyTestNametoClipboard;
{$ENDIF}
  end;

implementation

uses Clipbrd;

{ TTestGUITestRunner }

function TTestGUITestRunner.SampleSuite: ITestSuite;
begin
  Result := TTestSuite.Create('Sample Suite');
  Result.AddTest(TSampleTestSetup.Create(TSampleTest.Suite,
    'Sample SetUp Decorator on Suite'));
  Result.AddTest(TSampleTestSetup.Create(TSampleTest.Create('Succeed'),
    'Sample SetUp Decorator on Test'));
end;

procedure TTestGUITestRunner.SetUp;
begin
  fFailsOnNoChecksExecuted := False;
  FGUIRunner := Crack_TGUITestRunner.Create(nil);
end;

procedure TTestGUITestRunner.TearDown;
begin
  FGUIRunner.Free;
end;

procedure TTestGUITestRunner.TestDisplayOfDecorator;
var
  itemCount :Integer;
begin
  FGUIRunner.Suite := SampleSuite;
  FGUIRunner.AutoSaveAction.Checked := False;
  FGUIRunner.BreakOnFailuresAction.Checked := False;

  { GUITestRunner will show the decorator on a node. And the decorated test
    hierarchy underneath it. }
  itemCount := FGUIRunner.TestTree.Items.Count;
  check(itemCount = 6, 'wrong numer of items, was ' + IntToStr(itemCount));
end;

{$IFNDEF DUNIT_CLX}
procedure TTestGUITestRunner.Test_CopyTestNametoClipboard;
var node : TTreeNode;
begin
  AllowedMemoryLeakSize := 24;
  Clipboard.AsText := 'Test Case';
  FGUIRunner.CopyTestNameToClipboard(nil);
  Assert(Clipboard.AsText = 'Test Case');
  FGUIRunner.TestTree.Items.Clear;
  node := FGUIRunner.TestTree.Items.AddFirst(nil, 'Another Test');
  FGUIRunner.CopyTestNameToClipboard(node);
  Assert(Clipboard.AsText = 'Another Test');
end;
{$ENDIF}

{ TSampleTest }

procedure TSampleTest.Succeed;
begin
  check(true);
end;

{ TSampleTestSetup }

procedure TSampleTestSetup.SetUp;
begin
  { nothing - needed to override abstract }
end;

procedure TSampleTestSetup.TearDown;
begin
  { nothing - needed to override abstract }
end;

initialization
  RegisterTests('GUI Tests', [TTestGUITestRunner.Suite]);

end.
