unit XPTestedUnitUtilsTests;

interface

uses
  TestFrameWork,
  XPTestedUnitUtils;

type

  TXPTestedUnitUtilsTests = class(TTestCase)
  private
    FSectionNode: IXPSectionNode;
  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published

    // Test methods
    procedure TestEmptySectionNode;
    procedure TestOneClassNode;
    procedure TestEnabledDefaults;

  end;

implementation

uses
  SysUtils;

const
  VisibilityStrs: array [TXPClassVisibility] of string
    = ('cvNone', 'cvPrivate', 'cvProtected', 'cvPublic', 'cvPublished');

{ TXPTestedUnitUtilsTests }

procedure TXPTestedUnitUtilsTests.SetUp;
begin
  inherited;
  FSectionNode := XPTestedUnitUtils.CreateXPSectionNode(nil, usInterface);
end;

procedure TXPTestedUnitUtilsTests.TearDown;
begin
  inherited;
  FSectionNode := nil;
end;

procedure TXPTestedUnitUtilsTests.TestEmptySectionNode;
var
  ClassNode: IXPParserNode;
  idx: integer;

begin
  CheckEquals(0, FSectionNode.Count, 'Empty Count');
  FSectionNode.Children.Start;
  Check(not FSectionNode.Children.Next(ClassNode), 'Empty Iterator');
  Check(not FSectionNode.DeleteObserver(ClassNode), 'Empty Delete');
//  Check(not FSectionNode.Find(ClassNode, idx), 'Empty Find');
  idx := 0;
  Check(not FSectionNode.GetChild(idx, ClassNode), 'Empty Get[0]');
  idx := -1;
  Check(not FSectionNode.GetChild(idx, ClassNode), 'Empty Get[-1]');
  idx := 1;
  Check(not FSectionNode.GetChild(idx, ClassNode), 'Empty Get[1]');

  FSectionNode.Clear;
  CheckEquals(0, FSectionNode.Count, 'Count after Clear');
  FSectionNode.Children.Start;
  Check(not FSectionNode.Children.Next(ClassNode), 'Iterator:Next after Clear');
end;

procedure TXPTestedUnitUtilsTests.TestEnabledDefaults;
var
  ClassNode: IXPClassNode;
  idx: TXPClassVisibility;

begin
  ClassNode := CreateXPClassNode(FSectionNode, 'TMyClass');
  Check(ClassNode.Enabled, 'default ClassNode.Enabled');

  for idx := cvPrivate to cvPublished do
    CheckEquals(true, ClassNode.Visibilities[idx].Enabled,
      Format('default ClassNode.VisibilityEnabled[%s]', [VisibilityStrs[idx]]));
end;

procedure TXPTestedUnitUtilsTests.TestOneClassNode;
var
  ClassNode: IXPParserNode;
  ClassNodeDup: IXPParserNode;
  idx: integer;

begin
  ClassNode := CreateXPClassNode(nil, 'TMyClass');

  Check(not FSectionNode.InsertChild(-1, ClassNode),
    'Insert range error lower bound');
  Check(not FSectionNode.InsertChild(1, ClassNode),
    'Insert range error upper bound');
  Check(FSectionNode.InsertChild(0, ClassNode),
    'Insert child into empty tree');

  ClassNodeDup := ClassNode;

  CheckEquals(1, FSectionNode.Count, 'Count after Insert');
  CheckEquals(1, FSectionNode.ChildCount, 'ChildCount after Insert');
  Check(FSectionNode.DeleteChild(ClassNode), 'Delete(0)');
  CheckEquals(0, FSectionNode.Count, 'Count after Delete');
  CheckEquals(0, FSectionNode.ChildCount, 'ChildCount after Delete');

  Check(FSectionNode.AddChild(ClassNodeDup), 'Add first node');
  CheckEquals(1, FSectionNode.Count, 'Count after Add');
  Check(not FSectionNode.AddChild(ClassNodeDup), 'Add duplicate');
  CheckEquals(1, FSectionNode.Count, 'Count after duplicate Add');
  Check(not FSectionNode.InsertChild(0, ClassNodeDup), 'Insert duplicate');
  CheckEquals(1, FSectionNode.Count, 'Count after duplicate Insert');

  FSectionNode.Children.Start;
  Check(FSectionNode.Children.Next(ClassNode), 'Iterator:first Next');
  Check(not FSectionNode.Children.Next(ClassNode), 'Iterator:second Next');

  idx := 0;
  Check(FSectionNode.GetChild(idx, ClassNode), 'Get[0]');
  idx := -1;
  Check(not FSectionNode.GetChild(idx, ClassNode),
    'Get[-1] range error lower bound');
  idx := 1;
  Check(not FSectionNode.GetChild(idx, ClassNode),
    'Get[1] range error upper bound');

  Check(FSectionNode.DeleteChild(ClassNodeDup), 'Delete(0)');
  CheckEquals(0, FSectionNode.Count, 'Count after Delete');
  Check(not FSectionNode.DeleteChild(ClassNodeDup), 'Delete(0) repeat');
  CheckEquals(0, FSectionNode.Count, 'Count after Delete repeat');

  Check(FSectionNode.AddChild(ClassNodeDup), 'Add first node');
  CheckEquals(1, FSectionNode.Count, 'Count after Add');
  FSectionNode.Clear;
  CheckEquals(0, FSectionNode.Count, 'Count after Clear');
  FSectionNode.Children.Start;
  Check(not FSectionNode.Children.Next(ClassNode),
    'Iterator:Next after Clear');

  ClassNode := CreateXPClassNode(FSectionNode, 'TMyClass');
  CheckEquals(1, FSectionNode.Count, 'Count after factory construction');
  CheckEquals(1, FSectionNode.ChildCount, 'ChildCount after factory construction');
end;

initialization

  TestFramework.RegisterTest('XPTestedUnitUtilsTests Suite',
    TXPTestedUnitUtilsTests.Suite);

end.

