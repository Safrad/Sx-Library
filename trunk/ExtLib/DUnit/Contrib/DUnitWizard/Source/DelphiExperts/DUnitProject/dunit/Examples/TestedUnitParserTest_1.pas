unit TestedUnitParserTest_1;

interface

uses
  TestFrameWork,
  XPTestedUnitParser,
  Classes;          // TStream;

function GlobalFunction1: integer;
procedure GlobalProcedure1;

type

  TXPStubClass1 = class(TTestCase)

    procedure StubPublishedMethod(const idx: integer);

  private

    FParser: IXPTestedUnitParser;
    FStream: TStream;

    property PrivateProperty1;

    procedure StubPrivateMethod;

    property PrivateProperty2;

  public

    procedure StubPublicMethod;
    class procedure StubPublicClassMethod;

    protected procedure SetUp; override;
    procedure TearDown; override;

  published

    property PublishedProperty1;
    property PublishedProperty2;

    procedure TestParse;
    procedure TestGetError;
    procedure TestExistsMethods;
    procedure TestInitiallyEmptyMethods;
    procedure TestMethodCount;
    procedure TestReParse;
    property PublishedProperty3;
    procedure TestPrivateMethodCount;
    procedure TestProtectedMethodCount;
    procedure TestPublicMethodCount;
    procedure TestPublishedMethodCount;
  end;

  TTClass = class of TXPStubClass1;

  TXPStubClass6 = class(TXPStubClass5, IXPTestedUnitParser, IUnknown);

  TXPStubClass5 = class;

  TXPStubClass2 = class

    procedure StubPublishedMethod;

  private

    FParser: IXPTestedUnitParser;
    FStream: TStream;

    procedure StubPrivateMethod(const idx: integer);

  public

    procedure StubPublicMethod;
    class procedure StubPublicClassMethod;

    property PublicProperty1;
    protected procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestParse;
    procedure TestGetError;
    procedure TestExistsMethods;
    procedure TestInitiallyEmptyMethods;
    property PublishedProperty1;
    property PublishedProperty2;
    procedure TestMethodCount;
    procedure TestPrivateMethodCount;
    procedure TestProtectedMethodCount;
    procedure TestPublicMethodCount;
    procedure TestPublishedMethodCount;
  end;

type
  TProceduralType1 = procedure;

procedure GlobalProcedure2;
function GlobalFunction2: integer;

implementation

uses
  XPTestedUnitUtils,
  SysUtils;

type
  TXPStubClass3 = class

    procedure StubPublishedMethod;

  private

    FParser: IXPTestedUnitParser;
    FStream: TStream;

    procedure StubPrivateMethod;

  public

    procedure StubPublicMethod;
    class procedure StubPublicClassMethod;

    property PublicProperty1;
    protected procedure SetUp; override;
    procedure TearDown; override;

  published

    procedure TestParse;
    procedure TestGetError;
    procedure TestExistsMethods;
    procedure TestInitiallyEmptyMethods;
    property PublishedProperty1;
    property PublishedProperty2;
    procedure TestMethodCount;
    procedure TestPrivateMethodCount;
    procedure TestProtectedMethodCount;
    procedure TestPublicMethodCount;
    procedure TestPublishedMethodCount;
  end;

  TXPStubClass4 = class(TXPStubClass3);

{ TXPStubClass1 }

procedure TXPStubClass1.SetUp;
begin
  inherited;
  FParser := XPTestedUnitParser.CreateXPTestedUnitParser;
  FStream := TFileStream.Create('XPTestedUnitParserTests.pas', fmOpenRead);
end;

procedure TXPStubClass1.TearDown;
begin
  FStream.Free;
  FParser := nil;
  inherited;
end;

procedure TXPStubClass1.TestInitiallyEmptyMethods;
var
  MethodNode: TXPMethodNode;

begin
  FParser.Classes.Iterator.Rest;
  Check(not FParser.Classes.Iterator.Next(MethodNode));
end;

procedure TXPStubClass1.TestExistsMethods;
begin
  Check(FParser.Classes.Iterator <> nil);
end;

procedure TXPStubClass1.TestGetError;
var
  Description: string;
  Error: TXPParserError;

begin
  FParser.GetError(Description, Error);
  Check(Error = peNone);
end;

procedure TXPStubClass1.TestParse;
begin
  Check(FParser.Parse(FStream));
end;

procedure TXPStubClass1.TestMethodCount;
var
  MethodNode: TXPMethodNode;
  Count: integer;

begin
  Count := 0;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    System.Inc(Count);

  CheckEquals(MethodCount, Count);
end;

procedure TXPStubClass1.TestPrivateMethodCount;
var
  MethodNode: TXPMethodNode;
  Count: integer;

begin
  Count := 0;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    if MethodNode.Visibility = cvPrivate then
      System.Inc(Count);

  CheckEquals(PrivateMethodCount, Count);
end;

procedure TXPStubClass1.TestProtectedMethodCount;
var
  MethodNode: TXPMethodNode;
  Count: integer;

begin
  Count := 0;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    if MethodNode.Visibility = cvProtected then
      System.Inc(Count);

  CheckEquals(ProtectedMethodCount, Count);
end;

procedure TXPStubClass1.TestPublicMethodCount;
var
  MethodNode: TXPMethodNode;
  Count: integer;

begin
  Count := 0;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    if MethodNode.Visibility = cvPublic then
      System.Inc(Count);

  CheckEquals(PublicMethodCount, Count);
end;

procedure TXPStubClass1.TestPublishedMethodCount;
var
  MethodNode: TXPMethodNode;
  Count: integer;

begin
  Count := 0;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    if MethodNode.Visibility = cvPublished then
      System.Inc(Count);

  CheckEquals(PublishedMethodCount, Count);
end;

procedure TXPStubClass1.StubPrivateMethod;
begin

end;

procedure TXPStubClass1.StubPublicMethod;
begin

end;

procedure TXPStubClass1.StubPublishedMethod(const idx: integer);
begin

end;

class procedure TXPStubClass1.StubPublicClassMethod;
begin

end;

procedure TXPStubClass1.TestReParse;
var
  MethodNode: TXPMethodNode;
  Count: integer;
  StreamPos: integer;

begin
  Count := 0;
  StreamPos := FStream.Position;
  FParser.Parse(FStream);
  FStream.Position := StreamPos;
  FParser.Parse(FStream);
  FParser.Classes.Iterator.Reset;

  while FParser.Classes.Iterator.Next(MethodNode) do
    System.Inc(Count);

  CheckEquals(MethodCount, Count);
end;

{ TXPStubClass2 }

procedure TXPStubClass2.SetUp;
begin
  inherited;

end;

procedure TXPStubClass2.StubPrivateMethod(const idx: integer);
begin

end;

class procedure TXPStubClass2.StubPublicClassMethod;
begin

end;

procedure TXPStubClass2.StubPublicMethod;
begin

end;

procedure TXPStubClass2.StubPublishedMethod;
begin

end;

procedure TXPStubClass2.TearDown;
begin
  inherited;

end;

procedure TXPStubClass2.TestExistsMethods;
begin

end;

procedure TXPStubClass2.TestGetError;
begin

end;

procedure TXPStubClass2.TestInitiallyEmptyMethods;
begin

end;

procedure TXPStubClass2.TestMethodCount;
begin

end;

procedure TXPStubClass2.TestParse;
begin

end;

procedure TXPStubClass2.TestPrivateMethodCount;
begin

end;

procedure TXPStubClass2.TestProtectedMethodCount;
begin

end;

procedure TXPStubClass2.TestPublicMethodCount;
begin

end;

procedure TXPStubClass2.TestPublishedMethodCount;
begin

end;

{ TXPStubClass3 }

procedure TXPStubClass3.SetUp;
begin
  inherited;

end;

procedure TXPStubClass3.StubPrivateMethod;
begin

end;

class procedure TXPStubClass3.StubPublicClassMethod;
begin

end;

procedure TXPStubClass3.StubPublicMethod;
begin

end;

procedure TXPStubClass3.StubPublishedMethod;
begin

end;

procedure TXPStubClass3.TearDown;
begin
  inherited;

end;

procedure TXPStubClass3.TestExistsMethods;
begin

end;

procedure TXPStubClass3.TestGetError;
begin

end;

procedure TXPStubClass3.TestInitiallyEmptyMethods;
begin

end;

procedure TXPStubClass3.TestMethodCount;
begin

end;

procedure TXPStubClass3.TestParse;
begin

end;

procedure TXPStubClass3.TestPrivateMethodCount;
begin

end;

procedure TXPStubClass3.TestProtectedMethodCount;
begin

end;

procedure TXPStubClass3.TestPublicMethodCount;
begin

end;

procedure TXPStubClass3.TestPublishedMethodCount;
begin

end;

type
  TXPStubClass5 = class(TXPStubClass4);

initialization

  TestFramework.RegisterTest('TestedUnitParserTest_1 Suite',
    TXPStubClass1.Suite);

end.

