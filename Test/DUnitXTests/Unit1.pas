unit Unit1;

interface

uses
  DUnitX.TestFramework;

type
  [TestFixture]
  TMyTestObject = class(TObject) 
  public
    // Sample Methods
    // Simple single Test
    [Test]
    procedure Test1;
    // Test with TestCase Attribute to supply parameters.
    [Test]
    [TestCase('TestA','1,2')]
    [TestCase('TestB','3,4')]
    procedure Test2(const AValue1 : Integer;const AValue2 : Integer);
  end;

implementation

procedure TMyTestObject.Test1;
begin
  Assert.Pass('Ok');
end;

procedure TMyTestObject.Test2(const AValue1 : Integer;const AValue2 : Integer);
begin
  Assert.Pass('Ok');
end;

initialization
  TDUnitX.RegisterTestFixture(TMyTestObject);
end.
