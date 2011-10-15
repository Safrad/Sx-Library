unit Unit2Test;

interface

uses
  TestFramework, Unit2;

type
  TTestSuperObject = class(TTestCase)
  private
    FSuperObject: TSuperObject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testSuperObject;
  end;

implementation

{ TTestSuperObject }

procedure TTestSuperObject.Setup;
begin
  FSuperObject := TSuperObject.Create;
end;

procedure TTestSuperObject.TearDown;
begin
  FSuperObject.Free;
end;

procedure TTestSuperObject.testSuperObject;
begin
  check(FSuperObject.DoSomethingSuper);
end;

initialization
  RegisterTest('', TTestSuperObject.Suite);

end.

