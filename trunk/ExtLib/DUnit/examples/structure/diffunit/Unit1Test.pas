unit Unit1Test;

interface

uses
  TestFramework, Unit1;

type
  TTestMyObject = class(TTestCase)
  private
    FMyObject: TMyObject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testMyObject;
  end;

implementation

{ TTestMyObject }

procedure TTestMyObject.Setup;
begin
  FMyObject := TMyObject.Create;
end;

procedure TTestMyObject.TearDown;
begin
  FMyObject.Free;
end;

procedure TTestMyObject.testMyObject;
begin
  try
    FMyObject.DoSomething;
  except
    check(false);
  end;
end;

initialization
  RegisterTest('', TTestMyObject.Suite);

end.
