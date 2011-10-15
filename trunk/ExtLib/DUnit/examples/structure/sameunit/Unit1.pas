unit Unit1;

interface

uses
  {$IFDEF TESTING}
  TestFramework,
  {$ENDIF}
  Classes, SysUtils;

type
  TMyObject = class(TObject)
  public
    procedure DoSomething;
  end;

  {$IFDEF TESTING}
  TTestMyObject = class(TTestCase)
  private
    FMyObject: TMyObject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testMyObject;
  end;
  {$ENDIF}

implementation

{ TMyObject }

procedure TMyObject.DoSomething;
begin
  // do something
end;

{$IFDEF TESTING}
function Suite: ITestSuite;
begin
  Result := TTestSuite.Create('Test MyObject');
  Result.AddTest(TTestMyObject.Create('testMyObject'));
end;

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
{$ENDIF}

initialization
  {$IFDEF TESTING}
    RegisterTest('', TTestMyObject.Suite);
  {$ENDIF}

end.


