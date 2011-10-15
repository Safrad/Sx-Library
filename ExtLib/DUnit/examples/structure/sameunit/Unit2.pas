unit Unit2;

interface

uses
  {$IFDEF TESTING}
  TestFramework,
  {$ENDIF}
  Classes, SysUtils;

type
  TSuperObject = class(TObject)
  public
    function DoSomethingSuper: boolean;
  end;

  {$IFDEF TESTING}
  TTestSuperObject = class(TTestCase)
  private
    FSuperObject: TSuperObject;
  public
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure testSuperObject;
  end;

  function Suite: ITestSuite;
  {$ENDIF}

implementation

{ TSuperObject }

function TSuperObject.DoSomethingSuper :boolean;
begin
  // do something ...
  Result := (Random < 0.5);
end;

{$IFDEF TESTING}
function Suite: ITestSuite;
begin
  Result := TTestSuite.Create(TTestSuperObject);
end;

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
{$ENDIF}

initialization
  {$IFDEF TESTING}
    RegisterTest('', TTestSuperObject.Suite);
  {$ENDIF}

end.
 
