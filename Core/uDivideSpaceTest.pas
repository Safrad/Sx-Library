unit uDivideSpaceTest;

interface

uses
  TestFramework, uDivideSpaceOptions;

type
  TDivideSpaceTest = class(TTestCase)
  private
    procedure TestForOptions(ADivideSpaceOptions: TDivideSpaceOptions);
  published
    procedure ConstructorTest;
    procedure FailTest;
    procedure OneBlockTest;
    procedure HorizontalBarTest;
    procedure WindowTest;
  end;

implementation

uses
  uDivideSpace, uGraph, Math, Types, SysUtils;

function ExampleRect: TRect;
begin
  Result := Rect(0, 0, 4, 4);
end;


{ TDivideSpaceTest }

procedure TDivideSpaceTest.ConstructorTest;
var
  DivideSpace: TDivideSpace;
begin
  DivideSpace := TDivideSpace.Create;
  try
    // No Code
  finally
    DivideSpace.Free;
  end;
end;

procedure TDivideSpaceTest.FailTest;
var
  DivideSpaceOptions: TDivideSpaceOptions;
  DivideSpace: TDivideSpace;
begin
  DivideSpace := TDivideSpace.Create;
  try
    DivideSpaceOptions := Default(TDivideSpaceOptions);
    DivideSpaceOptions.Horizontal.Divided := True;
    StartExpectingException(EArgumentException);
    DivideSpace.Divide2D(ExampleRect, DivideSpaceOptions);
    StopExpectingException;
  finally
    DivideSpace.Free;
  end;
end;

procedure TDivideSpaceTest.OneBlockTest;
begin
  TestForOptions(OneBlockDivideSpaceOptions);
end;

procedure TDivideSpaceTest.HorizontalBarTest;
begin
  TestForOptions(HorizontalBarDivideSpaceOptions(1));
  TestForOptions(HorizontalBarDivideSpaceOptions(3));
end;

procedure TDivideSpaceTest.WindowTest;
begin
  TestForOptions(WindowDivideSpaceOptions(1));
  TestForOptions(WindowDivideSpaceOptions(3));
end;

procedure TDivideSpaceTest.TestForOptions(ADivideSpaceOptions: TDivideSpaceOptions);
var
  DivideSpace: TDivideSpace;
  RectArray: TRectArray;
begin
  DivideSpace := TDivideSpace.Create;
  try
    RectArray := DivideSpace.Divide2D(ExampleRect, ADivideSpaceOptions);
  finally
    DivideSpace.Free;
  end;
end;

initialization
  RegisterTest('Divide Space Test', TDivideSpaceTest.Suite);

end.

