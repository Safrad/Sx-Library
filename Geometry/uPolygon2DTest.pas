//* Unit test for uPolygon2D

unit uPolygon2DTest;

interface

uses TestFrameWork;

type
  TPolygon2DTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uGeometry2D, uPolygon2D;

{ TPolygonTest }

procedure TPolygon2DTest.Test;
var
  Polygon: TPolygon2D;
  Pt: TPoint2D;
begin
  Polygon := TPolygon2D.Create;
  try
    Polygon.AddVertex(CreatePoint2D(-1, -1));

    Polygon.AddVertex(CreatePoint2D(1, -1));

    Polygon.AddVertex(CreatePoint2D(1, 1));

    Polygon.AddVertex(CreatePoint2D(-1, 1));

    Pt.X := 0;
    Pt.Y := 0;
    Check(Polygon.ContainPoint(Pt) = True);

    Pt.X := 2;
    Pt.Y := 0;
    Check(Polygon.ContainPoint(Pt) = False);

    Pt.X := 0;
    Pt.Y := 2;
    Check(Polygon.ContainPoint(Pt) = False);

    // Border
    Pt.X := 1;
    Pt.Y := 0;
    Check(Polygon.ContainPoint(Pt) = False);

    // Corner
    Pt.X := 1;
    Pt.Y := 1;
    Check(Polygon.ContainPoint(Pt) = False);
  finally
    Polygon.Free;
  end;
end;

initialization
	RegisterTest('Polygon2D Test', TPolygon2DTest.Suite);
end.

