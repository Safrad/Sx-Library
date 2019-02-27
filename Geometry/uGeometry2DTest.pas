//* Unit test for uGeometry2D

unit uGeometry2DTest;

interface

uses TestFrameWork;

type
  TGeometry2DTest = class(TTestCase)
  published
    procedure Test;
    procedure TestPrecision;
  end;

implementation

uses
  uTypes,
  uGeometry2D;

function SameValue(const V1, V2: FG): Boolean;
begin
  Result := Abs(V1 - V2) < (V1 / 1e19);
end;

{ TGeometry2DTest }

procedure TGeometry2DTest.Test;
var
  Point1, Point2, Point: TPoint2D;
  LineA: TLine2D;
  LineB: TLine2D;
  LineSegmentA: TLineSegment2D;
  LineSegmentB: TLineSegment2D;
begin
  Point1 := CreatePoint2D(0, 0);
  Point2 := CreatePoint2D(-2, 2);
  Check(GetSquaredDistance2D(Point1, Point2) = 8);

  LineA := CreateLine2D(CreatePoint2D(10, 10), CreatePoint2D(20, 20));
  LineB := CreateLine2D(CreatePoint2D(20, 10), CreatePoint2D(10, 20));
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);
  Check(Point.X = 15);
  Check(Point.Y = 15);

  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineSegmentB := CreateLineSegment2D(CreatePoint2D(10, 0), CreatePoint2D(0, 10));
  LineA := LineSegmentToLine2D(LineSegmentA);
  LineB := LineSegmentToLine2D(LineSegmentB);
  Check(ExistsTwoLineSegmentsIntersection2D(LineSegmentA.F, LineSegmentA.T, LineSegmentB.F, LineSegmentB.T) = True);
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);
  Check(Point.X = 5);
  Check(Point.Y = 5);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(10, 0), CreatePoint2D(20, -10));
  LineSegmentA := LineToLineSegment2D(LineA);
  LineSegmentB := LineToLineSegment2D(LineB);
  Check(ExistsTwoLineSegmentsIntersection2D(LineSegmentA, LineSegmentB) = False);
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);
  Check(Point.X = 5);
  Check(Point.Y = 5);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(11, 11), CreatePoint2D(12, 12));
  LineSegmentA := LineToLineSegment2D(LineA);
  LineSegmentB := LineToLineSegment2D(LineB);
  Check(ExistsTwoLineSegmentsIntersection2D(LineSegmentA, LineSegmentB) = False);
  Check(TwoLineIntersection2D(LineA, LineB, Point) = False);
  Check(Point.X = 0);
  Check(Point.Y = 0);

  LineA := CreateLine2D(CreatePoint2D(7, 10), CreatePoint2D(7, 10));
  LineB := CreateLine2D(CreatePoint2D(-10, 20), CreatePoint2D(12, 20));
  Check(TwoLineIntersection2D(LineA, LineB, Point) = False);
  Check(Point.X = 0);
  Check(Point.Y = 0);

  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineSegmentB := CreateLineSegment2D(CreatePoint2D(10, 0), CreatePoint2D(0, 10));
  Check(ExistsTwoLineSegmentsIntersection2D(LineSegmentA, LineSegmentB) = True);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(10, 0), CreatePoint2D(20, -10));
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(11, 11), CreatePoint2D(12, 12));
  Check(TwoLineIntersection2D(LineA, LineB, Point) = False);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(10, 0), CreatePoint2D(20, -10));
  Check(AreLinesParallel2D(LineA, LineB) = False);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineB := CreateLine2D(CreatePoint2D(11, 11), CreatePoint2D(12, 12));
  Check(AreLinesParallel2D(LineA, LineB) = True);

  LineA := CreateLine2D(CreatePoint2D(0, 0), CreatePoint2D(1000000, 0));
  LineB := CreateLine2D(CreatePoint2D(0, 0.001), CreatePoint2D(1000000, 0.00099999));
  TwoLineIntersection2D(LineA, LineB, Point);
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);

  LineA := CreateLine2D(CreatePoint2D(-100, 0), CreatePoint2D(100, 0));
  LineB := CreateLine2D(CreatePoint2D(0, 0.0000001), CreatePoint2D(1, 0.0000000999999999999));
  Check(TwoLineIntersection2D(LineA, LineB, Point) = True);

  LineA := CreateLine2D(CreatePoint2D(7, 7), CreatePoint2D(7, 17));
  LineB := CreateLine2D(CreatePoint2D(-10, 5), CreatePoint2D(10, 5));
  TwoLineIntersection2D(LineA, LineB, Point);
  Check(Point.X = 7);
  Check(Point.Y = 5);

  LineA := CreateLine2D(CreatePoint2D(-1, 2000), CreatePoint2D(10, 1999));
  LineB := CreateLine2D(CreatePoint2D(-1, 10), CreatePoint2D(10, -10));
  TwoLineIntersection2D(LineA, LineB, Point);

  // Parallel lines
  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 0));
  LineSegmentB := CreateLineSegment2D(CreatePoint2D(0, 5), CreatePoint2D(10, 5));
  Check(ExistsTwoLineSegmentsIntersection2D(LineSegmentA, LineSegmentB) = False);

  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 0));
  LineSegmentB := CreateLineSegment2D(CreatePoint2D(-10, 5), CreatePoint2D(20, 5));
  Check(TwoLineSegmentsDistance2D(LineSegmentA, LineSegmentB) = 5);

  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  LineSegmentB := CreateLineSegment2D(CreatePoint2D(13, 14), CreatePoint2D(14, 15));
  Check(TwoLineSegmentsDistance2D(LineSegmentA, LineSegmentB) = 5);

  Point := CreatePoint2D(0, 0);
  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 0), CreatePoint2D(10, 10));
  Check(PointLineSegmentDistance2D(Point, LineSegmentA) = 0);

  Point := CreatePoint2D(0, 0);
  LineSegmentA := CreateLineSegment2D(CreatePoint2D(-10, 5), CreatePoint2D(10, 5));
  Check(PointLineSegmentDistance2D(Point, LineSegmentA) = 5);

  Point := CreatePoint2D(0, 0);
  LineSegmentA := CreateLineSegment2D(CreatePoint2D(0, 6), CreatePoint2D(0, 6));
  Check(PointLineSegmentDistance2D(Point, LineSegmentA) = 6);

  Point := CreatePoint2D(0, 0);
  LineSegmentA := CreateLineSegment2D(CreatePoint2D(3, 4), CreatePoint2D(120, 5));
  Check(PointLineSegmentDistance2D(Point, LineSegmentA) = 5);

  Point := CreatePoint2D(0, 0);
  LineA := CreateLine2D(CreatePoint2D(200, 5), CreatePoint2D(300, 5));
  Check(PointLineDistance2D(Point, LineA) = 5);
end;

procedure TGeometry2DTest.TestPrecision;
var
  A, B: TPoint2D;
  D1, D2: TGeometryFloat2D;
begin
  A := CreatePoint2D(10e5, pi);
  B := CreatePoint2D(pi, 10e-5);
  D1 := Sqrt(GetSquaredDistance2D(A, B));
  D2 := GetDistance2D(A, B);
  Check(SameValue(D1, D2));

  if SizeOf(TGeometryFloat2D) >= 10 then
  begin
    A := CreatePoint2D(0, 0);
    B := CreatePoint2D(1e3, 1e-4);
    D1 := Sqrt(GetSquaredDistance2D(A, B));
    D2 := GetDistance2D(A, B);
    Check(SameValue(D1, D2));

    A := CreatePoint2D(10, 5);
    B := CreatePoint2D(7, 3);
    D1 := Sqrt(GetSquaredDistance2D(A, B));
    D2 := GetDistance2D(A, B);
    Check(SameValue(D1, D2));
  end;
end;

initialization
	RegisterTest('Geometry 2D Test', TGeometry2DTest.Suite);
end.

