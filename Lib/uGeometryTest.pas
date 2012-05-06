// Unit test for uGeometry

unit uGeometryTest;

interface

uses TestFrameWork;

type
  TGeometryTest = class(TTestCase)
  published
    procedure Test;
    procedure PrecisionTest;
  end;

implementation

uses
  uGeometry, uXYPolygon;

function SameValue(const V1, V2: Extended): Boolean;
begin
  Result := Abs(V1 - V2) < (V1 / 1e19);
end;

{ TGeometryTest }

procedure TGeometryTest.Test;
var
  Pt1, Pt2, Pt: TXYPt;
  VectorA: TXYVector;
  VectorB: TXYVector;
begin
  Pt1 := CreatePt(0, 0);
  Pt2 := CreatePt(-2, 2);
  Assert(GetSquaredDistance(Pt1, Pt2) = 8);

  VectorA := CreateVector(CreatePt(10, 10), CreatePt(20, 20));
  VectorB := CreateVector(CreatePt(20, 10), CreatePt(10, 20));
  Assert(LineIntersect(VectorA, VectorB, Pt) = True);
  Assert(Pt.X = 15);
  Assert(Pt.Y = 15);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(10, 0), CreatePt(0, 10));
  Assert(VectorCrossVector(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = True);
  Assert(LineIntersect(VectorA, VectorB, Pt) = True);
  Assert(Pt.X = 5);
  Assert(Pt.Y = 5);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(10, 0), CreatePt(20, -10));
  Assert(VectorCrossVector(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = False);
  Assert(LineIntersect(VectorA, VectorB, Pt) = True);
  Assert(Pt.X = 5);
  Assert(Pt.Y = 5);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(11, 11), CreatePt(12, 12));
  Assert(VectorCrossVector(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = False);
  Assert(LineIntersect(VectorA, VectorB, Pt) = False);
  Assert(Pt.X = 0);
  Assert(Pt.Y = 0);

  VectorA := CreateVector(CreatePt(7, 10), CreatePt(7, 10));
  VectorB := CreateVector(CreatePt(-10, 20), CreatePt(12, 20));
  Assert(LineIntersect(VectorA, VectorB, Pt) = False);
  Assert(Pt.X = 0);
  Assert(Pt.Y = 0);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(10, 0), CreatePt(0, 10));
  Assert(LineCrossLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = True);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(10, 0), CreatePt(20, -10));
  Assert(LineCrossLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = True);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(11, 11), CreatePt(12, 12));
  Assert(LineCrossLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = True);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(10, 0), CreatePt(20, -10));
  Assert(LineParallelLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = False);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(11, 11), CreatePt(12, 12));
  Assert(LineParallelLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = True);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(1000000, 0));
  VectorB := CreateVector(CreatePt(0, 0.001), CreatePt(1000000, 0.00099999));
  LineIntersect(VectorA, VectorB, Pt);
  Assert(LineIntersect(VectorA, VectorB, Pt) = True);

  VectorA := CreateVector(CreatePt(-100, 0), CreatePt(100, 0));
  VectorB := CreateVector(CreatePt(0, 0.0000001), CreatePt(1, 0.0000000999999999999));
  Assert(LineIntersect(VectorA, VectorB, Pt) = True);

  VectorA := CreateVector(CreatePt(7, 7), CreatePt(7, 17));
  VectorB := CreateVector(CreatePt(-10, 5), CreatePt(10, 5));
  LineIntersect(VectorA, VectorB, Pt);
  Assert(Pt.X = 7);
  Assert(Pt.Y = 5);

  VectorA := CreateVector(CreatePt(-1, 2000), CreatePt(10, 1999));
  VectorB := CreateVector(CreatePt(-1, 10), CreatePt(10, -10));
  LineIntersect(VectorA, VectorB, Pt);

  // Parallel lines
  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 0));
  VectorB := CreateVector(CreatePt(0, 5), CreatePt(10, 5));
  Assert(LineCrossLine(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = False);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 0));
  VectorB := CreateVector(CreatePt(-10, 5), CreatePt(20, 5));
  Assert(LineLineDistance(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = 5);

  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  VectorB := CreateVector(CreatePt(13, 14), CreatePt(14, 15));
  Assert(LineLineDistance(VectorA.F, VectorA.T, VectorB.F, VectorB.T) = 5);

  Pt1 := CreatePt(0, 0);
  VectorA := CreateVector(CreatePt(0, 0), CreatePt(10, 10));
  Assert(PointVectorDistance(Pt1, VectorA) = 0);

  Pt1 := CreatePt(0, 0);
  VectorA := CreateVector(CreatePt(-10, 5), CreatePt(10, 5));
  Assert(PointVectorDistance(Pt1, VectorA) = 5);

  Pt1 := CreatePt(0, 0);
  VectorA := CreateVector(CreatePt(0, 6), CreatePt(0, 6));
  Assert(PointVectorDistance(Pt1, VectorA) = 6);

  Pt1 := CreatePt(0, 0);
  VectorA := CreateVector(CreatePt(3, 4), CreatePt(120, 5));
  Assert(PointVectorDistance(Pt1, VectorA) = 5);

  Pt1 := CreatePt(0, 0);
  VectorA := CreateVector(CreatePt(200, 5), CreatePt(300, 5));
  Assert(PointLineDistance(Pt1, VectorA) = 5);
end;

procedure TGeometryTest.PrecisionTest;
var
  A, B: TXYPt;
  D1, D2: TFloat;
begin
  A := CreatePt(10e5, pi);
  B := CreatePt(pi, 10e-5);
  D1 := Sqrt(GetSquaredDistance(A, B));
  D2 := GetDistance(A, B);
  Assert(SameValue(D1, D2));

  A := CreatePt(0, 0);
  B := CreatePt(1e3, 1e-4);
  D1 := Sqrt(GetSquaredDistance(A, B));
  D2 := GetDistance(A, B);
  Assert(SameValue(D1, D2));

  A := CreatePt(10, 5);
  B := CreatePt(7, 3);
  D1 := Sqrt(GetSquaredDistance(A, B));
  D2 := GetDistance(A, B);
  Assert(SameValue(D1, D2));
end;


initialization
	RegisterTest('Geometry Test', TGeometryTest.Suite);
end.

