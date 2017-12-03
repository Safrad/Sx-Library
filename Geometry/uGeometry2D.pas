// Basic 2D geometric functions for point, line and line segment
// Line is infinite length
// Line segment is part of line (finite length)
// Vector is finite length (direction) Vector (1, 1) is equal to (2, 2)

unit uGeometry2D;

interface

uses uTypes;

type
  // Can be changed
//  TGeometryFloat2D = F4; // Fastest
//  TGeometryFloat2D = F8; // Fast
  TGeometryFloat2D = FG; // Precise

  TPoint2D = record
		X: TGeometryFloat2D;
    Y: TGeometryFloat2D;
  end;

  TLine2D = record
    F: TPoint2D;
    T: TPoint2D;
  end;

  TLineSegment2D = record
    F: TPoint2D;
    T: TPoint2D;
  end;

	TRectVertex = (rvLT, rvRT, rvRB, rvLB); // LeftTop, RightTop, RightBottom, LeftBottom

const
  CenterPoint2D: TPoint2D = (X: 0; Y: 0);

//** Point functions
function CreatePoint2D(const X, Y: TGeometryFloat2D): TPoint2D;
function EqualPoint2D(const APoint2D1, APoint2D2: TPoint2D): BG;
function OppositePoint2D(const APoint2D: TPoint2D): TPoint2D;
function DistanceFromCenter2D(const APoint2D: TPoint2D): TGeometryFloat2D;
function MultiplyPoint2D(const APoint2D: TPoint2D; const AMultiplicator: TGeometryFloat2D): TPoint2D;
function DividePoint2D(const APoint2D: TPoint2D; const ADivider: TGeometryFloat2D): TPoint2D;
function RotatePoint2D(const APoint2D: TPoint2D; const AAngle: TGeometryFloat2D): TPoint2D;
function ProjectionPoint2D(const APoint2D1: TPoint2D; const APoint2D2: TPoint2D): TGeometryFloat2D;
//* Return squared distance between A and B
function GetSquaredDistance2D(const A, B: TPoint2D): TGeometryFloat2D;

//* Return distance between A and B
function GetDistance2D(const A, B: TPoint2D): TGeometryFloat2D;

function Subtract2D(AVec1, AVec2: TPoint2D): TPoint2D;

//** Line functions

// Create new line
function CreateLine2D(const AFrom, ATo: TPoint2D): TLine2D;

//* Determine if 2 lines or line segments are parallel
function AreLinesParallel2D(LineAP1, LineAP2, LineBP1, LineBP2: TPoint2D): BG; overload;
function AreLinesParallel2D(const ALine1, ALine2: TLine2D): BG; overload;

//* Find the intersection of 2 lines.
function TwoLineIntersection2D(LineAP1, LineAP2, LineBP1, LineBP2: TPoint2D; out PtOfIntersection: TPoint2D): BG; overload;
function TwoLineIntersection2D(const LineA, LineB: TLine2D; out PtOfIntersection: TPoint2D): BG; overload;

//* return shortes distance between point and line segment
function PointLineSegmentDistance2D(const P, F0, T0: TPoint2D): TGeometryFloat2D; overload;
function PointLineSegmentDistance2D(const P: TPoint2D; const L: TLineSegment2D): TGeometryFloat2D; overload;

//* return shortes distance between point and line
function PointLineDistance2D(const P, F0, T0: TPoint2D): TGeometryFloat2D; overload;
function PointLineDistance2D(const P: TPoint2D; const L: TLine2D): TGeometryFloat2D; overload;

//* Line segment functions

// Create new line segment
function CreateLineSegment2D(const AFrom, ATo: TPoint2D): TLineSegment2D;

function LineToLineSegment2D(const ALine2D: TLine2D): TLineSegment2D;
function LineSegmentToLine2D(const ALineSegment2D: TLineSegment2D): TLine2D;

//* Determine if 2 line segments cross given their end-points
function ExistsTwoLineSegmentsIntersection2D(LineSegmentAP1, LineSegmentAP2, LineSegmentBP1, LineSegmentBP2: TPoint2D): BG; overload;
function ExistsTwoLineSegmentsIntersection2D(ALineSegment1, ALineSegment2: TLineSegment2D): BG; overload;

//* return shortes distance between two line segments
function TwoLineSegmentsDistance2D(const F0, T0, F1, T1: TPoint2D): TGeometryFloat2D; overload;
function TwoLineSegmentsDistance2D(const ALineSegment1, ALineSegment2: TLineSegment2D): TGeometryFloat2D; overload;

//* Return orientation
function Oriented2D(const Pt: TPoint2D; const Size: TPoint2D; const Orientation: TRectVertex; const Precision: TGeometryFloat2D): TPoint2D;
function OrientedEx2D(const Pt: TPoint2D; const Size: TPoint2D; const Orientation: TRectVertex): TPoint2D;


implementation

uses
	Math;

function CreatePoint2D(const X, Y: TGeometryFloat2D): TPoint2D;
begin
	Result.X := X;
	Result.Y := Y;
end;

function EqualPoint2D(const APoint2D1, APoint2D2: TPoint2D): BG;
begin
	Result := (APoint2D1.X = APoint2D2.X) and (APoint2D1.Y = APoint2D2.Y);
end;

function OppositePoint2D(const APoint2D: TPoint2D): TPoint2D;
begin
	Result.X := -APoint2D.X;
	Result.Y := -APoint2D.Y;
end;

function DistanceFromCenter2D(const APoint2D: TPoint2D): TGeometryFloat2D;
begin
  Result := Hypot(APoint2D.X, APoint2D.Y);
end;

function MultiplyPoint2D(const APoint2D: TPoint2D; const AMultiplicator: TGeometryFloat2D): TPoint2D;
begin
	Result.X := AMultiplicator * APoint2D.X;
	Result.Y := AMultiplicator * APoint2D.Y;
end;

function DividePoint2D(const APoint2D: TPoint2D; const ADivider: TGeometryFloat2D): TPoint2D;
begin
	Result.X := APoint2D.X / ADivider;
	Result.Y := APoint2D.Y / ADivider;
end;

function RotatePoint2D(const APoint2D: TPoint2D; const AAngle: TGeometryFloat2D): TPoint2D;
var
	L: TGeometryFloat2D;
	A: TGeometryFloat2D;
begin
	if AAngle = 0 then
		Result := APoint2D
	else
	begin
		A := ArcTan2(APoint2D.Y, APoint2D.X) + AAngle;
		L := DistanceFromCenter2D(APoint2D);
		Result.X := Cos(DegToRad(A)) * L;
		Result.Y := Sin(DegToRad(A)) * L;
	end;
end;

function ProjectionPoint2D(const APoint2D1: TPoint2D; const APoint2D2: TPoint2D): TGeometryFloat2D;
var
	A1, A2: TGeometryFloat2D;
begin
	A1 := ArcTan2(APoint2D1.Y, APoint2D1.X);
	A2 := ArcTan2(APoint2D2.Y, APoint2D2.X);

	Result := DistanceFromCenter2D(APoint2D1) * Cos(A1 - A2);
end;

function GetSquaredDistance2D(const A, B: TPoint2D): TGeometryFloat2D;
begin
	Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function GetDistance2D(const A, B: TPoint2D): TGeometryFloat2D;
begin
	Result := Hypot(A.X - B.X, A.Y - B.Y);
end;

function Subtract2D(AVec1, AVec2: TPoint2D): TPoint2D;
begin
	Result.X := AVec1.X - AVec2.X;
	Result.Y := AVec1.Y - AVec2.Y;
end;

function CreateLine2D(const AFrom, ATo: TPoint2D): TLine2D;
begin
  Result.F := AFrom;
  Result.T := ATo;
end;

function Oriented2D(const Pt: TPoint2D; const Size: TPoint2D; const Orientation: TRectVertex; const Precision: TGeometryFloat2D): TPoint2D;
begin
	case Orientation of
	rvLT:
	begin
		Result.X := Pt.X + Size.X / 2 + Precision;
		Result.Y := Pt.Y + Size.Y / 2 + Precision;
	end;
	rvRT:
	begin
		Result.X := Pt.X - Size.X / 2 - Precision;
		Result.Y := Pt.Y + Size.Y / 2 + Precision;
	end;
	rvRB:
	begin
		Result.X := Pt.X - Size.X / 2 - Precision;
		Result.Y := Pt.Y - Size.Y / 2 - Precision;
	end;
	rvLB:
	begin
		Result.X := Pt.X + Size.X / 2 + Precision;
		Result.Y := Pt.Y - Size.Y / 2 - Precision;
	end;
	end;
end;

function OrientedEx2D(const Pt: TPoint2D; const Size: TPoint2D; const Orientation: TRectVertex): TPoint2D;
begin
	case Orientation of
	rvLT:
	begin
		Result.X := Pt.X - Size.X / 2;
		Result.Y := Pt.Y - Size.Y / 2;
	end;
	rvRT:
	begin
		Result.X := Pt.X + Size.X / 2;
		Result.Y := Pt.Y - Size.Y / 2;
	end;
	rvRB:
	begin
		Result.X := Pt.X + Size.X / 2;
		Result.Y := Pt.Y + Size.Y / 2;
	end;
	rvLB:
	begin
		Result.X := Pt.X - Size.X / 2;
		Result.Y := Pt.Y + Size.Y / 2;
	end;
	end;
end;

function AreLinesParallel2D(LineAP1, LineAP2, LineBP1, LineBP2: TPoint2D): BG;
var
	diffLA, diffLB: TPoint2D;
begin
	diffLA := Subtract2D(LineAP2, LineAP1);
	diffLB := Subtract2D(LineBP2, LineBP1);

  Result := diffLA.X * diffLB.Y = diffLA.Y * diffLB.X;
end;

function AreLinesParallel2D(const ALine1, ALine2: TLine2D): BG;
begin
  Result := AreLinesParallel2D(ALine1.F, ALine1.T, ALine2.F, ALine2.T);
end;

function ExistsTwoLineIntersection2D(LineAP1, LineAP2, LineBP1, LineBP2: TPoint2D): BG;
var
	diffLA, diffLB, diffLAB: TPoint2D;
  Parallel: BG;
begin
	diffLA := Subtract2D(LineAP2, LineAP1);
	diffLB := Subtract2D(LineBP2, LineBP1);

  Parallel := diffLA.X * diffLB.Y = diffLA.Y * diffLB.X;

  if not Parallel then
    Result := True
  else
  begin
    diffLAB := Subtract2D(LineAP1, LineBP1);
    Result := diffLA.X * diffLAB.Y = diffLA.Y * diffLAB.X;
  end;
end;

function CreateLineSegment2D(const AFrom, ATo: TPoint2D): TLineSegment2D;
begin
  Result.F := AFrom;
  Result.T := ATo;
end;

function LineToLineSegment2D(const ALine2D: TLine2D): TLineSegment2D;
begin
  Result := TLineSegment2D(ALine2D);
end;

function LineSegmentToLine2D(const ALineSegment2D: TLineSegment2D): TLine2D;
begin
  Result := TLine2D(ALineSegment2D);
end;

function ExistsTwoLineSegmentsIntersection2D(LineSegmentAP1, LineSegmentAP2, LineSegmentBP1, LineSegmentBP2: TPoint2D): BG;
var
	diffLA, diffLB: TPoint2D;
	CompareA, CompareB: TGeometryFloat2D;
begin
	Result := False;

	diffLA := Subtract2D(LineSegmentAP2, LineSegmentAP1);
	diffLB := Subtract2D(LineSegmentBP2, LineSegmentBP1);

	CompareA := diffLA.X*LineSegmentAP1.Y - diffLA.Y*LineSegmentAP1.X;
	CompareB := diffLB.X*LineSegmentBP1.Y - diffLB.Y*LineSegmentBP1.X;

	if ( ((diffLA.X*LineSegmentBP1.Y - diffLA.Y*LineSegmentBP1.X) < CompareA) xor
			((diffLA.X*LineSegmentBP2.Y - diffLA.Y*LineSegmentBP2.X) < CompareA) ) and
			( ((diffLB.X*LineSegmentAP1.Y - diffLB.Y*LineSegmentAP1.X) < CompareB) xor
			((diffLB.X*LineSegmentAP2.Y - diffLB.Y*LineSegmentAP2.X) < CompareB) ) then
		Result := True;
end;

function ExistsTwoLineSegmentsIntersection2D(ALineSegment1, ALineSegment2: TLineSegment2D): BG;
begin
  Result := ExistsTwoLineSegmentsIntersection2D(ALineSegment1.F, ALineSegment1.T, ALineSegment2.F, ALineSegment2.T);
end;

function TwoLineIntersection2D(LineAP1, LineAP2, LineBP1, LineBP2: TPoint2D; out PtOfIntersection: TPoint2D): BG; overload;
var
	LDetLineA, LDetLineB, LDetDivInv, DetDiv: TGeometryFloat2D;
	LDiffLA, LDiffLB: TPoint2D;
begin
  try
    LDetLineA := LineAP1.X*LineAP2.Y - LineAP1.Y*LineAP2.X;
    LDetLineB := LineBP1.X*LineBP2.Y - LineBP1.Y*LineBP2.X;

    LDiffLA := Subtract2D(LineAP1, LineAP2);
    LDiffLB := Subtract2D(LineBP1, LineBP2);

    DetDiv := (LDiffLA.X * LDiffLB.Y) - (LDiffLA.Y * LDiffLB.X);
    if DetDiv = 0  then
    begin
      Result := False;
      PtOfIntersection := CenterPoint2D;
      Exit;
    end;
    LDetDivInv := 1 / DetDiv;

    PtOfIntersection.X := ((LDetLineA * LDiffLB.X) - (LDiffLA.X * LDetLineB)) * LDetDivInv;
    PtOfIntersection.Y := ((LDetLineA * LDiffLB.Y) - (LDiffLA.Y * LDetLineB)) * LDetDivInv;
    Result := True;
  except
    // Point is in infinite or out of float range
    Result := False;
    PtOfIntersection := CenterPoint2D;
  end;
end;

function TwoLineIntersection2D(const LineA, LineB: TLine2D; out PtOfIntersection: TPoint2D): BG; overload;
begin
  Result := TwoLineIntersection2D(LineA.F, LineA.T, LineB.F, LineB.T, PtOfIntersection);
end;

function PointLineDistance2D(const P, F0, T0: TPoint2D): TGeometryFloat2D;
var D: TGeometryFloat2D;
begin
	D := GetDistance2D(T0, F0);
	if D = 0 then
		Result := GetDistance2D(P, F0)
	else
		Result := Abs((T0.X - F0.X) * (F0.Y - P.Y) - (F0.X - P.X) * (T0.Y - F0.Y)) / D;
end;

function PointLineDistance2D(const P: TPoint2D; const L: TLine2D): TGeometryFloat2D; overload;
begin
  Result := PointLineDistance2D(P, L.F, L.T);
end;

function PointLineSegmentDistance2D(const P, F0, T0: TPoint2D): TGeometryFloat2D;
const
	c = 100;
var
	D: TGeometryFloat2D;
	N1, N2: TPoint2D;
begin
	N1.X := P.X + c * (F0.Y - T0.Y);
	N1.Y := P.Y - c * (F0.X - T0.X);
	N2.X := P.X - c * (F0.Y - T0.Y);
	N2.Y := P.Y + c * (F0.X - T0.X);
	if not ExistsTwoLineSegmentsIntersection2D(F0, T0, N1, N2) then
	begin
		Result := Min(GetDistance2D(P, F0), GetDistance2D(P, T0));
	end
	else
	begin
		D := GetDistance2D(T0, F0);
		if D = 0 then
			Result := GetSquaredDistance2D(P, F0)
		else
			Result := Abs((T0.X - F0.X) * (F0.Y - P.Y) - (F0.X - P.X) * (T0.Y - F0.Y)) / D;
	end;
end;

function PointLineSegmentDistance2D(const P: TPoint2D; const L: TLineSegment2D): TGeometryFloat2D;
begin
  Result := PointLineSegmentDistance2D(P, L.F, L.T);
end;

function TwoLineSegmentsDistance2D(const F0, T0, F1, T1: TPoint2D): TGeometryFloat2D;
begin
	Result :=
		Min(
			Min(PointLineSegmentDistance2D(F0, F1, T1), PointLineSegmentDistance2D(T0, F1, T1)),
			Min(PointLineSegmentDistance2D(F1, F0, T0), PointLineSegmentDistance2D(T1, F0, T0)));
end;

function TwoLineSegmentsDistance2D(const ALineSegment1, ALineSegment2: TLineSegment2D): TGeometryFloat2D;
begin
  Result := TwoLineSegmentsDistance2D(ALineSegment1.F, ALineSegment1.T, ALineSegment2.F, ALineSegment2.T);
end;

end.
