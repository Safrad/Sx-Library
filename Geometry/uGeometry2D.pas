// Basic 2D geometric functions for point, line, vector and polygon
// Vector is finite length
// Line is infinite length

unit uGeometry;

interface

type
  TFloat = Extended;

	PXYPt = ^TXYPt;
	TXYPt = record
		X, Y: TFloat;
	end;

  // Can represent vector or straignt line
  TXYVector = record
    F: TXYPt;
    T: TXYPt;
  end;
const
  ZeroPt: TXYPt = (X: 0; Y: 0);

type
	TRectVertex = (rvLT, rvRT, rvRB, rvLB); // LeftTop, RightTop, RightBottom, LeftBottom

function CreatePt(X, Y: TFloat): TXYPt;
function CreateVector(F, T: TXYPt): TXYVector;

//* return squared distance between A and B
function GetSquaredDistance(const A, B: TXYPt): TFloat;

//* return distance between A and B
function GetDistance(const A, B: TXYPt): TFloat;

//* Rerturn orientation
function Oriented(const Pt: TXYPt; const Size: TXYPt; const Orientation: TRectVertex; const Precision: TFloat): TXYPt;
function OrientedEx(const Pt: TXYPt; const Size: TXYPt; const Orientation: TRectVertex): TXYPt;

//* Determine if 2 vectors cross given their end-points
function VectorCrossVector(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;

//* Determine if 2 lines are parallel
function LineParallelLine(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;

//* Determine if 2 lines cross given their end-points
function LineCrossLine(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;

//* Find the intersection of 2 lines.
function LineIntersect(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt; out PtOfIntersection: TXYPt): Boolean; overload;
function LineIntersect(LineA, LineB: TXYVector; out PtOfIntersection: TXYPt): Boolean; overload;

//* return shortes distance between point and vector
function PointVectorDistance(const P, F0, T0: TXYPt): TFloat; overload;
function PointVectorDistance(const P: TXYPt; const L: TXYVector): TFloat; overload;

//* return shortes distance between point and line
function PointLineDistance(const P, F0, T0: TXYPt): TFloat; overload;
function PointLineDistance(const P: TXYPt; const L: TXYVector): TFloat; overload;

//* return shortes distance between lines
function LineLineDistance(const F0, T0, F1, T1: TXYPt): TFloat;

implementation

uses
	Math;

function CreatePt(X, Y: TFloat): TXYPt;
begin
  Result.X := X;
  Result.Y := Y;
end;

function CreateVector(F, T: TXYPt): TXYVector;
begin
  Result.F := F;
  Result.T := T;
end;

function GetSquaredDistance(const A, B: TXYPt): TFloat;
begin
	Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y);
end;

function GetDistance(const A, B: TXYPt): TFloat;
begin
	Result := Hypot(A.X - B.X, A.Y - B.Y);
end;

function Subtract(AVec1, AVec2: TXYPt): TXYPt;
begin
	Result.X := AVec1.X - AVec2.X;
	Result.Y := AVec1.Y - AVec2.Y;
end;

function Oriented(const Pt: TXYPt; const Size: TXYPt; const Orientation: TRectVertex; const Precision: TFloat): TXYPt;
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

function OrientedEx(const Pt: TXYPt; const Size: TXYPt; const Orientation: TRectVertex): TXYPt;
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

function LineParallelLine(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;
var
	diffLA, diffLB: TXYPt;
begin
	diffLA := Subtract(LineAP2, LineAP1);
	diffLB := Subtract(LineBP2, LineBP1);

  Result := diffLA.X * diffLB.Y = diffLA.Y * diffLB.X;
end;

function LineCrossLine(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;
var
	diffLA, diffLB, diffLAB: TXYPt;
  Parallel: Boolean;
begin
	diffLA := Subtract(LineAP2, LineAP1);
	diffLB := Subtract(LineBP2, LineBP1);

  Parallel := diffLA.X * diffLB.Y = diffLA.Y * diffLB.X;

  if not Parallel then
    Result := True
  else
  begin
    diffLAB := Subtract(LineAP1, LineBP1);
    Result := diffLA.X * diffLAB.Y = diffLA.Y * diffLAB.X;
  end;
end;

function VectorCrossVector(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt): Boolean;
var
	diffLA, diffLB: TXYPt;
	CompareA, CompareB: TFloat;
begin
	Result := False;

	diffLA := Subtract(LineAP2, LineAP1);
	diffLB := Subtract(LineBP2, LineBP1);

	CompareA := diffLA.X*LineAP1.Y - diffLA.Y*LineAP1.X;
	CompareB := diffLB.X*LineBP1.Y - diffLB.Y*LineBP1.X;

	if ( ((diffLA.X*LineBP1.Y - diffLA.Y*LineBP1.X) < CompareA) xor
			((diffLA.X*LineBP2.Y - diffLA.Y*LineBP2.X) < CompareA) ) and
			( ((diffLB.X*LineAP1.Y - diffLB.Y*LineAP1.X) < CompareB) xor
			((diffLB.X*LineAP2.Y - diffLB.Y*LineAP2.X) < CompareB) ) then
		Result := True;
end;

function LineIntersect(LineAP1, LineAP2, LineBP1, LineBP2: TXYPt; out PtOfIntersection: TXYPt): Boolean;
var
	LDetLineA, LDetLineB, LDetDivInv, DetDiv: TFloat;
	LDiffLA, LDiffLB: TXYPt;
begin
  try
    LDetLineA := LineAP1.X*LineAP2.Y - LineAP1.Y*LineAP2.X;
    LDetLineB := LineBP1.X*LineBP2.Y - LineBP1.Y*LineBP2.X;

    LDiffLA := Subtract(LineAP1, LineAP2);
    LDiffLB := Subtract(LineBP1, LineBP2);

    DetDiv := (LDiffLA.X * LDiffLB.Y) - (LDiffLA.Y * LDiffLB.X);
    if DetDiv = 0  then
    begin
      Result := False;
      PtOfIntersection := ZeroPt;
      Exit;
    end;
    LDetDivInv := 1 / DetDiv;

    PtOfIntersection.X := ((LDetLineA * LDiffLB.X) - (LDiffLA.X * LDetLineB)) * LDetDivInv;
    PtOfIntersection.Y := ((LDetLineA * LDiffLB.Y) - (LDiffLA.Y * LDetLineB)) * LDetDivInv;
    Result := True;
  except
    // Point is in infinite or out of float range
    Result := False;
    PtOfIntersection := ZeroPt;
  end;
end;

function LineIntersect(LineA, LineB: TXYVector; out PtOfIntersection: TXYPt): Boolean;
begin
  Result := LineIntersect(LineA.F, LineA.T, LineB.F, LineB.T, PtOfIntersection);
end;

function PointLineDistance(const P, F0, T0: TXYPt): TFloat;
var D: TFloat;
begin
	D := GetDistance(T0, F0);
	if D = 0 then
		Result := GetDistance(P, F0)
	else
		Result := Abs((T0.X - F0.X) * (F0.Y - P.Y) - (F0.X - P.X) * (T0.Y - F0.Y)) / D;
end;

function PointLineDistance(const P: TXYPt; const L: TXYVector): TFloat; overload;
begin
  Result := PointLineDistance(P, L.F, L.T);
end;

function PointVectorDistance(const P, F0, T0: TXYPt): TFloat;
const
	c = 100;
var
	D: TFloat;
	N1, N2: TXYPt;
begin
	N1.X := P.X + c * (F0.Y - T0.Y);
	N1.Y := P.Y - c * (F0.X - T0.X);
	N2.X := P.X - c * (F0.Y - T0.Y);
	N2.Y := P.Y + c * (F0.X - T0.X);
	if not VectorCrossVector(F0, T0, N1, N2) then
	begin
		Result := Min(GetDistance(P, F0), GetDistance(P, T0));
	end
	else
	begin
		D := GetDistance(T0, F0);
		if D = 0 then
			Result := GetSquaredDistance(P, F0)
		else
			Result := Abs((T0.X - F0.X) * (F0.Y - P.Y) - (F0.X - P.X) * (T0.Y - F0.Y)) / D;
	end;
end;

function PointVectorDistance(const P: TXYPt; const L: TXYVector): TFloat;
begin
  Result := PointVectorDistance(P, L.F, L.T);
end;

function LineLineDistance(const F0, T0, F1, T1: TXYPt): TFloat;
begin
	Result :=
		Min(
			Min(PointVectorDistance(F0, F1, T1), PointVectorDistance(T0, F1, T1)),
			Min(PointVectorDistance(F1, F0, T0), PointVectorDistance(T1, F0, T0)));
end;

end.
