// Basic 3D geometric functions for point

unit uGeometry3D;

interface

uses uTypes;

type
  // Can be changed
//  TGeometryFloat3D = F4; // Fastest
//  TGeometryFloat3D = F8; // Fast
  TGeometryFloat3D = FG; // Precise

	TSize3D = record
		W: TGeometryFloat3D; // Width
    H: TGeometryFloat3D; // Height
    D: TGeometryFloat3D; // Depth
	end;

	TPoint3D = record
		X: TGeometryFloat3D;
    Y: TGeometryFloat3D;
    Z: TGeometryFloat3D;
	end;

const
  CenterPoint3D: TPoint3D = (X: 0; Y: 0; Z: 0);

//** Point functions
function CreatePoint3D(const X, Y, Z: TGeometryFloat3D): TPoint3D;
function EqualPoint3D(const APoint3D1, APoint3D2: TPoint3D): BG;
function OppositePoint3D(const APoint3D: TPoint3D): TPoint3D;
function DistanceFromCenter3D(const APoint3D: TPoint3D): TGeometryFloat3D;
function MultiplyPoint3D(const APoint3D: TPoint3D; const AMultiplicator: TGeometryFloat3D): TPoint3D;
function DividePoint3D(const APoint3D: TPoint3D; const ADivider: TGeometryFloat3D): TPoint3D;
//* Return squared distance between A and B
function GetSquaredDistance3D(const A, B: TPoint3D): TGeometryFloat3D;

//* Return distance between A and B
function GetDistance3D(const A, B: TPoint3D): TGeometryFloat3D;

function Subtract3D(AVec1, AVec2: TPoint3D): TPoint3D;

implementation

uses
  Math;

function CreatePoint3D(const X, Y, Z: TGeometryFloat3D): TPoint3D;
begin
	Result.X := X;
	Result.Y := Y;
	Result.Z := Z;
end;

function EqualPoint3D(const APoint3D1, APoint3D2: TPoint3D): BG;
begin
	Result := (APoint3D1.X = APoint3D2.X) and (APoint3D1.Y = APoint3D2.Y) and (APoint3D1.Z = APoint3D2.Z);
end;

function OppositePoint3D(const APoint3D: TPoint3D): TPoint3D;
begin
	Result.X := -APoint3D.X;
	Result.Y := -APoint3D.Y;
	Result.Z := -APoint3D.Z;
end;

function DistanceFromCenter3D(const APoint3D: TPoint3D): TGeometryFloat3D;
begin
  Result := Hypot(Hypot(APoint3D.X, APoint3D.Y), APoint3D.Z);
end;

function MultiplyPoint3D(const APoint3D: TPoint3D; const AMultiplicator: TGeometryFloat3D): TPoint3D;
begin
	Result.X := AMultiplicator * APoint3D.X;
	Result.Y := AMultiplicator * APoint3D.Y;
	Result.Z := AMultiplicator * APoint3D.Z;
end;

function DividePoint3D(const APoint3D: TPoint3D; const ADivider: TGeometryFloat3D): TPoint3D;
begin
	Result.X := APoint3D.X / ADivider;
	Result.Y := APoint3D.Y / ADivider;
	Result.Z := APoint3D.Z / ADivider;
end;

function GetSquaredDistance3D(const A, B: TPoint3D): TGeometryFloat3D;
begin
	Result := Sqr(A.X - B.X) + Sqr(A.Y - B.Y) + Sqr(A.Z - B.Z);
end;

function GetDistance3D(const A, B: TPoint3D): TGeometryFloat3D;
begin
	Result := Hypot(Hypot(A.X - B.X, A.Y - B.Y), A.Z - B.Z);
end;

function Subtract3D(AVec1, AVec2: TPoint3D): TPoint3D;
begin
	Result.X := AVec1.X - AVec2.X;
	Result.Y := AVec1.Y - AVec2.Y;
	Result.Z := AVec1.Z - AVec2.Z;
end;

end.
