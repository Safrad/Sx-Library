unit uXYPolygon;

interface

uses
  uGeometry;

type
	TXYPtDynArray = array of TXYPt;

//* return true if Point is inside polygon
function PtInPolygon(const X, Y: TFloat; const Polygon: TXYPtDynArray): Boolean; overload;
function PtInPolygon(const Pt: TXYPt; const Polygon: TXYPtDynArray): Boolean; overload;

//* return true if part of rectangle is inside polygon
function OverlapRectPolygon(const Polygon: TXYPtDynArray; const LabelPt: TXYPt; const Size: TXYPt): Boolean;

//* return shortest distance between line and polygon
function LinePolygonDistance(const F0, T0: TXYPt; const Polygon: TXYPtDynArray): TFloat;

type
  TPolygon = class
  private
    FVertexes: TXYPtDynArray;
    FCount: Integer;
  public
    procedure AddVertex(const Pt: TXYPt);
    property Vertexes: TXYPtDynArray read FVertexes;

    function PtIn(const APt: TXYPt): Boolean;
    function OverlapRect(const LabelPt: TXYPt; const Size: TXYPt): Boolean;
    function LineDistance(const F0, T0: TXYPt): TFloat;
  end;

implementation

uses Math;

function PtInPolygon(const X, Y: TFloat; const Polygon: TXYPtDynArray): Boolean;
var
	i, j: Integer;
begin
	Result := False;
	j := Length(Polygon) - 1;
	for i := 0 to Length(Polygon) - 1 do
	begin
		if ((Polygon[i].Y <= Y) and (Y < Polygon[j].Y)) or
				((Polygon[j].Y <= Y) and (Y < Polygon[i].Y)) then
		begin
			if (x < (Polygon[j].X - Polygon[i].X) *	(y - Polygon[i].Y) / (Polygon[j].Y - Polygon[i].Y) + Polygon[i].X) then
			begin
				Result := not Result;
			end;
		end;
		j := i;
	end;
end;

function PtInPolygon(const Pt: TXYPt; const Polygon: TXYPtDynArray): Boolean;
begin
  Result :=  PtInPolygon(Pt.X, Pt.Y, Polygon);
end;

function OverlapRectPolygon(const Polygon: TXYPtDynArray; const LabelPt: TXYPt; const Size: TXYPt): Boolean;
var
	P: array[TRectVertex] of TXYPt;
	rv: TRectVertex;
	i, j: Integer;
begin
	Result := False;
	if Length(Polygon) <= 1 then Exit;
	for rv := Low(rv) to High(rv) do
	begin
		P[rv] := OrientedEx(LabelPt, Size, rv);
	end;
	for i := 0 to Length(Polygon) - 1 do
	begin
		j := i + 1;
		if j >= Length(Polygon) then
			j := 0;
		if VectorCrossVector(Polygon[i], Polygon[j], P[rvLT], P[rvRT]) or
			VectorCrossVector(Polygon[i], Polygon[j], P[rvRT], P[rvRB]) or
			VectorCrossVector(Polygon[i], Polygon[j], P[rvRB], P[rvLB]) or
			VectorCrossVector(Polygon[i], Polygon[j], P[rvLB], P[rvLT]) then
		begin
			Result := True;
			Break;
		end;
	end;

	if Result = False then
	begin
		Result :=
			// Check if whole polygon is inside rectangle
			((Polygon[0].X >= P[rvLT].X) and (Polygon[0].X <= P[rvRB].X) and
			(Polygon[0].Y >= P[rvLT].Y) and (Polygon[0].Y <= P[rvRB].Y)) or
			// Check if whole rectangle is inside polygon
			(PtInPolygon(LabelPt.X, LabelPt.Y, Polygon));
	end;
end;

function LinePolygonDistance(const F0, T0: TXYPt; const Polygon: TXYPtDynArray): TFloat;
var i: Integer;
begin
	Result := Infinity;
	if Length(Polygon) = 0 then Exit;
	for i := 0 to Length(Polygon) - 2 do
	begin
		Result := Min(Result, LineLineDistance(F0, T0, Polygon[i], Polygon[i + 1]));
	end;
	Result := Min(Result, LineLineDistance(F0, T0, Polygon[Length(Polygon) - 1], Polygon[0]));
end;

{ TPolygon }

procedure TPolygon.AddVertex(const Pt: TXYPt);
begin
  SetLength(FVertexes, FCount + 1);
  FVertexes[FCount] := Pt;
  Inc(FCount);
end;

function TPolygon.LineDistance(const F0, T0: TXYPt): TFloat;
begin
  Result := LinePolygonDistance(F0, T0, FVertexes);
end;

function TPolygon.OverlapRect(const LabelPt, Size: TXYPt): Boolean;
begin
  Result := OverlapRectPolygon(FVertexes, LabelPt, Size);
end;

function TPolygon.PtIn(const APt: TXYPt): Boolean;
begin
  Result := PtInPolygon(APt, FVertexes);
end;

(*
function PolygonSize(const Polygon: TXYPtDynArray): TFloat;
var
	x1, y1, x2, y2: TFloat;
	i: Integer;
begin
	Result := 0;
	x1 := Polygon[0].X;
	y1 := Polygon[0].Y;
	i := 0;
	while (i <= Length(Polygon)) do
	begin
		if i >= Length(Polygon) then
		begin
			x2 := Polygon[0].X;
			y2 := Polygon[0].Y;
		end
		else
		begin
			x2 := Polygon[i].X;
			y2 := Polygon[i].Y;
		end;

		Result := Result + (x2 - x1) * (y2 + y1);
		x1 := x2;
		y1 := y2;
		Inc(i);
	end;
	Result := Abs(Result) / 2;
end;
*)

(*
function PolygonMiddle(const Polygon: TXYPtDynArray): TXYPt;
var
	i: Integer;
	PolygonLength: Integer;
begin
	PolygonLength := Length(Polygon);
	Result.X := 0;
	Result.Y := 0;
	if PolygonLength > 0 then
	begin
		for i := 0 to PolygonLength - 1 do
		begin
			Result.X := Result.X + Polygon[i].X;
			Result.Y := Result.Y + Polygon[i].Y;
		end;
		Result.X := Result.X / PolygonLength;
		Result.Y := Result.Y / PolygonLength;
	end;
end;
*)

end.
