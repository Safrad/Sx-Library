//* Unit test for uXYPolygon

unit uXYPolygonTest;

interface

uses TestFrameWork;

type
  TXYPolygonTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uGeometry, uXYPolygon;

{ TPolygonTest }

procedure TXYPolygonTest.Test;
var
  Polygon: TPolygon;
  Pt: TXYPt;
begin
  Polygon := TPolygon.Create;
  try
    Polygon.AddVertex(CreatePt(-1, -1));

    Polygon.AddVertex(CreatePt(1, -1));

    Polygon.AddVertex(CreatePt(1, 1));

    Polygon.AddVertex(CreatePt(-1, 1));

    Pt.X := 0;
    Pt.Y := 0;
    Check(Polygon.PtIn(Pt) = True);

    Pt.X := 2;
    Pt.Y := 0;
    Check(Polygon.PtIn(Pt) = False);

    Pt.X := 0;
    Pt.Y := 2;
    Check(Polygon.PtIn(Pt) = False);

    // Border
    Pt.X := 1;
    Pt.Y := 0;
    Check(Polygon.PtIn(Pt) = False);

    // Corner
    Pt.X := 1;
    Pt.Y := 1;
    Check(Polygon.PtIn(Pt) = False);
  finally
    Polygon.Free;
  end;
end;

initialization
	RegisterTest('XYPolygon Test', TXYPolygonTest.Suite);
end.

