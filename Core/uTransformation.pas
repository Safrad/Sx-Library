unit uTransformation;

interface

uses
  Windows,
  uTypes,
  uGeometry2D;

type
  TTransformation = class
  private
    FZoom: FG;
    FOffset: TPoint2D;
    procedure SetOffset(const Value: TPoint2D);
    procedure SetZoom(const Value: FG);

    function CXToSX(const ACX: FG): FG;
    function CYToSY(const ACY: FG): FG;
    function SXToCX(const ASX: FG): FG;
    function SYToCY(const ASY: FG): FG;
  public
    constructor Create;

    property Zoom: FG read FZoom write SetZoom;
    property Offset: TPoint2D read FOffset write SetOffset;

    function Transform(const APoint: TPoint2D): TPoint2D;
    function TransformToInt(const APoint: TPoint2D): TPoint;
    function InverseTransform(const APoint: TPoint2D): TPoint2D;
  end;

implementation

{ TTransformation }

constructor TTransformation.Create;
begin
  FZoom := 1;
  FOffset := CenterPoint2D;
end;

function TTransformation.CXToSX(const ACX: FG): FG;
begin
  Result := Zoom * ACX + Offset.X;
end;

function TTransformation.CYToSY(const ACY: FG): FG;
begin
  Result := Zoom * ACY + Offset.Y;
end;

function TTransformation.SXToCX(const ASX: FG): FG;
begin
  Result := (ASX - Offset.X) / Zoom;
end;

function TTransformation.SYToCY(const ASY: FG): FG;
begin
  Result := (ASY - Offset.Y) / Zoom;
end;

function TTransformation.InverseTransform(const APoint: TPoint2D): TPoint2D;
begin
	Result.X := SXToCX(APoint.X);
	Result.Y := SYToCY(APoint.Y);
end;

procedure TTransformation.SetOffset(const Value: TPoint2D);
begin
  FOffset := Value;
end;

procedure TTransformation.SetZoom(const Value: FG);
begin
  FZoom := Value;
end;

function TTransformation.Transform(const APoint: TPoint2D): TPoint2D;
begin
	Result.X := CXToSX(APoint.X);
	Result.Y := CYToSY(APoint.Y);
end;

function TTransformation.TransformToInt(const APoint: TPoint2D): TPoint;
begin
	Result.X := Round(CXToSX(APoint.X));
	Result.Y := Round(CYToSY(APoint.Y));
end;

end.