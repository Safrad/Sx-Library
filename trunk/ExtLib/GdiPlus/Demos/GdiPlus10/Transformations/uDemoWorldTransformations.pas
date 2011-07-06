unit uDemoWorldTransformations;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoWorldTransformations = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
    procedure Example4;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoWorldTransformations }

{$REGION}
/// Affine transformations include rotating, scaling, reflecting, shearing, and
/// translating. In Microsoft Windows GDI+, the <A>IGPMatrix</A> interface
/// provides the foundation for performing affine transformations on vector
/// drawings, images, and text.
///
/// The world transformation is a property of the <A>IGPGraphics</A> interface.
/// The numbers that specify the world transformation are stored in a
/// <A>IGPMatrix</A> object, which represents a 3×3 matrix. The <A>IGPMatrix</A> and
/// <A>IGPGraphics</A> interfaces have several methods for setting the numbers in
/// the world transformation matrix. The examples in this section manipulate
/// rectangles because rectangles are easy to draw and it is easy to see the
/// effects of transformations on rectangles.
///
/// We start by creating a 50 by 50 rectangle and locating it at the origin
/// (0, 0). The origin is at the upper-left corner of the client area. The
/// rectangle is drawn in red.

procedure TDemoWorldTransformations.Example1;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Red, 2);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The following code applies a scaling transformation that expands the
/// rectangle by a factor of 1.75 in the x direction and shrinks the rectangle
/// by a factor of 0.5 in the y direction. The result is drawn in green.

procedure TDemoWorldTransformations.Example2;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.LimeGreen, 2);
  Graphics.ScaleTransform(1.75, 0.5);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The result is a rectangle that is longer in the x direction and shorter in
/// the y direction than the original.
///
/// To rotate the rectangle instead of scaling it, use the following code.
/// The result is drawn in blue.

procedure TDemoWorldTransformations.Example3;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Blue, 2);
  Graphics.RotateTransform(28);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// To translate the rectangle, use the following code (drawn in purple)

procedure TDemoWorldTransformations.Example4;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Fuchsia, 2);
  Graphics.TranslateTransform(150, 150);
  Graphics.DrawRectangle(Pen, Rect);
end;
{$ENDREGION}

procedure TDemoWorldTransformations.Run;
begin
  Example1;
  Example2;
  Graphics.ResetTransform;
  Example3;
  Graphics.ResetTransform;
  Example4;
end;

initialization
  RegisterDemo('Transformations\Using the World Transformation', TDemoWorldTransformations);

end.
