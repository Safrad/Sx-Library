unit uDemoTransformationOrder;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoTransformationOrder = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoTransformationOrder }

{$REGION}
/// A single <A>IGPMatrix</A> object can store a single transformation or a
/// sequence of transformations. The latter is called a composite
/// transformation. The matrix of a composite transformation is obtained by
/// multiplying the matrices of the individual transformations.
///
/// In a composite transformation, the order of the individual transformations
/// is important. For example, if you first rotate, then scale, then translate,
/// you get a different result than if you first translate, then rotate, then
/// scale. In Microsoft Windows GDI+, composite transformations are built from
/// left to right. If S, R, and T are scale, rotation, and translation matrices
/// respectively, then the product SRT (in that order) is the matrix of the
/// composite transformation that first scales, then rotates, then translates.
/// The matrix produced by the product SRT is different from the matrix produced
/// by the product TRS.
///
/// One reason order is significant is that transformations like rotation and
/// scaling are done with respect to the origin of the coordinate system.
/// Scaling an object that is centered at the origin produces a different result
/// than scaling an object that has been moved away from the origin. Similarly,
/// rotating an object that is centered at the origin produces a different
/// result than rotating an object that has been moved away from the origin.
///
/// The following example combines scaling, rotation and translation (in that
/// order) to form a composite transformation. The argument
/// <A>MatrixOrderAppend</A> passed to the <A>RotateTransform</A> method
/// specifies that the rotation will follow the scaling. Likewise, the argument
/// <A>MatrixOrderAppend</A> passed to the <A>TranslateTransform</A> method
/// specifies that the translation will follow the rotation. The result is
/// drawn in red.

procedure TDemoTransformationOrder.Example1;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Red, 6);
  Graphics.ScaleTransform(1.75, 0.5);
  Graphics.RotateTransform(28, MatrixOrderAppend);
  Graphics.TranslateTransform(150, 150, MatrixOrderAppend);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The following example makes the same method calls as the previous example,
/// but the order of the calls is reversed. The resulting order of operations is
/// first translate, then rotate, then scale, which produces a very different
/// result than first scale, then rotate, then translate. The result is drawn
/// in green.

procedure TDemoTransformationOrder.Example2;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Lime, 6);
  Graphics.TranslateTransform(150, 150);
  Graphics.RotateTransform(28, MatrixOrderAppend);
  Graphics.ScaleTransform(1.75, 0.5, MatrixOrderAppend);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// One way to reverse the order of the individual transformations in a
/// composite transformation is to reverse the order of a sequence of method
/// calls. A second way to control the order of operations is to change the
/// matrix order argument. The following example is the same as the previous
/// example, except that MatrixOrderAppend has been changed to
/// MatrixOrderPrepend. The matrix multiplication is done in the order SRT,
/// where S, R, and T are the matrices for scale, rotate, and translate,
/// respectively. The order of the composite transformation is first scale, then
/// rotate, then translate. The result in drawn in yellow.

procedure TDemoTransformationOrder.Example3;
var
  Rect: TGPRect;
  Pen: IGPPen;
begin
  Rect.Initialize(0, 0, 50, 50);
  Pen := TGPPen.Create(TGPColor.Yellow, 2);
  Graphics.TranslateTransform(150, 150, MatrixOrderPrepend);
  Graphics.RotateTransform(28, MatrixOrderPrepend);
  Graphics.ScaleTransform(1.75, 0.5, MatrixOrderPrepend);
  Graphics.DrawRectangle(Pen, Rect);
end;

/// The result of the preceding example is the same result that we achieved in
/// the first example of this section (that's why the yellow rectangle overlays
/// the red rectangle). This is because we reversed both the order of the method
/// calls and the order of the matrix multiplication.
{$ENDREGION}

procedure TDemoTransformationOrder.Run;
begin
  Example1;
  Graphics.ResetTransform;
  Example2;
  Graphics.ResetTransform;
  Example3;
end;

initialization
  RegisterDemo('Transformations\Why Transformation Order Is Significant', TDemoTransformationOrder);

end.
