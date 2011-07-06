unit uDemoColorMatrixAlpha;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoColorMatrixAlpha = class(TDemo)
  strict private
    procedure DrawRegular;
    procedure DrawAlpha;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoColorMatrixAlpha }

{$REGION}
/// The <A>IGPBitmap</A> interface (which inherits from the <A>IGPImage</A>
/// interface) and the <A>IGPImageAttributes</A> interface provide functionality
/// for getting and setting pixel values. You can use the <A>IGPImageAttributes</A>
/// interface to modify the alpha values for an entire image, or you can call
/// the <A>IGPBitmap.SetPixel</A> method to modify individual pixel values. For
/// more information on setting individual pixel values, see the next example
/// <A>Setting the Alpha Values of Individual Pixels</A>.
///
/// The following example draws a wide black line and then displays an opaque
/// image that covers part of that line.

procedure TDemoColorMatrixAlpha.DrawRegular;
var
  Bitmap: IGPBitmap;
  Pen: IGPPen;
begin
  Bitmap := TGPBitmap.Create('Texture1.jpg');
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 25);
  Graphics.DrawLine(Pen, TGPPoint.Create(10, 35), TGPPoint.Create(200, 35));
  Graphics.DrawImage(Bitmap, 30, 0, Bitmap.Width, Bitmap.Height);
end;

/// The top illustration above shows the resulting image, which is drawn at
/// (30, 0). Note that the wide black line doesn't show through the image.
///
/// The <A>IGPImageAttributes</A> interface has many properties that you can use
/// to modify images during rendering. In the following example, an
/// <A>IGPImageAttributes</A> object is used to set all the alpha values to 80
/// percent of what they were. This is done by initializing a color matrix and
/// setting the alpha scaling value in the matrix to 0.8. The color matrix is
/// passed to the <A>IGPImageAttributes.SetColorMatrix</A> method, and the
/// <A>IGPImageAttributes</A> object is passed to the <A>DrawImage</A> method of a
/// <A>IGPGraphics</A> object.

procedure TDemoColorMatrixAlpha.DrawAlpha;
const
  ColorMatrix: TGPColorMatrix = (
    M: ((1.0, 0.0, 0.0, 0.0, 0.0),
        (0.0, 1.0, 0.0, 0.0, 0.0),
        (0.0, 0.0, 1.0, 0.0, 0.0),
        (0.0, 0.0, 0.0, 0.8, 0.0),
        (0.0, 0.0, 0.0, 0.0, 1.0)));
var
  Bitmap: IGPBitmap;
  Pen: IGPPen;
  Attr: IGPImageAttributes;
  Width, Height: Integer;
begin
  Bitmap := TGPBitmap.Create('Texture1.jpg');
  Pen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0), 25);
  Attr := TGPImageAttributes.Create;
  Attr.SetColorMatrix(ColorMatrix, ColorMatrixFlagsDefault, ColorAdjustTypeBitmap);
  Graphics.DrawLine(Pen, TGPPoint.Create(10, 135), TGPPoint.Create(200, 135));
  Width := Bitmap.Width;
  Height := Bitmap.Height;
  Graphics.DrawImage(Bitmap,
    TGPRect.Create(30, 100, Width, Height), // Destination rectangle
    0, 0, Width, Height,                  // Source rectangle
    UnitPixel, Attr);
end;
{$ENDREGION}

procedure TDemoColorMatrixAlpha.Run;
begin
  DrawRegular;
  DrawAlpha;
end;

initialization
  RegisterDemo('Alpha Blending Lines and Fills\Using a Color Matrix to Set Alpha Values in Images', TDemoColorMatrixAlpha);

end.
