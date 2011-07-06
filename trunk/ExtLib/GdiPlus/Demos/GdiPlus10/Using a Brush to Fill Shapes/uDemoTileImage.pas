unit uDemoTileImage;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoTileImage = class(TDemo)
  strict private
    procedure Tile;
    procedure FlipHorizontally;
    procedure FlipVertically;
    procedure FlipXY;
  strict protected
    procedure Run; override;
  end;

implementation

{$REGION}
/// Just as tiles can be placed next to each other to cover a floor, rectangular
/// images can be placed next to each other to fill (tile) a shape. To tile the
/// interior of a shape, use a texture brush. When you construct a
/// <A>IGPTextureBrush</A> object, one of the arguments you pass to the
/// constructor is an <A>IGPImage</A> object. When you use the texture brush to
/// paint the interior of a shape, the shape is filled with repeated copies of
/// this image.
///
/// The wrap mode property of the <A>IGPTextureBrush</A> object determines how the
/// image is oriented as it is repeated in a rectangular grid. You can make all
/// the tiles in the grid have the same orientation, or you can make the image
/// flip from one grid position to the next. The flipping can be horizontal,
/// vertical, or both. The following examples demonstrate tiling with different
/// types of flipping.
///
/// <H>Tiling an Image</H>
/// This example uses a 75×75 image to tile a 200×200 rectangle:

procedure TDemoTileImage.Tile;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
  BlackPen: IGPPen;
begin
  Image := TGPImage.Create('HouseAndTree.gif');
  Brush := TGPTextureBrush.Create(Image);
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));

  Graphics.FillRectangle(Brush, TGPRect.Create(0, 0, 200, 200));
  Graphics.DrawRectangle(BlackPen, TGPRect.Create(0, 0, 200, 200));
end;

/// Top-left image: note that all tiles have the same orientation; there is no
/// flipping.
///
/// <H>Flipping an Image Horizontally While Tiling</H>
/// This example uses a 75×75 image to fill a 200×200 rectangle. The wrap mode
/// is set to flip the image horizontally.

procedure TDemoTileImage.FlipHorizontally;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
  BlackPen: IGPPen;
begin
  Image := TGPImage.Create('HouseAndTree.gif');
  Brush := TGPTextureBrush.Create(Image);
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));

  Brush.WrapMode := WrapModeTileFlipX;
  Graphics.FillRectangle(Brush, TGPRect.Create(300, 0, 200, 200));
  Graphics.DrawRectangle(BlackPen, TGPRect.Create(300, 0, 200, 200));
end;

/// The top-right image shows how the rectangle is tiled with the image. Note
/// that as you move from one tile to the next in a given row, the image is
/// flipped horizontally.
///
/// <H>Flipping an Image Vertically While Tiling</H>
/// This example uses a 75×75 image to fill a 200×200 rectangle. The wrap mode
/// is set to flip the image vertically.

procedure TDemoTileImage.FlipVertically;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
  BlackPen: IGPPen;
begin
  Image := TGPImage.Create('HouseAndTree.gif');
  Brush := TGPTextureBrush.Create(Image);
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));

  Brush.WrapMode := WrapModeTileFlipY;
  Graphics.FillRectangle(Brush, TGPRect.Create(0, 300, 200, 200));
  Graphics.DrawRectangle(BlackPen, TGPRect.Create(0, 300, 200, 200));
end;

/// The bottom-left illustration shows how the rectangle is tiled with the
/// image. Note that as you move from one tile to the next in a given column,
/// the image is flipped vertically.
///
/// <H>Flipping an Image Horizontally and Vertically While Tiling</H>
/// This example uses a 75×75 image to tile a 200×200 rectangle. The wrap mode
/// is set to flip the image both horizontally and vertically.

procedure TDemoTileImage.FlipXY;
var
  Image: IGPImage;
  Brush: IGPTextureBrush;
  BlackPen: IGPPen;
begin
  Image := TGPImage.Create('HouseAndTree.gif');
  Brush := TGPTextureBrush.Create(Image);
  BlackPen := TGPPen.Create(TGPColor.Create(255, 0, 0, 0));

  Brush.WrapMode := WrapModeTileFlipXY;
  Graphics.FillRectangle(Brush, TGPRect.Create(300, 300, 200, 200));
  Graphics.DrawRectangle(BlackPen, TGPRect.Create(300, 300, 200, 200));
end;

/// The bottom-right illustration shows how the rectangle is tiled by the image.
/// Note that as you move from one tile to the next in a given row, the image is
/// flipped horizontally, and as you move from one tile to the next in a given
/// column, the image is flipped vertically.
{$ENDREGION}

procedure TDemoTileImage.Run;
begin
  Tile;
  FlipHorizontally;
  FlipVertically;
  FlipXY;
end;

initialization
  RegisterDemo('Using a Brush to Fill Shapes\Tiling a Shape with an Image', TDemoTileImage);

end.
