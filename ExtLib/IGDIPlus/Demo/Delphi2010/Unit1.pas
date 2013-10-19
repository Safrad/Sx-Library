unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;

type
  TForm1 = class(TForm)
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses IGDIPlus;

{$R *.dfm}

procedure TForm1.FormPaint(Sender: TObject);
var
  AGraphics       : IGPGraphics;
  AFont           : IGPFont;
  ALeftTopCorner  : TGPPointF;
  ARect           : TGPRectF;
  ARect1          : TGPRectF;
  APath           : IGPGraphicsPath;

begin
  AGraphics := TGPGraphics.Create( Canvas );
  AGraphics.SmoothingMode := SmoothingModeAntiAlias;
  AGraphics.TextRenderingHint := TextRenderingHintAntiAlias;

  ALeftTopCorner := MakePointF( 20, 20 );
  AFont := TGPFont.Create( 'Microsoft Sans Serif', 40, [ fsBold ] );
  ARect := AGraphics.GetStringBoundingBoxF( 'Welcome to IGDI+', AFont, ALeftTopCorner );
  ARect1 := GPInflateRectF( ARect, 10, 10 );

  // Draw a fancy rounded rectangle.
  AGraphics.DrawRoundRectangleF(
              TGPPen.Create( TGPLinearGradientBrush.Create( GPInflateRectF( ARect1, 2, 2 ), aclRed, aclBlue, LinearGradientModeVertical ), 4 ),
              TGPPathGradientBrush.Create(
                  TGPGraphicsPath.Create().AddRoundRectangleF( ARect1, MakeSizeF( 20, 20 ) )
                   )
                .SetInterpolationColorArrays( [ aclGreen, aclCyan, aclYellow ], [ 0, 0.3, 1 ] )
                .SetCenterPointF( MakePointF( 250, 50 ))
                .SetFocusScales( 0.87, 0.2 ),
              ARect1, MakeSizeF( 20, 20 ) );

  // Draw a text with semitransparent shadow.
  AGraphics.DrawStringF( 'Welcome to IGDI+',
                        AFont,
                        MakePointF( 23, 23 ),
                        TGPSolidBrush.Create( MakeARGBColor( 50, aclBlack )) )

           .DrawStringF( 'Welcome to IGDI+',
                        AFont,
                        ALeftTopCorner,
                        TGPLinearGradientBrush.Create( ARect, aclRed, aclBlue, LinearGradientModeForwardDiagonal ));


  // Draw a closed curve.
  AGraphics.DrawClosedCurveF( TGPPen.Create( aclRed, 3 ), TGPSolidBrush.Create( aclBlue ),
              [
              MakePointF( 60,  160 ),
              MakePointF( 150, 180 ),
              MakePointF( 200, 140 ),
              MakePointF( 180, 220 ),
              MakePointF( 120, 200 ),
              MakePointF( 80,  260 )
              ] );

  // Draw a semitransparent star.
  APath := TGPGraphicsPath.Create();
  APath.AddLinesF(
    [
    MakePointF( 75, 0 ),
    MakePointF( 100, 50 ),
    MakePointF( 150, 50 ),
    MakePointF( 112, 75 ),
    MakePointF( 150, 150 ),
    MakePointF( 75, 100 ),
    MakePointF( 0, 150 ),
    MakePointF( 37, 75 ),
    MakePointF( 0, 50 ),
    MakePointF( 50, 50 )
     ] );

  AGraphics.TranslateTransform( 420, 30 )
           .FillPath(
              TGPPathGradientBrush.Create( APath )
                .SetCenterColor( MakeColor( 200, 255, 0, 0))
                .SetSurroundColors(
                  [
                  MakeColor(80, 0, 0, 0),
                  MakeColor(80, 0, 255, 0),
                  MakeColor(80, 0, 0, 255),
                  MakeColor(80, 255, 255, 255),
                  MakeColor(80, 0, 0, 0),
                  MakeColor(80, 0, 255, 0),
                  MakeColor(80, 0, 0, 255),
                  MakeColor(80, 255, 255, 255),
                  MakeColor(80, 0, 0, 0),
                  MakeColor(80, 0, 255, 0)
                  ] ),

              APath );

  // Draw rotated ellipse.
  AGraphics.ResetTransform()
           .TranslateTransform( 300, 160 )
           .RotateTransform( 30 )
           .DrawEllipseF(
              TGPPen.Create( aclRed, 3 ),
              TGPLinearGradientBrush.Create( MakePointF( 0, 0 ), MakePointF( 20, 20 ), aclYellow, aclGreen )
              .SetWrapMode( WrapModeTileFlipX ),
              0, 0, 200, 80 );
end;

end.
