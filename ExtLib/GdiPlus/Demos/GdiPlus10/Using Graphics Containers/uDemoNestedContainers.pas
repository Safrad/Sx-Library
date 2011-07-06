unit uDemoNestedContainers;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoNestedContainers = class(TDemo)
  strict private
    procedure Example1;
    procedure Example2;
    procedure Example3;
  strict protected
    procedure Run; override;
  end;

implementation

{ TDemoNestedContainers }

{$REGION}
/// Microsoft Windows GDI+ provides containers that you can use to temporarily
/// replace or augment part of the state in a <A>IGPGraphics</A> object. You
/// create a container by calling the <A>BeginContainer</A> method of a
/// <A>IGPGraphics</A> object. You can call <A>BeginContainer</A> repeatedly to
/// form nested containers.
///
/// <H>Transformations in Nested Containers</H>
/// The following example creates a <A>IGPGraphics</A> object and a container
/// within that <A>IGPGraphics</A> object. The world transformation of the
/// <A>IGPGraphics</A> object is a translation 100 units in the x direction and 80
/// units in the y direction. The world transformation of the container is a
/// 30-degree rotation. The code makes the call
///
/// <C>DrawRectangle(Pen, -60, -30, 120, 60)</C>
///
/// twice. The first call to <A>DrawRectangle</A> is inside the container; that
/// is, the call is in between the calls to <A>BeginContainer</A> and
/// <A>EndContainer</A>. The second call to <A>DrawRectangle</A> is after the
/// call to <A>EndContainer</A>.

procedure TDemoNestedContainers.Example1;
var
  Pen: IGPPen;
  Container: TGPGraphicsContainer;
begin
  Pen := TGPPen.Create(TGPColor.Red);
  Graphics.TranslateTransform(100, 80);

  Container := Graphics.BeginContainer;
    Graphics.RotateTransform(30);
    Graphics.DrawRectangle(Pen, -60, -30, 120, 60);
  Graphics.EndContainer(Container);

  Graphics.DrawRectangle(Pen, -60, -30, 120, 60);
end;

/// In the preceding code, the rectangle drawn from inside the container is
/// transformed first by the world transformation of the container (rotation)
/// and then by the world transformation of the <A>IGPGraphics</A> object
/// (translation). The rectangle drawn from outside the container is transformed
/// only by the world transformation of the <A>IGPGraphics</A> object
/// (translation). The top-left illustration above shows the two rectangles.
///
/// <H>Clipping in Nested Containers</H>
/// The following example illustrates how nested containers handle clipping
/// regions. The code creates a container within a <A>IGPGraphics</A> object. The
/// clipping region of the <A>IGPGraphics</A> object is a rectangle, and the
/// clipping region of the container is an ellipse. The code makes two calls to
/// the <A>DrawLine</A> method. The first call to <A>DrawLine</A> is inside the
/// container, and the second call to <A>DrawLine</A> is outside the container
/// (after the call to <A>EndContainer</A>). The first line is clipped by the
/// intersection of the two clipping regions. The second line is clipped only by
/// the rectangular clipping region of the <A>IGPGraphics</A> object.

procedure TDemoNestedContainers.Example2;
var
  Container: TGPGraphicsContainer;
  RedPen, BluePen: IGPPen;
  AquaBrush, GreenBrush: IGPBrush;
  Path: IGPGraphicsPath;
  Region: IGPRegion;
begin
  RedPen := TGPPen.Create(TGPColor.Red, 2);
  BluePen := TGPPen.Create(TGPColor.Blue, 2);
  AquaBrush := TGPSolidBrush.Create(TGPColor.Create(255, 180, 255, 255));
  GreenBrush := TGPSolidBrush.Create(TGPColor.Create(255, 150, 250, 130));

  Graphics.SetClip(TGPRect.Create(250, 65, 150, 120));
  Graphics.FillRectangle(AquaBrush, 250, 65, 150, 120);

  Container := Graphics.BeginContainer;
    // Create a path that consists of a single ellipse.
    Path := TGPGraphicsPath.Create;
    Path.AddEllipse(275, 50, 100, 150);

    // Construct a region based on the path.
    Region := TGPRegion.Create(Path);
    Graphics.FillRegion(GreenBrush, Region);

    Graphics.Clip := Region;
    Graphics.DrawLine(RedPen, 250, 0, 550, 300);
  Graphics.EndContainer(Container);

  Graphics.DrawLine(BluePen, 270, 0, 570, 300);
end;

/// As the illustrations above show, transformations and clipping regions are
/// cumulative in nested containers. If you set the world transformations of the
/// container and the <A>IGPGraphics</A> object, both transformations will apply
/// to items drawn from inside the container. The transformation of the
/// container will be applied first, and the transformation of the
/// <A>IGPGraphics</A> object will be applied second. If you set the clipping
/// regions of the container and the <A>IGPGraphics</A> object, items drawn from
/// inside the container will be clipped by the intersection of the two clipping
/// regions.
///
/// <H>Quality Settings in Nested Containers</H>
/// Quality settings (<A>SmoothingMode</A>, <A>TextRenderingHint</A>, and the
/// like) in nested containers are not cumulative; rather, the quality settings
/// of the container temporarily replace the quality settings of a
/// <A>IGPGraphics</A> object. When you create a new container, the quality
/// settings for that container are set to default values. For example, suppose
/// you have a <A>IGPGraphics</A> object with a smoothing mode of
/// <A>SmoothingModeAntiAlias</A>. When you create a container, the smoothing
/// mode inside the container is the default smoothing mode. You are free to set
/// the smoothing mode of the container, and any items drawn from inside the
/// container will be drawn according to the mode you set. Items drawn after the
/// call to <A>EndContainer</A> will be drawn according to the smoothing mode
/// (SmoothingModeAntiAlias) that was in place before the call to
/// <A>BeginContainer</A>.
///
/// <H>Several Layers of Nested Containers</H>
/// You are not limited to one container in a <A>IGPGraphics</A> object. You can
/// create a sequence of containers, each nested in the preceding, and you can
/// specify the world transformation, clipping region, and quality settings of
/// each of those nested containers. If you call a drawing method from inside
/// the innermost container, the transformations will be applied in order,
/// starting with the innermost container and ending with the outermost
/// container. Items drawn from inside the innermost container will be clipped
/// by the intersection of all the clipping regions.
///
/// The following example sets the text rendering hint of the <A>IGPGraphics</A>
/// object to <A>TextRenderingHintAntiAlias</A>. The code creates two
/// containers, one nested within the other. The text rendering hint of the
/// outer container is set to <A>TextRenderingHintSingleBitPerPixel</A>, and the
/// text rendering hint of the inner container is set to
/// <A>TextRenderingHintAntiAlias</A>. The code draws three strings: one from
/// the inner container, one from the outer container, and one from the
/// <A>IGPGraphics</A> object itself.

procedure TDemoNestedContainers.Example3;
var
  InnerContainer, OuterContainer: TGPGraphicsContainer;
  Brush: IGPBrush;
  Family: IGPFontFamily;
  Font: IGPFont;
begin
  Brush := TGPSolidBrush.Create(TGPColor.Blue);
  Family := TGPFontFamily.Create('Times New Roman');
  Font := TGPFont.Create(Family, 36, FontStyleRegular, UnitPixel);

  Graphics.TextRenderingHint := TextRenderingHintAntiAlias;

  OuterContainer := Graphics.BeginContainer;
    Graphics.TextRenderingHint := TextRenderingHintSingleBitPerPixel;

    InnerContainer := Graphics.BeginContainer;
      Graphics.TextRenderingHint := TextRenderingHintAntiAlias;
      Graphics.DrawString('Inner Container', Font, TGPPointF.Create(20, 210), Brush);
    Graphics.EndContainer(InnerContainer);

    Graphics.DrawString('Outer Container', Font, TGPPointF.Create(20, 250), Brush);
  Graphics.EndContainer(OuterContainer);

  Graphics.DrawString('Graphics Object', Font, TGPPointF.Create(20, 290), Brush);
end;

/// The last illustration above the three strings. The strings drawn from the
/// inner container and the <A>IGPGraphics</A> object are smoothed by
/// antialiasing. The string drawn from the outer container is not smoothed by
/// antialiasing because of the <A>TextRenderingHintSingleBitPerPixel</A>
/// setting.
{$ENDREGION}

procedure TDemoNestedContainers.Run;
begin
  Example1;
  Graphics.ResetTransform;
  Example2;
  Graphics.ResetClip;
  Example3;
end;

initialization
  RegisterDemo('Using Graphics Containers\Nested Graphics Containers', TDemoNestedContainers);

end.
