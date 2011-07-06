unit uDemoRecordMetafiles;

interface

uses
  Windows,
  GdiPlus,
  uDemo;

type
  TDemoRecordMetafiles = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

uses
  SysUtils;

{$REGION}
/// The <A>IGPMetafile</A> interface, which inherits from the <A>IGPImage</A>
/// interface, allows you to record a sequence of drawing commands. The recorded
/// commands can be stored in memory, saved to a file, or saved to a stream.
/// Metafiles can contain vector graphics, raster images, and text.
///
/// The following example creates a <A>IGPMetafile</A> object. The code uses the
/// <A>IGPMetafile</A> object to record a sequence of graphics commands and then
/// saves the recorded commands in a file named SampleMetafileRecording.emf.
/// Note that the <A>TGPMetafile</A> constructor receives a device context handle,
/// and the <A>TGPGraphics</A> constructor receives the <A>IGPMetafile</A> object.
/// The recording stops (and the recorded commands are saved to the file) when
/// the <A>IGPGraphics</A> object goes out of scope. The last line of code display
/// the metafile by passing the <A>IGPMetafile</A> object to the <A>DrawImage</A>
/// method of the <A>IGPGraphics</A> object. Note that the code uses the same
/// <A>IGPMetafile</A> object to record and to display (play back) the metafile.

procedure TDemoRecordMetafiles.Run;
var
  DC: HDC;
  Metafile: IGPMetafile;
  MetaGraphics: IGPGraphics;
  GreenPen: IGPPen;
  SolidBrush: IGPSolidBrush;
  FontFamily: IGPFontFamily;
  Font: IGPFont;
begin
  DeleteFile('SampleMetafileRecording.emf');
  DC := GetDC(0);
  try
    Metafile := TGPMetafile.Create('SampleMetafileRecording.emf', DC);
    MetaGraphics := TGPGraphics.Create(Metafile);
    GreenPen := TGPPen.Create(TGPColor.Create(255, 0, 255, 0));
    SolidBrush := TGPSolidBrush.Create(TGPColor.Create(255, 0, 0, 255));

    // Add a rectangle and an ellipse to the metafile.
    MetaGraphics.DrawRectangle(GreenPen, TGPRect.Create(50, 10, 25, 75));
    MetaGraphics.DrawEllipse(GreenPen, TGPRect.Create(100, 10, 25, 75));

    // Add an ellipse (drawn with antialiasing) to the metafile.
    MetaGraphics.SmoothingMode := SmoothingModeHighQuality;
    MetaGraphics.DrawEllipse(GreenPen, TGPRect.Create(150, 10, 25, 75));

    // Add some text (drawn with antialiasing) to the metafile.
    FontFamily := TGPFontFamily.Create('Arial');
    Font := TGPFont.Create(FontFamily, 24, FontStyleRegular, UnitPixel);

    MetaGraphics.TextRenderingHint := TextRenderingHintAntiAlias;
    MetaGraphics.RotateTransform(30);
    MetaGraphics.DrawString('Smooth Text', Font, TGPPointF.Create(50, 50), SolidBrush);

    MetaGraphics := nil; // End of recording metafile.
  finally
    ReleaseDC(0, DC);
  end;

  // Play back the metafile.
  Graphics.DrawImage(Metafile, 200, 100);
end;

/// <B>Note</B> To record a metafile, you must construct a <A>IGPGraphics</A>
/// object based on a <A>IGPMetafile</A> object. The recording of the metafile
/// ends when that <A>IGPGraphics</A> object is deleted or goes out of scope.
///
/// A metafile contains its own graphics state, which is defined by the
/// <A>IGPGraphics</A> object used to record the metafile. Any properties of the
/// <A>IGPGraphics</A> object (clip region, world transformation, smoothing mode,
/// and the like) that you set while recording the metafile will be stored in
/// the metafile. When you display the metafile, the drawing will be done
/// according to those stored properties.
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Recording Metafiles', TDemoRecordMetafiles);

end.
