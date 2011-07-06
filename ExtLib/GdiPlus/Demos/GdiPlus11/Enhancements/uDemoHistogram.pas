unit uDemoHistogram;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoHistogram = class(TDemo)
  strict protected
    procedure Run; override;
  end;

implementation

uses
  Math;

{ TDemoHistogram }

{$REGION}
/// In GDI+ 1.1, the <A>IGPBitmap</A> interface has an extra method called
/// <A>GetHistogram</A>. This method returns a histogram with the statistics
/// of the colors used in the bitmap.
///
/// The method accepts a single parameter with the type of histogram you want.
/// You can request a histogram for a single channel (for example
/// <A>HistogramFormatB</A>) or for multiple channels (for example
/// <A>HistogramFormatRGB</A>).
///
/// The example below request the histogram of the RGB channels of a bitmap
/// and draws the histogram next to the bitmap.
///
/// The <A>IGPHistogram.ChannelCount</A> property returns the number of channels
/// in the histogram. For single channel histograms this value is 1; for RGB
/// histograms this value is 3; and for ARGB/PARGB histograms this value is 4.
///
/// The <A>IGPHistogram.EntryCount</A> property returns the number of entries in
/// the histogram for each channel. For 8 bit per channel bitmaps, this value
/// is usually 256.
///
/// Finally, the <A>Values</A> property (which is the default array property
/// for <A>IGPHistogram</A>) returns the histogram values for the given channl
/// and entry index. This value is the number of times a pixel with the given
/// intensity appears in the bitmap. For example, when <B>IGPHistogram[2,30]</B>
/// returns 1000, this means that there are 1000 pixels in the bitmap that have
/// a Blue (channel 2) intensity of 30.
///
/// The example below finds the maximum value in the histogram and uses that
/// value the scale all values so the histogram can be drawn with the same
/// height as the bitmap.

procedure TDemoHistogram.Run;
var
  Bitmap: IGPBitmap;
  Histogram: IGPHistogram;
  I, J, X, Y, MaxVal: Integer;
  Scale: Double;
  Pen: IGPPen;
begin
  Bitmap := TGPBitmap.Create('ImageFileSmall.jpg');
  Graphics.DrawImage(Bitmap, 10, 10, Bitmap.Width, Bitmap.Height);

  // Retrieve RGB histogram of bitmap
  Histogram := Bitmap.GetHistogram(HistogramFormatRGB);

  // Determine the maximum value in the histogram
  MaxVal := 0;
  for J := 0 to Histogram.ChannelCount - 1 do
    for I := 0 to Histogram.EntryCount - 1 do
      MaxVal := Max(MaxVal, Histogram[J, I]);

  // Scale maximum value so it displays as the height of the bitmap
  Scale := Bitmap.Height / MaxVal;

  // Draw the histogram next to the bitmap
  Pen := TGPPen.Create(0);
  X := 10 + Bitmap.Width + 10;
  Y := 10 + Bitmap.Height;
  for I := 0 to Histogram.ChannelCount - 1 do
  begin
    if (I = 0) then
      Pen.Color := TGPColor.Create(128, 255, 0, 0)
    else if (I = 1) then
      Pen.Color := TGPColor.Create(128, 0, 255, 0)
    else
      Pen.Color := TGPColor.Create(128, 0, 0, 255);

    for J := 0 to Histogram.EntryCount - 1 do
      Graphics.DrawLine(Pen, X + J, Y, X + J, Y - Scale * Histogram[I, J]);
  end;
end;

/// The illustration above shows that the bitmap contains a lot of reds at high
/// intensities. This is mostly the skin color of the apples and pears in the
/// bitmap. At lower intensities, the blues dominate slightly, as these colors
/// appear in the darker grapes and leaves.
{$ENDREGION}

initialization
  RegisterDemo('Enhancements\Getting the Histogram of a Bitmap', TDemoHistogram);

end.
