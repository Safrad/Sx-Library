unit uDemoCachedBitmap;

interface

uses
  Windows,
  GdiPlus,
  uDemo;

type
  TDemoCachedBitmap = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoCachedBitmap }

class function TDemoCachedBitmap.Outputs: TDemoOutputs;
begin
  Result := [doGraphic, doText];
end;

{$REGION}
/// <A>IGPImage</A> and <A>IGPBitmap</A> objects store images in a device-independent
/// format. A <A>IGPCachedBitmap</A> object stores an image in the format of the
/// current display device. Rendering an image stored in a <A>CachedBitmap</A>
/// object is fast because no processing time is spent converting the image to
/// the format required by the display device.
///
/// The following example creates a <A>IGPBitmap</A> object and a
/// <A>IGPCachedBitmap</A> object from the file Texture1.jpg. The <A>IGPBitmap</A>
/// and the <A>IGPCachedBitmap</A> are each drawn 30,000 times. If you run the
/// code, you will see that the <A>IGPCachedBitmap</A> images are drawn
/// substantially faster than the <A>IGPBitmap</A> images.
///  You can check the drawing times by clicking on the "Text" tab above.

procedure TDemoCachedBitmap.Run;
var
  Bitmap: IGPBitmap;
  Cached: IGPCachedBitmap;
  Width, Height, J, K: Integer;
  Freq, ST, ET: Int64;
  Seconds: Double;
begin
  QueryPerformanceFrequency(Freq);
  Bitmap := TGPBitmap.Create('Texture1.jpg');
  Width := Bitmap.Width;
  Height := Bitmap.Height;
  Cached := TGPCachedBitmap.Create(Bitmap, Graphics);

  QueryPerformanceCounter(ST);
  J := 0;
  while (J < 300) do
  begin
    for K := 0 to 999 do
      Graphics.DrawImage(Bitmap, J, J div 2, Width, Height);
    Inc(J, 10);
  end;
  QueryPerformanceCounter(ET);
  Seconds := (ET - ST) / Freq;
  TextOutput.Add(Format('DrawImage: %.2f seconds',[Seconds]));

  QueryPerformanceCounter(ST);
  J := 0;
  while (J < 300) do
  begin
    for K := 0 to 999 do
      Graphics.DrawCachedBitmap(Cached, J, 150 + J div 2);
    Inc(J, 10);
  end;
  QueryPerformanceCounter(ET);
  Seconds := (ET - ST) / Freq;
  TextOutput.Add(Format('DrawCachedBitmap: %.2f seconds',[Seconds]));
end;

/// <B>Note</B> A <A>IGPCachedBitmap</A> object matches the format of the display
/// device at the time the <A>IGPCachedBitmap</A> object was constructed. If the
/// user of your program changes the display settings, your code should
/// construct a new <A>IGPCachedBitmap</A> object. The <A>DrawCachedBitmap</A>
/// method will fail if you pass it a <A>IGPCachedBitmap</A> object that was
/// created prior to a change in the display format.
{$ENDREGION}

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Using a Cached Bitmap to Improve Performance', TDemoCachedBitmap);

end.
