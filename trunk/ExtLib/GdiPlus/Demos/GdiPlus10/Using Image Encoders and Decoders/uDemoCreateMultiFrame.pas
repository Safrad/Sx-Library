unit uDemoCreateMultiFrame;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoCreateMultiFrame = class(TDemo)
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

{ TDemoCreateMultiFrame }

class function TDemoCreateMultiFrame.Outputs: TDemoOutputs;
begin
  Result := [doGraphic, doText];
end;

{$REGION}
/// With certain file formats, you can save multiple images (frames) to a single
/// file. For example, you can save several pages to a single TIFF file. To save
/// the first page, call the <A>Save</A> method of the <A>IGPImage<A> interface.
/// To save subsequent pages, call the <A>SaveAdd</A> method of the
/// <A>IGPImage</A> interface.
///
/// The following example creates a TIFF file with four pages. The images that
/// become the pages of the TIFF file come from four disk files. The code first
/// constructs four <A>IGPImage</A> objects: Multi, Page2, Page3, and Page4. At
/// first, Multi contains only the image from Shapes.bmp, but eventually it
/// contains all four images. As the individual pages are added to the multi
/// <A>IGPImage</A> object, they are also added to the disk file MultiFrame.tif.
///
/// Note that the code calls <A>Save</A> (not <A>SaveAdd</A>) to save the first
/// page. The first argument passed to the <A>Save</A> method is the name of the
/// disk file that will eventually contain several frames. The second argument
/// passed to the <A>Save</A> method specifies the encoder that will be used to
/// convert the data in the multi <A>IGPImage</A> object to the format (in this
/// case TIFF) required by the disk file. That same encoder is used
/// automatically by all subsequent calls to the <A>SaveAdd</A> method of the
/// multi <A>IGPImage<A> object.
///
/// The third argument passed to the <A>Save</A> method is an
/// <A>IGPEncoderParameters</A> object. The <A>IGPEncoderParameters<A> object has
/// a single parameter of type EncoderSaveFlag. The value of the parameter
/// is EncoderValueMultiFrame.
///
/// The code saves the second, third, and fourth pages by calling the
/// <A>SaveAdd</A> method of the multi <A>IGPImage<A> object. The first argument
/// passed to the <A>SaveAdd<A/> method is an <A>IGPImage</A> object. The image in
/// that <A>IGPImage</A> object is added to the multi <A>IGPImage</A> object and is
/// also added to the MultiFrame.tif disk file. The second argument passed to
/// the <A>SaveAdd</A> method is the same <A>IGPEncoderParameters</A> object that
/// was used by the <A>Save</A> method. The difference is that the Value of the
/// parameter now is EncoderValueFrameDimensionPage.

procedure TDemoCreateMultiFrame.Run;
var
  Params: IGPEncoderParameters;
  Multi, Page2, Page3, Page4: IGPImage;
  X, I, PageCount: Integer;
begin
  Params := TGPEncoderParameters.Create;
  Multi := TGPImage.Create('Shapes.bmp');
  Page2 := TGPImage.Create('Cereal.gif');
  Page3 := TGPImage.Create('Iron.jpg');
  Page4 := TGPImage.Create('House.png');

  // Save the first page (frame).
  Params.Add(EncoderSaveFlag, EncoderValueMultiFrame);
  Multi.Save('MultiFrame.tif', TGPImageFormat.Tiff, Params);
  TextOutput.Add('Page 1 saved successfully');

  // Save the second page (frame).
  Params.Clear;
  Params.Add(EncoderSaveFlag, EncoderValueFrameDimensionPage);
  Multi.SaveAdd(Page2, Params);
  TextOutput.Add('Page 2 saved successfully');

  // Save the third page (frame).
  Multi.SaveAdd(Page3, Params);
  TextOutput.Add('Page 3 saved successfully');

  // Save the fourth page (frame).
  Multi.SaveAdd(Page4, Params);
  TextOutput.Add('Page 4 saved successfully');

  // Close the multiframe file.
  Params.Clear;
  Params.Add(EncoderSaveFlag, EncoderValueFlush);
  Multi.SaveAdd(Params);
  TextOutput.Add('File closed successfully');

  // Reload the TIFF file
  Multi := TGPImage.Create('MultiFrame.tif');
  PageCount := Multi.GetFrameCount(FrameDimensionPage);
  X := 0;
  for I := 0 to PageCount - 1 do
  begin
    Multi.SelectActiveFrame(FrameDimensionPage, I);
    Graphics.DrawImage(Multi, X, 0, Multi.Width, Multi.Height);
    Inc(X, Multi.Width + 10);
  end;
end;

/// The last section of the code retrieves the individual frames from a
/// multiple-frame TIFF file. When the TIFF file was created, the individual
/// frames were added to the Page dimension. The code displays each of the four
/// pages.
///
/// The code constructs an <A>IGPImage</A> object from the multiple-frame TIFF
/// file. To retrieve the individual frames (pages), the code calls the
/// <A>SelectActiveFrame</A> method of that </A>IGPImage</A> object. The first
/// argument passed to the <A>SelectActiveFrame</A> method is a GUID that
/// specifies the dimension in which the frames were previously added to the
/// multiple-frame TIFF file. The GUID FrameDimensionPage is used here. Other
/// GUIDs you can use are FrameDimensionTime and FrameDimensionResolution. The
/// second argument passed to the <A>SelectActiveFrame</A> method is the
/// zero-based index of the desired page.
{$ENDREGION}

initialization
  RegisterDemo('Using Image Encoders and Decoders\Creating and Saving a Multiple-Frame Image', TDemoCreateMultiFrame);

end.
