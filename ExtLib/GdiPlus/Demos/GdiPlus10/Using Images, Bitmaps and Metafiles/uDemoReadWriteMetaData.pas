unit uDemoReadWriteMetaData;

interface

uses
  GdiPlus,
  uDemo;

type
  TDemoReadWriteMetaData = class(TDemo)
  strict private
    procedure ReadMetaDataCount;
    procedure ReadMetaData;
    procedure WriteMetaData;
  strict protected
    procedure Run; override;
  public
    class function Outputs: TDemoOutputs; override;
  end;

implementation

uses
  SysUtils;

{ TDemoReadWriteMetaData }

class function TDemoReadWriteMetaData.Outputs: TDemoOutputs;
begin
  Result := [doText];
end;

{$REGION}
/// Some image files contain metadata that you can read to determine features of
/// the image. For example, a digital photograph might contain metadata that you
/// can read to determine the make and model of the camera used to capture the
/// image. With Microsoft Windows GDI+, you can read existing metadata, and you
/// can also write new metadata to image files.
///
/// GDI+ provides a uniform way of storing and retrieving metadata from image
/// files in various formats. In GDI+, a piece of metadata is called a property
/// item. You can store and retrieve metadata by calling the
/// <A>SetPropertyItem</A> and <A>GetPropertyItem</A> methods of
/// the <A>IGPImage</A> interface, and you don't have to be concerned about the
/// details of how a particular file format stores that metadata.
///
/// GDI+ currently supports metadata for the TIFF, JPEG, Exif, and PNG file
/// formats. The Exif format, which specifies how to store images captured by
/// digital still cameras, is built on top of the TIFF and JPEG formats. Exif
/// uses the TIFF format for uncompressed pixel data and the JPEG format for
/// compressed pixel data.
///
/// GDI+ defines a set of property tags that identify property items. Certain
/// tags are general-purpose; that is, they are supported by all of the file
/// formats mentioned in the preceding paragraph. Other tags are special-purpose
/// and apply only to certain formats. If you try to save a property item to a
/// file that does not support that property item, GDI+ ignores the request.
/// More specifically, the <A>IGPImage.SetPropertyItem</A> method raises a
/// PropertyNotSupported exception.
///
/// You can determine the property items that are stored in an image file by
/// accessing the <A>IGPImage.PropertyIdList</A> property. If you try to retrieve
/// a property item that is not in the file, GDI+ ignores the request. More
/// specifically, the <A>IGPImage.GetPropertyItem</A> method raises a
/// PropertyNotFound exception.
///
/// <H>Reading Metadata from a File</H>
/// The following routine uses the <A>PropertyIdList</A> property of an
/// <A>IGPImage</A> object to determine how many pieces of metadata are in the
/// file FakePhoto.jpg.

procedure TDemoReadWriteMetaData.ReadMetaDataCount;
var
  Bitmap: IGPBitmap;
begin
  TextOutput.Add('TDemoReadWriteMetaData.ReadMetaDataCount');
  Bitmap := TGPBitmap.Create('FakePhoto.jpg');
  TextOutput.Add(Format('There are %d pieces of metadata in the file.',
    [Bitmap.PropertyIdList.Count]));
  TextOutput.Add('');
end;

/// The preceding code, along with a particular file, FakePhoto.jpg, produced
/// the output that you can see in the output area above.
///
/// GDI+ stores an individual piece of metadata in a <A>IGPPropertyItem</A>
/// object. You can access the <A>PropertyItems</A> property of the <A>IGPImage</A>
/// class to retrieve all the metadata from a file. The <A>PropertyItems</A>
/// property returns an array of <A>IGPPropertyItem</A> objects.
///
/// A <A>IGPPropertyItem</A> object has the following four public members:
///
///  -Id: A tag that identifies the metadata item. Examples of values that can
/// be assigned to Id are PropertyTagImageTitle, PropertyTagEquipMake,
/// PropertyTagExifExposureTime, and the like.
///  -Length: The length, in bytes, of the array of values pointed to by the
/// Value data member. Note that if the type data member is set to
/// PropertyTagTypeASCII, then the length data member is the length of a
/// null-terminated character string, including the NULL terminator.
///  -ValueType: The data type of the values in the array pointed to by the
/// Value data member. Some examples of types are PropertyTagTypeByte and
/// PropertyTagTypeASCII.
///  -Value: A pointer to an array of values.
///
/// The following example reads and displays the pieces of metadata in the file
/// FakePhoto.jpg.

procedure TDemoReadWriteMetaData.ReadMetaData;
const
  PropertyTypes: array [1..10] of String = ('PropertyTagTypeByte',
    'PropertyTagTypeASCII', 'PropertyTagTypeShort', 'PropertyTagTypeLong',
    'PropertyTagTypeRational', '', 'PropertyTagTypeUndefined', '',
    'PropertyTagTypeSLONG', 'PropertyTagTypeSRational');
var
  Bitmap: IGPBitmap;
  Props: IGPPropertyItems;
  Prop: IGPPropertyItem;
  I: Integer;
begin
  TextOutput.Add('TDemoReadWriteMetaData.ReadMetaData');
  Bitmap := TGPBitmap.Create('FakePhoto.jpg');
  Props := Bitmap.PropertyItems;
  I := 0;
  for Prop in Props do
  begin
    TextOutput.Add(Format('Property Item %d',[I]));
    TextOutput.Add(Format('  Id: $%x',[Prop.Id]));
    if (Prop.ValueType >= Low(PropertyTypes)) and (Prop.ValueType <= High(PropertyTypes)) then
      TextOutput.Add(Format('  ValueType: %s',[PropertyTypes[Prop.ValueType]]))
    else
      TextOutput.Add(Format('  ValueType: %d',[Prop.ValueType]));
    if (Prop.ValueType = PropertyTagTypeASCII) then
      TextOutput.Add(Format('  Value: %s',[PAnsiChar(Prop.Value)]));
    TextOutput.Add(Format('  Length: %d',[Prop.Length]));
    TextOutput.Add('');
    Inc(I);
  end;
end;

/// The output of the preceding code shows a hexadecimal Id number for each
/// property item. You can look up those Id numbers in PropertyTagXXX-constants
/// in the GdiPlus unit and find out that they represent the following property
/// tags.
///  -$0320: PropertyTagImageTitle
///  -$5090: PropertyTagLuminanceTable
///  -$5091: PropertyTagChrominanceTable
///
/// The first (index 0) property item in the list has <B>Id</B>
/// PropertyTagImageType and <B>ValueType</B> PropertyTagTypeASCII.
/// The example above displays the value of that property item (the string
/// 'Fake Photograph').
///
/// <H>Writing Metadata to a File</H>
/// To write an item of metadata to an <A>IGPImage</A> object, initialize a
/// <A>IGPPropertyItem</A> object and then pass that <A>IGPPropertyItem</A> object
/// to the <A>SetPropertyItem</A> method of the <A>Image</A> object.
///
/// The following example writes one item (the image title) of metadata to an
/// <A>IGPImage</A> object and then saves the image in the disk file
/// FakePhoto2.jpg.

procedure TDemoReadWriteMetaData.WriteMetaData;
var
  Bitmap: IGPBitmap;
  Prop: IGPPropertyItem;
  Title: AnsiString;
begin
  TextOutput.Add('TDemoReadWriteMetaData.WriteMetaData');
  Bitmap := TGPBitmap.Create('FakePhoto.jpg');
  Title := 'Modified Title for Fake Photograph';
  Prop := TGPPropertyItem.Create;
  Prop.Id := PropertyTagImageTitle;
  Prop.ValueType := PropertyTagTypeASCII;
  Prop.Length := Length(Title) + 1; // string length including NULL terminator
  Prop.Value := PAnsiChar(Title);
  Bitmap.SetPropertyItem(Prop);
  Bitmap.Save('FakePhoto2.jpg', TGPImageFormat.Jpeg);

  // Read back meta data
  Bitmap := TGPBitmap.Create('FakePhoto2.jpg');
  Prop := Bitmap.GetPropertyItem(PropertyTagImageTitle);
  if Assigned(Prop) and (Prop.ValueType = PropertyTagTypeASCII) then
    TextOutput.Add('Title: ' + PAnsiChar(Prop.Value));
end;
{$ENDREGION}

procedure TDemoReadWriteMetaData.Run;
begin
  ReadMetaDataCount;
  ReadMetaData;
  WriteMetaData;
end;

initialization
  RegisterDemo('Using Images, Bitmaps and Metafiles\Reading and Writing Metadata', TDemoReadWriteMetaData);

end.
