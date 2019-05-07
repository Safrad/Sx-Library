{==============================================================================
Component simple read Exif section in Jpeg/Jfif Files.
More information about Exif at www.exif.org


Ñomponent written by SimBa aka Dimoniusis
You may use this component absolutely free.

You may talk with me via
e-mail: dimonius@mail333.com
ICQ: 11152101
Web: http://dimonius.da.ru


Changes:
Version 1.3 
 - some more ifd tags implemented
 - some bugs fixes

Version 1.2 (Some code by Jim Wood,  e-mail: jwood@visithink.com)
 - some more ifd tags implemented
 - corrected work with ReadOnly files

Version 1.1 (By Ive, e-mail: ive@lilysoft.com)
 - works now with Motorola and Intel byte order tags
 - better offset calculation
 - some more ifd tags implemented
 - some format functions for rational values
 - naming convention changed a little


NOTE: far away from being complete but it seems to
      work with all example files from www.exif.org

 - Ive (c) 2003




==============================================================================}

unit Exif;

interface

uses
  uTypes,
	uInputFormat,
  uFile,
	Classes, SysUtils;

type
  TIfdTag = packed record
    ID      : U2;       //Tag number
    Typ     : U2;       //Type tag
    Count   : U4;   //tag length
    Offset  : U4;   //Offset / Value
  end;

  TExif = class(TObject)
  private
    FImageDesc          : String;     //Picture description
    FMake               : String;     //Camera manufacturer
    FModel              : String;     //Camere model
    FOrientation        : Byte;       //Image orientation - 1 normal
    FOrientationDesc    : String;     //Image orientation description
    FCopyright          : String;     //Copyright
    FValid              : Boolean;    //Has valid Exif header
    FDateTime           : String;     //Date and Time of Change
    FDateTimeOriginal   : String;     //Original Date and Time
    FDateTimeDigitized  : String;     //Camshot Date and Time
    FUserComments       : String;     //User Comments

    FExposure           : String;     //Exposure
    FFstops             : String;
    FShutterSpeed       : string;
    FAperture           : string;
    FMaxAperture        : string;

    FExposureProgram    : Byte;
    FExposureProgramDesc: string;
    FPixelXDimension    : Cardinal;
    FPixelYDimension    : Cardinal;
    FXResolution        : Cardinal;
    FYResolution        : Cardinal;
    FMeteringMode       : byte;
    FMeteringMethod     : string;
    FLightSource        : Byte;
    FLightSourceDesc    : string;
    FFlash              : Byte;
    FFlashDesc          : string;
    FISO                : Word;
    FSoftware           : string;
    FArtist             : string;
    FCompressedBPP      : string;

    FFile                : TFile;
    ifdp                : Cardinal;
    FSwap               : boolean;
    function  ReadAsci(const Offset, Count: Cardinal): AnsiString;
    function  ReadRatio(const Offset: Cardinal; frac: boolean): string; overload;
    function  ReadRatio(const Offset: Cardinal): single; overload;
    procedure ReadTag(var tag: TIfdTag);
    procedure Init;
    function ReadLongIntValue(const Offset: Cardinal): LongInt;
    function GetDateTime: TDateTime;
    function GetDateTimeDigitized: TDateTime;
    function GetDateTimeOriginal: TDateTime;
  public
    constructor Create;
    procedure ReadFromFile(const FileName: TFileName);

    property Valid: Boolean read FValid;
    property ImageDesc: String read FImageDesc;
    property Make: String read FMake;
    property Model: String read FModel;
    property Orientation: Byte read FOrientation;
    property OrientationDesc: String read FOrientationDesc;
    property Copyright: String read FCopyright;
    property DateTime: TDateTime read GetDateTime;
    property DateTimeOriginal: TDateTime read GetDateTimeOriginal;
    property DateTimeDigitized: TDateTime read GetDateTimeDigitized;
    property UserComments: String read FUserComments;
    property Software: String read FSoftware;
    property Artist: String read FArtist;

    property Exposure: String read FExposure;
    property ExposureProgram: byte read FExposureProgram;
    property ExposureProgramDesc: string read FExposureProgramDesc;
    property FStops: String read FFStops;
    property ShutterSpeed: String read FShutterSpeed;
    property Aperture: String read FAperture;
    property MaxAperture: String read FMaxAperture;
    property CompressedBPP: String read FCompressedBPP;

    property PixelXDimension: Cardinal read FPixelXDimension;
    property PixelYDimension: Cardinal read FPixelYDimension;
    property XResolution: Cardinal read FXResolution;
    property YResolution: Cardinal read FYResolution;
    property MeteringMode: byte read FMeteringMode;
    property MeteringMethod: string read FMeteringMethod;
    property LightSource: byte read FLightSource;
    property LightSourceDesc: string read FLightSourceDesc;
    property Flash: byte read FFlash;
    property FlashDesc: string read FFlashDesc;
    property ISO: Word read FISO;
	end;

implementation

uses
  Math;

type
  TMarker = packed record // 9
    Marker  : U2;      //Section marker
    Len     : U2;      //Length Section
    Indefin : Array [0..4] of AnsiChar; //Indefiner - "Exif" 00, "JFIF" 00 and ets
  end;

  TIFDHeader = packed record // 11
    pad       : U1; //00h
    ByteOrder : U2; //II (4D4D) or MM
    i42       : U2; //2A00 (magic number from the 'Hitchhikers Guide'
    Offset    : U4; //0th offset IFD
    Count     : U2;     // number of IFD entries
  end;


function SwapLong(Value: Cardinal): Cardinal;
asm bswap eax end;

procedure TExif.ReadTag(var tag: TIfdTag);
begin
  FFile.BlockRead(tag, 12);
  if FSwap then with tag do begin // motorola or intel byte order ?
    ID  := Swap(ID);
    Typ := Swap(Typ);
    Count := SwapLong(Count);
    if (Typ=1) or (Typ=3) then
      Offset := (Offset shr 8) and $FF
    else
      Offset  := SwapLong(Offset);
    end
  else with tag do begin
    if ID<>$8827 then  //ISO Metering Mode not need conversion
      if (Typ=1) or (Typ=3) then
        Offset := Offset and $FF; // other bytes are undefined but maybe not zero
  end;
end;


function TExif.ReadAsci(const Offset, Count: Cardinal): AnsiString;
var
  fp: U8;
  i: Word;
begin
  Result := '';
  if Count > 1024 * 1024 then Exit;
  SetLength(Result,Count);
  fp:= FFile.FilePos; //Save file offset
  try
    FFile.Seek(Offset);
    try
      i:=1;
      repeat
        FFile.BlockRead(Result[i], 1);
        inc(i);
      until (i>=Count) or (Result[i-1]=#0);
      if i<=Count then Result:=Copy(Result,1,i-1);
    except
      Result:='';
    end;
    Result:=AnsiString(TrimRight(Result));
  finally
    FFile.Seek(fp);
  end;
end;

function TExif.ReadLongIntValue(const Offset: Cardinal): LongInt;
var
  fp: U8;
begin
  fp:=FFIle.FilePos; //Save file offset
  try
    FFile.Seek(Offset);
    try
      FFile.BlockRead(Result, sizeof(Result));
      if FSwap then Result:=SwapLong(Cardinal(Result));
    except
      Result:=0;
    end;
  finally
    FFile.Seek(fp); //Restore file offset
  end;
end;

function TExif.ReadRatio(const Offset: Cardinal; frac: boolean): string;
var
  fp: U8;
  nom,denom: U4;
begin
  fp:=FFile.FilePos; //Save file offset
  try
    FFile.Seek(Offset);
    try
      FFile.BlockRead(nom, SizeOf(nom));
      FFile.BlockRead(denom, SizeOf(denom));
      if FSwap then begin     // !!!
        nom := SwapLong(nom);
        denom := SwapLong(denom);
      end;
      if frac then begin
        if denom = 0 then
          Result := ''
        else
          str((nom/denom):1:2, result);
        if (length(result)>0) and (result[length(result)]='0') then Result:=copy(Result,1,length(Result)-1);
      end else
        if denom<>1000000 then
          Result:=IntToStr(nom)+ '/' +IntToStr(denom)
        else
          Result:='0';
    except
      Result:='';
    end;
  finally
    FFile.Seek(fp);     //Restore file offset
  end;
end;


function TExif.ReadRatio(const Offset: Cardinal): single;
var
  fp: U8;
  nom,denom: U4;
begin
  fp:=FFile.FilePos; //Save file offset
  try
    FFile.Seek(Offset);
    try
      FFile.BlockRead(nom,SizeOf(nom));
      FFile.BlockRead(denom,SizeOf(denom));
      if FSwap then begin     // !!!
        nom := SwapLong(nom);
        denom := SwapLong(denom);
      end;
      Result:=nom/denom;
    except
      Result:=0.0;
    end;
  finally
    FFile.Seek(fp);     //Restore file offset
  end;
end;


procedure TExif.Init;
begin
  ifdp:=0;

  FImageDesc:='';
  FMake:='';
  FModel:='';
  FOrientation:=0;
  FOrientationDesc:='';
  FDateTime:='';
  FCopyright:='';
  FValid:=False;
  FDateTimeOriginal:='';
  FDateTimeDigitized:='';
  FUserComments:='';
  FExposure:='';
  FFstops:='';
  FShutterSpeed := '';
  FAperture := '';
  FExposureProgram:=0;
  FExposureProgramDesc:='';
  FPixelXDimension:=0;
  FPixelYDimension:=0;
  FMeteringMode:=0;
  FMeteringMethod:='';
  FLightSource:=0;
  FLightSourceDesc:='';
  FFlash:=0;
  FFlashDesc:='';
  FISO:=0;
  FCompressedBPP:='';
  FArtist:='';
  FSoftware:='';
  FMaxAperture:='';
  FXResolution:=0;
  FYResolution:=0;
end;


constructor TExif.Create;
begin
  Init;
end;


procedure TExif.ReadFromFile(const FileName: TFileName);
const
  orient   : Array[1..9] of String=('Normal','Mirrored','Rotated 180','Rotated 180, mirrored','Rotated 90 left, mirrored','Rotated 90 right','Rotated 90 right, mirrored','Rotated 90 left','Unknown');
  ExplType : Array[1..9] of String=('Unknown','Manual Control','Normal Program','Aperture Priority', 'Shutter Priority', 'Creative Program','Action Program','Portrait Mode','Landscape Mode');
  Meter    : Array[0..7] of String=('Unknown','Average','Center Weighted Average','Spot','Multi Spot','Pattern','Partial','Other');
var
  Marker:      TMarker;
  ifd:    TIFDHeader;
  off0:   Cardinal; //Null Exif Offset
  tag:    TIfdTag;
  i:      Integer;
  n:      Single;
  SOI:    U2; //2 bytes SOI marker. FF D8 (Start Of Image)
  IfdCnt: Word;
  Tmp   : string;

begin
  Assert(SizeOf(Marker) = 9);
  Assert(SizeOf(ifd) = 11);
  Assert(SizeOf(tag) = 12);

  if not FileExists(FileName) then exit;
  Init;

  FFile := TFile.Create;
  try
    if not FFile.Open(FileName, fmReadOnly) then
      Exit;

    FFile.BlockRead(SOI, SizeOf(SOI));
    if SOI=$D8FF then begin //Is this Jpeg
      FFile.BlockRead(Marker, SizeOf(Marker));

      if Marker.Marker=$E0FF then begin //JFIF Marker Found
        FFile.Seek(20); //Skip JFIF Header
        FFile.BlockRead(Marker, SizeOf(Marker));
      end;

      //Search Exif start marker;
      if Marker.Marker<>$E1FF then begin
        i:=0;
        repeat
          FFile.BlockRead(SOI,2); //Read bytes.
          inc(i);
        until (FFile.Eof or (i>1000) or (SOI=$E1FF));
        //If we find maker
        if SOI=$E1FF then begin
          FFile.Seek(FFile.FilePos-2); //return Back on 2 bytes
          FFile.BlockRead(Marker, SizeOf(Marker));     //read Exif header
        end;
      end;

      if Marker.Marker=$E1FF then begin //If we found Exif Section. j.Indefin='Exif'.
        FValid:=True;
        off0:=FFile.FilePos+1;   //0'th offset Exif header
        FFile.BlockRead(ifd, SizeOf(ifd));  //Read IDF Header
        FSwap := ifd.ByteOrder=$4D4D; // II or MM  - if MM we have to swap
        if FSwap then begin
          ifd.Offset := SwapLong(ifd.Offset);
          ifd.Count  := Swap(ifd.Count);
        end;
        if ifd.Offset <> 8 then begin
          FFile.Seek(FFile.FilePos + U8(Abs(ifd.Offset)) - 8);
        end;

        if (ifd.Count=0) then ifd.Count:=100;

        for i := 1 to ifd.Count do begin
          ReadTag(tag);
          case tag.ID of
                0: break;
    // ImageDescription
            $010E: FImageDesc:=ReadAsci(tag.Offset+off0, tag.Count);
    // Make
            $010F: FMake:=ReadAsci(tag.Offset+off0, tag.Count);
    // Model
            $0110: FModel:=ReadAsci(tag.Offset+off0, tag.Count);
    // Orientation
            $0112: begin
                     FOrientation:= tag.Offset;
                     if FOrientation in [1..8] then
                       FOrientationDesc:=orient[FOrientation]
                     else
                       FOrientationDesc:=orient[9];//Unknown
                   end;
    // DateTime
            $0132: FDateTime:=ReadAsci(tag.Offset+off0, tag.Count);
    // CopyRight
            $8298: FCopyright:=ReadAsci(tag.Offset+off0, tag.Count);
    // Software
            $0131: FSoftware:=ReadAsci(tag.Offset+off0, tag.Count);
    // Artist
            $013B: FArtist:=ReadAsci(tag.Offset+off0, tag.Count);
    // Exif IFD Pointer
            $8769: ifdp:=Tag.Offset; //Read Exif IFD offset
    //XResolution
            $011A: FXResolution := ReadLongIntValue(Tag.Offset+off0);
    //YResolution
            $011B: FYResolution := ReadLongIntValue(Tag.Offset+off0);
          end;
        end;

        if ifdp>0 then begin
          FFile.Seek(ifdp+off0);
          FFile.BlockRead(IfdCnt,2);
          if FSwap then IfdCnt := swap(IfdCnt);
          for i := 1 to IfdCnt do begin
            ReadTag(tag);
    {
            You may simple realize read this info:

            Tag |Name of Tag

            9000 ExifVersion
            0191 ComponentsConfiguration
            0392 BrightnessValue
            0492 ExposureBiasValue
            0692 SubjectDistance
            0A92 FocalLength
            9092 SubSecTime
            9192 SubSecTimeOriginal
            9292 SubSecTimeDigitized
            A000 FlashPixVersion
            A001 Colorspace
    }
            case tag.ID of
                  0: break;
    // ExposureTime
              $829A: FExposure:=ReadRatio(tag.Offset+off0, false)+' seconds';
    // Compressed Bits Per Pixel
              $9102: FCompressedBPP:=ReadRatio(tag.Offset+off0, true);
    // F-Stop
              $829D: FFStops:=ReadRatio(tag.Offset+off0, true);
    // FDateTimeOriginal
              $9003: FDateTimeOriginal:=ReadAsci(tag.OffSet+off0,tag.Count);
    // DateTimeDigitized
              $9004: FDateTimeDigitized:=ReadAsci(tag.OffSet+off0,tag.Count);
    // ShutterSpeed
              $9201: try
                       n:=ReadRatio(tag.Offset+off0);
                       if n<65535 then begin
                         str(power(2,n):1:0,tmp);
                         FShutterSpeed:='1/'+tmp+' seconds';
                       end else FShutterSpeed:='1 seconds';
                     except
                       FShutterSpeed:='';
                     end;
    //ISO Speed
              $8827: if Tag.Offset <65536 then FISO:=Tag.Offset;
    // Aperture
              $9202: FAperture:=ReadRatio(tag.Offset+off0, true);
    // Max Aperture
              $9205: FMaxAperture:=ReadRatio(tag.Offset+off0, true);
    // UserComments
              $9286: FUserComments:=ReadAsci(tag.OffSet+off0,tag.Count);
    // Metering Mode
              $9207: begin
                       FMeteringMode := Tag.OffSet;
                       if Tag.OffSet in [0..6] then
                         FMeteringMethod := Meter[Tag.OffSet]
                       else
                         if Tag.OffSet=7 then
                           FMeteringMethod := Meter[7]  //Other
                         else
                           FMeteringMethod := Meter[0]; //Unknown
                     end;
    // Light Source
               $9208: begin
                       FLightSource:=Tag.OffSet;
                       case Tag.OffSet of
                          0: FLightSourceDesc := 'Unknown';
                          1: FLightSourceDesc := 'Daylight';
                          2: FLightSourceDesc := 'Flourescent';
                          3: FLightSourceDesc := 'Tungsten';
                         10: FLightSourceDesc := 'Flash';
                         17: FLightSourceDesc := 'Standard Light A';
                         18: FLightSourceDesc := 'Standard Light B';
                         19: FLightSourceDesc := 'Standard Light C';
                         20: FLightSourceDesc := 'D55';
                         21: FLightSourceDesc := 'D65';
                         22: FLightSourceDesc := 'D75';
                        255: FLightSourceDesc := 'Other';
                       else
                         FLightSourceDesc := 'Unknown';
                       end;
                     end;
    //Flash
              $9209: begin
                       FFlash:=Tag.OffSet;
                       case Tag.OffSet of
                         0: FFlashDesc := 'No Flash';
                         1: FFlashDesc := 'Flash';
                         5: FFlashDesc := 'Flash No Strobe';
                         7: FFlashDesc := 'Flash Strobe';
                        25: FFlashDesc := 'Flash (Auto)';
                       else
                         FFlashDesc := 'No Flash';
                       end;
                     end;
    //Exposure
              $8822: begin
                       FExposureProgram:=Tag.OffSet;
                       if Tag.OffSet in [1..8] then
                         FExposureProgramDesc := ExplType[Tag.OffSet]
                       else
                         FExposureProgramDesc := ExplType[9];
                     end;
    //PixelXDimension
               $A002: FPixelXDimension := Tag.Offset;
    //PixelYDimension
               $A003: FPixelYDimension := Tag.Offset;
            end;
          end;
        end;
      end;
    end;
  finally
    FFile.Close;
    FFile.Free;
  end;
end;

function TExif.GetDateTime: TDateTime;
begin
  if FDateTime = '0000-00-00 00:00:00' then
    Result := 0
   else
  	Result := SToDateTime(FDateTime, ifIO);
end;

function TExif.GetDateTimeDigitized: TDateTime;
begin
  if FDateTimeDigitized = '0000:00:00 00:00:00' then
    Result := 0
   else
	  Result := SToDateTime(FDateTimeDigitized, ifIO);
end;

function TExif.GetDateTimeOriginal: TDateTime;
begin
  if FDateTimeOriginal = '0000:00:00 00:00:00' then
    Result := 0
   else
  	Result := SToDateTime(FDateTimeOriginal, ifIO);
end;

end.
