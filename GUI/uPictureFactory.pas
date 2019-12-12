unit uPictureFactory;

interface

uses
  UITypes,

  uTypes,
  uDBitmap,
  uFileFactory;

type
  TPictureFactory = class(TFileFactory)
  private
    FBrightness: SG;
    FContrast: SG;
    FGamma: SG;
    FGrayscale: SG;
    FGradient: SG;
    procedure SetBrightness(const Value: SG);
    procedure SetContrast(const Value: SG);
    procedure SetGamma(const Value: SG);
    procedure SetGradient(const Value: SG);
    procedure SetGrayscale(const Value: SG);
  public
    property Brightness: SG read FBrightness write SetBrightness;
    property Contrast: SG read FContrast write SetContrast;
    property Gamma: SG read FGamma write SetGamma;
    property Grayscale: SG read FGrayscale write SetGrayscale;
    property Gradient: SG read FGradient write SetGradient;
    constructor Create;
    function GetBitmap(const Name: string): TDBitmap; overload;
    function GetBitmap(const Name: string; const Width, Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = TColorRec.SysNone): TDBitmap; overload;
    function HasBitmap(const Name: string): BG;
  end;

var
  PictureFactory: TPictureFactory;

implementation

uses
  uObjectFactory, uMath, uDrawStyle, uColor, uFiles,
  SysUtils, Math;

{ TPictureFactory }

function TPictureFactory.GetBitmap(const Name: string): TDBitmap;
var
  FO: TFactoryObject;
  i, j: SG;
  FileName: TFileName;
  Bmp: TDBitmap;
begin
  FO := FindOrCreate(Name);

  if FO.CustomObject = nil then
  begin
    for j := 0 to Paths.Count - 1 do
      for i := 0 to AllowedExtensions.Count - 1 do
      begin
        FileName := Paths[j] + Name + '.' + AllowedExtensions[i];
        if FileExists(FileName) then
        begin
          FO.CustomObject := TDBitmap.Create;
          Bmp := TDBitmap(FO.CustomObject);
          Bmp.LoadFromFile(FileName);
          Result := TDBitmap(FO.CustomObject);
          Exit;
        end;
      end;
  end;
  Result := TDBitmap(FO.CustomObject);
end;

constructor TPictureFactory.Create;
var
  i: SG;
begin
	inherited;
  FContrast := 256;
	AllowedExtensions.Clear;
	for i := 0 to Length(PrefferedExt) - 1 do
		AllowedExtensions.Add(PrefferedExt[i]);
end;

function TPictureFactory.GetBitmap(const Name: string; const Width,
  Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = TColorRec.SysNone): TDBitmap;
var
  W, H: SG;
  IsTransparent: BG;
  Co: array[0..3] of TColor;
  C: TRGBA;
  Original: TDBitmap;
begin
  Result := nil;
  Original := GetBitmap(Name);
  if Original = nil then Exit;

  BitmapCopy(Result, Original);

  if (BackgroundColor <> TColorRec.SysNone) and (Result.Transparent = False) then
    Result.TryTransparent;
  IsTransparent := Result.Transparent and (BackgroundColor <> TColorRec.SysNone);
  if IsTransparent then
  begin
    Result.Transparent := False;
	  Result.ChangeColor(Result.TransparentColor, BackgroundColor);
  end;

	if KeepRatio then
  begin
	  W := Result.Width;
	  H := Result.Height;
  	SetScale(W, H, Width, Height);
		Result.Resize(W, H);
  end
  else
		Result.Resize(Width, Height);

  if IsTransparent then
  begin
    Result.TransparentColor := BackgroundColor;
    Result.Transparent := True;
  end;

  if FGradient <> 0 then
  begin
    C.R := Min($7F + FGradient, 254);
    C.G := C.R;
    C.B := C.R;
    C.A := 0;
    Co[0] := C.L;
    C.R := Max($7F - FGradient, 1);
    C.G := C.R;
    C.B := C.R;
    C.A := 0;
    Co[1] := C.L;
    Co[2] := Co[0];
    Co[3] := Co[1];
    Result.GenerateRGB(gfFade2x, Co, efAdd127 , nil);
  end;
  if (FBrightness <> 0) or (FContrast <> 256) or (FGamma <> 0) or (FGrayscale <> 0) then
    Result.Colors(Result, Result.GetFullRect, FBrightness, FContrast, FGamma, FGrayscale, True, True, True, nil);
end;

function TPictureFactory.HasBitmap(const Name: string): BG;
var
  i, j: SG;
  FileName: TFileName;
begin
  Result := False;
  for j := 0 to Paths.Count - 1 do
    for i := 0 to  AllowedExtensions.Count - 1 do
    begin
      FileName := Paths[j] + Name + '.' + AllowedExtensions[i];
      if FileExists(FileName) then
      begin
        Result := True;
        Exit;
      end;
  end;
end;

procedure TPictureFactory.SetBrightness(const Value: SG);
begin
  FBrightness := Value;
end;

procedure TPictureFactory.SetContrast(const Value: SG);
begin
  FContrast := Value;
end;

procedure TPictureFactory.SetGamma(const Value: SG);
begin
  FGamma := Value;
end;

procedure TPictureFactory.SetGradient(const Value: SG);
begin
  FGradient := Value;
end;

procedure TPictureFactory.SetGrayscale(const Value: SG);
begin
  FGrayscale := Value;
end;

end.

