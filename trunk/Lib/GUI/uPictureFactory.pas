unit uPictureFactory;

interface

uses
  Graphics,
  uTypes,
  uDBitmap,
  uFileFactory;

type
  TPictureFactory = class(TFileFactory)
  public
    Brightness: SG;
    Contrast: SG;
    Gamma: SG;
    Grayscale: SG;
    Gradient: SG;
    constructor Create;
    function GetBitmap(const Name: string): TDBitmap; overload;
    function GetBitmap(const Name: string; const Width, Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = clNone): TDBitmap; overload;
  end;

implementation

uses
  uObjectFactory, uMath, uDrawStyle, uColor, SysUtils, Math;

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
      for i := 0 to  AllowedExtensions.Count - 1 do
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
  Contrast := 256;
	AllowedExtensions.Clear;
	for i := 0 to Length(PrefferedExt) - 1 do
		AllowedExtensions.Add(PrefferedExt[i]);
end;

function TPictureFactory.GetBitmap(const Name: string; const Width,
  Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = clNone): TDBitmap;
var
  W, H: SG;
  IsTransparent: BG;
  Co: array[0..3] of TColor;
  C: TRGBA;
begin
  Result := GetBitmap(Name);
  if Result = nil then Exit;

  if (BackgroundColor <> clNone) and (Result.Transparent = False) then
    Result.TryTransparent;
  IsTransparent := Result.Transparent and (BackgroundColor <> clNone);
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

  if Gradient <> 0 then
  begin
    C.R := Min($7F + Gradient, 254);
    C.G := C.R;
    C.B := C.R;
    C.A := 0;
    Co[0] := C.L;
    C.R := Max($7F - Gradient, 1);
    C.G := C.R;
    C.B := C.R;
    C.A := 0;
    Co[1] := C.L;
    Co[2] := Co[0];
    Co[3] := Co[1];
    Result.GenerateRGB(gfFade2x, Co, efAdd127 , nil);
  end;
  if (Brightness <> 0) or (Contrast <> 256) or (Gamma <> 0) or (Grayscale <> 0) then
    Result.Colors(Result, Result.GetFullRect, Brightness, Contrast, Gamma, Grayscale, True, True, True, nil);
end;

end.

