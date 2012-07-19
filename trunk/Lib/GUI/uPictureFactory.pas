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
    constructor Create;
    function GetBitmap(const Name: string): TDBitmap; overload;
    function GetBitmap(const Name: string; const Width, Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = clNone): TDBitmap; overload;
  end;

implementation

uses uObjectFactory, uMath, SysUtils;

{ TPictureFactory }

function TPictureFactory.GetBitmap(const Name: string): TDBitmap;
var
  FO: TFactoryObject;
  i: SG;
  FileName: TFileName;
begin
  FO := FindOrCreate(Name);

  if FO.CustomObject = nil then
  begin
    for i := 0 to  Length(PrefferedExt) - 1 do
    begin
      FileName := Path + Name + '.' + PrefferedExt[i];
      if FileExists(FileName) then
      begin
		    FO.CustomObject := TDBitmap.Create;
		    TDBitmap(FO.CustomObject).LoadFromFile(FileName);
      end;
    end;
  end;
  Result := TDBitmap(FO.CustomObject);
end;

constructor TPictureFactory.Create;
begin
	inherited;
end;

function TPictureFactory.GetBitmap(const Name: string; const Width,
  Height: SG; const KeepRatio: BG = True; const BackgroundColor: TColor = clNone): TDBitmap;
var
  W, H: SG;
begin
  Result := GetBitmap(Name);
  if Result = nil then Exit;

  if Result.Transparent and (BackgroundColor <> clNone) then
	  Result.ChangeColor(Result.TransparentColor, BackgroundColor);

	if KeepRatio then
  begin
	  W := Result.Width;
	  H := Result.Height;
  	SetScale(W, H, Width, Height);
		Result.Resize(W, H);
  end
  else
		Result.Resize(Width, Height);
end;

end.

