unit uPictureFactory;

interface

uses
  Graphics,
  uTypes,
  uFileFactory;

type
  TPictureFactory = class(TFileFactory)
  public
    function GetBitmap(const Name: string): TBitmap; overload;
    function GetBitmap(const Name: string; const Width, Height: SG): TBitmap; overload;
  end;

implementation

uses uObjectFactory;

{ TPictureFactory }

function TPictureFactory.GetBitmap(const Name: string): TBitmap;
var
  FO: TFactoryObject;
begin
  FO := FindOrCreate(Name);

  if FO.CustomObject = nil then
  begin
    FO.CustomObject := TBitmap.Create;
    TBitmap(FO.CustomObject).LoadFromFile(Path + Name + '.bmp');
  end;
  Result := TBitmap(FO.CustomObject);
end;

function TPictureFactory.GetBitmap(const Name: string; const Width,
  Height: SG): TBitmap;
begin
  Result := GetBitmap(Name);
  // TODO Resize
end;

end.

