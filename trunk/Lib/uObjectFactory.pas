unit uObjectFactory;

interface

uses
  uTypes,
  uData;

type
  TFactoryObject = class
  private
    Name: string;
    LastUsed: U4;
    ReferenceCount: SG;
  public
    CustomObject: TObject;
    constructor Create;
    destructor Destroy; override;
  end;

  TObjectFactory = class
  private
    Objects: TData;
  protected
    function FoundObject(const Name: string): TFactoryObject;
    procedure ReleaseObject(AObject: TObject);
//    procedure CreateObject(const Name: string); virtual; abstract;
    function FindOrCreate(const Name: string): TFactoryObject;
    procedure FreeMemory;
    procedure DeleteOld(const Period: UG); // in [ms]
  public

  end;

implementation

uses
  uSimulation,
  SysUtils;

{ TFactoryObject }

constructor TFactoryObject.Create;
begin
//  GetGTime;
//  LastUsed := GTime;
end;

destructor TFactoryObject.Destroy;
begin
  inherited;
  FreeAndNil(CustomObject);
end;


{ TObjectFactory }

procedure TObjectFactory.DeleteOld(const Period: UG);
var
  FO: TFactoryObject;
  i: SG;
begin
  i := 0;
  while i < Objects.Count do
  begin
    FO := TFactoryObject(Objects.GetObject(i));
    if (FO.ReferenceCount <= 0) and (IntervalFrom(FO.LastUsed) >= Period) then
      Objects.Delete(i)
    else
      Inc(i);
  end;
end;

function TObjectFactory.FindOrCreate(const Name: string): TFactoryObject;
begin
  Result := FoundObject(Name);
  if Result = nil then
    Result := TFactoryObject.Create;
  Inc(Result.ReferenceCount);
  GetGTime;
  Result.LastUsed := GTime;
end;

function TObjectFactory.FoundObject(const Name: string): TFactoryObject;
var
  FO: TFactoryObject;
  i: SG;
begin
  Result := nil;

  i := 0;
  while i < Objects.Count do
  begin
    FO := TFactoryObject(Objects.GetObject(i));
    if FO.Name = Name then
    begin
      Result := FO;
      Exit;
    end;
    Inc(i);
  end;
end;

procedure TObjectFactory.FreeMemory;
var
  FO: TFactoryObject;
  i: SG;
begin
  i := 0;
  while i < Objects.Count do
  begin
    FO := TFactoryObject(Objects.GetObject(i));
    if FO.ReferenceCount <= 0 then
    begin
      Objects.Delete(i);
    end
    else
      Inc(i);
  end;
end;

procedure TObjectFactory.ReleaseObject(AObject: TObject);
begin
  // LazyRelease
{  if AObject.ReferenceCount <= 0 then

  else
    Dec(AObject, ReferenceCount); TODO }
end;

end.
