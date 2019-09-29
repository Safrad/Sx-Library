unit uObjectFactory;

interface

uses
  uTypes,
  uData,
  uTimeSpan;

type
  TFactoryObject = class
  private
    Name: string;
    LastUsed: U8;
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
    procedure ReleaseObject(const AObject: TObject);
    procedure ReplaceObject(const Name: string; const AObject: TObject);
//    procedure CreateObject(const Name: string); virtual; abstract;
    function FindOrCreate(const Name: string): TFactoryObject;
    procedure DeleteUnusedObjects;
    procedure DeleteOld(const Period: TTimeSpan);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,

  uMainTimer,
  uMath;

{ TFactoryObject }

constructor TFactoryObject.Create;
begin

end;

destructor TFactoryObject.Destroy;
begin
  FreeAndNil(CustomObject);

  inherited;
end;


{ TObjectFactory }

constructor TObjectFactory.Create;
begin
  Objects := TData.Create;
end;

procedure TObjectFactory.DeleteOld(const Period: TTimeSpan);
var
  FO: TFactoryObject;
  i: SG;
begin
  i := 0;
  while i < Objects.Count do
  begin
    FO := TFactoryObject(Objects.GetObject(i));
    if (FO.ReferenceCount <= 0) and (MainTimer.IntervalFrom(FO.LastUsed) >= Period.Ticks) then
      Objects.Delete(i)
    else
      Inc(i);
  end;
end;

procedure TObjectFactory.DeleteUnusedObjects;
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

destructor TObjectFactory.Destroy;
begin
  FreeAndNil(Objects);

  inherited;
end;

function TObjectFactory.FindOrCreate(const Name: string): TFactoryObject;
begin
  Result := FoundObject(Name);
  if Result = nil then
  begin
    Result := TFactoryObject.Create;
    Result.Name := Name;
    Objects.Add(Result);
  end;
  Inc(Result.ReferenceCount);
  Result.LastUsed := MainTimer.Value.Ticks;
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

procedure TObjectFactory.ReleaseObject(const AObject: TObject);
begin
  // LazyRelease
{  if AObject.ReferenceCount <= 0 then

  else
    Dec(AObject, ReferenceCount); TODO }
end;

procedure TObjectFactory.ReplaceObject(const Name: string;
  const AObject: TObject);
var
  FO: TFactoryObject;
begin
  FO := FoundObject(Name);
  if FO <> nil then
  begin
    FreeAndNil(FO.CustomObject);
    FO.CustomObject := AObject;
  end;
end;

end.
