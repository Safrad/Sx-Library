unit uRenderTask;

interface

uses
  uDBitmap,
  uGraphicObjects,
  uTransformation,
  uClipping,
  uAsyncTask;

type
  TRenderTask = class(TAsyncTask)
  private
    FTarget: TDBitmap;
    FTransformation: TTransformation;
    FClipping: TClipping;
    FGraphicObjects: TGraphicObjects;
    procedure SetTarget(const Value: TDBitmap);
    procedure SetClipping(const Value: TClipping);
    procedure SetGraphicObjects(const Value: TGraphicObjects);
    procedure SetTransformation(const Value: TTransformation);
  public
    destructor Destroy; override;

    procedure Execute; override;

    property Target: TDBitmap read FTarget write SetTarget;
    property Transformation: TTransformation read FTransformation write SetTransformation;
    property Clipping: TClipping read FClipping write SetClipping;
    property GraphicObjects: TGraphicObjects read FGraphicObjects write SetGraphicObjects;
  end;

implementation

uses
  Types,

  uGraphicObject;

{ TRenderTask }

destructor TRenderTask.Destroy;
begin
  FClipping.Free;

  inherited;
end;

procedure TRenderTask.Execute;
var
//  GraphicObject: TGraphicObject;
  i: Integer;
begin
  inherited;

//  for GraphicObject in FGraphicObjects do
  for i := 0 to FGraphicObjects.Count - 1 do
  begin
    FGraphicObjects[i].Render(FTarget, FTransformation, FClipping);
  end;
end;

procedure TRenderTask.SetClipping(const Value: TClipping);
begin
  FClipping := Value;
end;

procedure TRenderTask.SetGraphicObjects(const Value: TGraphicObjects);
begin
  FGraphicObjects := Value;
end;

procedure TRenderTask.SetTarget(const Value: TDBitmap);
begin
  FTarget := Value;
end;

procedure TRenderTask.SetTransformation(const Value: TTransformation);
begin
  FTransformation := Value;
end;

end.

