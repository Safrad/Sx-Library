unit uGraphicObjectsRenderer;

interface

uses
  uGraphicObjects,
  uThreadPool,
  uTransformation,
  uDBitmap;

type
  TGraphicObjectsRenderer = class
  private
    FThreadPool: TThreadPool;
    FTargetBitmap: TDBitmap;
    FTransformation: TTransformation;
    FGraphicObjects: TGraphicObjects;
    procedure SetThreadPool(const Value: TThreadPool);
    procedure SetTargetBitmap(const Value: TDBitmap);
    procedure SetTransformation(const Value: TTransformation);
    procedure SetGraphicObjects(const Value: TGraphicObjects);
  public
    procedure Render;

    property ThreadPool: TThreadPool read FThreadPool write SetThreadPool;
    property TargetBitmap: TDBitmap read FTargetBitmap write SetTargetBitmap;
    property Transformation: TTransformation read FTransformation write SetTransformation;
    property GraphicObjects: TGraphicObjects read FGraphicObjects write SetGraphicObjects;
  end;

implementation

uses
  uTypes,
  uClipping,
  uGraphicObject,
  uRenderTask,
  uDivideSpace,
  uDivideSpaceOptions,
  uGraph;

{ TGraphicObjectsRenderer }

procedure TGraphicObjectsRenderer.Render;
var
  Task: TRenderTask;
  RectArray: TRectArray;
  DivideSpace: TDivideSpace;
  i: SG;
  Clipping: TClipping;
begin
	inherited;

  RectArray := nil;

  DivideSpace := TDivideSpace.Create;
  try
    RectArray := DivideSpace.Divide2D(TargetBitmap.GetFullrect, WindowDivideSpaceOptions(256));
  finally
    DivideSpace.Free;
  end;

//  RectArray := SortRectArray(RectArray, CenterOfRect(TfMain(ParentForm).BmpD.GetFullRect), False);

  FThreadPool.ClearTasks;
  for i := 0 to Length(RectArray) - 1 do
  begin
    Clipping := TClipping.Create;
    Clipping.ClippingRect := RectArray[i];

    Task := TRenderTask.Create;
    Task.Target := TargetBitmap;
    Task.Transformation := Transformation;
    Task.Clipping := Clipping;
    Task.GraphicObjects := GraphicObjects; // TODO : Select only some (clip)
    FThreadPool.AddTask(Task);
  end;
end;

procedure TGraphicObjectsRenderer.SetGraphicObjects(const Value: TGraphicObjects);
begin
  FGraphicObjects := Value;
end;

procedure TGraphicObjectsRenderer.SetTargetBitmap(const Value: TDBitmap);
begin
  FTargetBitmap := Value;
end;

procedure TGraphicObjectsRenderer.SetThreadPool(const Value: TThreadPool);
begin
  FThreadPool := Value;
end;

procedure TGraphicObjectsRenderer.SetTransformation(const Value: TTransformation);
begin
  FTransformation := Value;
end;

end.
