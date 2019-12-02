unit uGraphicObjectsRendererTest;

interface

uses
  uTypes,
  uGraphicObjects,
  uGeometry2D,
  TestFrameWork;

type
  TGraphicObjectsRendererTest = class(TTestCase)
  private
    const
      MaxResolution = 12000;

    function ExamplePointGraphicsObjects: TGraphicObjects;
    function ExampleBarGraphicsObjects: TGraphicObjects;
    function ExampleLineGraphicsObjects: TGraphicObjects;
    procedure Test(const AGraphicObjects: TGraphicObjects);
  published
    procedure TestPoint;
    procedure TestLine;
    procedure TestBar;
  end;

implementation

uses
  SysUtils,
  UITypes,

  uCPU,
  uTemporaryDirectory,
  uFiles,
  uStrings,
  uOutputFormat,
  uThreadPool,
  uTransformation,
  uPointGraphicObject,
  uLine,
  uBar,
  uStopwatch,
  uDBitmap,
  uDrawStyle,
  uSxRandomGenerator,
  uGraphicObjectsRenderer;

{ TGraphicObjectsRendererTest }

function TGraphicObjectsRendererTest.ExamplePointGraphicsObjects: TGraphicObjects;
var
  SxRandomGenerator: TSxRandomGenerator;
  Point: TPointGraphicObject;
  i: SG;
  R: TRect2D;
begin
  Result := TGraphicObjects.Create(True);
  SxRandomGenerator := TSxRandomGenerator.Create;
  try
    for i := 0 to 49999 do
    begin
      Point := TPointGraphicObject.Create;
      try
        R.F.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.F.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.T.X := R.F.X;
        R.T.Y := R.F.Y;

        Point.Position := R;
        Point.Depth := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        Point.Color := TColor(SxRandomGenerator.RandomU4);
        Point.Effect := TEffect(SxRandomGenerator.RangeU4(U4(High(TEffect))));
        Result.Add(Point);
      except
        on E: Exception do
        begin
          Point.Free;
          raise;
        end;
      end;
    end;
  finally
    SxRandomGenerator.Free;
  end;
end;

function TGraphicObjectsRendererTest.ExampleLineGraphicsObjects: TGraphicObjects;
var
  SxRandomGenerator: TSxRandomGenerator;
  Line: TLine;
  i: SG;
  R: TRect2D;
begin
  Result := TGraphicObjects.Create(True);
  SxRandomGenerator := TSxRandomGenerator.Create;
  try
    for i := 0 to 999 do
    begin
      Line := TLine.Create;
      try
        if i and 1 = 0 then
        begin
          // Horizontal
          R.F.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
          R.F.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
          R.T.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
          R.T.Y := R.F.Y;
        end
        else
        begin
          // Vertical
          R.F.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
          R.F.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
          R.T.X := R.F.X;
          R.T.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        end;
        R.Normalize;

        Line.Position := R;
        Line.Depth := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        Line.Color := TColor(SxRandomGenerator.RandomU4);
        Line.Effect := TEffect(SxRandomGenerator.RangeU4(U4(High(TEffect))));
        Result.Add(Line);
      except
        on E: Exception do
        begin
          Line.Free;
          raise;
        end;
      end;
    end;
  finally
    SxRandomGenerator.Free;
  end;
end;

function TGraphicObjectsRendererTest.ExampleBarGraphicsObjects: TGraphicObjects;
var
  SxRandomGenerator: TSxRandomGenerator;
  Bar: TBar;
  i: SG;
  R: TRect2D;
begin
  Result := TGraphicObjects.Create(True);
  SxRandomGenerator := TSxRandomGenerator.Create;
  try
    for i := 0 to 199 do
    begin
      Bar := TBar.Create;
      try
        R.F.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.F.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.T.X := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.T.Y := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        R.Normalize;

        Bar.Position := R;
        Bar.Depth := SxRandomGenerator.RangeU4(MaxResolution) / MaxResolution;
        Bar.Color := TColor(SxRandomGenerator.RandomU4);
        Bar.Effect := TEffect(SxRandomGenerator.RangeU4(U4(High(TEffect))));
        Result.Add(Bar);
      except
        on E: Exception do
        begin
          Bar.Free;
          raise;
        end;
      end;
    end;
  finally
    SxRandomGenerator.Free;
  end;
end;

procedure TGraphicObjectsRendererTest.Test(const AGraphicObjects: TGraphicObjects);
var
	GraphicObjectsRenderer: TGraphicObjectsRenderer;
  ThreadCount: SG;
  Bmp: TDBitmap;
  ThreadPool: TThreadPool;
  Transformation: TTransformation;
  Stopwatch: TStopwatch;
  FileName: TFileName;
  FileSize, LastFileSize: SG;
  OutputPath: string;
begin
  OutputPath := TemporaryDirectory.ProcessTempDir;

  ThreadPool := TThreadPool.Create;
  Transformation := TTransformation.Create;
  Transformation.Zoom := 3000;

	GraphicObjectsRenderer := TGraphicObjectsRenderer.Create;
  try
    GraphicObjectsRenderer.GraphicObjects := AGraphicObjects;
    Bmp := TDBitmap.Create;
    Bmp.SetSize(4000, 3000);
    GraphicObjectsRenderer.TargetBitmap := Bmp;

    GraphicObjectsRenderer.Transformation := Transformation;

    GraphicObjectsRenderer.ThreadPool := ThreadPool;

    Stopwatch := TStopwatch.Create;
    try
      LastFileSize := 0;
      for ThreadCount := 1 to 2 * CPU.LogicalProcessorCount do
      begin
        Bmp.Bar(TColors.Black, ef16);
        ThreadPool.MaxThreads.Value := ThreadCount;
        Stopwatch.Restart;
        GraphicObjectsRenderer.Render;
        GraphicObjectsRenderer.ThreadPool.WaitForNoWork;
        Stopwatch.Stop;
        WriteStringToFile(OutputPath + 'time.txt', Stopwatch.Elapsed.ToStringInSeconds + FileSep, True);

        FileName := OutputPath + IntToStr(ThreadCount) + '.bmp';
        GraphicObjectsRenderer.TargetBitmap.SaveToFile(FileName);
        FileSize := GetFileSizeU(FileName);
        if ThreadCount > 1 then
        begin
          CheckTrue(FileSize = LastFileSize);
        end;
        LastFileSize := FileSize;
      end;
    finally
      Stopwatch.Free;
    end;

	finally
    Transformation.Free;
    GraphicObjectsRenderer.GraphicObjects.Free;
    GraphicObjectsRenderer.TargetBitmap.Free;
  	GraphicObjectsRenderer.Free;
    ThreadPool.Free;
	end;
end;

procedure TGraphicObjectsRendererTest.TestPoint;
begin
  Test(ExamplePointGraphicsObjects);
end;

procedure TGraphicObjectsRendererTest.TestLine;
begin
  Test(ExampleLineGraphicsObjects);
end;

procedure TGraphicObjectsRendererTest.TestBar;
begin
  Test(ExamplebarGraphicsObjects);
end;

initialization
	RegisterTest('Graphic Objects Renderer Test', TGraphicObjectsRendererTest.Suite);
end.
