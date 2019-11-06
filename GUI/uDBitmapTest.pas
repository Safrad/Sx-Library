unit uDBitmapTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TDBitmapTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uDBitmap,
  uFiles;

{ TDBitmapTest }

procedure TDBitmapTest.Test;
var
  B: TDBitmap;
begin
	B := TDBitmap.Create;
  try
    B.LoadFromFile(DataDir + 'Example Raster Graphics\Abalone.png');
    B.ResizeUp(600, 800);
	finally
  	B.Free;
	end;
end;

initialization
	RegisterTest('DBitmap Test', TDBitmapTest.Suite);
end.
