unit uPictureFactoryTest;

interface

uses TestFrameWork;

type
  TPictureFactoryTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  uPictureFactory,
  uDBitmap,
  uFiles;

{ TPictureFactoryTest }

procedure TPictureFactoryTest.Test;
var
  PictureFactory: TPictureFactory;
  Bmp: TDBitmap;
begin
  PictureFactory := TPictureFactory.Create;
  try
    PictureFactory.Paths.Add(GraphDir);
    Bmp := PictureFactory.GetBitmap('my');
    Check(Bmp = nil);
    Bmp := PictureFactory.GetBitmap('my');
    Check(Bmp = nil);
    Bmp := PictureFactory.GetBitmap('LibTest');
    Check(Bmp <> nil);
    Bmp := PictureFactory.GetBitmap('Lib');
    Check(Bmp = nil);
  finally
    PictureFactory.Free;
  end;
end;

initialization
	RegisterTest('Picture Factory Test', TPictureFactoryTest.Suite);
end.
