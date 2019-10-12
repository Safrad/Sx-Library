unit uSVGReaderTest;

interface

uses
  uTypes,
  SysUtils,
  uSVGReader,
  TestFrameWork;

type
  TSVGReaderTest = class(TTestCase)
  private
    procedure TestReadFile(const AFileName: TFileName);
    procedure TestReadDirectory(const ADir: string);
  published
    procedure Test;
  end;

implementation

uses
  uFolder,
  uFiles;

{ TSVGReaderTest }

procedure TSVGReaderTest.Test;
begin
  TestReadDirectory(DataDir + 'Example Scalable Vector Graphics\');
end;

procedure TSVGReaderTest.TestReadDirectory(const ADir: string);
var
  Folder: TFolder;
  i: Integer;
begin
  Folder := TFolder.Create;
  try
    Folder.Path := ADir;
    Folder.Extensions := ['svg'];
    Folder.Read;
    for i := 0 to Folder.Count - 1 do
    begin
      TestReadFile(ADir + TFileItem(Folder.Files.GetObject(i)).Name);
    end;
  finally
    Folder.Free;
  end;

end;

procedure TSVGReaderTest.TestReadFile(const AFileName: TFileName);
var
  SVGReader: TSVGReader;
begin
  SVGReader := TSVGReader.Create;
  try
//    SVGReader.ReadFromFile(AFileName); TODO
  finally
    SVGReader.Free;
  end;

end;

initialization
	RegisterTest('SVG Reader Test', TSVGReaderTest.Suite);
end.
