unit uWaveTest;

interface

uses
  SysUtils,
  TestFramework;

type
  TWaveTest = class(TTestCase)
  private
    procedure ReadWave(const AFileName: TFileName);
  published
    procedure TestReadFromFile;
  end;

implementation

uses
  uTypes,
  uFiles,
  uFolder,
  uWave;

{ TWaveTest }

procedure TWaveTest.ReadWave(const AFileName: TFileName);
var
  Wave: TWave;
begin
  Wave := TWave.Create;
  try
    Wave.ReadFromFile(AFileName);
  finally
    Wave.Free;
  end;
end;

procedure TWaveTest.TestReadFromFile;
var
  Folder: TFolder;
  FileItem: TFileItem;
begin
  Folder := TFolder.Create;
  try
    Folder.Path := DataDir + 'Wave\';
    Folder.Read;

    FileItem := TFileItem(Folder.Files.First);
    while FileItem <> nil do
    begin
      ReadWave(Folder.Path + FileItem.Name);
      FileItem := TFileItem(Folder.Files.Next);
    end;
  finally
    Folder.Free;
  end;
end;

initialization
  RegisterTest('Wave Test', TWaveTest.Suite);

end.
