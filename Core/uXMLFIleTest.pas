unit uXMLFIleTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TXMLFileTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  uXMLFile,
  uOperatingSystem;

{ TXMLFileTest }

procedure TXMLFileTest.Test;
var
  XMLFile: TXMLFile;
  TempDir: string;
  FileName: TFileName;
  s: string;
begin
  TempDir := OperatingSystem.TemporaryDirectory.ProcessTempDir;
  FileName := TempDir + 'TestConfiguration.xml';

  XMLFile := TXMLFile.Create(FileName);
  try
    XMLFile.WriteString('Main', 'Key0', 'Value0');
    XMLFile.WriteString('Main', 'Key1', 'Value1');
    XMLFile.WriteString('Second Section', 'Key0', 'Value0b');
  finally
    XMLFile.Free;
  end;

  XMLFile := TXMLFile.Create(FileName);
  try
    s := XMLFile.ReadString('Main', 'Key0', '');
    CheckEquals('Value0', s);
    s := XMLFile.ReadString('Main', 'Key1', '');
    CheckEquals('Value1', s);
    s := XMLFile.ReadString('Second Section', 'Key0', '');
    CheckEquals('Value0b', s);
  finally
    XMLFile.Free;
  end;

end;

initialization
	RegisterTest('XML File Test', TXMLFileTest.Suite);
end.
