unit uDelphiTest;

interface

uses TestFrameWork;

type
  TDelphiTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  SysUtils,
  
  uFiles,
  uStrings,
  uCSVFile,
  uDelphi;

{ TDelphiTest }

procedure TDelphiTest.Test;
var
//  Versions: TArrayOfSG;
  DelphiVersion: TDelphiVersion;
  s: string;
begin
//  Versions := GetAvailableDelphiVersions;
  s :=
    'ShortName' + CSVSep +
    'MajorVersion' + CSVSep +
    'Registry' + CSVSep +
    'dcc' + FileSep;
  for DelphiVersion := TDelphiVersion(0) to TDelphiVersion(GetDelphiVersionCount - 1) do
    s := s +
      GetDelphiShortName(DelphiVersion) + CSVSep +
      IntToStr(GetDelphiMajorVersion(DelphiVersion)) + CSVSep +
      GetDelphiRegistryName(DelphiVersion) + CSVSep +
      IntToStr(GetDelphiCompilerVersion(DelphiVersion)) +
      FileSep;
  WriteStringToFile(TempDir + 'DelphiVersions.csv', s, False);
end;

initialization
	RegisterTest('DelphiTest', TDelphiTest.Suite);
end.
