unit uDelphiTest;

interface

implementation

uses
  SysUtils,
  uTypes,
  uFiles,
  uStrings,
  uCSVFile,
  uDelphi;

procedure Test;
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
  Test;

end.
