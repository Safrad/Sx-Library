unit uDelphiTest;

interface

uses TestFrameWork;

type
  TDelphiTest = class(TTestCase)
  published
    procedure TestDelphiVersion;
    procedure TestPackageVersion;
  end;

implementation

uses
  SysUtils,
  
  uFiles,
  uStrings,
  uCSVFile,
  uDelphi;

{ TDelphiTest }

procedure TDelphiTest.TestDelphiVersion;
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
      IntToStr(GetMajorVersion(DelphiVersion)) + CSVSep +
      GetDelphiRegistryName(DelphiVersion) + CSVSep +
      IntToStr(GetDelphiCompilerVersion(DelphiVersion)) +
      FileSep;
  WriteStringToFile(TempDir + 'DelphiVersions.csv', s, False);
end;

procedure TDelphiTest.TestPackageVersion;
  procedure AssertEquals(const V1, V2: TDelphiVersion);
  begin
    CheckEquals(Integer(V1), Integer(V2));
  end;
begin
  AssertEquals(dvDelphi3, GetPackageVersion('aaa_100.dpk'));
  AssertEquals(dvDelphi2006, GetPackageVersion('Cool2006.dpk'));
  AssertEquals(dvDelphi6, GetPackageVersion('Cool6plus.dpk'));
  AssertEquals(dvDelphiXE6, GetPackageVersion('CoolDXE6.dpk'));
  AssertEquals(dvDelphiXE6, GetPackageVersion('CoolXE6.dpk'));
  AssertEquals(dvDelphiXE5, GetPackageVersion('Cool_XE5.dpk'));
  AssertEquals(dvDelphi2006, GetPackageVersion('Cool180.dpk'));
end;

initialization
	RegisterTest('DelphiTest', TDelphiTest.Suite);
end.
