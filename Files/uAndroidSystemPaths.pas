unit uAndroidSystemPaths;

interface

uses
  SysUtils,

  uCustomSystemPaths;

type
  TAndroidSystemPaths = class(TCustomSystemPaths)
  strict protected
    function ForceGetAppDataDir: string; override;
    function ForceGetCommonAppDataDir: string; override;
    function ForceGetCommonLocalAppDataDir: string; override;
    function ForceGetCompanyAppDataDir: string; override;
    function ForceGetCompanyLocalAppDataDir: string; override;
    function ForceGetExeFileName: TFileName; override;
    function ForceGetExeParameters: string; override;
    function ForceGetHomeDir: string; override;
    function ForceGetLocalAppDataDir: string; override;
    function ForceGetModuleFileName: TFileName; override;
    function ForceGetStartDir: string; override;
    function ForceGetUserProfileDir: string; override;
    function ForceGetWorkDir: string; override;
  end;

implementation

uses
  System.IOUtils,

  uTypes,
  uStrings,
  uProjectInfo,
  uFiles;

{ TAndroidSystemPaths }

function TAndroidSystemPaths.ForceGetAppDataDir: string;
begin
  Result := CorrectDirF(TPath.GetHomePath);
end;

function TAndroidSystemPaths.ForceGetCommonAppDataDir: string;
begin
  Result := ParentDirF(AppDataDir, 2);
end;

function TAndroidSystemPaths.ForceGetCommonLocalAppDataDir: string;
begin
  Result := ParentDirF(LocalAppDataDir, 2);
end;

function TAndroidSystemPaths.ForceGetCompanyAppDataDir: string;
begin
  raise ENotSupportedException.Create('CompanyAppDataDir not supported.');
end;

function TAndroidSystemPaths.ForceGetCompanyLocalAppDataDir: string;
begin
  Result := ParentDirF(LocalAppDataDir, 1);
end;

function TAndroidSystemPaths.ForceGetExeFileName: TFileName;
begin
  Result := ParamStr(0);
end;

function TAndroidSystemPaths.ForceGetExeParameters: string;
var
  i: SG;
begin
  Result := '';
  for i := 1 to ParamCount do
    AppendStr(Result, ParamStr(i));
end;

function TAndroidSystemPaths.ForceGetHomeDir: string;
begin
  Result := TPath.GetHomePath;
end;

function TAndroidSystemPaths.ForceGetLocalAppDataDir: string;
begin
  Result := AppDataDir + 'Local\';
  ForceGetUserProfileDir;
end;

function TAndroidSystemPaths.ForceGetModuleFileName: TFileName;
begin
  Result := GetModuleFileNameFunc(HInstance);
end;

function TAndroidSystemPaths.ForceGetStartDir: string;
begin
  Result := AppDataDir;
end;

function TAndroidSystemPaths.ForceGetUserProfileDir: string;
begin
  Result := TPath.GetDocumentsPath;
end;

function TAndroidSystemPaths.ForceGetWorkDir: string;
begin
  Result := ExtractFilePath(ExeFileName);
end;

end.
