unit uCustomSystemPaths;

interface

uses
  SysUtils;

type
  TCustomSystemPaths = class abstract
  strict private
    FLocalAppDataDir: string;
    FAppDataDir: string;
    FDataDir: string;
    FSoundsDir: string;
    FModuleFileName: TFileName;
    FCompanyLocalAppDataDir: string;
    FCompanyAppDataDir: string;
    FGraphDir: string;
    FExeFileName: TFileName;
    FExeParameters: string;
    FUserProfileDir: string;
    FStartDir: string;
    FWorkDir: string;
    FCommonLocalAppDataDir: string;
    FCommonAppDataDir: string;
    FHomeDir: string;
    function GetAppDataDir: string;
    function GetCommonAppDataDir: string;
    function GetCommonLocalAppDataDir: string;
    function GetCompanyAppDataDir: string;
    function GetCompanyLocalAppDataDir: string;
    function GetDataDir: string;
    function GetExeFileName: TFileName;
    function GetExeParameters: string;
    function GetGraphDir: string;
    function GetHomeDir: string;
    function GetLocalAppDataDir: string;
    function GetModuleFileName: TFileName;
    function GetSoundsDir: string;
    function GetStartDir: string;
    function GetUserProfileDir: string;
    function GetWorkDir: string;
  strict protected
    function ForceGetDataDir: string; virtual;
    function ForceGetGraphDir: string; virtual;
    function ForceGetSoundsDir: string; virtual;

    function ForceGetAppDataDir: string; virtual; abstract;
    function ForceGetCommonAppDataDir: string; virtual; abstract;
    function ForceGetCommonLocalAppDataDir: string; virtual; abstract;
    function ForceGetCompanyAppDataDir: string; virtual; abstract;
    function ForceGetCompanyLocalAppDataDir: string; virtual; abstract;
    function ForceGetExeFileName: TFileName; virtual; abstract;
    function ForceGetExeParameters: string; virtual; abstract;
    function ForceGetHomeDir: string; virtual; abstract;
    function ForceGetLocalAppDataDir: string; virtual; abstract;
    function ForceGetModuleFileName: TFileName; virtual; abstract;
    function ForceGetStartDir: string; virtual; abstract;
    function ForceGetUserProfileDir: string; virtual; abstract;
    function ForceGetWorkDir: string; virtual; abstract;
  public
    /// <summary>
    /// Actual Dir
    /// </summary>
    property StartDir: string read GetStartDir;
    /// <summary>
    /// EXE file, data files (Read only)
    /// </summary>
    property WorkDir: string read GetWorkDir;
    /// <summary>
    /// (Read only)
    /// </summary>
    property GraphDir: string read GetGraphDir;
    /// <summary>
    /// (Read only)
    /// </summary>
    property SoundsDir: string read GetSoundsDir;
    /// <summary>
    /// Input data (Read only)
    /// </summary>
    property DataDir: string read GetDataDir;
    /// <summary>
    ///
    /// </summary>
    property UserProfileDir: string read GetUserProfileDir;
    /// <summary>
    /// User specific configuration files (Ini, Autosaves) (Read and Write)
    /// </summary>
    property AppDataDir: string read GetAppDataDir;
    /// <summary>
    /// User machine specific configuration files (Logs, Ini) (Read and Write)
    /// </summary>
    property LocalAppDataDir: string read GetLocalAppDataDir;
    /// <summary>
    ///
    /// </summary>
    property CompanyAppDataDir: string read GetCompanyAppDataDir;
    /// <summary>
    ///
    /// </summary>
    property CompanyLocalAppDataDir: string read GetCompanyLocalAppDataDir;
    /// <summary>
    /// User documnets (Read and Write)
    /// </summary>
    property HomeDir: string read GetHomeDir;
    /// <summary>
    /// Application Data
    /// </summary>
    property CommonAppDataDir: string read GetCommonAppDataDir;
    /// <summary>
    /// Local Application Data
    /// </summary>
    property CommonLocalAppDataDir: string read GetCommonLocalAppDataDir;
    /// <summary>
    ///
    /// </summary>
    property ExeFileName: TFileName read GetExeFileName;
    /// <summary>
    ///
    /// </summary>
    property ExeParameters: string read GetExeParameters;
    /// <summary>
    ///
    /// </summary>
    property ModuleFileName: TFileName read GetModuleFileName;
  end;

implementation

{ TCustomSystemPaths }

function TCustomSystemPaths.ForceGetDataDir: string;
begin
  Result := WorkDir + 'Data' + PathDelim;
  Assert(Result <> '');
end;

function TCustomSystemPaths.ForceGetGraphDir: string;
begin
  Result := WorkDir + 'Graphics' + PathDelim;
  Assert(Result <> '');
end;

function TCustomSystemPaths.ForceGetSoundsDir: string;
begin
  Result := WorkDir + 'Sounds' + PathDelim;
  Assert(Result <> '');
end;

function TCustomSystemPaths.GetAppDataDir: string;
begin
  if FAppDataDir = '' then
    FAppDataDir := ForceGetAppDataDir;
  Assert(FAppDataDir <> '');
  Result := FAppDataDir;
end;

function TCustomSystemPaths.GetCommonAppDataDir: string;
begin
  if FCommonAppDataDir = '' then
    FCommonAppDataDir := ForceGetCommonAppDataDir;
  Assert(FCommonAppDataDir <> '');
  Result := FCommonAppDataDir;
end;

function TCustomSystemPaths.GetCommonLocalAppDataDir: string;
begin
  if FCommonLocalAppDataDir = '' then
    FCommonLocalAppDataDir := ForceGetCommonLocalAppDataDir;
  Assert(FCommonLocalAppDataDir <> '');
  Result := FCommonLocalAppDataDir;
end;

function TCustomSystemPaths.GetCompanyAppDataDir: string;
begin
  if FCompanyAppDataDir = '' then
    FCompanyAppDataDir := ForceGetCompanyAppDataDir;
  Assert(FCompanyAppDataDir <> '');
  Result := FCompanyAppDataDir;
end;

function TCustomSystemPaths.GetCompanyLocalAppDataDir: string;
begin
  if FCompanyLocalAppDataDir = '' then
    FCompanyLocalAppDataDir := ForceGetCompanyLocalAppDataDir;
  Assert(FCompanyLocalAppDataDir <> '');
  Result := FCompanyLocalAppDataDir;
end;

function TCustomSystemPaths.GetDataDir: string;
begin
  if FDataDir = '' then
    FDataDir := ForceGetDataDir;
  Assert(FDataDir <> '');
  Result := FDataDir;
end;

function TCustomSystemPaths.GetExeFileName: TFileName;
begin
  if FExeFileName = '' then
    FExeFileName := ForceGetExeFileName;
  Assert(FExeFileName <> '');
  Result := FExeFileName;
end;

function TCustomSystemPaths.GetExeParameters: string;
begin
  if FExeParameters = '' then
    FExeParameters := ForceGetExeParameters;
  Assert(FExeParameters <> '');
  Result := FExeParameters;
end;

function TCustomSystemPaths.GetGraphDir: string;
begin
  if FGraphDir = '' then
    FGraphDir := ForceGetGraphDir;
  Assert(FGraphDir <> '');
  Result := FGraphDir;
end;

function TCustomSystemPaths.GetHomeDir: string;
begin
  if FHomeDir = '' then
    FHomeDir := ForceGetHomeDir;
  Assert(FHomeDir <> '');
  Result := FHomeDir;
end;

function TCustomSystemPaths.GetLocalAppDataDir: string;
begin
  if FLocalAppDataDir = '' then
    FLocalAppDataDir := ForceGetLocalAppDataDir;
  Assert(FLocalAppDataDir <> '');
  Result := FLocalAppDataDir;
end;

function TCustomSystemPaths.GetModuleFileName: TFileName;
begin
  if FModuleFileName = '' then
    FModuleFileName := ForceGetModuleFileName;
  Assert(FModuleFileName <> '');
  Result := FModuleFileName;
end;

function TCustomSystemPaths.GetSoundsDir: string;
begin
  if FSoundsDir = '' then
    FSoundsDir := ForceGetSoundsDir;
  Assert(FSoundsDir <> '');
  Result := FSoundsDir;
end;

function TCustomSystemPaths.GetStartDir: string;
begin
  if FStartDir = '' then
    FStartDir := ForceGetStartDir;
  Assert(FStartDir <> '');
  Result := FStartDir;
end;

function TCustomSystemPaths.GetUserProfileDir: string;
begin
  if FUserProfileDir = '' then
    FUserProfileDir := ForceGetUserProfileDir;
  Assert(FUserProfileDir <> '');
  Result := FUserProfileDir;
end;

function TCustomSystemPaths.GetWorkDir: string;
begin
  if FWorkDir = '' then
    FWorkDir := ForceGetWorkDir;
  Assert(FWorkDir <> '');
  Result := FWorkDir;
end;

end.
