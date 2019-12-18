unit uWindowsSystemPaths;

{$ZEROBASEDSTRINGS OFF}

interface

uses
  SysUtils,

  uCustomSystemPaths;

type
  TWindowsSystemPaths = class(TCustomSystemPaths)
  strict private
    function GetCompanySuffix: string;

    function GetWinDir: string;
    function GetSysDir: string;
    function GetProgramFilesDir: string;
  strict protected
    FWinDir: string;
    FSysDir: string;
    FProgramFilesDir: string;

    function ForceGetWinDir: string;
    function ForceGetSysDir: string;
    function ForceGetProgramFilesDir: string;

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
  public
    /// <summary>
    /// Windows 98: Shared configuration files (Read and Write)
    /// i. e. 'C:\Windows\'
    /// </summary>
    property WinDir: string read GetWinDir;
    /// <summary>
    /// i. e. 'C:\Windows\System32\'
    /// </summary>
    property SysDir: string read GetSysDir;
    /// <summary>
    /// i. e. 'C:\Program Files\'
    /// </summary>
    property ProgramFilesDir: string read GetProgramFilesDir;
  end;

implementation

uses
  System.IOUtils,
  Winapi.Windows,

  uTypes,
  uStrings,
  uFiles,
  uStartupEnvironment,
  uProjectInfo;

{ TWindowsSystemPaths }

function TWindowsSystemPaths.ForceGetAppDataDir: string;
begin
  Result := CompanyAppDataDir + GetProjectInfo(piInternalName) + PathDelim;
end;

function TWindowsSystemPaths.ForceGetCommonAppDataDir: string;
begin
	Result := GetEnvironmentVariable('APPDATA');
	if Result = '' then
    Result := WinDir + 'Application Data' + PathDelim;
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetCommonLocalAppDataDir: string;
begin
	Result := StartupEnvironment.FindValue('localappdata');
	if Result = '' then
	begin
		Result := UserProfileDir;
		CorrectDir(Result);
		if DirectoryExists(Result + 'AppData\Local\') then
			AppendStr(Result, 'AppData\Local\')
		else
			AppendStr(Result, 'Local Settings\Application Data\'); // Used for Windows XP w/o Service Pack
	end
  else
		CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetCompanyAppDataDir: string;
begin
  Result := CommonAppDataDir + GetCompanySuffix;
end;

function TWindowsSystemPaths.ForceGetCompanyLocalAppDataDir: string;
begin
	Result := CommonLocalAppDataDir + GetCompanySuffix;
end;

function TWindowsSystemPaths.ForceGetExeFileName: TFileName;
var
  CommandLine: string;
	CommandLinePair: TStringPair;
begin
  CommandLine := GetCommandLine;
	CommandLinePair := SplitCommandLine(CommandLine);
  Result := CommandLinePair.Name;
end;

function TWindowsSystemPaths.ForceGetExeParameters: string;
var
  CommandLine: string;
	CommandLinePair: TStringPair;
begin
  CommandLine := GetCommandLine;
	CommandLinePair := SplitCommandLine(CommandLine);
  Result := CommandLinePair.Value;
end;

function TWindowsSystemPaths.ForceGetHomeDir: string;
begin
  Result := TPath.GetHomePath;
end;

function TWindowsSystemPaths.ForceGetLocalAppDataDir: string;
begin
	Result := CommonLocalAppDataDir + GetProjectInfo(piInternalName) + PathDelim;
end;

function TWindowsSystemPaths.ForceGetModuleFileName: TFileName;
begin
  Result := GetModuleFileNameFunc(HInstance);
end;

function TWindowsSystemPaths.ForceGetProgramFilesDir: string;
begin
	Result := GetEnvironmentVariable('ProgramFiles');
	if Result = '' then
    Result := 'C' + DriveDelim + PathDelim + 'Program Files' + PathDelim;
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetStartDir: string;
begin
	GetDir(0, Result);
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetSysDir: string;
var
	NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := GetSystemDirectory(PChar(Result), MAX_PATH);
	SetLength(Result, NewLength);
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetUserProfileDir: string;
begin
	Result := StartupEnvironment.FindValue('UserProfile');
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetWinDir: string;
var
	NewLength: SG;
begin
	SetLength(Result, MAX_PATH);
	NewLength := GetWindowsDirectory(PChar(Result), MAX_PATH);
	SetLength(Result, NewLength);
	CorrectDir(Result);
end;

function TWindowsSystemPaths.ForceGetWorkDir: string;
var
  i: SG;
  ExecutableFileName: string;
begin
	Result := '';
  // Split ExeFileName to WorkDir and InternalName
  ExecutableFileName := ModuleFileName;
  for i := Length(ExecutableFileName) downto 0 do
  begin
    if i = 0 then
    begin
      Break;
    end;
    if (ExecutableFileName[i] = PathDelim) then
    begin
      Result := Copy(ModuleFileName, 1, i);
      Break;
    end;
  end;
	if Result = '' then
    Result := StartDir;
end;

function TWindowsSystemPaths.GetCompanySuffix: string;
begin
  Result := GetProjectInfo(piCompanyName);
  if Result <> '' then
    AppendStr(Result, PathDelim);
end;

function TWindowsSystemPaths.GetProgramFilesDir: string;
begin
  if FProgramFilesDir = '' then
    FProgramFilesDir := ForceGetProgramFilesDir;
  Assert(FProgramFilesDir <> '');
end;

function TWindowsSystemPaths.GetWinDir: string;
begin
  if FWinDir = '' then
    FWinDir := ForceGetWinDir;
  Assert(FWinDir <> '');
  Result := FWinDir;
end;

function TWindowsSystemPaths.GetSysDir: string;
begin
  if FSysDir = '' then
    FSysDir := ForceGetSysDir;
  Assert(FSysDir <> '');
  Result := FSysDir;
end;

end.

