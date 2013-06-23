unit uDelphi;

interface

uses
  uTypes, Registry;

type
  TDelphiVersion = (
    dvPascal1, dvPascal2, dvPascal3, dvPascal4, dvPascal5, {dvPascal55,} dvPascal6, dvPascal7,
    dvDelphi1, dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5, dvDelphi6, dvDelphi7,
    dvDelphi8, dvDelphi2005, dvDelphi2006, dvDelphi2007,
    dvDelphi2009, dvDelphi2010, dvDelphiXE, dvDelphiXE2 {,..});

const
  ReleaseYear: array[TDelphiVersion] of string = (
    '1983-11-20', '1984-04-17', '1986-09-17', '1987-11-20', '1988-08-24', {'1989-05-02',} '1990-10-23', '1992-10-27',
    '1995-02-14', '1996-02-10', '1997-08-05', '1998-06-17', '1999-08-10', '2001-05-21', '2002-08-09',
    '2003-12-17', '2004-10-22', '2005-11-11', '2007-03-19',
    '2008-08-29', '2009-08-26', '2010-11-03', '2011-08-31');
  LastWin16Delphi = dvDelphi2;
  FirstWin32Delphi = dvDelphi3;
  FirstUnicodeDelphi = dvDelphi2009;
  FirstWin64Delphi = dvDelphiXE2;

type
  TSystemPlatform = (spWin16, spWin32, spWin64);
const
  SystemPlatformStr: array[TSystemPlatform] of string = ('16', '32', '64');
  SystemPlatformXStr: array[TSystemPlatform] of string = ('16', 'x86', 'x64');

type
  TCompiler = record
    DelphiVersion: TDelphiVersion;
    SystemPlatform: TSystemPlatform;
  end;
  TCompilers = array of TCompiler;

// Delphi 7, BDS 7, BDS 8
function GetDelphiRegistryName(const ADelphiVersion: TDelphiVersion): string;

// 7, 14, 15
function GetDelphiMajorVersion(const ADelphiVersion: TDelphiVersion): SG;

// 15, 21, 22
function GetDelphiCompilerVersion(const ADelphiVersion: TDelphiVersion): SG;

// 7, 2010, XE
function GetDelphiShortName(const ADelphiVersion: TDelphiVersion): string;

// XE (15, dcc 22)
function GetDelphiFullName(const ADelphiVersion: TDelphiVersion): string;

function GetCompilerFullName(const ACompiler: TCompiler): string;

function GetCompilers(const ADelphiVersion: TDelphiVersion): TCompilers;

function GetDelphiRegPath(const ADelphiVersion: TDelphiVersion): string;
function GetDelphiPathOnly(const Reg: TRegistry; const RegPath: string): string;
function GetDelphiPath(const ADelphiVersion: TDelphiVersion): string;
function DelphiLibSuffix(const Compiler: TCompiler): string;
function GetDCCFileName(const Compiler: TCompiler): string;
function ReplaceDelphiVariables(SearchPaths: string; const ADelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform): string;
function GetDelphiVersionCount: SG;
function GetAvailableDelphiVersions: TArrayOfSG;
function GetAvailableCompilers: TCompilers;

function GetDelphiVersion(const AName: string): TDelphiVersion;
function GetDelphiCompiler(const AName: string): TCompiler;

implementation

uses
  Windows, SysUtils,
  uFiles,
  uMath, uStrings, uLog;

const
	FirstBDS = dvDelphi8;
  BDSStartFrom = 2;
	FirstCodeGear = dvDelphi2009;
	FirstEmbarcadero = dvDelphi2010;

function GetDelphiRegPath(const ADelphiVersion: TDelphiVersion): string;
var
	RegPath: string;
begin
  if ADelphiVersion < dvDelphi1 then
  begin
    Result := '';
    Exit;
  end;

	RegPath := 'Software' + PathDelim;
	if ADelphiVersion < FirstCodeGear then
		RegPath := RegPath + 'Borland\'
	else if ADelphiVersion < FirstEmbarcadero then
		RegPath := RegPath + 'CodeGear\'
	else
		RegPath := RegPath + 'Embarcadero\';

	if ADelphiVersion < FirstBDS then
		RegPath := RegPath + 'Delphi\' + IntToStr(SG(ADelphiVersion) - SG(dvDelphi1) + 1)
	else
		RegPath := RegPath + 'BDS\' + IntToStr(SG(ADelphiVersion) - SG(FirstBDS) + BDSStartFrom);
	RegPath := RegPath + '.0' + PathDelim;
	Result := RegPath;
end;

function GetDelphiRegistryName(const ADelphiVersion: TDelphiVersion): string;
begin
  if ADelphiVersion < dvDelphi1 then
    Result := ''
	else if ADelphiVersion < FirstBDS then
		Result := 'Delphi ' + IntToStr(SG(ADelphiVersion) - SG(dvDelphi1) + 1)
	else
		Result := 'BDS ' + IntToStr(SG(ADelphiVersion) - SG(FirstBDS) + BDSStartFrom);
end;

function GetPascalMajorVersion(const ADelphiVersion: TDelphiVersion): SG;
begin
  Result := SG(ADelphiVersion) - SG(dvPascal1) + 1;
end;

function GetDelphiMajorVersion(const ADelphiVersion: TDelphiVersion): SG;
begin
  Result := SG(ADelphiVersion) - SG(dvDelphi1) + 1;
  if ADelphiVersion >= dvDelphi2010 then
    Inc(Result);
end;

function GetDelphiCompilerVersion(const ADelphiVersion: TDelphiVersion): SG;
begin
  Result := SG(ADelphiVersion) + 1;
  if ADelphiVersion >= dvDelphi4 then
    Inc(Result);
end;

function GetDelphiYear(const ADelphiVersion: TDelphiVersion): SG;
begin
  case ADelphiVersion of
  dvDelphi2005: Result := 2005;
  dvDelphi2006: Result := 2006;
  dvDelphi2007: Result := 2007;
  dvDelphi2009: Result := 2009;
  dvDelphi2010: Result := 2010;
  else Result := 0;
  end;
end;

function GetDelphiShortName(const ADelphiVersion: TDelphiVersion): string;
begin
  if ADelphiVersion < dvDelphi1 then
    Result := 'P' + IntToStr(GetPascalMajorVersion(ADelphiVersion))
  else if ADelphiVersion <= dvDelphi8 then
    Result := IntToStr(GetDelphiMajorVersion(ADelphiVersion))
  else if ADelphiVersion < dvDelphiXE then
    Result := IntToStr(GetDelphiYear(ADelphiVersion))
  else if ADelphiVersion >= dvDelphiXE then
  begin
    Result := 'XE';
    if ADelphiVersion > dvDelphiXE then
    	Result := Result + IntToStr(SG(ADelphiVersion) - SG(dvDelphiXE) + 1);
  end;
  // Add newer versions here
end;

function GetDelphiFullName(const ADelphiVersion: TDelphiVersion): string;
begin
  Result := GetDelphiShortName(ADelphiVersion) + ' (' + IntToStr(GetDelphiMajorVersion(ADelphiVersion)) +', dcc' + IntToStr(GetDelphiCompilerVersion(ADelphiVersion)) + ')';
end;

function GetCompilerFullName(const ACompiler: TCompiler): string;
begin
  Result := GetDelphiFullName(ACompiler.DelphiVersion);
  if ACompiler.SystemPlatform <> spWin32 then
  begin
    Result := Result + CharSpace + SystemPlatformXStr[ACompiler.SystemPlatform];
  end;
end;

function GetCompilers(const ADelphiVersion: TDelphiVersion): TCompilers;
var
  c: SG;
begin
  c := 0;
  if ADelphiVersion <= LastWin16Delphi then
  begin
    SetLength(Result, c + 1);
    Result[c].DelphiVersion := ADelphiVersion;
    Result[c].SystemPlatform := spWin16;
    Inc(c);
  end;
  if ADelphiVersion >= FirstWin32Delphi then
  begin
    SetLength(Result, c + 1);
    Result[c].DelphiVersion := ADelphiVersion;
    Result[c].SystemPlatform := spWin32;
    Inc(c);
  end;
  if ADelphiVersion >= FirstWin64Delphi then
  begin
    SetLength(Result, c + 1);
    Result[c].DelphiVersion := ADelphiVersion;
    Result[c].SystemPlatform := spWin64;
  end;
end;

function GetDelphiPathOnly(const Reg: TRegistry; const RegPath: string): string;
begin
  if Reg.OpenKeyReadOnly(RegPath) then
  begin
    {$ifdef Console}
    Writeln('Key ' + RegPath + ' found.');
    {$endif}
    Result := CorrectDirF(Reg.ReadString('RootDir'));
    Reg.CloseKey;
  end
  else
  begin
    {$ifdef Console}
    Writeln('Key ' + RegPath + ' not found.');
    {$endif}
    Result := '';
  end;
end;

function GetDelphiPath(const ADelphiVersion: TDelphiVersion): string;
var
	Reg: TRegistry;
  RegPath: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		RegPath := GetDelphiRegPath(ADelphiVersion);
    if not Reg.KeyExists(RegPath) then
  		Reg.RootKey := HKEY_LOCAL_MACHINE;
    Result := GetDelphiPathOnly(Reg, RegPath);
	finally
		Reg.Free;
	end;
end;

function DelphiLibSuffix(const Compiler: TCompiler): string;
begin
  Result := 'Lib';
  if Compiler.DelphiVersion > dvDelphi7 then
    Result := Result + 'Release' + PathDelim + 'win' + SystemPlatformStr[Compiler.SystemPlatform];
end;

function GetDCCFileName(const Compiler: TCompiler): string;
begin
  Result := GetDelphiPath(Compiler.DelphiVersion) + 'Bin\dcc' + SystemPlatformStr[Compiler.SystemPlatform] + '.exe';
end;

procedure ReplaceEnv(var Paths: string);
const
	Prefix = '$(';
	Suffix = ')';
var
	i, p, p2: SG;
	EnvName, EnvVar: string;
begin
	i := 1;
	while i < Length(Paths) do
	begin
		p := PosEx(Prefix, Paths, i);
		if p = 0 then Exit;
		p2 := PosEx(Suffix, Paths, p);
		EnvName := Copy(Paths, p + Length(Prefix), p2 - p - 2);

		EnvVar := GetEnvironmentVariable(EnvName);
		if EnvVar <> '' then
		begin
			Delete(Paths, p, p2 - p + Length(Suffix));
			Insert(EnvVar, Paths, p);
			i := p + Length(EnvVar);
		end
		else
		begin
			i := p + Length(EnvName);
			if LogWarning then LogAdd('Environment Variable ' + EnvName + ' not found.');
		end;
	end;
end;

function ReplaceDelphiVariables(SearchPaths: string; const ADelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform): string;
var
  DelphiPath: string;
  DelphiPath2: string;
begin
  DelphiPath := GetDelphiPath(ADelphiVersion);
	DelphiPath2 := DelLastChar(DelphiPath);
  // Tools|Options|Environment Options|Environmental Variables
  Replace(SearchPaths, '$(DELPHI)', DelphiPath2);
  Replace(SearchPaths, '$(BDS)', DelphiPath2);
  Replace(SearchPaths, '$(BDSDIR)', DelphiPath2);
  Replace(SearchPaths, '$(BDSLIB)', DelphiPath2 + '\Lib');
  Replace(SearchPaths, '$(BDSBIN)', DelphiPath2 + '\Bin');
  Replace(SearchPaths, '$(BDSCOMMON)', DelphiPath2 + '\Bin');
  Replace(SearchPaths, '$(BDSINCLUDE)', DelphiPath2 + '\Include');

  Replace(SearchPaths, '$(PLATFORM)', 'Win' + SystemPlatformStr[SystemPlatform]);
  Replace(SearchPaths, '$(Platform)', 'Win' + SystemPlatformStr[SystemPlatform]);
  Replace(SearchPaths, '$(CONFIG)', 'Release');
  Replace(SearchPaths, '$(Config)', 'Release');

  Replace(SearchPaths, '$(DCC_Define)', '');
  Replace(SearchPaths, '$(DCC_UnitAlias)', '');

  Replace(SearchPaths, '$(BDSUSERDIR)', TempDir);
  Replace(SearchPaths, '$(BDSCOMMONDIR)', TempDir);
  ReplaceEnv(SearchPaths);
  Result := SearchPaths;
end;

function GetDelphiVersionCount: SG;
begin
  Result := CurrentYear - 1995 + 7;
end;

function GetAvailableDelphiVersions: TArrayOfSG;
var
  DelphiVersion: TDelphiVersion;
  Count: SG;
  DelphiPath: string;
begin
  Count := 0;
  for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
	begin
    DelphiPath := GetDelphiPath(DelphiVersion);
    if DirectoryExists(DelphiPath) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := SG(DelphiVersion);
      Inc(Count);
    end;
  end;
end;

function GetAvailableCompilers: TCompilers;
var
  DelphiVersion: TDelphiVersion;
  SystemPlatform: TSystemPlatform;
  Count: SG;
  DelphiPath: string;
  DCCFileName: string;
  Compiler: TCompiler;
begin
  Count := 0;
  for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
	begin
    DelphiPath := GetDelphiPath(DelphiVersion);
    if DirectoryExists(DelphiPath) then
    begin
      for SystemPlatform := Low(SystemPlatform) to High(SystemPlatform) do
      begin
        Compiler.DelphiVersion := DelphiVersion;
        Compiler.SystemPlatform := SystemPlatform;
        DCCFileName := GetDCCFileName(Compiler);
        if FileExists(DCCFileName) then
        begin
          SetLength(Result, Count + 1);
          Result[Count] := Compiler;
          Inc(Count);
        end;
      end;
    end;
  end;
end;

function GetDelphiVersion(const AName: string): TDelphiVersion;
var
  DelphiVersion: TDelphiVersion;
begin
  Result := dvPascal1;
  for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
	begin
    if CompareText(GetDelphiShortName(DelphiVersion), AName) = 0 then
    begin
      Result := DelphiVersion;
      Break;
    end;
  end;
end;

function GetDelphiCompiler(const AName: string): TCompiler;
var
  DelphiVersion: TDelphiVersion;
  sp: TSystemPlatform;
begin
  Result.DelphiVersion := dvDelphi1;
  Result.SystemPlatform := spWin32;
  for sp := Low(sp) to High(sp) do
  begin
    if Pos(SystemPlatformXStr[sp], AName) <> 0 then
      Result.SystemPlatform := sp;
  end;

  for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
	begin
    if Pos(GetDelphiShortName(DelphiVersion), AName) <> 0 then
    begin
      Result.DelphiVersion := DelphiVersion;
    end;
  end;
end;

end.