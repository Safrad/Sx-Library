unit uDelphi;

interface

uses
  uTypes, Registry;

const
  FirstUnicodeDelphi = 12; // Delphi 2009

function GetDelphiName(const AVersion: SG): string;
function GetDelphiRegPath(const DelphiVersion: SG): string;
function GetDelphiPathOnly(const Reg: TRegistry; const RegPath: string): string;
function GetDelphiPath(const DelphiVersion: SG): string;
function ReplaceDelphiVariables(SearchPaths: string; const DelphiVersion: SG): string;
function GetAvailableDelphiVersions: TArrayOfSG;

implementation

uses
  Windows, SysUtils,
  uMath, uStrings, uLog;

const
	DelphiToBDS = 7; // <= 7 Delphi, >= 8 BDS
	BorlandToCodeGear = 11; // <= 11 : Borland, >= 12 CodeGear
  UnusedVersion = 13;
	CodeGearToEmbarcadero = 12; // <= 12 CodeGear, >= 14 Embarcadero

(*	if DelphiVersion <= Break1 then
		Result := ProgramFilesDir + 'Borland\Delphi' + IntToStr(DelphiVersion) + '\'
	else if DelphiVersion <= Break2 then
		Result := ProgramFilesDir + 'Borland\BDS\' + IntToStr(DelphiVersion - Break1) + '.0\'
	else
		Result := ProgramFilesDir + 'Embarcadero\RAD Studio\' + IntToStr(DelphiVersion - Break1) + '.0\'; *)


function GetDelphiRegPath(const DelphiVersion: SG): string;
var
	RegPath: string;
begin
	RegPath := 'Software' + PathDelim;
	if DelphiVersion <= BorlandToCodeGear then
		RegPath := RegPath + 'Borland\'
	else if DelphiVersion <= CodeGearToEmbarcadero then
		RegPath := RegPath + 'CodeGear\'
	else
		RegPath := RegPath + 'Embarcadero\';

	if DelphiVersion <= DelphiToBDS then
		RegPath := RegPath + 'Delphi\' + IntToStr(DelphiVersion)
	else
		RegPath := RegPath + 'BDS\' + IntToStr(DelphiVersion - DelphiToBDS);
	RegPath := RegPath + '.0' + PathDelim;
	Result := RegPath;
end;


function GetDelphiName(const AVersion: SG): string;
begin
	if AVersion <= DelphiToBDS then
		Result := 'Delphi ' + IntToStr(AVersion)
	else
		Result := 'BDS ' + IntToStr(AVersion - DelphiToBDS);
  Result := Result + ' (' + IntToStr(AVersion + 8) + ')';
end;

function GetDelphiPathOnly(const Reg: TRegistry; const RegPath: string): string;
begin
  if Reg.OpenKeyReadOnly(RegPath) then
  begin
    Writeln('Key ' + RegPath + ' found.');
    Result := CorrectDirF(Reg.ReadString('RootDir'));
    Reg.CloseKey;
  end
  else
  begin
    Writeln('Key ' + RegPath + ' not found.');
    Result := '';
  end;
end;

function GetDelphiPath(const DelphiVersion: SG): string;
var
	Reg: TRegistry;
  RegPath: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		RegPath := GetDelphiRegPath(DelphiVersion);
    Result := GetDelphiPathOnly(Reg, RegPath);
	finally
		Reg.Free;
	end;
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
			MainLogAdd('Environment Variable ' + EnvName + ' not found.', mlWarning);
		end;
	end;
end;

function ReplaceDelphiVariables(SearchPaths: string; const DelphiVersion: SG): string;
var
  DelphiPath: string;
  DelphiPath2: string;
begin
  DelphiPath := GetDelphiPath(DelphiVersion);
	DelphiPath2 := DelLastChar(DelphiPath);
  Replace(SearchPaths, '$(DELPHI)', DelphiPath2);
  Replace(SearchPaths, '$(BDS)', DelphiPath2);
  Replace(SearchPaths, '$(BDSLIB)', DelphiPath2 + '\Lib');
  Replace(SearchPaths, '$(BDSBIN)', DelphiPath2 + '\Bin');
  Replace(SearchPaths, '$(BDSCOMMON)', DelphiPath2 + '\Bin');
  Replace(SearchPaths, '$(BDSINCLUDE)', DelphiPath2 + '\Include');
  Replace(SearchPaths, '$(PLATFORM)', 'Win32');
  Replace(SearchPaths, '$(Platform)', 'Win32');
  ReplaceEnv(SearchPaths);
  Result := SearchPaths;
end;

function GetAvailableDelphiVersions: TArrayOfSG;
var
  DelphiVersion: SG;
  Count: SG;
  DelphiPath: string;
begin
  Count := 0;
  for DelphiVersion := 1 to (GetActualYear - 1995) do
	begin
    DelphiPath := GetDelphiPath(DelphiVersion);
    if DirectoryExists(DelphiPath) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := DelphiVersion;
      Inc(Count);
    end;
  end;
end;

end.
