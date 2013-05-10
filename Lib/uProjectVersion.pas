unit uProjectVersion;

interface

uses uTypes, uParserMsg;

type
	TVersionSuffix = (vsFinal, vsAlpha, vsBeta);
	TSubVersion = (svBuild, svRelease, svMinor, svMajor);
	TProjectVersion = packed record
		VersionSuffix: TVersionSuffix;
		case Integer of
		0: (Build, Release, Minor, Major: U2);
		1: (SubVersions: array[TSubVersion] of U2);
		2: (All: S8);
	end;

const
	FirstGreater = 1;
	FirstLess = -1;
	BothSame = 0;

function IsNAVersion(const ProjectVersion: TProjectVersion): BG;
function CreateVersion(const Version: string; const Messages: TParserMessages = nil): TProjectVersion;
function VersionToStr(const ProjectVersion: TProjectVersion): string;
function MajorAndMinorVersionToStr(const ProjectVersion: TProjectVersion): string;
function CompareVersion(const Version1, Version2: TProjectVersion): SG; overload;
function CompareVersion(const Version1, Version2: string): SG; overload;

implementation

uses
	SysUtils,
	uStrings, uInputFormat, Math;

function IsNAVersion(const ProjectVersion: TProjectVersion): BG;
begin
	Result := (ProjectVersion.Major = 0) and
		(ProjectVersion.Minor = 0) and
		(ProjectVersion.Release = 0) and
		(ProjectVersion.Build = 0);
end;

function CreateVersion(const Version: string; const Messages: TParserMessages = nil): TProjectVersion;
var
	InLineIndex: SG;
begin
	Result.VersionSuffix := vsFinal;
	Result.Major := 0;
	Result.Minor := 0;
	Result.Release := 0;
	Result.Build := 0;
	Result.VersionSuffix := vsFinal;
	if Version <> NAStr then
	begin
		InLineIndex := 1;
		try
			Result.Major := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Minor := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Release := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Build := StrToValI(ReadToChar(Version, InLineIndex, '+'), False, 0, 0, MaxInt, 1, Messages);
			if Copy(Version, InLineIndex, MaxInt) = '+' then
				Result.VersionSuffix := vsBeta;
		except

		end;
	end;
end;

function VersionToStr(const ProjectVersion: TProjectVersion): string;
begin
	if (ProjectVersion.Major <> 0) or (ProjectVersion.Minor <> 0) or
			(ProjectVersion.Release <> 0) or (ProjectVersion.Build <> 0) then
	begin
		Result :=
			IntToStr(ProjectVersion.Major) + '.' +
			IntToStr(ProjectVersion.Minor) + '.' +
			IntToStr(ProjectVersion.Release) + '.' +
			IntToStr(ProjectVersion.Build);
	end
	else
	begin
		Result := NAStr;
	end;

	if ProjectVersion.VersionSuffix = vsBeta then
		Result := Result + CharSpace + 'beta'
	else if ProjectVersion.VersionSuffix = vsAlpha then
		Result := Result + CharSpace + 'alpha';
end;

function MajorAndMinorVersionToStr(const ProjectVersion: TProjectVersion): string;
begin
	if (ProjectVersion.Major <> 0) or (ProjectVersion.Minor <> 0) or
			(ProjectVersion.Release <> 0) or (ProjectVersion.Build <> 0) then
	begin
		Result :=
			IntToStr(ProjectVersion.Major);
    if ProjectVersion.Minor <> 0 then
  		Result := Result +
  			'.' + IntToStr(ProjectVersion.Minor);
	end
	else
	begin
		Result := NAStr;
	end;
end;


(**
* @return 0 if Version1 = Version2, 1 if Version1 > Version2, -1 if Version1 < Version2
*)
function CompareVersion(const Version1, Version2: TProjectVersion): SG; overload;
begin
	if Version1.Major = Version2.Major then
	begin
		if Version1.Minor = Version2.Minor then
		begin
			if Version1.Release = Version2.Release then
			begin
				if Version1.Build = Version2.Build then
				begin
					Result := BothSame;
				end
				else if Version1.Build > Version2.Build then
					Result := FirstGreater
				else
					Result := FirstLess;
			end
			else if Version1.Release > Version2.Release then
				Result := FirstGreater
			else
				Result := FirstLess;
		end
		else if Version1.Minor > Version2.Minor then
			Result := FirstGreater
		else
			Result := FirstLess;
	end
	else if Version1.Major > Version2.Major then
		Result := FirstGreater
	else
		Result := FirstLess;
end;

function CompareVersion(const Version1, Version2: string): SG; overload;
var
	Version1a, Version2a: TProjectVersion;
begin
	Version1a := CreateVersion(Version1);
	Version2a := CreateVersion(Version2);
	Result := CompareVersion(Version1a, Version2a);
end;


end.
