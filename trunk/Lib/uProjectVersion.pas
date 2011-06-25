// * File:     Lib\uProjectVersion.pas
// * Created:  2009-05-11
// * Modified: 2009-09-21
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uProjectVersion;

interface

uses uTypes, uParserMsg;

type
	TSubVersion = (svMajor, svMinor, svRelease, svBuild);
	TProjectVersion = record
		case Integer of
		0: (Major, Minor, Release, Build: SG);
		1: (SubVersions: array[TSubVersion] of SG);
	end;

const
	FirstGreater = 1;
	FirstLess = -1;
	BothSame = 0;

function IsNAVersion(const ProjectVersion: TProjectVersion): BG;
function CreateVersion(const Version: string; const Messages: TParserMessages = nil): TProjectVersion;
function VersionToStr(const ProjectVersion: TProjectVersion): string;
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
	Result.Major := 0;
	Result.Minor := 0;
	Result.Release := 0;
	Result.Build := 0;
	if Version <> NAStr then
	begin
		InLineIndex := 1;
		try
			Result.Major := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Minor := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Release := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.Build := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
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
