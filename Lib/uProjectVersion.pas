//* File:     Lib\uProjectVersion.pas
//* Created:  2009-05-11
//* Modified: 2009-05-13
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uProjectVersion;

interface

uses uTypes, uParserMsg;

type
	TProjectVersion = record
		MajorVersion, MinorVersion, ReleaseVersion, BuildVersion: SG;
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
	Result := (ProjectVersion.MajorVersion = 0) and
		(ProjectVersion.MinorVersion = 0) and
		(ProjectVersion.ReleaseVersion = 0) and
		(ProjectVersion.BuildVersion = 0);
end;

function CreateVersion(const Version: string; const Messages: TParserMessages = nil): TProjectVersion;
var
	InLineIndex: SG;
begin
	Result.MajorVersion := 0;
	Result.MinorVersion := 0;
	Result.ReleaseVersion := 0;
	Result.BuildVersion := 0;
	if Version <> 'N/A' then
	begin
		InLineIndex := 1;
		try
			Result.MajorVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.MinorVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.ReleaseVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
			Result.BuildVersion := StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages);
		except

		end;
	end;
end;

function VersionToStr(const ProjectVersion: TProjectVersion): string;
begin
	if (ProjectVersion.MajorVersion <> 0) or (ProjectVersion.MinorVersion <> 0) or
			(ProjectVersion.ReleaseVersion <> 0) or (ProjectVersion.BuildVersion <> 0) then
	begin
		Result :=
			IntToStr(ProjectVersion.MajorVersion) + '.' +
			IntToStr(ProjectVersion.MinorVersion) + '.' +
			IntToStr(ProjectVersion.ReleaseVersion) + '.' +
			IntToStr(ProjectVersion.BuildVersion);
	end
	else
	begin
		Result := 'N/A';
	end;
end;
(**
* @return 0 if Version1 = Version2, 1 if Version1 > Version2, -1 if Version1 < Version2
*)
function CompareVersion(const Version1, Version2: TProjectVersion): SG; overload;
begin
	if Version1.MajorVersion = Version2.MajorVersion then
	begin
		if Version1.MinorVersion = Version2.MinorVersion then
		begin
			if Version1.ReleaseVersion = Version2.ReleaseVersion then
			begin
				if Version1.BuildVersion = Version2.BuildVersion then
				begin
					Result := BothSame;
				end
				else if Version1.BuildVersion > Version2.BuildVersion then
					Result := FirstGreater
				else
					Result := FirstLess;
			end
			else if Version1.ReleaseVersion > Version2.ReleaseVersion then
				Result := FirstGreater
			else
				Result := FirstLess;
		end
		else if Version1.MinorVersion > Version2.MinorVersion then
			Result := FirstGreater
		else
			Result := FirstLess;
	end
	else if Version1.MajorVersion > Version2.MajorVersion then
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
