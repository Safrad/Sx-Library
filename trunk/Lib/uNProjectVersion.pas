unit uNProjectVersion;

interface

uses uTypes, uParserMsg;

type
	TSubVersion = (svMajor, svMinor, svRelease, svBuild);
	TProjectVersion = class
	private
		FSubVersions: array[TSubVersion] of string;
	public
		constructor Create;

		function ReadFromString(const Version: string; const Messages: TParserMessages = nil): BG;
		function ToString: string;
		function ToStrictString: string;

		function Compare(const Version: TProjectVersion): SG;

		function GetAsString(SubVersion: TSubVersion): string;
		procedure SetSubVersion(SubVersion: TSubVersion; NewValue: string);

		property Build: string read FSubVersions[svBuild];
		property Release: string read FSubVersions[svRelease];
		property Minor: string read FSubVersions[svMinor];
		property Major: string read FSubVersions[svMajor];
	end;

const
	FirstGreater = 1;
	FirstLess = -1;
	BothSame = 0;

function CompareVersion(const Version1, Version2: string): SG; overload;

implementation

uses
	SysUtils,
	uStrings, uInputFormat, Math;
{
function IsNAVersion(const ProjectVersion: TProjectVersion): BG;
begin
	Result := (ProjectVersion.Major = 0) and
		(ProjectVersion.Minor = 0) and
		(ProjectVersion.Release = 0) and
		(ProjectVersion.Build = 0);
end;
}

(**
* @return 0 if Version1 = Version2, 1 if Version1 > Version2, -1 if Version1 < Version2
*)
function CompareVersion(const Version1, Version2: string): SG; overload;
var
	Version1a, Version2a: TProjectVersion;
begin
	Version1a := TProjectVersion.Create;
	Version1a.ReadFromString(Version1);
	Version2a := TProjectVersion.Create;
	Version2a.ReadFromString(Version2);
	Result := Version1a.Compare(Version2a);
end;

{ TProjectVersion }

function TProjectVersion.Compare(const Version: TProjectVersion): SG;
begin
	if Major = Version.Major then
	begin
		if Minor = Version.Minor then
		begin
			if Release = Version.Release then
			begin
				if Build = Version.Build then
				begin
					Result := BothSame;
				end
				else if Build > Version.Build then
					Result := FirstGreater
				else
					Result := FirstLess;
			end
			else if Release > Version.Release then
				Result := FirstGreater
			else
				Result := FirstLess;
		end
		else if Minor > Version.Minor then
			Result := FirstGreater
		else
			Result := FirstLess;
	end
	else if Major > Version.Major then
		Result := FirstGreater
	else
		Result := FirstLess;
end;

constructor TProjectVersion.Create;
begin
	inherited;
end;

function TProjectVersion.GetAsString(SubVersion: TSubVersion): string;
begin
	Result := FSubVersions[SubVersion];
end;

function TProjectVersion.ReadFromString(const Version: string;
	const Messages: TParserMessages): BG;
var
	InLineIndex: SG;
begin
	Result := True;
	Finalize(FSubVersions);
	if Version <> NAStr then
	begin
		InLineIndex := 1;
		try
			FSubVersions[svMajor] := IntToStr(StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages));
			FSubVersions[svMinor] := IntToStr(StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages));
			FSubVersions[svRelease] := IntToStr(StrToValI(ReadToChar(Version, InLineIndex, '.'), False, 0, 0, MaxInt, 1, Messages));
			FSubVersions[svBuild] := IntToStr(StrToValI(ReadToChar(Version, InLineIndex, #0), False, 0, 0, MaxInt, 1, Messages));
		except

		end;
	end;
end;

procedure TProjectVersion.SetSubVersion(SubVersion: TSubVersion;
	NewValue: string);
begin
	FSubVersions[SubVersion] := NewValue;
end;

function TProjectVersion.ToStrictString: string;
var
	Nums: array[TSubVersion] of SG;
	SubVersion: TSubVersion;
	s: string;
	p: SG;
begin
	for SubVersion := Low(SubVersion) to High(SubVersion) do
	begin
		s := FSubVersions[SubVersion];
		p := Pos(':', s);
		if p <> 0 then
			s := Copy(s, p + 1, MaxInt);
		Replace(s, ['M', 'S', 'P', 'external'], ['', '', '', '']);
		Nums[SubVersion] := StrToValI(s, False, 0, 0, MaxInt, 1, nil)
	end;
	Result := '';
	for SubVersion := Low(SubVersion) to High(SubVersion) do
		Result := Result + IntToStr(Nums[SubVersion]) + '.';
	Result := DelLastChar(Result);
end;

function TProjectVersion.ToString: string;
begin
	if True {(ProjectVersion.Major <> '') or (ProjectVersion.Minor <> '') or
			(ProjectVersion.Release <> '') or (ProjectVersion.Build <> '')} then
	begin
{		Result := '';
		if ProjectVersion.Build <> '' then
			Result := ProjectVersion.Build + Result;  TODO }
		Result := Major + '.' +
			Minor + '.' +
			Release + '.' +
			Build;
	end
	else
	begin
		Result := NAStr;
	end;
end;

end.
