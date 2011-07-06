unit uRegToCfg;

interface

uses
	uTypes,
	uStrings,
	uFiles,
	uFile,
	SysUtils,
	Registry,
	Windows;

procedure PathsToReg;
procedure RegistryToDcc32Cfg;
function GetDelphiRegPath(const DelphiVersion: SG): string;
function GetDelphiPath(const DelphiVersion: SG): string;

implementation

uses
	uMath, uMsg,
	Classes;

const
	DelphiToBDS = 7; // <= 7 Delphi
	BorlandToCodeGear = 12; // <= 12 : Borland
	CodeGearToEmbarcadero = 13; // = 13 CodeGear, > 13 Embarcadero

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
			Warning('Variable ' + EnvName + ' not found.');
		end;
	end;
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

procedure RegistryToDcc32Cfg;
var
	Reg: TRegistry;
	FileName: TFileName;
	s: string;
	InLineIndex: SG;
	SearchPaths: string;
	RegPath: string;
	Path: string;
	DelphiPath, DelphiPath2: string;
	DelphiVersion: Integer;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := 1 to (GetActualYear - 1995) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
      DelphiPath := GetDelphiPathOnly(Reg, RegPath);
			if DirectoryExists(DelphiPath) then
			begin
        FileName := DelphiPath + 'bin' + PathDelim + 'dcc32.cfg';
				Writeln('Creating ' + FileName);
				if Reg.OpenKey(RegPath + PathDelim + 'Library', False) then
				begin
					s := '-aWinTypes=Windows' + LineSep;
					s := s + '-r"' + DelphiPath + 'Lib"' + LineSep;
(*					if (DelphiVersion > 7) then
					begin
						s := s + '-u"' + DelphiPath + 'Lib\Indy10"' + FileSep;
						s := s + '-u"C:\My Documents\Delphi\ExtLib\GDIPlus\Lib"' + FileSep;
						s := s + '-u"C:\My Documents\Delphi\ExtLib\GraphicEx"' + FileSep;
					end; *)

					DelphiPath2 := DelLastChar(DelphiPath);
					SearchPaths := Reg.ReadString('Search Path');
					Replace(SearchPaths, '$(DELPHI)', DelphiPath2);
					Replace(SearchPaths, '$(BDS)', DelphiPath2);
					Replace(SearchPaths, '$(BDSLIB)', DelphiPath2 + '\Lib');
					Replace(SearchPaths, '$(BDSBIN)', DelphiPath2 + '\Bin');
					Replace(SearchPaths, '$(BDSINCLUDE)', DelphiPath2 + '\Include');
					Replace(SearchPaths, '$(PLATFORM)', 'Win32');

					ReplaceEnv(SearchPaths);

					InLineIndex := 1;
					while InLineIndex < Length(SearchPaths) do
					begin
						Path := ReadToChar(SearchPaths, InLineIndex, ';');
//						if (DelphiVersion <= 7) or (Pos('Delphi7', Path) = 0) then
						if Path <> '' then
							s := s + '-u"' + Path + '"' + LineSep;
					end;

					WriteStringToFile(FileName, s, False, fcAnsi);

					Reg.CloseKey;
				end;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

procedure CorrectPaths(const Strings: TStrings);
var
	i: SG;
begin
	for i := 0 to Strings.Count - 1 do
	begin
		if LastChar(Strings[i]) = '\' then
			Strings[i] := DelLastChar(Strings[i]);
	end;
end;

procedure LibToReg;
const
	SearchPathName = 'Search Path';
var
	Lines: TArrayOfString;
	LineCount: SG;
	i, j: Integer;
	DelphiVersion: SG;
	Reg: TRegistry;
	RegPath: string;
	Paths: string;
	Path: string;
	RegPaths: TStrings;
	InLineIndex: SG;
	Found: BG;
begin
	RegPaths := TStringList.Create;
	LineCount := 0;

	ReadStringsFromFile(StartDir + 'lib.txt', Lines, LineCount);
	for i := 0 to LineCount - 1 do
	begin
		if LastChar(Lines[i]) = '\' then
			Lines[i] := DelLastChar(Lines[i]);
	end;

	Reg := TRegistry.Create(KEY_ALL_ACCESS);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := 1 to (GetActualYear - 1995) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
				if Reg.OpenKey(RegPath + PathDelim + 'Library', False) then
				begin
					Paths := Reg.ReadString(SearchPathName);
					InLineIndex := 1;
					RegPaths.Clear;
					while InLineIndex <= Length(Paths) do
					begin
						Path := ReadToChar(Paths, InLineIndex, ';');
						if Path <> '' then
							RegPaths.Add(Path);
					end;
					CorrectPaths(RegPaths);

					for i := 0 to LineCount - 1 do
					begin
						Path := Lines[i];
						if Path ='' then
							Continue;

						Found := False;
						for j := 0 to RegPaths.Count - 1 do
						begin
							if UpperCase(RegPaths[j]) = UpperCase(Path) then
							begin
								Found:= True;
								Break;
							end;
						end;
						if not Found then
						begin
							RegPaths.Add(Path);
						end;
					end;
{					RegPaths.Delimiter := ';';
					RegPaths.QuoteChar := #0;
					Paths := RegPaths.DelimitedText;}
					Paths := '';
					for i := 0 to RegPaths.Count - 1 do
					begin
						Paths := Paths + RegPaths[i] + ';';
					end;
					Paths := DelLastChar(Paths);

					Reg.WriteString(SearchPathName, Paths);

					Reg.CloseKey;
				end;
		end;
	finally
		Reg.Free;
		RegPaths.Free;
	end;

end;

procedure BplToReg;
var
	Lines: TArrayOfString;
	LineCount: SG;
	i, j: Integer;
	DelphiVersion: SG;
	Reg: TRegistry;
	Strings: TStrings;
	RegPath: string;
	Path: string;
	FileName: string;
begin
	Strings := TStringList.Create;
	LineCount := 0;

	ReadStringsFromFile(StartDir + 'bpl.txt', Lines, LineCount);

	Reg := TRegistry.Create(KEY_ALL_ACCESS);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := 1 to (GetActualYear - 1995) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
				if Reg.OpenKey(RegPath + PathDelim + 'Known Packages', False) then
				begin
					Reg.GetValueNames(Strings);

					for i := 0 to LineCount - 1 do
					begin
						Path := Lines[i];
						FileName := ExtractFileName(Path);
							for j := 0 to Strings.Count - 1 do
							if ExtractFileName(Strings[j]) = FileName then
							begin
								Reg.DeleteValue(Strings[j]);
								Break;
							end;
						Reg.WriteString(Path, IntToStr(i));
					end;

					Reg.CloseKey;
				end;
		end;
	finally
		Reg.Free;
	end;

end;

procedure PathsToReg;
begin
	BplToReg;
	LibToReg;
end;

end.
