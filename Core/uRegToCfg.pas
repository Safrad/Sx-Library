unit uRegToCfg;

interface

uses
	uTypes,
  uDelphi,
	uStrings,
	uFiles,
	uFile,
	SysUtils,
	Registry,
	Windows;

procedure RegistryToDccCfg; overload;
procedure RegistryToDccCfg(const Compiler: TCompiler); overload;

const
  RemovePrefix = '-';

procedure LibToReg(const Lines: TArrayOfString);
procedure BplToReg(const Lines: TArrayOfString);

implementation

uses
  Classes,

	uMath, uMsg,
  uLog,
  uFileCharset;

function GetDelphiLibraryPath(const RegPath: string; const DelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform): string;
begin
  if DelphiVersion <= dvDelphiXE then
    Result := RegPath + 'Library'
  else
  begin
    Result := RegPath + 'Library\' + SystemPlatformRegStr[SystemPlatform];
  end;
end;

function CommonCfgText(const DelphiVersion: TDelphiVersion): string;
var
  FileName: TFileName;
  i: TDelphiVersion;
begin
  Result := '';
  FileName := DataDir + 'default.cfg';
  if FileExistsEx(FileName) then
	  Result := Result + ReadStringFromFile(FileName) + FileSep;
	for i := dvDelphi1 to DelphiVersion do
  begin
	  FileName := DataDir + 'default-D' + GetDelphiShortName(i) + '.cfg';
	  if FileExistsEx(FileName) then
		  Result := Result + ReadStringFromFile(FileName) + FileSep;
  end;
end;

procedure WriteDcc(const Data: string; const DelphiPath: string; const Bits: string);
var
	FileName: TFileName;
begin
  FileName := DelphiPath + 'bin' + PathDelim + 'dcc' + Bits + '.cfg';
  WriteStringToFile(FileName, Data, False, fcAnsi);
end;

procedure RegistryToDccCfg;
var
	Reg: TRegistry;
	s: string;
	InLineIndex: SG;
	SearchPaths: string;
	RegPath: string;
	Path: string;
	DelphiPath: string;
	DelphiVersion: TDelphiVersion;
  Compilers: TCompilers;
  Compiler: TCompiler;
  i: SG;
begin
  Compilers := nil;
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
      DelphiPath := GetDelphiPathOnly(Reg, RegPath);
			if DirectoryExists(DelphiPath) then
			begin
        Compilers := GetCompilers(DelphiVersion);
        for i := 0 to Length(Compilers) - 1 do
        begin
          Compiler := Compilers[i];
          if Reg.OpenKey(GetDelphiLibraryPath(RegPath, Compiler.DelphiVersion, Compiler.SystemPlatform), False) then
          begin
            s := CommonCfgText(DelphiVersion);
            s := s + '-r"' + DelphiPath + DelphiLibSuffix(Compiler) + '"' + LineSep;
            SearchPaths := Reg.ReadString('Search Path');
            SearchPaths := ReplaceDelphiVariables(SearchPaths, Compiler.DelphiVersion, Compiler.SystemPlatform);

            InLineIndex := 1;
            while InLineIndex < Length(SearchPaths) do
            begin
              Path := ReadToChar(SearchPaths, InLineIndex, ';');
              if Path <> '' then
                s := s + '-u"' + Path + '"' + LineSep;
            end;

            WriteDcc(s, DelphiPath, SystemPlatformDccStr[Compiler.SystemPlatform]);

            Reg.CloseKey;
          end;
        end;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

procedure RegistryToDccCfg(const Compiler: TCompiler);
var
	Reg: TRegistry;
	s: string;
	InLineIndex: SG;
	SearchPaths: string;
	RegPath: string;
	Path: string;
	DelphiPath: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
    RegPath := GetDelphiRegPath(Compiler.DelphiVersion);
    DelphiPath := GetDelphiPathOnly(Reg, RegPath);
    if DirectoryExists(DelphiPath) then
    begin
        if Reg.OpenKey(GetDelphiLibraryPath(RegPath, Compiler.DelphiVersion, Compiler.SystemPlatform), False) then
        begin
          s := CommonCfgText(Compiler.DelphiVersion);
          s := s + '-r"' + DelphiPath + DelphiLibSuffix(Compiler) + '"' + LineSep;
          SearchPaths := Reg.ReadString('Search Path');
          SearchPaths := ReplaceDelphiVariables(SearchPaths, Compiler.DelphiVersion, Compiler.SystemPlatform);

          InLineIndex := 1;
          while InLineIndex < Length(SearchPaths) do
          begin
            Path := ReadToChar(SearchPaths, InLineIndex, ';');
            if Path <> '' then
              s := s + '-u"' + Path + '"' + LineSep;
          end;

          WriteDcc(s, DelphiPath, SystemPlatformDccStr[Compiler.SystemPlatform]);

          Reg.CloseKey;
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

procedure LibToReg(const Lines: TArrayOfString);
const
	SearchPathName = 'Search Path';
var
	i, j: Integer;
	Reg: TRegistry;
	RegPath: string;
	Paths: string;
	Path: string;
	RegPaths: TStrings;
	InLineIndex: SG;
	Found: BG;
  DelphiRegPath: string;
  Compilers: TCompilers;
  Compiler: TCompiler;
  ci: SG;
begin
	RegPaths := TStringList.Create;

	for i := 0 to Length(Lines) - 1 do
	begin
		if LastChar(Lines[i]) = '\' then
			Lines[i] := DelLastChar(Lines[i]);
	end;

	Reg := TRegistry.Create(KEY_ALL_ACCESS);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
    Compilers := GetAvailableCompilers;
		for ci := 0 to Length(Compilers) - 1 do
		begin
      Compiler := Compilers[ci];
			RegPath := GetDelphiRegPath(Compiler.DelphiVersion);

      DelphiRegPath := GetDelphiLibraryPath(RegPath, Compiler.DelphiVersion, Compiler.SystemPlatform);

      if Reg.OpenKey(DelphiRegPath, False) then
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

        for i := 0 to Length(Lines) - 1 do
        begin
          Path := Lines[i];
          Replace(Path,
            ['%DelphiShortName%', '%DelphiMajorVersion%'],
            [GetDelphiShortName(Compiler.DelphiVersion), IntToStr(GetMajorVersion(Compiler.DelphiVersion))]);
          if Path ='' then
            Continue;

          if FirstChar(Path) = RemovePrefix then
          begin
            Path := UpperCase(Copy(Path, 2, MaxInt));
            j := 0;
            while j < RegPaths.Count do
            begin
              if StartStr(Path, UpperCase(RegPaths[j])) then
              begin
                Information(RegPaths[j] + ' deleted.');
                RegPaths.Delete(j);
              end
              else
                Inc(j);
            end;
          end
          else
          begin
            Found := False;
            for j := 0 to RegPaths.Count - 1 do
            begin
              if UpperCase(RegPaths[j]) = UpperCase(Path) then
              begin
                Found := True;
                Break;
              end;
            end;
            if not Found then
            begin
              RegPaths.Add(Path);
            end;
          end;
        end;
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

procedure BplToReg(const Lines: TArrayOfString);
var
	i, j: Integer;
	DelphiVersion: TDelphiVersion;
	Reg: TRegistry;
	Strings: TStrings;
	RegPath: string;
	Path: string;
	FileName: string;
begin
	Strings := TStringList.Create;

	Reg := TRegistry.Create(KEY_ALL_ACCESS);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
      if Reg.OpenKey(RegPath + PathDelim + 'Known Packages', False) then
      begin
        Reg.GetValueNames(Strings);

        for i := 0 to Length(Lines) - 1 do
        begin
          Path := Lines[i];
          Replace(Path,
            ['%DelphiShortName%', '%DelphiMajorVersion%'],
            [GetDelphiShortName(DelphiVersion), IntToStr(GetMajorVersion(DelphiVersion))]);
          if FirstChar(Path) = RemovePrefix then
          begin
            Path := UpperCase(Copy(Path, 2, MaxInt));
            j := 0;
            while j < Strings.Count do
            begin
              if StartStr(Path, UpperCase(Strings[j])) then
              begin
                Reg.DeleteValue(Strings[j]);
                Information(Strings[j] + ' deleted.');
                Strings.Delete(j);
              end
              else
                Inc(j);
            end;
          end
          else
          begin
            FileName := ExtractFileName(Path);
            for j := 0 to Strings.Count - 1 do
              if ExtractFileName(Strings[j]) = FileName then
              begin
                Reg.DeleteValue(Strings[j]);
                Information(Strings[j] + ' deleted.');
                Break;
              end;
            Reg.WriteString(Path, IntToStr(i));
          end;
        end;

        Reg.CloseKey;
      end;
		end;
	finally
		Reg.Free;
	end;
end;

end.
