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

procedure CreatePathsToReg;
procedure RegistryToDccCfg;

implementation

uses
  uDelphi,
	uMath, uMsg,
  uLog,
	Classes;

function GetDelphiLibraryPath(const RegPath: string; const DelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform): string;
begin
  if DelphiVersion <= dvDelphiXE then
    Result := RegPath + PathDelim + 'Library'
  else
  begin
    Result := RegPath + PathDelim + 'Library\Win' + SystemPlatformStr[SystemPlatform];
  end;
end;

function CommonCfgText(const DelphiVersion: TDelphiVersion): string;
var
  s: string;
begin
  s := '-aWinTypes=Windows';
  if DelphiVersion > dvDelphiXE then
  begin
    s := s + ';Forms=Vcl.Forms';
    s := s + ';Graphics=Vcl.Graphics';
    s := s + ';Controls=Vcl.Controls';
    s := s + ';ExtCtrls=Vcl.ExtCtrls';
    s := s + ';StdCtrls=Vcl.StdCtrls';
    s := s + ';Consts=Vcl.Consts';
    s := s + ';Dialogs=Vcl.Dialogs';
    s := s + ';ComCtrls=Vcl.ComCtrls';
    s := s + ';Menus=Vcl.Menus';
    s := s + ';ClipBrd=Vcl.ClipBrd';
    s := s + ';ExtDlgs=Vcl.ExtDlgs';
    s := s + ';ActnList=Vcl.ActnList';
    s := s + ';ImgList=Vcl.ImgList';
    s := s + ';CheckLst=Vcl.CheckLst';
    s := s + ';Buttons=Vcl.Buttons';

    s := s + ';Jpeg=Vcl.Imaging.Jpeg';
    s := s + ';PngImage=Vcl.Imaging.PngImage';

    s := s + ';Windows=Winapi.Windows';
    s := s + ';Messages=Winapi.Messages';
    s := s + ';ActiveX=Winapi.ActiveX';
    s := s + ';mmsystem=Winapi.mmsystem';
    s := s + ';CommCtrl=Winapi.CommCtrl';
    s := s + ';ShellAPI=Winapi.ShellAPI';
    s := s + ';ShlObj=Winapi.ShlObj';

    s := s + ';TypInfo=System.TypInfo';
    s := s + ';SysUtils=System.SysUtils';
    s := s + ';Math=System.Math';
    s := s + ';Types=System.Types';
    s := s + ';Classes=System.Classes';
    s := s + ';Variants=System.Variants';
    s := s + ';IniFiles=System.IniFiles';

    s := s + ';Registry=System.Win.Registry';
    s := s + ';ComObj=System.Win.ComObj';


    s := s + ';XMLDoc=XML.XMLDoc';
    s := s + ';XMLIntf=XML.XMLIntf';
  end;
  s := s + LineSep;
  Result := s;
end;

procedure WriteDcc(const Data: string; const DelphiPath: string; const Bits: string);
var
	FileName: TFileName;
begin
  FileName := DelphiPath + 'bin' + PathDelim + 'dcc' + Bits + '.cfg';
  {$ifdef Console}
  Writeln('Creating ' + FileName);
  {$endif}
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
            s := s + '-r"' + DelphiPath + 'Lib"' + LineSep;
            SearchPaths := Reg.ReadString('Search Path');
            SearchPaths := ReplaceDelphiVariables(SearchPaths, Compiler.DelphiVersion, Compiler.SystemPlatform);

            InLineIndex := 1;
            while InLineIndex < Length(SearchPaths) do
            begin
              Path := ReadToChar(SearchPaths, InLineIndex, ';');
              if Path <> '' then
                s := s + '-u"' + Path + '"' + LineSep;
            end;

            WriteDcc(s, DelphiPath, SystemPlatformStr[Compiler.SystemPlatform]);

            Reg.CloseKey;
          end;
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

procedure LibToReg(ALibFileName: TFileName);
const
	SearchPathName = 'Search Path';
var
	Lines: TArrayOfString;
	LineCount: SG;
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
	LineCount := 0;

	ReadStringsFromFile(ALibFileName, Lines, LineCount);
	for i := 0 to LineCount - 1 do
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

        for i := 0 to LineCount - 1 do
        begin
          Path := Lines[i];
          Replace(Path,
            ['%DelphiShortName%', '%DelphiMajorVersion%'],
            [GetDelphiShortName(Compiler.DelphiVersion), IntToStr(GetDelphiMajorVersion(Compiler.DelphiVersion))]);
          if Path ='' then
            Continue;

          if FirstChar(Path) = '-' then
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

procedure BplToReg(const ABplFileName: TFileName);
var
	Lines: TArrayOfString;
	LineCount: SG;
	i, j: Integer;
	DelphiVersion: TDelphiVersion;
	Reg: TRegistry;
	Strings: TStrings;
	RegPath: string;
	Path: string;
	FileName: string;
begin
	Strings := TStringList.Create;
	LineCount := 0;

	ReadStringsFromFile(ABplFileName, Lines, LineCount);

	Reg := TRegistry.Create(KEY_ALL_ACCESS);
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		for DelphiVersion := dvDelphi1 to TDelphiVersion(GetDelphiVersionCount - 1) do
		begin
			RegPath := GetDelphiRegPath(DelphiVersion);
      if Reg.OpenKey(RegPath + PathDelim + 'Known Packages', False) then
      begin
        Reg.GetValueNames(Strings);

        for i := 0 to LineCount - 1 do
        begin
          Path := Lines[i];
          Replace(Path,
            ['%DelphiShortName%', '%DelphiMajorVersion%'],
            [GetDelphiShortName(DelphiVersion), IntToStr(GetDelphiMajorVersion(DelphiVersion))]);
          if FirstChar(Path) = '-' then
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

procedure CreatePathsToReg;
const
  LibFileName = 'lib.txt';
  BplFileName = 'bpl.txt';
var
  FileName: TFileName;
begin
  // Lib
  FileName := StartDir +'$' + LibFileName;
  if FileExists(FileName) then
  	LibToReg(FileName);
	LibToReg(StartDir + LibFileName);

  // Bpl
  FileName := StartDir + '$' + BplFileName;
  if FileExists(FileName) then
  	BplToReg(FileName);
	BplToReg(StartDir + BplFileName);
end;

end.
