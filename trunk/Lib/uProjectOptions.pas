// Reading of *.dproj and earlier *.dof files.

unit uProjectOptions;

interface

uses
  uTypes,
  SysUtils,
  uProjectInfo, uNProjectVersion,
  uDelphi,
  Classes;

type
  TExecutableType = (etProgram, etLibrary, etPackage);
const
  ExecutableTypes: array[TExecutableType] of string = ('exe', 'dll', 'bpl');

type
	TProjectInfos = array [TProjectInfoName] of string;

	TProjectOptions = class
  private
    FFileName: TFileName;
    procedure RWDproj(const AFileName: TFileName; const Save: BG);
    procedure ReadProjectVersionFromDof(const AFileName: TFileName; const Overwrite: BG = False);
    function GetExecutableType(const AFileName: TFileName): TExecutableType;
  public
    ExecutableType: TExecutableType;

		// Directories
		OutputDir: string;
		UnitOutputDir: string; // Replaced to Temp
		PackageDLLOutputDir: string;
		PackageDCPOutputDir: string;
		SearchPath: string;
		Conditionals: TStringList;
		DebugSourceDirs: string; // Not in cfg
		UsePackages: UG;
    Packages: string;
    // Linker
    MinStackSize: UG;
    MaxStackSize: UG;
    ImageBase: UG;
    RuntimeThemes: UG; // Custom

    // Version Info
		Version: TProjectVersion;
    // Version Info Keys
		ProjectInfos: TProjectInfos;

    BuildVersions: string;

    constructor Create;
    destructor Destroy; override;

    procedure AddConditionals(const AConditionals: string);

    procedure ReadFromFile(const AFileName: TFileName);
    procedure ReplaceFromFile(const AFileName: TFileName);
    procedure Update;
    function GetOutputFile: TFileName;
		procedure WriteToCfg(const CfgFileName: TFileName; const DelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform);
	end;

implementation

uses
  uStrings, uMath, uOutputFormat,
  XMLDoc, XMLIntf, Variants, IniFiles,
  uInputFormat, uFiles, uFile, uBackup, uMsg;

const
  DefaultMinStackSize = 16 * KB;
  DefaultMaxStackSize = 1 * MB;
  DefaultImageBase = 4 * MB;

  ConditionalSeparator = ';';

{ TProjectOptions }

procedure TProjectOptions.RWDproj(const AFileName: TFileName; const Save: BG);

	procedure ProcessNode(const Node: IXMLNode);
	const
		AttrName = 'Name';
		VerStr: array [TSubVersion] of string = ('MajorVer', 'MinorVer', 'Release', 'Build');
	var
		cNode: IXMLNode;
		Name: string;
    NodeValue: string;
		SubVersion: TSubVersion;
		p: TProjectInfoName;
	begin
		if (Node = nil) then
			Exit;
    NodeValue := '';
    try
      if (Node.IsTextElement = False) or VarIsNull(Node.NodeValue) then
        NodeValue := ''
      else
        NodeValue := Node.NodeValue;
    except
      // No Value
    end;

    Name := UpperCase(Node.NodeName);
    if Name = UpperCase('DCC_BplOutput') then
    begin
      OutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_ExeOutput') then
    begin
      OutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_DcuOutput') then
    begin
      UnitOutputDir := NodeValue;
    end
//		PackageDLLOutputDir: string;
//		PackageDCPOutputDir: string;
    else if Name = UpperCase('DCC_UnitSearchPath') then
    begin
      SearchPath := NodeValue;
    end
    else if Name = UpperCase('DCC_Define') then
    begin
      AddConditionals(NodeValue);
    end
//		DebugSourceDirs: string; // Not in cfg
//		 UsePackages
		else if Name = UpperCase('DCC_MinStackSize') then
    begin
      ImageBase := StrToValS8(NodeValue, False, 0, S8(DefaultMinStackSize), MaxInt, 1, nil);
    end
		else if Name = UpperCase('DCC_MaxStackSize') then
    begin
      ImageBase := StrToValS8(NodeValue, False, 0, S8(DefaultMaxStackSize), MaxInt, 1, nil);
    end
		else if Name = UpperCase('DCC_Image') then
    begin
      ImageBase := StrToValS8('$' + NodeValue, False, 0, S8(DefaultImageBase), MaxInt, 1, nil);
    end
		else if Name = UpperCase('VersionInfo') then
		begin
			if Node.HasAttribute(AttrName) then
			begin
				Name := Node.GetAttribute(AttrName);
				for SubVersion := Low(SubVersion) to High(SubVersion) do
				begin
					if Name = VerStr[SubVersion] then
					begin
						if Save then
							Node.NodeValue := Version.GetAsString(SubVersion)
						else
							Version.SetSubVersion(SubVersion, NodeValue);
						Break;
					end
				end;
			end;
		end
		else if Name = UpperCase('VersionInfoKeys') then
		begin
			if Node.HasAttribute(AttrName) then
			begin
				Name := Node.GetAttribute(AttrName);
				begin
					for p := Low(TProjectInfoName) to High(TProjectInfoName) do
					begin
						if ProjectInfoStr[p] = Name then
						begin
							try
								if Save then
									Node.NodeValue := ProjectInfos[p]
								else
								begin
									ProjectInfos[p] := NodeValue;
								end;
							except

							end;
							Break;
						end;
					end;
				end
			end;
		end;

		cNode := Node.ChildNodes.First;
		while cNode <> nil do
		begin
 			ProcessNode(cNode);
			cNode := cNode.NextSibling;
		end;
	end;

var
	XML: IXMLDocument;
	iNode: IXMLNode;
  Name: string;
begin
	if Save = False then
	begin
		// Default
//		Result.Version.CleanupInstance;
//		ProjectInfos[piFileDescription] := '';
	end;

	if FileExists(AFileName) then
	begin
		if Save then
			BackupFile(AFileName, bfTemp);
    try
      XML := TXMLDocument.Create(AFileName);
      XML.Active := True;
      try
        if XML.IsEmptyDoc then
          Exit;
        iNode := XML.DocumentElement.ChildNodes.First;
        while iNode <> nil do
        begin
          if (iNode.NodeName = 'PropertyGroup') then
          begin
            if iNode.HasAttribute('Condition') then
            begin
              Name := iNode.Attributes['Condition'];
              if Name = '''$(Cfg_2)''!=''''' then // DEBUG
              begin
                iNode := iNode.NextSibling;
                Continue;
              end;
            end;
          end;
          ProcessNode(iNode);
          iNode := iNode.NextSibling;
        end;
        iNode := nil;
        if Save then
        begin
          // TODO : Missing Byte Order Mark
          XML.SaveToFile(AFileName);
        end;
      finally
        XML.Active := False;
        XML := nil; // Release XML document
      end;
    except
      on E: Exception do
        ErrorMsg(E.Message);
    end;
	end
	else
	begin
		Warning('Dproj file %1 not found!', [AFileName]);
	end;
end;

procedure TProjectOptions.Update;
var
  Name: string;
begin
	ProjectInfos[piFileVersion] := Version.ToStrictString;
	// ProjectInfos[piFileDescription] := ProjectVersion.FileDescription;
	ProjectInfos[piLegalCopyright] := ReplaceF(ProjectInfos[piLegalCopyright], '%year%', NToS(CurrentYear, '0000'));

  Name := DelFileExt(ExtractFileName(FFileName));
	ProjectInfos[piInternalName] := Name;
	ProjectInfos[piOriginalFileName] := Name + '.exe';
	if ProjectInfos[piProductName] = '' then
		ProjectInfos[piProductName] := AddSpace(Name);
{	if Edition <> '' then
		ProjectVersion.ProjectInfos[piProductName] := ProjectVersion.ProjectInfos[piProductName] + CharSpace + Edition;}

	ProjectInfos[piProductVersion] := Version.ToString;

	ProjectInfos[piRelease] := DateTimeToS(Now, 0, ofIO);
	ProjectInfos[piWeb] := ReplaceF(ProjectInfos[piWeb], '%ProjectName%', Name);
end;

(*
procedure WriteDof(const FileName: TFileName; const ProjectName: string;
	const ProjectVersion: TMyProjectVersion);
var
	IniFile: TIniFile;
	p: TProjectInfoName;
begin
	BackupFile(FileName);
	IniFile := TIniFile.Create(FileName);
	try
		// Used for read!
		IniFile.WriteString('Version Info', 'Release', ProjectVersion.Version.Release);
		// Used for read!
		IniFile.WriteString('Version Info', 'Build', ProjectVersion.Version.Build);

		// IniFile.WriteInteger('Version Info', 'Locale', $0409); // English (United States)
		// IniFile.WriteInteger('Version Info', 'Locale', $0405); // Czech
		// IniFile.WriteInteger('Version Info', 'IncludeVerInfo', 1);
		IniFile.WriteInteger('Version Info', 'AutoIncBuild', 0);

		IniFile.EraseSection('Version Info Keys');
		// Used for read!
		// IniFile.WriteString('Version Info Keys', 'FileDescription', ProjectVersion.FileDescription);
		for p := Low(TProjectInfoName) to High(TProjectInfoName) do
			IniFile.WriteString('Version Info Keys', ProjectInfoStr[p], ProjectVersion.ProjectInfos[p]);
	finally
		IniFile.Free;
	end;
end;

procedure WriteProjectVersion(const ProjectFileName: TFileName; const ProjectName: string;
	ProjectVersion: TMyProjectVersion);
var
  Prefix: string;
  DProjFileName: TFileName;
  DofFileName: TFileName;
begin
  Prefix := DelFileExt(ProjectFileName);
  DofFileName := Prefix + '.dof';
  if FileExists(DofFileName) then
  	WriteDof(DofFileName, ProjectName, ProjectVersion);
  DProjFileName := Prefix + '.dproj';
	if FileExists(DProjFileName) then
		RWDproj(DProjFileName, ProjectVersion, True);
end;
*)

constructor TProjectOptions.Create;
begin
  MinStackSize := DefaultMinStackSize;
  MaxStackSize := DefaultMaxStackSize;
  ImageBase := DefaultImageBase;
  Conditionals := TStringList.Create;
  Conditionals.Duplicates := dupIgnore;
  Conditionals.Sorted := True;
  Conditionals.Delimiter := ConditionalSeparator;
end;

destructor TProjectOptions.Destroy;
begin
  FreeAndNil(Conditionals);
  FreeAndNil(Version);
  inherited;
end;

function TProjectOptions.GetExecutableType(
  const AFileName: TFileName): TExecutableType;
var
  s: string;
  ProgramPos, LibraryPos: SG;
  DPK, DPR: BG;
  DPRFileName: TFileName;
begin
  DPK := FileExists(DelFileExt(AFileName) + '.dpk');
  DPRFileName := DelFileExt(AFileName) + '.dpr';
  DPR := FileExists(DPRFileName);
  if DPK and DPR then
  begin
    Warning('DPR and DPK of same name exists!');
  end;
  if DPK then
  begin
    Result := etPackage;
    Exit;
  end;

  Result := etProgram;
  ReadStringFromFile(DPRFileName, s);
  s := LowerCase(s);
  LibraryPos := Pos('library', s);
  if (LibraryPos <> 0) then
  begin
    ProgramPos := Pos('program', s);
    if (ProgramPos = 0) or (LibraryPos < ProgramPos) then
      Result := etLibrary;
  end;
{  if Pos('program', s) <> 0 then
    Result := etProgram
  else if Pos('library', s) <> 0 then
    Result := etLibrary;}
{  else if Pos('package', s) <> 0 then
    Result := etPackage;}
end;

function TProjectOptions.GetOutputFile: TFileName;
begin
	if Pos(':', OutputDir) <> 0 then
		Result := OutputDir
	else
		Result := ExpandFileName(ExtractFilePath(FFileName) + OutputDir); // ../
	CorrectDir(string(Result));
	Result := Result + DelFileExt(ExtractFileName(FFileName)) + '.' + ExecutableTypes[ExecutableType];
end;

procedure TProjectOptions.ReadFromFile(const AFileName: TFileName);
begin
  FFileName := AFileName;
  ReplaceFromFile(AFileName);

  ExecutableType := GetExecutableType(AFileName);
end;

procedure TProjectOptions.ReadProjectVersionFromDof(const AFileName: TFileName; const Overwrite: BG = False);
const
	Section = 'Version Info';
	Directories = 'Directories';
	Linker = 'Linker';
var
	IniFile: TIniFile;
  ProjectInfoName: TProjectInfoName;
begin
	if FileExists(AFileName) then
	begin
		{$ifdef Console}
		Information('Reading file %1.', [AFileName]);
		{$endif}

		IniFile := TIniFile.Create(AFileName);
		try
      if Version = nil then
        Version := TProjectVersion.Create;
			Version.SetSubVersion(svMajor, IniFile.ReadString(Section, 'MajorVer', Version.Major));
			Version.SetSubVersion(svMinor, IniFile.ReadString(Section, 'MinorVer', Version.Minor));
			Version.SetSubVersion(svRelease, IniFile.ReadString(Section, 'Release', Version.Release));
			Version.SetSubVersion(svBuild, IniFile.ReadString(Section, 'Build', Version.Build));
      for ProjectInfoName := Low(ProjectInfoName) to High(ProjectInfoName) do
      begin
  			ProjectInfos[ProjectInfoName] := IniFile.ReadString
  				('Version Info Keys', ProjectInfoStr[ProjectInfoName], ProjectInfos[ProjectInfoName]);
      end;

      if Overwrite or (OutputDir = '') then
  			OutputDir := IniFile.ReadString(Directories, 'OutputDir', OutputDir);
      if Overwrite or (UnitOutputDir = '') then
  			UnitOutputDir := IniFile.ReadString(Directories, 'UnitOutputDir', UnitOutputDir);
      if Overwrite or (PackageDLLOutputDir = '') then
  			PackageDLLOutputDir := IniFile.ReadString(Directories, 'PackageDLLOutputDir', PackageDLLOutputDir);
      if Overwrite or (PackageDCPOutputDir = '') then
  			PackageDCPOutputDir := IniFile.ReadString(Directories, 'PackageDCPOutputDir', PackageDCPOutputDir);

      if SearchPath <> '' then
        SearchPath := SearchPath + ';';
 			SearchPath := SearchPath + IniFile.ReadString(Directories, 'SearchPath', SearchPath);

      AddConditionals(IniFile.ReadString(Directories, 'Conditionals', ''));
      if Overwrite or (DebugSourceDirs = '') then
  			DebugSourceDirs := IniFile.ReadString(Directories, 'DebugSourceDirs', DebugSourceDirs);
      UsePackages := IniFile.ReadInteger(Directories, 'UsePackages', UsePackages);
      Packages := IniFile.ReadString(Directories, 'Packages', Packages);

			MinStackSize := IniFile.ReadInteger(Linker, 'MinStackSize', MinStackSize);
			MaxStackSize := IniFile.ReadInteger(Linker, 'MaxStackSize', MaxStackSize);
			ImageBase := IniFile.ReadInteger(Linker, 'ImageBase', ImageBase);
      RuntimeThemes := IniFile.ReadInteger(Linker, 'RuntimeThemes', RuntimeThemes);

      BuildVersions := IniFile.ReadString('Build', 'Versions', BuildVersions);
		finally
			IniFile.Free;
		end;
	end
	else
	begin
		Warning('Dof file %1 not found!', [AFileName]);
	end;
end;

procedure TProjectOptions.ReplaceFromFile(const AFileName: TFileName);
const
	DProjMinimalVersion = 9;
var
	DProjFileName: TFileName;
begin
  if Version = nil then
    Version := TProjectVersion.Create;
	DProjFileName := DelFileExt(AFileName) + '.dproj';
	if {(DelphiVersion < DProjMinimalVersion) or} (FileExists(DProjFileName) = False) then
		ReadProjectVersionFromDof(DelFileExt(AFileName) + '.dof')
	else
  begin
    if FileExists(DelFileExt(AFileName) + '.dof') then
  		ReadProjectVersionFromDof(DelFileExt(AFileName) + '.dof');
		RWDproj(DProjFileName, False);
  end;
end;

procedure TProjectOptions.WriteToCfg(const CfgFileName: TFileName;
  const DelphiVersion: TDelphiVersion;
  const SystemPlatform: TSystemPlatform);
var
	Data: string;
  SearchPath: string;
  FileName: TFileName;
  i: TDelphiVersion;
begin
	{$ifdef Console}
	Information('Writing file %1.', [CfgFileName]);
	{$endif}

	Data := '';

	if OutputDir <> '' then
		Data := Data + '-E"' + OutputDir + '"' + FileSep;
//-LE"c:\program files\borland\delphi6\Projects\Bpl"
//-LN"c:\program files\borland\delphi6\Projects\Bpl"

	if UnitOutputDir <> '' then
		Data := Data + '-N"' + ReplaceDelphiVariables(UnitOutputDir, DelphiVersion, SystemPlatform) + '"' + FileSep;
	if PackageDLLOutputDir <> '' then
		Data := Data + '-LE"' + ReplaceDelphiVariables(PackageDLLOutputDir, DelphiVersion, SystemPlatform) + '"' + FileSep;
	if PackageDCPOutputDir <> '' then
		Data := Data + '-LN"' + ReplaceDelphiVariables(PackageDCPOutputDir, DelphiVersion, SystemPlatform) + '"' + FileSep;
	if UsePackages <> 0 then
		Data := Data + '-LU' + Packages + FileSep;

  SearchPath := ReplaceDelphiVariables(SearchPath, DelphiVersion, SystemPlatform);
	if SearchPath <> '' then
		Data := Data + '-U"' + SearchPath + '"' + FileSep;
	if SearchPath <> '' then
		Data := Data + '-O"' + SearchPath + '"' + FileSep;
	if SearchPath <> '' then
		Data := Data + '-I"' + SearchPath + '"' + FileSep;
	if SearchPath <> '' then
		Data := Data + '-R"' + SearchPath + '"' + FileSep;

	if Conditionals.Count > 0 then
		Data := Data + '-D' + Conditionals.DelimitedText + FileSep;

  Data := Data + '-$M' + IntToStr(MinStackSize) + ',' + IntToStr(MaxStackSize) + FileSep;
  Data := Data + '-K$' + NumToStr(ImageBase, 16) + FileSep;
  FileName := DataDir + 'default.cfg';
  if FileExistsEx(FileName) then
	  Data := Data +  ReadStringFromFile(FileName);
	for i := dvDelphi1 to DelphiVersion do
  begin
	  FileName := DataDir + 'default-D' + GetDelphiShortName(DelphiVersion) + '.cfg';
	  if FileExistsEx(FileName) then
		  Data := Data +  ReadStringFromFile(FileName);
  end;

	WriteStringToFile(CfgFileName, Data, False, fcAnsi);
	{$ifdef Console}
	Information('Done.');
	{$endif}
end;

procedure TProjectOptions.AddConditionals(const AConditionals: string);
var
  i: SG;
  Conditional: string;
begin
  i := 1;
  while i <= Length(AConditionals) do
  begin
    Conditional := ReadToChar(AConditionals, i, ConditionalSeparator);
    if (Conditional <> '') and (Conditional <> '$(DCC_Define)') then
			Conditionals.Add(Conditional);
  end;
end;

end.
