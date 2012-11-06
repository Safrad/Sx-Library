// Reading of *.dproj and earlier *.dof files.

unit uProjectOptions;

interface

uses
  uTypes,
  SysUtils,
  uProjectInfo, uNProjectVersion,
  uDelphi,
  Classes, XMLIntf;

type
  TExecutableType = (etProgram, etLibrary, etPackage);
const
  ExecutableTypes: array[TExecutableType] of string = ('exe', 'dll', 'bpl');

type
	TProjectInfos = array [TProjectInfoName] of string;

  TOutputWarning = (owDefault, owTrue, owFalse, owError);

  TCompilerWarning = (
			cwCOMPARING_SIGNED_UNSIGNED,
			cwXML_INVALID_NAME,
			cwUNIT_DEPRECATED,
			cwIMPLICIT_STRING_CAST_LOSS,
			cwUNSAFE_TYPE,
			cwLOCAL_PINVOKE,
			cwUNSUPPORTED_CONSTRUCT,
			cwXML_EXPECTED_CHARACTER,
			cwCASE_LABEL_RANGE,
			cwUNIT_INIT_SEQ,
			cwTYPEINFO_IMPLICITLY_ADDED,
			cwXML_UNKNOWN_ENTITY,
			cwASG_TO_TYPED_CONST,
			cwNO_RETVAL,
			cwCVT_NARROWING_STRING_LOST,
			cwSYMBOL_LIBRARY,
			cwCVT_WCHAR_TO_ACHAR,
			cwNO_CFG_FILE_FOUND,
			cwCVT_ACHAR_TO_WCHAR,
			cwIMPLICIT_STRING_CAST,
			cwSTRING_CONST_TRUNCED,
			cwDUPLICATE_CTOR_DTOR,
			cwIMAGEBASE_MULTIPLE,
			cwZERO_NIL_COMPAT,
			cwCOMBINING_SIGNED_UNSIGNED,
			cwUNIT_EXPERIMENTAL,
			cwHPPEMIT_IGNORED,
			cwFOR_LOOP_VAR_VARPAR,
			cwMESSAGE_DIRECTIVE,
			cwSYMBOL_PLATFORM,
			cwUNIT_PLATFORM,
			cwWIDECHAR_REDUCED,
			cwXML_NO_PARM,
			cwSUSPICIOUS_TYPECAST,
			cwIMPLICIT_IMPORT,
			cwGARBAGE,
			cwCONSTRUCTING_ABSTRACT,
			cwTYPED_CONST_VARPAR,
			cwFILE_OPEN_UNITSRC,
			cwXML_INVALID_NAME_START,
			cwPRIVATE_PROPACCESSOR,
			cwXML_CREF_NO_RESOLVE,
			cwINVALID_DIRECTIVE,
			cwSYMBOL_EXPERIMENTAL,
			cwUNSAFE_CODE,
			cwXML_WHITESPACE_NOT_ALLOWED,
			cwDUPLICATES_IGNORED,
			cwBAD_GLOBAL_SYMBOL,
			cwHRESULT_COMPAT,
			cwCOMPARISON_TRUE,
			cwLOCALE_TO_UNICODE,
			cwFOR_LOOP_VAR_UNDEF,
			cwHIDING_MEMBER,
			cwUNIT_NAME_MISMATCH,
			cwHIDDEN_VIRTUAL,
			cwXML_NO_MATCHING_PARM,
			cwEXPLICIT_STRING_CAST_LOSS,
			cwPACKAGED_THREADVAR,
			cwIMPLICIT_VARIANTS,
			cwEXPLICIT_STRING_CAST,
			cwUNIT_LIBRARY,
			cwOPTION_TRUNCATED,
			cwFILE_OPEN,
			cwBOUNDS_ERROR,
			cwUSE_BEFORE_DEF,
			cwFOR_VARIABLE,
			cwSYMBOL_DEPRECATED,
			cwPACKAGE_NO_LINK,
			cwUNSAFE_CAST,
			cwCOMPARISON_FALSE,
			cwCVT_WIDENING_STRING_LOST,
			cwUNICODE_TO_LOCALE,
			cwRLINK_WARNING);

var
  CompilerWarningsStr: array[TCompilerWarning] of string;

type
	TProjectOptions = class
  private
    FFileName: TFileName;
    FEnabled: BG;
    procedure WriteWarningsToNode(const Node: IXMLNode);
    procedure ReadProjectVersionFromDof(const AFileName: TFileName; const Overwrite: BG = False);
    function GetExecutableType(const AFileName: TFileName): TExecutableType;
  public
    ExecutableType: TExecutableType;
    // Compiler
    ShowHints: BG;
    ShowWarnings: BG;
    Warnings: array[TCompilerWarning] of TOutputWarning;

		// Directories
		OutputDir: string; // exe, dll
		UnitOutputDir: string; // Replaced to Temp
		PackageDLLOutputDir: string; // bpl
		PackageDCPOutputDir: string; // dcp
		Conditionals: TStringList;
		SearchPaths: TStringList;
		DebugSourceDirs: string; // Not in cfg
		UsePackages: BG;
    Packages: string;
    // Linker
    MinStackSize: UG;
    MaxStackSize: UG;
    ImageBase: UG;
    // GUI
    RuntimeThemes: BG; // New in Delphi 2007

    // Version Info
		Version: TProjectVersion;
    // Version Info Keys
		ProjectInfos: TProjectInfos;

    BuildVersions: string;

    constructor Create;
    destructor Destroy; override;

    procedure RWDproj(const AFileName: TFileName; const Save: BG);

    procedure AddConditionals(const AConditionals: string);
    procedure AddSearchPaths(const ASearchPaths: string);

    procedure ReadFromFile(const AFileName: TFileName);
    procedure ReplaceFromFile(const AFileName: TFileName);
    procedure Update;
    function GetOutputFile: TFileName;
		procedure WriteToCfg(const CfgFileName: TFileName; const DelphiVersion: TDelphiVersion; const SystemPlatform: TSystemPlatform);

    property Enabled: BG read FEnabled write FEnabled;
	end;

implementation

uses
  TypInfo,
  uStrings, uMath, uOutputFormat,
  XMLDoc, Variants, IniFiles,
  uInputFormat, uFiles, uFile, uBackup, uMsg;

const
  DefaultMinStackSize = 16 * KB;
  DefaultMaxStackSize = 1 * MB;
  DefaultImageBase = 4 * MB;

  ProjectListSeparator = ';';

function StrToBoolean(const Value: string): BG;
begin
  if UpperCase(Value) = 'TRUE' then
  	Result := True
  else
  	Result := False;
end;

function StrToOutputWarning(const Value: string): TOutputWarning;
begin
  if Value = 'ERROR' then
    Result := owError
  else if Value = 'TRUE' then
    Result := owTrue
  else if Value = 'FALSE' then
    Result := owFalse
  else
    Result := owDefault;
end;

function OutputWarningToStr(const Value: TOutputWarning): string;
begin
  case Value of
  owTrue: Result := 'true';
  owFalse: Result := 'false';
  owError: Result := 'error';
  else Result := '';
  end;
end;

procedure RepairDProj(const AFileName: TFileName);
var
  Data, Data2: string;
  Line: string;
  i: SG;
begin
  Data := ReadStringFromFile(AFileName);
//  Replace(Data, [''''], ['&apos;']);

  i := 1;
  while i < Length(Data) do
  begin
    Line := ReadToNewLine(Data, i);
    AppendStr(Data2, CharTab + Line + FullSep);
  end;
  WriteStringToFile(AFileName, Data2, False);
end;

{ TProjectOptions }

function FindOrCreateNode(const RootNode: IXMLNode; const Name: string): IXMLNode;
begin
  Result := RootNode.ChildNodes.FindNode(Name);
  if Result = nil then
  begin
    Result := RootNode.AddChild(Name);
  end;
end;

procedure TProjectOptions.WriteWarningsToNode(const Node: IXMLNode);
const
  AttrName = 'Name';
  VerStr: array [TSubVersion] of string = ('MajorVer', 'MinorVer', 'Release', 'Build');
var
  cNode: IXMLNode;
  Name: string;
  i: SG;
begin
  Name := UpperCase(Node.NodeName);
  for i := 0 to Length(CompilerWarningsStr) - 1 do
  begin
    if Warnings[TCompilerWarning(i)] <> owDefault then
    begin
      cNode := FindOrCreateNode(Node, CompilerWarningsStr[TCompilerWarning(i)]);
      cNode.NodeValue := OutputWarningToStr(Warnings[TCompilerWarning(i)]);
    end
    else
    begin
      cNode := Node.ChildNodes.FindNode(CompilerWarningsStr[TCompilerWarning(i)]);
      if cNode <> nil then
        Node.ChildNodes.Remove(cNode);
    end;
  end;


{		cNode := Node.ChildNodes.First;
  while cNode <> nil do
  begin
    ProcessNode(cNode);
    cNode := cNode.NextSibling;
  end;}
end;

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
    i: SG;
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
      PackageDLLOutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_DcpOutput') then
    begin
      PackageDCPOutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_ExeOutput') then
    begin
      OutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_DcuOutput') then
    begin
      UnitOutputDir := NodeValue;
    end
    else if Name = UpperCase('DCC_UnitSearchPath') then
    begin
      AddSearchPaths(NodeValue);
    end
    else if Name = UpperCase('DCC_Define') then
    begin
      AddConditionals(NodeValue);
    end
    else if Name = UpperCase('UsePackages') then
    begin
      UsePackages := StrToBoolean(NodeValue);
    end
    else if Name = UpperCase('UsePackage') then
    begin
      Packages := NodeValue;
    end
//		DebugSourceDirs: string; // Not in cfg
		else if Name = UpperCase('DCC_MinStackSize') then
    begin
      MinStackSize := StrToValS8(NodeValue, False, 0, S8(DefaultMinStackSize), MaxInt, 1, nil);
    end
		else if Name = UpperCase('DCC_MaxStackSize') then
    begin
      MaxStackSize := StrToValS8(NodeValue, False, 0, S8(DefaultMaxStackSize), MaxInt, 1, nil);
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
//							Node.NodeValue := Version.GetAsString(SubVersion)
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
//									Node.NodeValue := ProjectInfos[p]
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
		end
    else if Name = UpperCase('DCC_Hints') then
    begin
      if Save then
        Node.NodeValue := ShowHints
      else
        ShowHints := StrToBoolean(NodeValue);
    end
    else if Name = UpperCase('DCC_Warnings') then
    begin
      if Save then
        Node.NodeValue := ShowWarnings
      else
        ShowWarnings := StrToBoolean(NodeValue);
    end
    else
    begin
      NodeValue := UpperCase(NodeValue);
      for i := 0 to Length(CompilerWarningsStr) - 1 do
        if Name = CompilerWarningsStr[TCompilerWarning(i)] then
        begin
          if Save then
//            Node.NodeValue := Warnings[TCompilerWarning(i)]
          else
            Warnings[TCompilerWarning(i)] := StrToOutputWarning(NodeValue);
          Break;
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
      XML.Options := XML.Options + [doNodeAutoIndent];
      XML.NodeIndentStr := CharTab;
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
              end
              else if Name = '''$(Base)''!=''''' then
              begin
                if Save then
                  WriteWarningsToNode(iNode);
              end;
            end;
          end;
          ProcessNode(iNode);
          iNode := iNode.NextSibling;
        end;
        iNode := nil;
        if Save then
        begin
          XML.SaveToFile(AFileName);
          RepairDproj(AFileName);
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
	inherited;

  FEnabled := True;
  RuntimeThemes := True;

  ShowHints := True;
  ShowWarnings := True;

  MinStackSize := DefaultMinStackSize;
  MaxStackSize := DefaultMaxStackSize;
  ImageBase := DefaultImageBase;

  Conditionals := TStringList.Create;
  Conditionals.Duplicates := dupIgnore;
  Conditionals.Sorted := True;
  Conditionals.Delimiter := ProjectListSeparator;

  SearchPaths := TStringList.Create;
  SearchPaths.Duplicates := dupIgnore;
  SearchPaths.Sorted := True;
  SearchPaths.Delimiter := ProjectListSeparator;

  Version := TProjectVersion.Create;
end;

destructor TProjectOptions.Destroy;
begin
  FreeAndNil(SearchPaths);
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
  // TODO PackageDLLOutput
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
	VersionInfoSection = 'Version Info';
	DirectoriesSectionName = 'Directories';
	LinkerSectionName = 'Linker';
	BuildSectionName = 'Build';
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
    	if IniFile.SectionExists(BuildSectionName) then
      begin
        Enabled := IniFile.ReadBool(BuildSectionName, 'Enabled', Enabled);
        RuntimeThemes := IniFile.ReadBool(BuildSectionName, 'RuntimeThemes', RuntimeThemes);
      end;

			Version.SetSubVersion(svMajor, IniFile.ReadString(VersionInfoSection, 'MajorVer', Version.Major));
			Version.SetSubVersion(svMinor, IniFile.ReadString(VersionInfoSection, 'MinorVer', Version.Minor));
			Version.SetSubVersion(svRelease, IniFile.ReadString(VersionInfoSection, 'Release', Version.Release));
			Version.SetSubVersion(svBuild, IniFile.ReadString(VersionInfoSection, 'Build', Version.Build));
      for ProjectInfoName := Low(ProjectInfoName) to High(ProjectInfoName) do
      begin
  			ProjectInfos[ProjectInfoName] := IniFile.ReadString
  				('Version Info Keys', ProjectInfoStr[ProjectInfoName], ProjectInfos[ProjectInfoName]);
      end;

      if Overwrite or (OutputDir = '') then
  			OutputDir := IniFile.ReadString(DirectoriesSectionName, 'OutputDir', OutputDir);
      if Overwrite or (UnitOutputDir = '') then
  			UnitOutputDir := IniFile.ReadString(DirectoriesSectionName, 'UnitOutputDir', UnitOutputDir);
      if Overwrite or (PackageDLLOutputDir = '') then
  			PackageDLLOutputDir := IniFile.ReadString(DirectoriesSectionName, 'PackageDLLOutputDir', PackageDLLOutputDir);
      if Overwrite or (PackageDCPOutputDir = '') then
  			PackageDCPOutputDir := IniFile.ReadString(DirectoriesSectionName, 'PackageDCPOutputDir', PackageDCPOutputDir);

 			AddSearchPaths(IniFile.ReadString(DirectoriesSectionName, 'SearchPath', ''));

      AddConditionals(IniFile.ReadString(DirectoriesSectionName, 'Conditionals', ''));

      if Overwrite or (DebugSourceDirs = '') then
  			DebugSourceDirs := IniFile.ReadString(DirectoriesSectionName, 'DebugSourceDirs', DebugSourceDirs);
      UsePackages := IniFile.ReadBool(DirectoriesSectionName, 'UsePackages', UsePackages);
      Packages := IniFile.ReadString(DirectoriesSectionName, 'Packages', Packages);

			MinStackSize := IniFile.ReadInteger(LinkerSectionName, 'MinStackSize', MinStackSize);
			MaxStackSize := IniFile.ReadInteger(LinkerSectionName, 'MaxStackSize', MaxStackSize);
			ImageBase := IniFile.ReadInteger(LinkerSectionName, 'ImageBase', ImageBase);

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
  CfgSearchPath: string;
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
	if UsePackages then
		Data := Data + '-LU' + Packages + FileSep;

  CfgSearchPath := ReplaceDelphiVariables(SearchPaths.DelimitedText, DelphiVersion, SystemPlatform);
	if CfgSearchPath <> '' then
		Data := Data + '-U"' + CfgSearchPath + '"' + FileSep;
	if CfgSearchPath <> '' then
		Data := Data + '-O"' + CfgSearchPath + '"' + FileSep;
	if CfgSearchPath <> '' then
		Data := Data + '-I"' + CfgSearchPath + '"' + FileSep;
	if CfgSearchPath <> '' then
		Data := Data + '-R"' + CfgSearchPath + '"' + FileSep;

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
    Conditional := ReadToChar(AConditionals, i, ProjectListSeparator);
    if (Conditional <> '') and (Conditional <> '$(DCC_Define)') then
			Conditionals.Add(Conditional);
  end;
end;

procedure TProjectOptions.AddSearchPaths(const ASearchPaths: string);
var
  i: SG;
  SearchPath: string;
begin
  i := 1;
  while i <= Length(ASearchPaths) do
  begin
    SearchPath := ReadToChar(ASearchPaths, i, ProjectListSeparator);
    if (SearchPath <> '') and (SearchPath <> '$(DCC_UnitSearchPath)') then
			SearchPaths.Add(SearchPath);
  end;
end;

procedure Init;
var
  i: SG;
begin
  EnumToStrEx(TypeInfo(TCompilerWarning), CompilerWarningsStr);
  for i := 0 to Length(CompilerWarningsStr) - 1 do
  begin
    AddPrefix(CompilerWarningsStr[TCompilerWarning(i)], 'DCC_');
  end;
end;

initialization
{$IFNDEF NoInitialization}
  Init;
{$ENDIF NoInitialization}
end.
