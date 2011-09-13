// Reading of *.dproj and earlier *.dof files.

unit uProjectOptions;

interface

uses
  uTypes,
  SysUtils,
  uProjectInfo, uNProjectVersion;

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
    procedure ReadProjectVersionFromDof(const AFileName: TFileName);
    function GetExecutableType(const AFileName: TFileName): TExecutableType;
  public
    ExecutableType: TExecutableType;

		// Directories
		OutputDir: string;
		UnitOutputDir: string; // Replaced to Temp
		PackageDLLOutputDir: string;
		PackageDCPOutputDir: string;
		SearchPath: string;
		Conditionals: string;
		DebugSourceDirs: string; // Not in cfg
//		TODO : UsePackages
    // Linker
    MinStackSize: UG;
    MaxStackSize: UG;
    ImageBase: UG;

    // Version Info
		Version: TProjectVersion;
    // Version Info Keys
		ProjectInfos: TProjectInfos;

    constructor Create;
    destructor Destroy; override;

    procedure ReadFromFile(const AFileName: TFileName);
    procedure ReplaceFromFile(const AFileName: TFileName);
    procedure Update;
    function GetOutputFile: TFileName;
	end;

implementation

uses
  uStrings, uMath, uOutputFormat,
  XMLDoc, XMLIntf, Variants, IniFiles,
  uInputFormat, uFiles, uMsg;

{ TProjectOptions }

procedure TProjectOptions.RWDproj(const AFileName: TFileName; const Save: BG);

	procedure ProcessNode(const Node: IXMLNode);
	const
		AttrName = 'Name';
		VerStr: array [TSubVersion] of string = ('MajorVer', 'MinorVer', 'Release', 'Build');
	var
		cNode: IXMLNode;
		Name: string;
		SubVersion: TSubVersion;
		p: TProjectInfoName;
	begin
		if Node = nil then
			Exit;

    // TODO : <DCC_
		if Node.NodeName = 'VersionInfo' then
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
							Version.SetSubVersion(SubVersion, Node.NodeValue);
						Break;
					end
				end;
			end;
		end
		else if Node.NodeName = 'VersionInfoKeys' then
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
									if not VarIsNull(Node.NodeValue) then
										ProjectInfos[p] := Node.NodeValue;
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
			BackupFile(AFileName);
		XML := TXMLDocument.Create(AFileName);
		XML.Active := True;
		try
			if XML.IsEmptyDoc then
				Exit;
			iNode := XML.DocumentElement.ChildNodes.First;
			while iNode <> nil do
			begin
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
	ProjectInfos[piLegalCopyright] := ReplaceF(ProjectInfos[piLegalCopyright], '%year%', NToS(GetActualYear, '0000'));

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
  MinStackSize := 16 * KB;
  MaxStackSize := 64 * KB;
  ImageBase := 4 * MB;
end;

destructor TProjectOptions.Destroy;
begin
  FreeAndNil(Version);
  inherited;
end;

function TProjectOptions.GetExecutableType(
  const AFileName: TFileName): TExecutableType;
var
  s: string;
  FileExt: string;
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
    if (LibraryPos < ProgramPos) then
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

procedure TProjectOptions.ReadProjectVersionFromDof(const AFileName: TFileName);
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

			OutputDir := IniFile.ReadString(Directories, 'OutputDir', OutputDir);
			UnitOutputDir := IniFile.ReadString(Directories, 'UnitOutputDir', UnitOutputDir);
			PackageDLLOutputDir := IniFile.ReadString(Directories, 'PackageDLLOutputDir', PackageDLLOutputDir);
			PackageDCPOutputDir := IniFile.ReadString(Directories, 'PackageDCPOutputDir', PackageDCPOutputDir);
			SearchPath := IniFile.ReadString(Directories, 'SearchPath', SearchPath);
			Conditionals := IniFile.ReadString(Directories, 'Conditionals', Conditionals);

			DebugSourceDirs := IniFile.ReadString(Directories, 'DebugSourceDirs', DebugSourceDirs);


			MinStackSize := IniFile.ReadInteger(Linker, 'MinStackSize', MinStackSize);
			MaxStackSize := IniFile.ReadInteger(Linker, 'MaxStackSize', MaxStackSize);
			ImageBase := IniFile.ReadInteger(Linker, 'ImageBase', ImageBase);
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

end.
