unit uDof2Cfg;

interface

uses
	SysUtils;

type
	TDof = record
		// Directories
		OutputDir: string;
		UnitOutputDir: string; // Replaced to Temp
		PackageDLLOutputDir: string;
		PackageDCPOutputDir: string;
		SearchPath: string;
		Conditionals: string;
		DebugSourceDirs: string; // Not in cfg
//		UsePackages // TODO
	end;

function GetProjectInfoFromDof(const DofFileName: TFileName): TDof;
procedure DofToCfg(const FileNamePrefix: string);

implementation

uses
	uTypes,
	uStrings,
	uFiles,
	uFile,
	Registry,
	Windows,
	IniFiles,
	uMsg;

function GetProjectInfoFromDof(const DofFileName: TFileName): TDof;
const
	Section = 'Version Info';
	Directories = 'Directories';
var
	IniFile: TIniFile;
begin
	if FileExists(DofFileName) then
	begin
		{$ifdef Console}
		Information('Reading file %1.', [DofFileName]);
		{$endif}
		IniFile := TIniFile.Create(DofFileName);
		try
			Result.OutputDir := IniFile.ReadString(Directories, 'OutputDir', '');
			Result.UnitOutputDir := IniFile.ReadString(Directories, 'UnitOutputDir', '');
			Result.PackageDLLOutputDir := IniFile.ReadString(Directories, 'PackageDLLOutputDir', '');
			Result.PackageDCPOutputDir := IniFile.ReadString(Directories, 'PackageDCPOutputDir', '');
			Result.SearchPath := IniFile.ReadString(Directories, 'SearchPath', '');
			Result.Conditionals := IniFile.ReadString(Directories, 'Conditionals', '');
			Result.DebugSourceDirs := IniFile.ReadString(Directories, 'DebugSourceDirs', '');

{			Result.Version.Major := IniFile.ReadInteger(Section, 'MajorVer', 0);
			Result.Version.Minor := IniFile.ReadInteger(Section, 'MinorVer', 0);
			Result.Version.Release := IniFile.ReadInteger(Section, 'Release', 0);
			Result.Version.Build := IniFile.ReadInteger(Section, 'Build', 0);
			Result.ProjectInfos[piFileDescription] := IniFile.ReadString
				('Version Info Keys', 'FileDescription', '');}
		finally
			IniFile.Free;
		end;
	end
	else
	begin
		Warning('Dof file %1 not found!', [DofFileName]);
{		Result.Version.Major := 0;
		Result.Version.Minor := 0;
		Result.Version.Release := 0;
		Result.Version.Build := 0;
		Result.ProjectInfos[piFileDescription] := '';}
		Halt(1);
	end;
end;

// TODO Replace Dir

procedure WriteProjectInfoToCfg(const CfgFileName: TFileName; const ProjectInfo: TDof);
var
	Data: string;
begin
	{$ifdef Console}
	Information('Writing file %1.', [CfgFileName]);
	{$endif}
	Data := '';
	if ProjectInfo.OutputDir <> '' then
		Data := Data + '-E"' + ProjectInfo.OutputDir + '"' + FileSep;
//-LE"c:\program files\borland\delphi6\Projects\Bpl"
//-LN"c:\program files\borland\delphi6\Projects\Bpl"

	if ProjectInfo.UnitOutputDir <> '' then
		Data := Data + '-N"' + ProjectInfo.UnitOutputDir + '"' + FileSep;
	if ProjectInfo.PackageDLLOutputDir <> '' then
		Data := Data + '-LE"' + ProjectInfo.PackageDLLOutputDir + '"' + FileSep;
	if ProjectInfo.PackageDCPOutputDir <> '' then
		Data := Data + '-LN"' + ProjectInfo.PackageDCPOutputDir + '"' + FileSep;

	if ProjectInfo.SearchPath <> '' then
		Data := Data + '-U"' + ProjectInfo.SearchPath + '"' + FileSep;
	if ProjectInfo.SearchPath <> '' then
		Data := Data + '-O"' + ProjectInfo.SearchPath + '"' + FileSep;
	if ProjectInfo.SearchPath <> '' then
		Data := Data + '-I"' + ProjectInfo.SearchPath + '"' + FileSep;
	if ProjectInfo.SearchPath <> '' then
		Data := Data + '-R"' + ProjectInfo.SearchPath + '"' + FileSep;

	if ProjectInfo.Conditionals <> '' then
		Data := Data + '-D' + ProjectInfo.Conditionals + FileSep;
	WriteStringToFile(CfgFileName, Data, False, fcAnsi);
	{$ifdef Console}
	Information('Done.');
	{$endif}
end;

procedure DofToCfg(const FileNamePrefix: string);
var
	ProjectInfo: TDof;
begin
	ProjectInfo := GetProjectInfoFromDof(FileNamePrefix + '.dof');
	WriteProjectInfoToCfg(FileNamePrefix + '.cfg', ProjectInfo);
end;

end.
