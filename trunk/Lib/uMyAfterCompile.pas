unit uMyAfterCompile;

interface

uses uTypes;

procedure MyAfterCompile(const ProjectFileName: string; const Succeeded, IsCodeInsight: BG);

implementation

uses
	SysUtils,
	uStrings, uFiles, uCSVFile, uOutputFormat;

procedure MyAfterCompile(const ProjectFileName: string; const Succeeded, IsCodeInsight: BG);
var
	F: TextFile;
	ProjectDir, ProjectName: string;
	FileName: TFileName;
begin
	ProjectName := DelFileExt(ExtractFileName(ProjectFileName));
	if ProjectName = '' then Exit; // No opened project
	ProjectDir := ExtractFilePath(ProjectFileName);
	if ProjectDir = '' then Exit;

	FileName := ProjectDir + '_' + ProjectName + '-compile.csv';
	AssignFile(F, FileName);
	try
		if not FileExists(FileName) then
		begin
			Rewrite(F);
			WriteLn(F, CSVRemark + 'DateTime' + CSVSep + 'Succeeded' + CSVSep + 'IsCodeInsight');
		end
		else
			Append(F);
		WriteLn(F, DateTimeToS(Now, 3, ofIO) + CSVSep + IntToStr(SG(Succeeded)) + CSVSep + IntToStr(SG(IsCodeInsight)));
	finally
		CloseFile(F);
	end;
end;

end.
