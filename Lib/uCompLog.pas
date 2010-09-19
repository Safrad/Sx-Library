unit uCompLog;

interface

uses
	Windows, Messages, SysUtils, Classes,
	ExptIntf, ToolIntf, Menus, ToolsAPI;

type
	TAddInNotifier = class(TNotifierObject, IOTAIDENotifier, IOTAIDENotifier50)
	public
		procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean); overload;
		procedure BeforeCompile(const Project: IOTAProject; IsCodeInsight: Boolean; var Cancel: Boolean); overload;
		procedure AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean); overload;
		procedure AfterCompile(Succeeded: Boolean); overload;
		procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: string; var Cancel: Boolean);
	end;

implementation

uses uTypes, uFormat, uStrings;

procedure TAddInNotifier.AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
var
	F : TextFile;
	ProjectDir, S, ProjectName : string;
	FileName: TFileName;
	CompileC: SG;
begin
	ProjectName := ToolServices.GetProjectName;
	if ProjectName = '' then Exit; // No opened project
	ProjectDir := ExtractFilePath(ProjectName);
	if ProjectDir = '' then Exit;

	// Compile Count
	FileName := ProjectDir + 'CompileC.log';
	if FileExists(FileName) then
	begin
		AssignFile(F, FileName);
		Reset(F);
		Read(F, S);
		CloseFile(F);
		CompileC := StrToInt(S);
	end
	else
		CompileC := 0;
	Inc(CompileC);
	AssignFile(F, FileName);
	Rewrite(F);
	Write(F, IntToStr(CompileC));
	CloseFile(F);

	// Compile History
	FileName := ProjectDir + 'CompileH.log';
	AssignFile(F, FileName);
	if not FileExists(FileName) then
		Rewrite(F)
	else
		Append(F);
	WriteLn(F, DateTimeToS(Now, 3) + CharTab + 'Succeeded: ' + FalseTrue[SG(Succeeded)] + ', IsCodeInsight: ' + FalseTrue[SG(IsCodeInsight)]);
	CloseFile(F);
end;

procedure TAddInNotifier.AfterCompile(Succeeded: Boolean);
begin
	//
end;

procedure TAddInNotifier.BeforeCompile(const Project: IOTAProject;
	var Cancel: Boolean);
begin
	//
end;

procedure TAddInNotifier.BeforeCompile(const Project: IOTAProject;
	IsCodeInsight: Boolean; var Cancel: Boolean);
begin
	//
end;

procedure TAddInNotifier.FileNotification(NotifyCode: TOTAFileNotification;
	const FileName: string; var Cancel: Boolean);
begin
	//
end;

var
	NotifierIndex: Integer;

initialization
	NotifierIndex := (BorlandIDEServices as IOTAServices50).AddNotifier(TAddInNotifier.Create);
finalization
	(BorlandIDEServices as IOTAServices50).RemoveNotifier(NotifierIndex);
end.
