//* File:     Lib\GUI\uCompLog.pas
//* Created:  1998-01-01
//* Modified: 2007-05-07
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

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

uses uTypes, uOutputFormat, uStrings, uCSVFile;

procedure TAddInNotifier.AfterCompile(Succeeded: Boolean; IsCodeInsight: Boolean);
var
	F : TextFile;
	ProjectDir, ProjectName : string;
	FileName: TFileName;
//	CompileC: SG;
begin
	if ToolServices <> nil then
	begin
		ProjectName := ToolServices.GetProjectName;
		if ProjectName = '' then Exit; // No opened project
		ProjectDir := ExtractFilePath(ProjectName);
		if ProjectDir = '' then Exit;

		// Compile Count
{		FileName := ProjectDir + 'CompileC.log';
		if FileExists(FileName) then
		begin
			AssignFile(F, FileName);
			try
				Reset(F);
				Read(F, S);
			finally
				CloseFile(F);
			end;
			CompileC := StrToInt(S);
		end
		else
			CompileC := 0;
		Inc(CompileC);
		AssignFile(F, FileName);
		try
			Rewrite(F);
			Write(F, IntToStr(CompileC));
		finally
			CloseFile(F);
		end;}

		// Compile History
{		FileName := ProjectDir + 'CompileH.log';
		AssignFile(F, FileName);
		if not FileExists(FileName) then
			Rewrite(F)
		else
			Append(F);
		WriteLn(F, DateTimeToS(Now, 3, ofIO) + CharTab + 'Succeeded: ' + FalseTrue[SG(Succeeded)] + ', IsCodeInsight: ' + FalseTrue[SG(IsCodeInsight)]);
		CloseFile(F);}
		FileName := ProjectDir + '_Compile.csv';
		AssignFile(F, FileName);
		if not FileExists(FileName) then
		begin
			Rewrite(F);
			WriteLn(F, CSVRemark + 'DateTime' + CSVSep + 'Succeeded' + CSVSep + 'IsCodeInsight');
		end
		else
			Append(F);
		WriteLn(F, DateTimeToS(Now, 3, ofIO) + CSVSep + IntToStr(SG(Succeeded)) + CSVSep + IntToStr(SG(IsCodeInsight)));
		CloseFile(F);
	end;
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
