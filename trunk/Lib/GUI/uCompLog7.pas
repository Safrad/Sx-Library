unit uCompLog7;

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

uses uTypes, uMyAfterCompile;

var
	NotifierIndex: SG;

{ TAddInNotifier }

procedure TAddInNotifier.AfterCompile(Succeeded: Boolean);
begin
	//
end;

procedure TAddInNotifier.AfterCompile(Succeeded, IsCodeInsight: Boolean);
begin
	if ToolServices <> nil then
	begin
		MyAfterCompile(ToolServices.GetProjectName, Succeeded, IsCodeInsight);
	end;
end;

procedure TAddInNotifier.BeforeCompile(const Project: IOTAProject;
	IsCodeInsight: Boolean; var Cancel: Boolean);
begin
	//
end;

procedure TAddInNotifier.BeforeCompile(const Project: IOTAProject;
	var Cancel: Boolean);
begin
	//
end;

procedure TAddInNotifier.FileNotification(NotifyCode: TOTAFileNotification;
	const FileName: string; var Cancel: Boolean);
begin
	//
end;

initialization
	NotifierIndex := (BorlandIDEServices as IOTAServices50).AddNotifier(TAddInNotifier.Create);
finalization
	(BorlandIDEServices as IOTAServices50).RemoveNotifier(NotifierIndex);
end.
