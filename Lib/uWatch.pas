//* File:     Lib\uWatch.pas
//* Created:  1998-07-01
//* Modified: 2007-11-25
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uWatch;

interface

uses
	uTypes,
	SysUtils, Messages;

type
	TWatchFileChanged = procedure(const FileName: TFileName);
	TWatchFileChangedEx = procedure(const FileName: TFileName) of Object;

procedure WatchAddFile(const FileName: TFileName; OnChange: TWatchFileChanged); overload;
procedure WatchAddFile(const FileName: TFileName; OnChange: TWatchFileChangedEx); overload;
procedure WatchChange(const FileName: TFileName; const Changed: BG);
procedure WatchRemoveFile(const FileName: TFileName);

implementation

uses
	Windows, Controls, Forms,
	uFiles, uStrings, uMsg, uMath, uData, uOutputFormat;

type
	PWatchedFile = ^TWatchedFile;
	TWatchedFile = packed record
		LastWriteTime: TFileTime; // 8
		FileName: TFileName; // 4
		Changed: B4; // 4
		OnChange: TWatchFileChanged;
	end;
var
	WatchedFiles: TData;
	ActivateCount: UG;

type
	TOb = class(TObject)
		function AppProc(var Message: TMessage): BG;
	end;

function TOb.AppProc(var Message: TMessage): BG;
var
	i: SG;
	LastWriteTime: TFileTime;
	YesToAll, Reload: BG;
	s: string;
	WatchedFile: PWatchedFile;
begin
	Result := False;
	case Message.Msg of
	CM_ACTIVATE:
	begin
		Inc(ActivateCount); if ActivateCount > 2 then ActivateCount := 2;
		if ActivateCount = 1 then
		begin
			while ActivateCount > 0 do
			begin
				YesToAll := False;
				for i := 0 to WatchedFiles.Count - 1 do
				begin
					WatchedFile := WatchedFiles[i];
					if FileExists(WatchedFile.FileName) and GetFileModified(WatchedFile.FileName, LastWriteTime) then
					begin
						if U8(WatchedFile.LastWriteTime) = 0 then
							WatchedFile.LastWriteTime := LastWriteTime
						else if U8(LastWriteTime) <> U8(WatchedFile.LastWriteTime) then
						begin // Modified outside
							WatchedFile.LastWriteTime := LastWriteTime;
							Reload := False;
							if YesToAll then
								Reload := True
							else
							begin
								s := WatchedFile.FileName + LineSep + 'has been modified ' + DateTimeToS(FileTimeToDateTime(LastWriteTime), 0, ofDisplay) + ' outside of the application';
								if WatchedFile.Changed then
									s := s + ' and with the application';
								s := s + LineSep + 'Reload it?';
								case Confirmation(s, [mbYes, mbNo, mbYesToAll, mbNoToAll]) of
								mbYes: Reload := True;
								mbYesToAll:
								begin
									Reload := True;
									YesToAll := True;
								end;
								mbNoToAll: Break;
								end;
							end;
							if Reload then
								try
									if Assigned(WatchedFile.OnChange) then
										WatchedFile.OnChange(WatchedFile.FileName);
								finally

								end;
						end;
					end;
				end;
				Dec(ActivateCount);
			end;
		end;
	end;
	end;
end;

var
	Initialized: BG;
	Ob: TOb;

procedure Initialize;
begin
	Initialized := True;
	Ob := TOb.Create;
//	if (not (csDesigning in ComponentState)) then
	Application.HookMainWindow(Ob.AppProc);
end;

procedure Finalize;
begin
	if Initialized then
	begin
		Initialized := False;
//		if (not (csDesigning in ComponentState)) then
		Application.UnHookMainWindow(Ob.AppProc);
		FreeAndNil(Ob);
	end;
end;

procedure WatchAddFile(const FileName: TFileName; OnChange: TWatchFileChanged);
var WatchedFile: PWatchedFile;
begin
	if not Initialized then
		Initialize;
	WatchedFile := WatchedFiles.Add;
	WatchedFile.FileName := FileName;
	WatchedFile.Changed := False;
	WatchedFile.OnChange := OnChange;
	if FileExists(WatchedFile.FileName) then
		GetFileModified(WatchedFile.FileName, WatchedFile.LastWriteTime)
	else
		U8(WatchedFile.LastWriteTime) := 0;
end;

procedure WatchAddFile(const FileName: TFileName; OnChange: TWatchFileChangedEx);
//var
//	OnChangeEx: TWatchFileChanged;
begin
// TODO:
//	OnChangeEx := OnChange;
//	WatchAddFile(FileName, OnChangeEx);
end;

procedure WatchChange(const FileName: TFileName; const Changed: BG);
var i: SG;
begin
	i := 0;
	while i < WatchedFiles.Count do
	begin
		if PWatchedFile(WatchedFiles[i]).FileName = FileName then // TODO Upcase
		begin
			PWatchedFile(WatchedFiles[i]).Changed := Changed;
		end;
		Inc(i);
	end;
end;

procedure WatchRemoveFile(const FileName: TFileName);
var i: SG;
begin
	i := 0;
	while i < WatchedFiles.Count do
	begin
		if PWatchedFile(WatchedFiles[i]).FileName = FileName then // TODO Upcase
		begin
			WatchedFiles.Delete(i);
			Break;
		end;
		Inc(i);
	end;
end;

initialization
	WatchedFiles := TData.Create;
	WatchedFiles.ItemSize := SizeOf(TWatchedFile);
finalization
	Finalize;
	FreeAndNil(WatchedFiles);
end.
