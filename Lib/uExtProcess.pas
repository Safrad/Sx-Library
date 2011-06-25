//* File:     Lib\uExtProcess.pas
//* Created:  2008-07-26
//* Modified: 2008-07-20
//* Version:  1.1.41.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uExtProcess;

interface

uses
	uTypes, Windows;

(**
	@return process exit code if success or High(UG)
*)
function RunAndWaitForApplication(const CommandLine: string; const CurrentDir: string; const FWindowState: SG): UG;


var
	ProcessInfo: TProcessInformation;

implementation

uses
	uMsg, uLog,
	SysUtils;

function WaitForApplication(hProcess: UG): UG;
begin
	repeat
		Sleep(LoopSleepTime);
		Result := WaitForSingleObject(hProcess, LoopSleepTime);
	until not (Result = WAIT_TIMEOUT);
end;

function RunAndWaitForApplication(const CommandLine: string; const CurrentDir: string; const FWindowState: SG): UG;
var
	StartupInfo: TStartupInfo;
begin
	Result := High(Result);

	FillChar(StartupInfo, SizeOf(StartupInfo), 0);
	StartupInfo.cb := SizeOf(StartupInfo);
	StartupInfo.dwFlags := STARTF_USESHOWWINDOW or // STARTF_USEPOSITION or
		STARTF_USESIZE or STARTF_USECOUNTCHARS or STARTF_USEFILLATTRIBUTE;
//			StartupInfo.dwX := R.Left;
//			StartupInfo.dwY := R.Top;
			StartupInfo.dwXCountChars := 128;
			StartupInfo.dwYCountChars := 1024;
			StartupInfo.dwXSize := {Screen.Width;} StartupInfo.dwXCountChars * 8;
			StartupInfo.dwYSize := 400; // StartupInfo.dwYCountChars * 8;
			StartupInfo.dwFillAttribute := FOREGROUND_INTENSITY or BACKGROUND_BLUE;

	StartupInfo.wShowWindow := FWindowState;

	MainLog.Add('CreateProcess: ' + CommandLine, mlDebug);
	if CreateProcess(
		nil,
		PChar(CommandLine),
		nil,                
		nil,
		False,
		CREATE_NEW_CONSOLE or
		NORMAL_PRIORITY_CLASS,
		nil,
		PChar(CurrentDir),
		StartupInfo,
		ProcessInfo) then
	begin
		if WaitForApplication(ProcessInfo.hProcess) = WAIT_FAILED then
			IOError(CommandLine, GetLastError);
		if not GetExitCodeProcess(ProcessInfo.hProcess, Result) then
			IOError(CommandLine, GetLastError);
		MainLog.Add('ExitCode: ' + IntToStr(Result), mlDebug);
	end
	else
	begin
		IOError(CommandLine, GetLastError);
	end;
end;

end.
