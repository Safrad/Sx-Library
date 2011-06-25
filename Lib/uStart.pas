//* File:     Lib\uStart.pas
//* Created:  2007-05-22
//* Modified: 2007-05-27
//* Version:  1.1.37.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uStart;

interface

uses uTypes, uDIniFile;

function GetRunCount: UG;
function RunFirstTime: BG;
function GetRunTime: U8;
function GetStartProgramTime: UG;
procedure RWStart(const MainIni: TDIniFile; const Save: BG);

implementation

uses
	uSimulation, uFiles,
	Windows;

var
	GRunCount: UG;
	GRunTime: U8;
	GStartProgramTime: U4;

	GRunProgramTime: U8;

function GetRunCount: UG;
begin
	Result := GRunCount;
end;

function RunFirstTime: BG;
begin
	Assert(GRunCount > 0);
	Result := GRunCount <= 1;
end;

function GetRunTime: U8;
begin
	Result := GRunTime;
end;

function GetStartProgramTime: UG;
begin
	Result := GStartProgramTime;
end;

procedure RWStart(const MainIni: TDIniFile; const Save: BG);
const
	Section = 'Statistics';
begin
	if Save then
		GRunTime := U8(TimeDifference(GetTickCount, GStartProgramTime)) + GRunProgramTime;

	if Assigned(MainIni) then
	begin
		MainIni.RWNum(Section, 'RunCount', GRunCount, Save);
		MainIni.RWNum(Section, 'RunTime', GRunTime, Save);
		MainIni.RWNum(Section, 'ReadCount', ReadCount, Save);
		MainIni.RWNum(Section, 'ReadBytes', ReadBytes, Save);
		MainIni.RWNum(Section, 'WriteCount', WriteCount, Save);
		MainIni.RWNum(Section, 'WriteBytes', WriteBytes, Save);
		if Save = False then
		begin
			Inc(GRunCount);
//			StartProgramTime := GetTickCount;
			GRunProgramTime := GRunTime;
		end;
	end;
end;

initialization
	GStartProgramTime := GetTickCount;
end.
