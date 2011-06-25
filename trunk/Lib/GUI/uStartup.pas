//* File:     Lib\GUI\uStartup.pas
//* Created:  2009-05-11
//* Modified: 2009-05-13
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uStartup;

interface

uses uTypes;

function RegisterStartup: BG;
function UnregisterStartup: BG;
function IsRegisteredStartup: BG;

implementation

uses
	Windows, Registry,
	uProjectInfo, uFiles;
const
	RunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';

function RegExt(const T: SG): BG;
var
	Reg: TRegistry;
begin
	Result := False;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		if Reg.OpenKey(RunKey, False) then
		begin
			case T of
			0:
			begin
				Result := Reg.ValueExists(GetProjectInfo(piInternalName));
			end;
			1:
			begin
				Reg.WriteString(GetProjectInfo(piInternalName), '"' + ExeFileName + '" -Minimized');
				Result := True;
			end;
			2:
			begin
				if Reg.ValueExists(GetProjectInfo(piInternalName)) then
				begin
					Result := Reg.DeleteValue(GetProjectInfo(piInternalName));
				end;
			end;
			end;
			Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

function RegisterStartup: BG;
begin
	Result := RegExt(1);
end;

function UnregisterStartup: BG;
begin
	Result := RegExt(2);
end;

function IsRegisteredStartup: BG;
begin
	Result := RegExt(0);
end;

end.
