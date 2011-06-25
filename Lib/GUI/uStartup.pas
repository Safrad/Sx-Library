// * File:     Lib\GUI\uStartup.pas
// * Created:  2009-05-11
// * Modified: 2009-10-12
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uStartup;

interface

uses uTypes;

type
	TObjectChange = (ocTest, ocCreate, ocRemove);

function RegisterStartup(const Params: string = ''): BG;
function UnregisterStartup: BG;
function IsRegisteredStartup: BG;

implementation

uses
	Windows, Registry,
	uProjectInfo, uFiles;
const
	RunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';

function RegExt(const ObjectChange: TObjectChange; Params: string = ''): BG;
var
	Reg: TRegistry;
begin
	Result := False;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CURRENT_USER;
		if Reg.OpenKey(RunKey, False) then
		begin
			case ObjectChange of
			ocTest:
			begin
				Result := Reg.ValueExists(GetProjectInfo(piInternalName));
			end;
			ocCreate:
			begin
				if Params = '' then
					Params := '-Minimized';
				Reg.WriteString(GetProjectInfo(piInternalName), '"' + ExeFileName + '" ' + Params);
				Result := True;
			end;
			ocRemove:
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

function RegisterStartup(const Params: string = ''): BG;
begin
	Result := RegExt(ocCreate, Params);
end;

function UnregisterStartup: BG;
begin
	Result := RegExt(ocRemove);
end;

function IsRegisteredStartup: BG;
begin
	Result := RegExt(ocTest);
end;

end.
