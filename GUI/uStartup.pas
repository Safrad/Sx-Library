unit uStartup;

interface

uses
  uTypes;

const
  MinimizedParameter = '-minimized';

type
	TObjectChange = (ocTest, ocCreate, ocRemove);

function RegisterStartup(const AParameters: string = MinimizedParameter): BG;
function UnregisterStartup: BG;
function IsRegisteredStartup: BG;

implementation

uses
	Winapi.Windows, System.Win.Registry,
	uProjectInfo, uSystemPaths, uStrings;

const
	RunKey = 'Software\Microsoft\Windows\CurrentVersion\Run';

function RegExt(const ObjectChange: TObjectChange; const AParameters: string = ''): BG;
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
				Reg.WriteString(GetProjectInfo(piInternalName), JoinFileNameAndParameters(SystemPaths.ExeFileName, AParameters));
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

function RegisterStartup(const AParameters: string = MinimizedParameter): BG;
begin
	Result := RegExt(ocCreate, AParameters);
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
