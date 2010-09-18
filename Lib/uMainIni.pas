// Build: 07/2000-07/2000 Author: Safranek David

unit uMainIni;

interface

uses uDIni;

procedure MainIniCreate;
procedure MainIniFree;

var
	MainIni: TDIniFile;

implementation

uses uFiles;

procedure MainIniCreate;
label LRetry;
{var
	Reg: TRegistry;
	F: file;
	ErrorCode: Integer;
	Key: string;}
begin
{	if Save = False then
	begin
		DefaultIniFileName := MainIniName DelFileExt(ExeFileName) + '.ini';
		IniFileName := DefaultIniFileName;
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Key := 'Software\SafranekDavid\' + Application.Title;
			if Reg.KeyExists(Key) then
			begin
				Reg.OpenKey(Key, False);

				if Reg.ValueExists('IniFile') then
					IniFileName := FullDir(Reg.ReadString('IniFile'));
				Reg.CloseKey;
			end;
		finally
			Reg.Free;
		end;
		LIniFileName := IniFileName;
	end;}
	MainIni := TDIniFile.Create(DelFileExt(ExeFileName) + '.ini');
end;

procedure MainIniFree;
begin
	MainIni.Free; MainIni := nil;
{	if FileStatus = fsClose then
	begin
		ErrorMessage('RWFree Not open');
		Exit;
	end;

	FileStatus := fsClose;}
{	if (IniFileName = DefaultIniFileName) then
	begin
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Reg.OpenKey('Software\SafranekDavid\' + Application.Title, True);
			Reg.DeleteValue('IniFile');
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end
	else if (IniFileName <> LIniFileName) then
	begin
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_CURRENT_USER;
			Reg.OpenKey('Software\SafranekDavid\' + Application.Title, True);
			Reg.WriteString('IniFile', ShortDir(IniFileName));
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end;}
end;

end.
