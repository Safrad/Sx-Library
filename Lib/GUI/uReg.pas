unit uReg;

interface

uses
	uTypes,
	Windows;

type
	TFileTypesOperation = (foCreate, foDelete, foExists);

var
	MyDocuments: string; // User documnets (Read and Write)

function RootKeyToStr(const RootKey: HKEY): string;
function RegValue(const RootKey: HKEY; const Key: string; const Name: string): string;

function CustomFileType(
	const FileTypesOperation: TFileTypesOperation;
	const FileType, FileTypeCaption, Icon: string;
	const MenuCaptions: array of string;
	const OpenPrograms: array of string
	): Boolean;

function ShellFolder(const Name: string; const Common: BG = False): string;

implementation

uses
	SysUtils, Registry,
	uStrings, uFiles, uMsg, uCSVFile;

function WinNTDeleteKey(const Reg: TRegistry; const Key: string): Boolean;
	var CanDelete: Boolean;
begin
	Result := False;
	if Reg.OpenKey(Key, False) then
	begin
		CanDelete := not Reg.HasSubKeys;
		Reg.CloseKey;
		if CanDelete then
		begin
			Result := Reg.DeleteKey(Key);
		end
		else
		begin
			Warning('Can not delete key %1 with subkeys.', [Reg.CurrentPath + '\' + Key]);
		end;
	end;
end;

procedure CreateExt(const FileType, FileTypeCaption, Icon: string);
var
	Reg: TRegistry;
	InternalName, Key: string;
begin
	if FileType = '' then Exit;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		Key := '.' + FileType;
		if Reg.KeyExists(Key) then
		begin
			if Reg.OpenKeyReadOnly(Key) then
			begin
				InternalName := Reg.ReadString('');
				Reg.CloseKey;
			end;
		end;

		if InternalName = '' then
		begin
			InternalName := FileType + 'file';
			Reg.Access := KEY_SET_VALUE; // BUG: OpenKeyReadOnly change Access!
			if Reg.OpenKey(Key, True) then
			begin
				Reg.WriteString('', InternalName);
				Reg.CloseKey;
			end;
		end;
	finally
		Reg.Free;
	end;


	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;
		// Caption
		if FileTypeCaption <> '' then // Folder or no change
		begin
			Key := InternalName;
//      if (CanReplace) or (Reg.KeyExists(Key) = False) then
			begin
				if Reg.OpenKey(Key, True) then
				begin
					Reg.WriteString('', FileTypeCaption);
					Reg.CloseKey;
				end;
			end;
		end;
	finally
		Reg.Free;
	end;

	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;
		// Icon
		if Icon <> '' then
		begin
			Key := InternalName + '\DefaultIcon';
//      if (CanReplace) or (Reg.KeyExists(Key) = False) then
			begin
				if Reg.OpenKey(Key, True) then
				begin
					Reg.WriteString('', Icon);
					Reg.CloseKey;
				end;
			end;
		end;
	finally
		Reg.Free;
	end;

	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		Key := InternalName + '\shell';
		if not Reg.KeyExists(Key) then
		begin
			Reg.CreateKey(Key);
		end;

		// Commans
{   if OpenProgram <> '' then
		begin
			ProgramCaption := DelFileExt(ExtractFileName(OpenProgram));

			Key := InternalName + '\Shell\' + ProgramCaption;
			if not Reg.KeyExists(Key) then
			begin
				Flags := $00000001;
				Reg.OpenKey(Key, True);
				Reg.WriteBinaryData('EditFlags', Flags, 4);
				Reg.CloseKey;
			end;

			Key := InternalName + '\Shell\' + ProgramCaption + '\Command';
			if not Reg.KeyExists(Key) then
			begin
				Reg.OpenKey(Key, True);
				Reg.WriteString('', OpenProgram + ' "%1"'); // for Win, %1 for Dos !!!
				Reg.CloseKey;
			end;
		end;}
	finally
		Reg.Free;
	end;
end;

procedure DeleteExt(const FileType: string);
var
	Reg: TRegistry;
	InternalName, Key: string;
begin
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		Key := '.' + FileType;
		if Reg.KeyExists(Key) then
		begin
			if Reg.OpenKeyReadOnly(Key) then
			begin
				InternalName := Reg.ReadString('');
				Reg.CloseKey;
				if Reg.KeyExists(InternalName) then
				begin
					Reg.OpenKeyReadOnly(InternalName + '\shell');
					if Reg.HasSubKeys = False then
					begin
						Reg.CloseKey;
						if WinNTDeleteKey(Reg, InternalName + '\shell') then
						if WinNTDeleteKey(Reg, InternalName + '\DefaultIcon') then
						if WinNTDeleteKey(Reg, InternalName) then
						if Reg.KeyExists(Key) then
						begin
							WinNTDeleteKey(Reg, Key);
						end;
					end
					else
						Reg.CloseKey;
				end;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

function IsFolder(const FileType: string): BG;
begin
	Result := (FileType = 'Folder') or (FileType = 'Directory');
end;

function ExistsExt(const FileType: string): Boolean;
var
	Reg: TRegistry;
	InternalName, Key: string;
begin
	Result := False;
	if IsFolder(FileType) then
	begin
		Result := True;
		Exit;
	end;
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		Key := '.' + FileType;
		if Reg.KeyExists(Key) then
		begin
			if Reg.OpenKeyReadOnly(Key) then
			begin
				InternalName := Reg.ReadString('');
				Reg.CloseKey;
				if Reg.KeyExists(InternalName) then
				begin
					Result := True;
				end;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

procedure CreateCommand(const FileType: string;
	const MenuCaption: string; const OpenProgram: string);
var
	Reg: TRegistry;
	InternalName, Key, ShortMenuCaption: string;
	Flags: U4;
begin
	if MenuCaption = '' then Exit;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		if IsFolder(FileType) then
		begin
			InternalName := FileType;
		end
		else
		begin
			Key := '.' + FileType;
			if Reg.KeyExists(Key) then
			begin
				if Reg.OpenKeyReadOnly(Key) then
				begin
					InternalName := Reg.ReadString('');
					Reg.CloseKey;
				end;
			end
			else
			begin
				Exit;
			end;
		end;

		Reg.Free;
		Reg := TRegistry.Create;
		Reg.RootKey := HKEY_CLASSES_ROOT;

		if Reg.KeyExists(InternalName) then
		begin
			ShortMenuCaption := DelCharsF(MenuCaption, ' ');
			Key := InternalName + '\shell\' + ShortMenuCaption;
			if Reg.OpenKey(Key, True) then
			begin
				if ShortMenuCaption <> MenuCaption then
					Reg.WriteString('', MenuCaption);

				if IsFolder(FileType) then
				begin
					// Enable modification
					Flags := $00000001;
					Reg.WriteBinaryData('EditFlags', Flags, SizeOf(Flags));
				end;
				Reg.CloseKey;
			end;
			Reg.Free;
			Reg := TRegistry.Create;
			Reg.RootKey := HKEY_CLASSES_ROOT;

			Key := InternalName + '\shell\' + ShortMenuCaption + '\command';
			if Reg.OpenKey(Key, True) then
			begin
				Reg.WriteString('', OpenProgram);
				Reg.CloseKey;
			end;
		end;
	finally
		Reg.Free;
	end;
end;

procedure DeleteCommand(const FileType: string;
	const MenuCaption: string);
var
	Reg: TRegistry;
	InternalName, Key, ShortMenuCaption: string;
begin
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		if IsFolder(FileType) then
		begin
			InternalName := FileType;
		end
		else
		begin
			Key := '.' + FileType;
			if Reg.KeyExists(Key) then
			begin
				if Reg.OpenKeyReadOnly(Key) then
				begin
					InternalName := Reg.ReadString('');
					Reg.CloseKey;
				end;
			end
			else
				Exit;
		end;

		ShortMenuCaption := DelCharsF(MenuCaption, ' ');
		Key := InternalName + '\shell\' + ShortMenuCaption;
		if Reg.KeyExists(Key) then
		begin
			if WinNTDeleteKey(Reg, Key + '\command') then
				WinNTDeleteKey(Reg, Key);
		end;
	finally
		Reg.Free;
	end;
end;

function ExistsCommand(const FileType: string;
	const MenuCaption: string): Boolean;
var
	Reg: TRegistry;
	InternalName, Key, ShortMenuCaption: string;
begin
	Result := False;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_CLASSES_ROOT;

		if IsFolder(FileType) then
		begin
			InternalName := FileType;
		end
		else
		begin
			Key := '.' + FileType;
			if Reg.KeyExists(Key) then
			begin
				Reg.OpenKeyReadOnly(Key);
				InternalName := Reg.ReadString('');
				Reg.CloseKey;
			end
			else
				Exit;
		end;

		ShortMenuCaption := DelCharsF(MenuCaption, ' ');
		Key := InternalName + '\shell\' + ShortMenuCaption;
		if Reg.KeyExists(Key) then
		begin
			Result := True;
		end;
	finally
		Reg.Free;
	end;
end;

function CustomFileType(
	const FileTypesOperation: TFileTypesOperation;
	const FileType, FileTypeCaption, Icon: string;
	const MenuCaptions: array of string;
	const OpenPrograms: array of string
	): Boolean;
var
	i: SG;
begin
	Result := True;
{	for i := 0 to Length(MenuCaptions) - 1 do
	begin
		WriteStringToFile(WorkDir + 't.csv', CSVCell(FileType) + CSVSep + CSVCell(FileTypeCaption) + CSVSep + CSVCell(Icon) + CSVSep + CSVCell(MenuCaptions[i]) + CSVSep + CSVCell(OpenPrograms[i]) + FileSep, True);
	end;
	Exit;}

	if High(MenuCaptions) <> High(OpenPrograms) then
	begin
		Warning('Illegal filetype parameters.');
		Exit;
	end;
	if FileType = '' then
	begin
		Warning('Illegal file extension.');
		Exit;
	end;
	case FileTypesOperation of
	foCreate:
	begin
		CreateExt(FileType, FileTypeCaption, Icon);
		for i := 0 to High(MenuCaptions) do
		begin
			CreateCommand(FileType, MenuCaptions[i], OpenPrograms[i]);
		end;
	end;
	foDelete:
	begin
		for i := 0 to High(MenuCaptions) do
		begin
			DeleteCommand(FileType, MenuCaptions[i]);
		end;
		DeleteExt(FileType);
	end;
	foExists:
	begin
		if ExistsExt(FileType) = False then
			Result := False
		else
			for i := 0 to High(MenuCaptions) do
			begin
				if ExistsCommand(FileType, MenuCaptions[i]) = False then
				begin
					Result := False;
					Break;
				end;
			end;
	end;
	end;
end;

function RootKeyToStr(const RootKey: HKEY): string;
begin
{$ifdef CPUX64}
	if RootKey = HKEY_CLASSES_ROOT then
    Result := 'HKEY_CLASSES_ROOT'
  else if RootKey = HKEY_CURRENT_USER then
    Result := 'HKEY_CURRENT_USER'
  else if	RootKey = HKEY_LOCAL_MACHINE then
    Result := 'HKEY_LOCAL_MACHINE'
	else if	RootKey = HKEY_USERS then
    Result := 'HKEY_USERS'
	else if	RootKey = HKEY_PERFORMANCE_DATA then
    Result := 'HKEY_PERFORMANCE_DATA'
	else if	RootKey = HKEY_CURRENT_CONFIG then
    Result := 'HKEY_CURRENT_CONFIG'
	else if	RootKey = HKEY_DYN_DATA then
    Result := 'HKEY_DYN_DATA'
	else
    Result := '';
{$else}
	case RootKey of
	HKEY_CLASSES_ROOT: Result := 'HKEY_CLASSES_ROOT';
	HKEY_CURRENT_USER: Result := 'HKEY_CURRENT_USER';
	HKEY_LOCAL_MACHINE: Result := 'HKEY_LOCAL_MACHINE';
	HKEY_USERS: Result := 'HKEY_USERS';
	HKEY_PERFORMANCE_DATA: Result := 'HKEY_PERFORMANCE_DATA';
	HKEY_CURRENT_CONFIG: Result := 'HKEY_CURRENT_CONFIG';
	HKEY_DYN_DATA: Result := 'HKEY_DYN_DATA';
	else Result := '';
	end;
{$endif}
end;

function RegValue(const RootKey: HKEY; const Key: string; const Name: string): string;
var
	Reg: TRegistry;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		Reg.RootKey := RootKey;
		if Reg.KeyExists(Key) then
		begin
			Reg.OpenKeyReadOnly(Key);
			Result := Reg.ReadString(Name);
			Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

function ShellFolder(const Name: string; const Common: BG = False): string;
var
	Reg: TRegistry;
	Key: string;
begin
	Reg := TRegistry.Create(KEY_QUERY_VALUE);
	try
		if Common then
			Reg.RootKey := HKEY_LOCAL_MACHINE
		else
			Reg.RootKey := HKEY_CURRENT_USER;

		Key := 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders\';
		if Reg.KeyExists(Key) then
		begin
			Reg.OpenKeyReadOnly(Key);
			Result := Reg.ReadString(Name);
			CorrectDir(Result);
			Reg.CloseKey;
		end;
	finally
		Reg.Free;
	end;
end;

initialization
{$IFNDEF NoInitialization}
	MyDocuments := ShellFolder('Personal');
	CorrectDir(MyDocuments);
{$ENDIF NoInitialization}
end.
