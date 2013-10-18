unit uRegistryClasses;

interface

uses
	Registry;

type
	TAccessPermission = (apCurrentUser, apLocalMachine, apClasses);

	TRegistryClasses = class
	private
		FAccessPermission: TAccessPermission;
		FRegistry: TRegistry;
		procedure SetAccessPermission(const Value: TAccessPermission);
		procedure Init;
		function Prefixed(const Key: string): string;
		procedure OpenOrCreateKey(const Key: string);
	public
		constructor Create;
		destructor Destroy; override;

		procedure CreateCommand(const FileType: string;
			const MenuCaption: string; const OpenProgram: string);
		procedure DeleteCommand(const FileType: string;
			const MenuCaption: string);
		function ExistsCommand(const FileType: string;
			const MenuCaption: string): Boolean;

		procedure Associate(const FileType, FileTypeCaption, Icon: string);
		procedure Dissociate(const FileType: string);
		function IsAssociated(const FileType: string): Boolean;

		property AccessPermission: TAccessPermission read FAccessPermission write SetAccessPermission;
	end;

implementation

uses
	uTypes, uStrings, uMsg,
	SysUtils,
	Windows;

function IsFolder(const FileType: string): BG;
begin
	Result := (FileType = 'Folder') or (FileType = 'Directory');
end;

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

{ TRegistryClasses }

procedure TRegistryClasses.Init;
begin
	if FRegistry = nil then
	begin
		FRegistry := TRegistry.Create;
		case AccessPermission of
		apCurrentUser: FRegistry.RootKey := HKEY_CURRENT_USER;
		apLocalMachine: FRegistry.RootKey := HKEY_LOCAL_MACHINE;
		apClasses: FRegistry.RootKey := HKEY_CLASSES_ROOT;
		end;
	end;
end;

procedure TRegistryClasses.SetAccessPermission(
	const Value: TAccessPermission);
begin
	FAccessPermission := Value;
	FreeAndNil(FRegistry);
end;

procedure TRegistryClasses.CreateCommand(const FileType: string;
	const MenuCaption: string; const OpenProgram: string);
var
	InternalName, Key0, Key, ShortMenuCaption: string;
	Flags: U4;
begin
	if MenuCaption = '' then Exit;

	Init;

	if IsFolder(FileType) then
	begin
		InternalName := FileType;
	end
	else
	begin
		Key := Prefixed('.' + FileType);
		if FRegistry.KeyExists(Key) then
		begin
			if FRegistry.OpenKey(Key, False) then
			begin
				InternalName := FRegistry.ReadString('');
				FRegistry.CloseKey;
			end;
		end
		else
		begin
			Exit;
		end;
	end;

	Key0 := Prefixed(InternalName);
	if FRegistry.KeyExists(Key0) then
	begin
		ShortMenuCaption := DelCharsF(MenuCaption, ' ');
		Key := Key0 + '\shell\' + ShortMenuCaption;
		OpenOrCreateKey(Key);
		if ShortMenuCaption <> MenuCaption then
			FRegistry.WriteString('', MenuCaption);

		if IsFolder(FileType) then
		begin
			// Enable modification
			Flags := $00000001;
			FRegistry.WriteBinaryData('EditFlags', Flags, SizeOf(Flags));
		end;
		FRegistry.CloseKey;

		Key := Prefixed(InternalName + '\shell\' + ShortMenuCaption + '\command');
		OpenOrCreateKey(Key);
		FRegistry.WriteString('', OpenProgram);
		FRegistry.CloseKey;
	end;
end;

procedure TRegistryClasses.DeleteCommand(const FileType: string;
	const MenuCaption: string);
var
	InternalName, Key, ShortMenuCaption: string;
begin
	Init;

	if IsFolder(FileType) then
	begin
		InternalName := FileType;
	end
	else
	begin
		Key := Prefixed('.' + FileType);
		if FRegistry.KeyExists(Key) then
		begin
			if FRegistry.OpenKey(Key, False) then
			begin
				InternalName := FRegistry.ReadString('');
				FRegistry.CloseKey;
			end;
		end
		else
			Exit;
	end;

	ShortMenuCaption := DelCharsF(MenuCaption, ' ');
	Key := Prefixed(InternalName + '\shell\' + ShortMenuCaption);
	if FRegistry.KeyExists(Key) then
	begin
		if WinNTDeleteKey(FRegistry, Key + '\command') then
			WinNTDeleteKey(FRegistry, Key);
	end;
end;

function TRegistryClasses.ExistsCommand(const FileType: string;
	const MenuCaption: string): Boolean;
var
	InternalName, Key, ShortMenuCaption: string;
begin
	Result := False;

	Init;

	if IsFolder(FileType) then
	begin
		InternalName := FileType;
	end
	else
	begin
		Key := Prefixed('.' + FileType);
		if FRegistry.KeyExists(Key) then
		begin
			FRegistry.OpenKey(Key, False);
			InternalName := FRegistry.ReadString('');
			FRegistry.CloseKey;
		end
		else
			Exit;
	end;

	ShortMenuCaption := DelCharsF(MenuCaption, ' ');
	Key := Prefixed(InternalName + '\shell\' + ShortMenuCaption);
	if FRegistry.KeyExists(Key) then
	begin
		Result := True;
	end;
end;

procedure TRegistryClasses.Associate(const FileType, FileTypeCaption, Icon: string);
var
	InternalName, Key: string;
begin
	if FileType = '' then Exit;

	Init;

	Key := Prefixed('.' + FileType);
	if FRegistry.KeyExists(Key) then
	begin
		if FRegistry.OpenKey(Key, False) then
		begin
			InternalName := FRegistry.ReadString('');
			FRegistry.CloseKey;
		end;
	end;

	if InternalName = '' then
	begin
		// New file type
		if IsFolder(FileType) then
			InternalName := FileType
		else
			InternalName := FileType + 'file';
		OpenOrCreateKey(Key);
		FRegistry.WriteString('', InternalName);
		FRegistry.CloseKey;
	end;

	// Caption
	if FileTypeCaption <> '' then // Folder or no change
	begin
		Key := Prefixed(InternalName);
//      if (CanReplace) or (Reg.KeyExists(Key) = False) then
		begin
			OpenOrCreateKey(Key);
			FRegistry.WriteString('', FileTypeCaption);
			FRegistry.CloseKey;
		end;
	end;

	// Icon
	if Icon <> '' then
	begin
		Key := Prefixed(InternalName + '\DefaultIcon');
//      if (CanReplace) or (Reg.KeyExists(Key) = False) then
		begin
			OpenOrCreateKey(Key);
			FRegistry.WriteString('', Icon);
			FRegistry.CloseKey;
		end;
	end;

	Key := Prefixed(InternalName + '\shell');
	if not FRegistry.KeyExists(Key) then
	begin
		FRegistry.CreateKey(Key);
	end;
end;

procedure TRegistryClasses.Dissociate(const FileType: string);
var
	InternalName, Key: string;
begin
	Init;

	Key := Prefixed('.' + FileType);
	if FRegistry.KeyExists(Key) then
	begin
		if FRegistry.OpenKey(Key, False) then
		begin
			InternalName := Prefixed(FRegistry.ReadString(''));
			FRegistry.CloseKey;
			if FRegistry.KeyExists(InternalName) then
			begin
				FRegistry.OpenKey(InternalName + '\shell', False);
				if FRegistry.HasSubKeys = False then
				begin
					FRegistry.CloseKey;
					if WinNTDeleteKey(FRegistry, InternalName + '\shell') then
					if WinNTDeleteKey(FRegistry, InternalName + '\DefaultIcon') then
					if WinNTDeleteKey(FRegistry, InternalName) then
					if FRegistry.KeyExists(Key) then
					begin
						WinNTDeleteKey(FRegistry, Key);
					end;
				end
				else
					FRegistry.CloseKey;
			end;
		end;
	end;
end;

function TRegistryClasses.IsAssociated(const FileType: string): Boolean;
var
	InternalName, Key: string;
begin
	Result := False;
	if IsFolder(FileType) then
	begin
		Result := True;
		Exit;
	end;

	Init;

	Key := Prefixed('.' + FileType);
	if FRegistry.KeyExists(Key) then
	begin
		if FRegistry.OpenKey(Key, False) then
		begin
			InternalName := Prefixed(FRegistry.ReadString(''));
			FRegistry.CloseKey;
			if FRegistry.KeyExists(InternalName) then
			begin
				Result := True;
			end;
		end;
	end;
end;

constructor TRegistryClasses.Create;
begin
	FAccessPermission := apCurrentUser;
end;

destructor TRegistryClasses.Destroy;
begin
	FreeAndNil(FRegistry);
end;

function TRegistryClasses.Prefixed(const Key: string): string;
begin
	case FAccessPermission of
	apClasses: Result := Key;
	else Result := 'Software\Classes\' + Key;
	end;
end;

procedure TRegistryClasses.OpenOrCreateKey(const Key: string);
begin
	if not FRegistry.OpenKey(Key, True) then
		raise EPrivilege.Create('Can not create registry key. Please run program as an administrator.'); 
end;

end.
