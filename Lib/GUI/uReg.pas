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
	uRegistryClasses,
	SysUtils, Registry,
	uStrings, uFiles, uMsg, uCSVFile;

function CustomFileType(
	const FileTypesOperation: TFileTypesOperation;
	const FileType, FileTypeCaption, Icon: string;
	const MenuCaptions: array of string;
	const OpenPrograms: array of string
	): Boolean;
var
	i: SG;
	RegistryClasses: TRegistryClasses;
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

	RegistryClasses := TRegistryClasses.Create;
	try
		case FileTypesOperation of
		foCreate:
		begin
			RegistryClasses.Associate(FileType, FileTypeCaption, Icon);
			for i := 0 to High(MenuCaptions) do
			begin
				RegistryClasses.CreateCommand(FileType, MenuCaptions[i], OpenPrograms[i]);
			end;
		end;
		foDelete:
		begin
			for i := 0 to High(MenuCaptions) do
			begin
				RegistryClasses.DeleteCommand(FileType, MenuCaptions[i]);
			end;
			RegistryClasses.Dissociate(FileType);
		end;
		foExists:
		begin
			if RegistryClasses.IsAssociated(FileType) = False then
				Result := False
			else
				for i := 0 to High(MenuCaptions) do
				begin
					if RegistryClasses.ExistsCommand(FileType, MenuCaptions[i]) = False then
					begin
						Result := False;
						Break;
					end;
				end;
		end;
		end;
	finally
		RegistryClasses.Free;
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
{$ENDIF NoInitialization}
end.
