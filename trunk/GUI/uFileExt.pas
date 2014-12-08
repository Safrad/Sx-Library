unit uFileExt;

interface

uses
	uDForm,
	Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
	Dialogs, uDImage, uDView, Menus, uDWinControl;

type
	TfFileExt = class(TDForm)
		DViewFileExtensions: TDView;
		PopupMenuFE: TPopupMenu;
		Register1: TMenuItem;
		Unregister1: TMenuItem;
		N1: TMenuItem;
		RegisterAll1: TMenuItem;
		UnregisterAll1: TMenuItem;
		procedure Register1Click(Sender: TObject);
		procedure FormCreate(Sender: TObject);
		procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
		procedure PopupMenuFEPopup(Sender: TObject);
		procedure DViewFileExtensionsGetData(Sender: TObject; var Data: String;
			ColIndex, RowIndex: Integer; Rect: TRect);
		procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	private
		{ Private declarations }
		procedure Init(Sender: TObject);
	public
		{ Public declarations }
	end;

procedure AddFileType(const FileType, FileTypeCaption, Icon: string;
	const MenuCaptions: array of string; const OpenPrograms: array of string);

procedure FormFileExt;
procedure FreeFileExt;
procedure RegisterAllFileTypes;

implementation

{$R *.dfm}

uses uTypes, uReg, uDIniFile, uMenus, uOutputFormat, uStrings, uDictionary;

type
	TFileType = packed record // 24
		FileType, FileTypeCaption, Icon: string; // 12
		Exists: B4;
		MenuCaptions: array of string; // 4
		OpenPrograms: array of string; // 4
	end;

var
	FileTypes: array of TFileType;
	FileTypeCount: SG;

	fFileExt: TfFileExt;

procedure FormFileExt;
begin
	if not Assigned(fFileExt) then
		fFileExt := TfFileExt.Create(nil);
	fFileExt.DViewFileExtensions.RowCount := FileTypeCount;
	fFileExt.DViewFileExtensions.DataChanged;
	fFileExt.Init(nil);
	fFileExt.ShowModal;
end;

procedure FreeFileExt;
begin
	FileTypeCount := 0;
	SetLength(FileTypes, 0);
	FormFree(TForm(fFileExt));
end;

procedure AddFileType(const FileType, FileTypeCaption, Icon: string;
	const MenuCaptions: array of string; const OpenPrograms: array of string);

var
	j: SG;
begin
	SetLength(FileTypes, FileTypeCount + 1);
	FileTypes[FileTypeCount].FileType := FileType;
	FileTypes[FileTypeCount].FileTypeCaption := FileTypeCaption;
	FileTypes[FileTypeCount].Icon := Icon;
	SetLength(FileTypes[FileTypeCount].MenuCaptions, Length(MenuCaptions));
	for j := 0 to Length(MenuCaptions) - 1 do
		FileTypes[FileTypeCount].MenuCaptions[j] := MenuCaptions[j];

	SetLength(FileTypes[FileTypeCount].OpenPrograms, Length(OpenPrograms));
	for j := 0 to Length(OpenPrograms) - 1 do
		FileTypes[FileTypeCount].OpenPrograms[j] := OpenPrograms[j];
	Inc(FileTypeCount);
end;

procedure TfFileExt.Init(Sender: TObject);

var
	i: SG;
	ExistCount: SG;
begin
	ExistCount := 0;
	for i := 0 to FileTypeCount - 1 do
	begin
		FileTypes[i].Exists := CustomFileType(foExists, FileTypes[i].FileType,
			FileTypes[i].FileTypeCaption, FileTypes[i].Icon, FileTypes[i].MenuCaptions,
			FileTypes[i].OpenPrograms);
		if FileTypes[i].Exists then
			Inc(ExistCount);
	end;
	Caption := Translate('File Extensions') + CharSpace + NToS(ExistCount) + ' / ' + NToS
		(FileTypeCount);
end;

procedure RegisterAllFileTypes;
var
	i: SG;
begin
	for i := 0 to FileTypeCount - 1 do
	begin
		CustomFileType(foCreate, FileTypes[i].FileType,
			FileTypes[i].FileTypeCaption, FileTypes[i].Icon, FileTypes[i].MenuCaptions,
			FileTypes[i].OpenPrograms);
	end;
end;

procedure TfFileExt.Register1Click(Sender: TObject);
var
	i, Tg: SG;
begin
	Tg := TComponent(Sender).Tag;
	for i := 0 to FileTypeCount - 1 do
	begin
		if (Tg >= 2) or (DViewFileExtensions.SelectedRows[i]) then
			CustomFileType(TFileTypesOperation(Tg and 1), FileTypes[i].FileType,
				FileTypes[i].FileTypeCaption, FileTypes[i].Icon, FileTypes[i].MenuCaptions,
				FileTypes[i].OpenPrograms);
	end;
	Init(Sender);
	DViewFileExtensions.Invalidate;
end;

procedure TfFileExt.FormCreate(Sender: TObject);
begin
	Background := baNone;
	MenuSet(PopupMenuFE);
	DViewFileExtensions.AddColumn('Extension', DViewFileExtensions.CellWidth('Folder'));
	DViewFileExtensions.AddColumn('Description', 128);
	DViewFileExtensions.AddColumn('Icon', 128);
	DViewFileExtensions.AddColumn('Action Name', 128);
	DViewFileExtensions.AddColumn('Action Filename', 128);

	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, False);
		DViewFileExtensions.Serialize(MainIni, False);
	end;
end;

procedure TfFileExt.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
	if Assigned(MainIni) then
	begin
		MainIni.RWFormPos(Self, True);
		DViewFileExtensions.Serialize(MainIni, True);
	end;
end;

procedure TfFileExt.PopupMenuFEPopup(Sender: TObject);

var
	i: SG;
	C, E: BG;
begin
	i := DViewFileExtensions.ActualRow;
	if (i >= 0) and (i < FileTypeCount) then
	begin
		C := FileTypes[i].Exists;
		E := True;
	end
	else
	begin
		E := False;
		C := False;
	end;
	Register1.Enabled := E;
	Unregister1.Enabled := E;
	Register1.Checked := C;
	Unregister1.Checked := not C;
end;

procedure TfFileExt.DViewFileExtensionsGetData(Sender: TObject; var Data: String;
	ColIndex, RowIndex: Integer; Rect: TRect);
begin
	if (ColIndex <> 0) or FileTypes[RowIndex].Exists then
		DViewFileExtensions.Bitmap.Canvas.Font.Style := []
	else
		DViewFileExtensions.Bitmap.Canvas.Font.Style := [fsStrikeOut];
	case ColIndex of
	0:
		Data := FileTypes[RowIndex].FileType;
	1:
		Data := FileTypes[RowIndex].FileTypeCaption;
	2:
		Data := FileTypes[RowIndex].Icon;
	3:
		if Length(FileTypes[RowIndex].MenuCaptions) > 0 then
			Data := FileTypes[RowIndex].MenuCaptions[0];
	4:
		if Length(FileTypes[RowIndex].OpenPrograms) > 0 then
			Data := FileTypes[RowIndex].OpenPrograms[0];
	end;
end;

procedure TfFileExt.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
	if Key = VK_ESCAPE then
		Close;
end;

end.
