unit uCommon;

interface

uses uWatch, uDForm;

{
Preconditions:
	MainMenu1 with submenus File1, Options1, Window1 and Help1

Usability in "Project file" (*.dpr):

begin
	Application.Title := '...';
	Application.Initialize;
	CommonCreate(nil);
	Application.CreateForm(TfMain, fMain);
	CommonForm(fMain);
	Application.Run;
	CommonFree;
end.
}

procedure CommonCreate(ReloadIni: TWatchFileChanged);
procedure CommonForm(const Form: TDForm);
procedure CommonFree;

implementation

uses
	uTypes, uDIni, uSplash, uMenus, MultiIns, uFiles, uAbout, uLog, uSounds, uFileExt,
	Classes, Menus, Windows, ExtCtrls;

var
	ViewSplashScreen: BG;

procedure CommonCreate(ReloadIni: TWatchFileChanged);
begin
	InitInstance;
	if (FirstInst = False) then
	begin
		// TODO: Send command line arguments to first instance
		// SendMessage();
		Halt(1);
	end;
	InitializeLog;
	MainIniCreate;
	WatchAddFile(MainIniFileName, ReloadIni);
	AboutRW(False);
	ViewSplashScreen := MainIni.ReadBool('Options', 'ViewSplashScreen', True);
	if ViewSplashScreen then
		ShowSplashScreen;
end;

procedure CommonForm(const Form: TDForm);
var
	i: SG;
	MainMenu1: TMainMenu;
begin
	MainMenu1 := nil;
	if Form <> nil then
		for i := 0 to Form.ComponentCount - 1 do
		begin
			if Form.Components[i] is TMenu then
			begin
				if Form.Components[i].Name = 'MainMenu1' then
				begin
					MainMenu1 := TMainMenu(Form.Components[i]);
					CommonFileMenu(TMenu(Form.Components[i]));
				end;
				MenuSet(Form.Components[i]);
			end;
		end;

	if MainMenu1 <> nil then
		for i := 0 to Form.ComponentCount - 1 do
		begin
			if (Form.Components[i] is TPanel) and (Form.Components[i].Name = 'PanelTool') then
				IconsFromMenu(MainMenu1, TPanel(Form.Components[i]));
		end;

	HideSplashScreen;
end;

procedure CommonFree;
begin
	WatchRemoveFile(MainIniFileName);
	AboutRW(True);
//	MainIni.WriteBool('Options', 'ViewSplashScreen', ViewSplashScreen);
	FreeSounds;
	FreeFileExt;
//	MainIniFree;
end;

end.
