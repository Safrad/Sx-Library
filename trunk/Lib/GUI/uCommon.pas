//* File:     Lib\GUI\uCommon.pas
//* Created:  2004-01-06
//* Modified: 2009-05-11
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uCommon;

interface

uses
	uTypes, uDForm,
	Menus;

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
	// Free forms
end.
}

procedure CommonCreate(const Special: BG = False);
procedure CommonForm(const Form: TDForm);
procedure CommonFree;

procedure CommonFileMenu(const Menu: TMenu);

implementation

uses
	uDIniFile, uSplash, uMenus, uMultiIns, uFiles, uAbout, uLog, uSounds, uFileExt, uParams, uAPI, uMsgDlg, uMsg, uStart,
	uStrings, uWebUpdate, uStartup,
	Classes, Windows, ExtCtrls, Forms, SysUtils;

type
	TCommonMenu = class(TObject)
	private
		AutomaticallyCheckForUpdate1: TMenuItem;
		RegisterStartup1: TMenuItem;
		ShowSplashScreen1: TMenuItem;
		LoggingLevel1: TMenuItem;
		procedure Exit1Click(Sender: TObject);
		procedure LocalHomepage1Click(Sender: TObject);
		procedure WebHomepage1Click(Sender: TObject);
		procedure ViewMessages1Click(Sender: TObject);
		procedure ViewParams1Click(Sender: TObject);
		procedure CheckForUpdate1Click(Sender: TObject);
		procedure AutomaticallyCheckForUpdate1Click(Sender: TObject);
		procedure About1Click(Sender: TObject);
		procedure ShowSplashScreen1Click(Sender: TObject);
		procedure RegisterStartup1Click(Sender: TObject);
		procedure ViewIniFile1Click(Sender: TObject);
		procedure ViewLogFile1Click(Sender: TObject);
		procedure ViewAllLogFiles1Click(Sender: TObject);
		procedure Sounds1Click(Sender: TObject);
		procedure SetLoggingLevel1Click(Sender: TObject);
	public
		procedure RWCommon(const Save: BG);
	end;
var
	CommonMenu: TCommonMenu;
var
	AutomaticallyCheckForUpdate: BG;

procedure CommonCreate(const Special: BG = False);
begin
	if not Special then
	begin
		InitInstance;
		InitializeLog;
	end;
	MainIni := TDIniFile.Create(MainIniFileName);
	MainIni.RegisterRW(CommonMenu.RWCommon);
	if not Special then
	begin
		if ViewSplashScreen then
			ShowSplashScreen;
		if AutomaticallyCheckForUpdate then
			CheckForUpdate(False);
	end;
end;

procedure CommonForm(const Form: TDForm);
var
	i: SG;
	Menu: TMenu;
begin
	ReadCommandLine(GetCommandLine);

	Menu := nil;
	if Form <> nil then
	begin
		for i := 0 to Form.ComponentCount - 1 do
		begin
			if Form.Components[i] is TMainMenu then
			begin
				Menu := TMainMenu(Form.Components[i]);
				Break;
			end;
		end;
		if Menu = nil then
			for i := 0 to Form.ComponentCount - 1 do
			begin
				if Form.Components[i] is TPopupMenu then
				begin
					Menu := TMainMenu(Form.Components[i]);
					Break;
				end;
			end;
	end;

	if Menu <> nil then
	begin
		CommonFileMenu(Menu);
		MenuSet(Menu);
		for i := 0 to Form.ComponentCount - 1 do
		begin
			if (Form.Components[i] is TPanel) and (Form.Components[i].Name = 'PanelTool') then
			begin
				IconsFromMenu(Menu, TPanel(Form.Components[i]));
				Break;
			end;
		end;
	end;

	HideSplashScreen;
end;

procedure CommonFree;
begin
	if MainIni <> nil then
		MainIni.UnregisterRW(CommonMenu.RWCommon);
	FreeSounds;
	FreeFileExt;
	Application.MainForm.Free; // Do not use FreeAndNil
	FreeAndNil(MainIni);
	FreeAndNil(MainLog);
end;

{ TCommonMenu }

procedure TCommonMenu.RWCommon(const Save: BG);
const
	Section = 'Options';
begin
	RWStart(MainIni, Save);
	if Save = False then ViewSplashScreen := True;
	MainIni.RWBool(Section, 'ViewSplashScreen', ViewSplashScreen, Save);
	if Save = False then AutomaticallyCheckForUpdate := True;
	MainIni.RWBool(Section, 'AutomaticallyCheckForUpdate', AutomaticallyCheckForUpdate, Save);
end;

procedure TCommonMenu.Exit1Click(Sender: TObject);
begin
	if Assigned(Application.MainForm) then
		Application.MainForm.Close;
end;

procedure TCommonMenu.WebHomepage1Click(Sender: TObject);
begin
	OpenWebHomepage;
end;

procedure TCommonMenu.LocalHomepage1Click(Sender: TObject);
begin
	OpenLocalHomepage;
end;

procedure TCommonMenu.ViewMessages1Click(Sender: TObject);
begin
	ShowMessages;
end;

procedure TCommonMenu.ViewParams1Click(Sender: TObject);
begin
	HelpParams;
end;

procedure TCommonMenu.CheckForUpdate1Click(Sender: TObject);
begin
	CheckForUpdate;
end;

procedure TCommonMenu.AutomaticallyCheckForUpdate1Click(Sender: TObject);
begin
	AutomaticallyCheckForUpdate := not AutomaticallyCheckForUpdate;
	AutomaticallyCheckForUpdate1.Checked := AutomaticallyCheckForUpdate;
end;

procedure TCommonMenu.About1Click(Sender: TObject);
begin
	ExecuteAbout(Application.MainForm, False);
end;

procedure TCommonMenu.SetLoggingLevel1Click(Sender: TObject);
begin
	MainLog.LoggingLevel := TMessageLevel(TMenuItem(Sender).Tag);
	LoggingLevel1.Items[TMenuItem(Sender).Tag].Checked := True;
end;

procedure TCommonMenu.ShowSplashScreen1Click(Sender: TObject);
begin
	ViewSplashScreen := not ViewSplashScreen;
	ShowSplashScreen1.Checked := ViewSplashScreen;
	if ViewSplashScreen then ShowSplashScreen(False) else HideSplashScreen(True);
end;

procedure TCommonMenu.RegisterStartup1Click(Sender: TObject);
begin
	if not IsRegisteredStartup then
	begin
		RegisterStartup1.Checked := RegisterStartup;
	end
	else
	begin
		RegisterStartup1.Checked := not UnregisterStartup;
	end;
end;

procedure TCommonMenu.ViewIniFile1Click(Sender: TObject);
begin
	APIOpen(MainIniFileName);
end;

procedure TCommonMenu.ViewLogFile1Click(Sender: TObject);
begin
	if Assigned(MainLog) then
		APIOpen(MainLog.FileName)
	else
		APIOpen(MainLogFileName);
end;

procedure TCommonMenu.ViewAllLogFiles1Click(Sender: TObject);
begin
	APIOpen(ExtractFilePath(MainLogFileName));
end;

procedure TCommonMenu.Sounds1Click(Sender: TObject);
begin
	FormSounds;
end;

procedure CommonFileMenu(const Menu: TMenu);
var
	File1, Options1, Help1, Log1: TMenuItem;
	M: TMenuItem;
	i: SG;
begin
	File1 := nil;
	Options1 := nil;
	Help1 := nil;
	for i := 0 to Menu.Items.Count - 1 do
	begin
		if Menu.Items[i].Name = 'File1' then
			File1 := Menu.Items[i];
		if Menu.Items[i].Name = 'Options1' then
			Options1 := Menu.Items[i];
		if Menu.Items[i].Name = 'Help1' then
			Help1 := Menu.Items[i];
	end;

	if Assigned(File1) then
	begin
		if File1.Count > 0 then
		begin
			M := TMenuItem.Create(File1);
			M.Caption := cLineCaption;
			File1.Add(M);
		end;

		M := TMenuItem.Create(File1);
		M.Name := 'Exit1';
		M.Caption := 'Exit';
		M.ShortCut := ShortCut(VK_F4, [ssAlt]);
		M.OnClick := CommonMenu.Exit1Click;
		File1.Add(M);
	end;

	if Assigned(Options1) then
	begin
		if Options1.Count > 0 then
		begin
			M := TMenuItem.Create(Options1);
			M.Caption := cLineCaption;
			Options1.Add(M);
		end;

		M := TMenuItem.Create(Options1);
		M.Name := 'ViewIniFile1';
		M.Caption := 'View Ini File';
		M.OnClick := CommonMenu.ViewIniFile1Click;
		Options1.Add(M);

		Log1 := TMenuItem.Create(Options1);
		Log1.Name := 'Log1';
		Log1.Caption := 'Log';
		Options1.Add(Log1);

		M := TMenuItem.Create(Log1);
		M.Name := 'ViewLogFile1';
		M.Caption := 'View Log File';
		M.OnClick := CommonMenu.ViewLogFile1Click;
		Log1.Add(M);

		M := TMenuItem.Create(Log1);
		M.Name := 'ViewAllLogFiles1';
		M.Caption := 'View All Log Files';
		M.OnClick := CommonMenu.ViewAllLogFiles1Click;
		Log1.Add(M);

		CommonMenu.LoggingLevel1 := TMenuItem.Create(Log1);
		CommonMenu.LoggingLevel1.Name := 'LoggingLevel1';
		CommonMenu.LoggingLevel1.Caption := 'Logging Level';
		Log1.Add(CommonMenu.LoggingLevel1);

		CommonMenu.RegisterStartup1 := TMenuItem.Create(Options1);
		CommonMenu.RegisterStartup1.Name := 'RegisterStartup1';
		CommonMenu.RegisterStartup1.Caption := 'Register Startup';
		CommonMenu.RegisterStartup1.OnClick := CommonMenu.RegisterStartup1Click;
		CommonMenu.RegisterStartup1.Checked := IsRegisteredStartup;
		Options1.Add(CommonMenu.RegisterStartup1);

		CommonMenu.ShowSplashScreen1 := TMenuItem.Create(Options1);
		CommonMenu.ShowSplashScreen1.Name := 'ShowSplashScreen1';
		CommonMenu.ShowSplashScreen1.Caption := 'Show Splash Screen';
		CommonMenu.ShowSplashScreen1.OnClick := CommonMenu.ShowSplashScreen1Click;
		CommonMenu.ShowSplashScreen1.Checked := ViewSplashScreen;
		Options1.Add(CommonMenu.ShowSplashScreen1);

		M := TMenuItem.Create(Options1);
		M.Name := 'Sounds1';
		M.Caption := 'Sounds...';
		M.OnClick := CommonMenu.Sounds1Click;
		Options1.Add(M);

		for i := 0 to Length(MessageLevelStr) - 1 do
		begin
			M := TMenuItem.Create(CommonMenu.LoggingLevel1);
			M.Name := ComponentName(MessageLevelStr[TMessageLevel(i)]) + '21';
			M.Caption := MessageLevelStr[TMessageLevel(i)];
			M.Tag:= i;
			M.OnClick := CommonMenu.SetLoggingLevel1Click;
			M.RadioItem := True;
			M.Checked := SG(MainLog.LoggingLevel) = i;
			CommonMenu.LoggingLevel1.Add(M);
		end;
	end;

	if Assigned(Help1) then
	begin
		if Help1.Count > 0 then
		begin
			M := TMenuItem.Create(Help1);
			M.Caption := cLineCaption;
			Help1.Add(M);
		end;

		M := TMenuItem.Create(Help1);
		M.Name := 'WebHomepage1';
		M.Caption := 'Web Homepage';
		M.OnClick := CommonMenu.WebHomepage1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'LocalHomepage1';
		M.Caption := 'Local Homepage';
		M.OnClick := CommonMenu.LocalHomepage1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'Messages1';
		M.Caption := 'View Messages...';
		M.OnClick := CommonMenu.ViewMessages1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'Parameters1';
		M.Caption := 'View Parameters...';
		M.OnClick := CommonMenu.ViewParams1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Caption := cLineCaption;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'CheckForUpdate1';
		M.Caption := 'Check For Update' + cDialogSuffix;
		M.OnClick := CommonMenu.CheckForUpdate1Click;
		Help1.Add(M);

		CommonMenu.AutomaticallyCheckForUpdate1 := TMenuItem.Create(Help1);
		CommonMenu.AutomaticallyCheckForUpdate1.Name := 'AutomaticallyCheckForUpdate1';
		CommonMenu.AutomaticallyCheckForUpdate1.Caption := 'Automatically Check For Update';
		CommonMenu.AutomaticallyCheckForUpdate1.OnClick := CommonMenu.AutomaticallyCheckForUpdate1Click;
		CommonMenu.AutomaticallyCheckForUpdate1.Checked := AutomaticallyCheckForUpdate;
		Help1.Add(CommonMenu.AutomaticallyCheckForUpdate1);

		M := TMenuItem.Create(Help1);
		M.Name := 'About';
		M.Caption := 'About' + cDialogSuffix;
		M.OnClick := CommonMenu.About1Click;
		Help1.Add(M);
	end;
end;

initialization
	CommonMenu := TCommonMenu.Create;
finalization
	FreeAndNil(CommonMenu);
end.
