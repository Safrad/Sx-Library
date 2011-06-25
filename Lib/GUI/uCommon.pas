//* File:     Lib\GUI\uCommon.pas
//* Created:  2004-01-06
//* Modified: 2008-02-16
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uCommon;

interface

uses
	uWatch, uDForm,
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

procedure CommonCreate(ReloadIni: TWatchFileChanged);
procedure CommonForm(const Form: TDForm);
procedure CommonFree;

procedure CommonFileMenu(const Menu: TMenu);

implementation

uses
	uTypes, uDIniFile, uSplash, uMenus, uMultiIns, uFiles, uAbout, uLog, uSounds, uFileExt, uParams, uAPI, uMsgDlg, uMsg,
	uStrings,
	Classes, Windows, ExtCtrls, Forms, SysUtils;

procedure RWCommon(const Save: BG);
begin
	AboutRW(Save);
	if Save = False then ViewSplashScreen := True;
	MainIni.RWBool('Options', 'ViewSplashScreen', ViewSplashScreen, Save);
end;

procedure CommonCreate(ReloadIni: TWatchFileChanged);
begin
	InitInstance;
	InitializeLog;
	MainIniCreate;
	WatchAddFile(MainIniFileName, ReloadIni);
	RWCommon(False);
	if ViewSplashScreen then
		ShowSplashScreen;
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
	WatchRemoveFile(MainIniFileName);
	RWCommon(True);
	FreeSounds;
	FreeFileExt;
	Application.MainForm.Free; // Do not use FreeAndNil
	MainIniFree;
	FreeAndNil(MainLog);
end;

type
	TOb = class(TObject)
	private
		ShowSplashScreen1: TMenuItem;
		LoggingLevel1: TMenuItem;
		procedure Exit1Click(Sender: TObject);
		procedure LocalHomepage1Click(Sender: TObject);
		procedure WebHomepage1Click(Sender: TObject);
		procedure ViewMessages1Click(Sender: TObject);
		procedure ViewParams1Click(Sender: TObject);
		procedure About1Click(Sender: TObject);
		procedure ShowSplashScreen1Click(Sender: TObject);
		procedure ViewIniFile1Click(Sender: TObject);
		procedure ViewLogFile1Click(Sender: TObject);
		procedure ViewAllLogFiles1Click(Sender: TObject);
		procedure Sounds1Click(Sender: TObject);
		procedure SetLoggingLevel1Click(Sender: TObject);
	end;
var
	Ob: TOb;

procedure TOb.Exit1Click(Sender: TObject);
begin
	if Assigned(Application.MainForm) then
		Application.MainForm.Close;
end;

procedure TOb.WebHomepage1Click(Sender: TObject);
begin
	OpenWebHomepage;
end;

procedure TOb.LocalHomepage1Click(Sender: TObject);
begin
	OpenLocalHomepage;
end;

procedure TOb.ViewMessages1Click(Sender: TObject);
begin
	ShowMessages;
end;

procedure TOb.ViewParams1Click(Sender: TObject);
begin
	HelpParams;
end;

procedure TOb.About1Click(Sender: TObject);
begin
	ExecuteAbout(Application.MainForm, False);
end;

procedure TOb.SetLoggingLevel1Click(Sender: TObject);
begin
	MainLog.LoggingLevel := TMessageLevel(TMenuItem(Sender).Tag);
	LoggingLevel1.Items[TMenuItem(Sender).Tag].Checked := True;
end;

procedure TOb.ShowSplashScreen1Click(Sender: TObject);
begin
	ViewSplashScreen := not ViewSplashScreen;
	ShowSplashScreen1.Checked := ViewSplashScreen;
	if ViewSplashScreen then ShowSplashScreen(False) else HideSplashScreen(True);
end;

procedure TOb.ViewIniFile1Click(Sender: TObject);
begin
	APIOpen(MainIniFileName);
end;

procedure TOb.ViewLogFile1Click(Sender: TObject);
begin
	if Assigned(MainLog) then
		APIOpen(MainLog.FileName)
	else
		APIOpen(MainLogFileName);
end;

procedure TOb.ViewAllLogFiles1Click(Sender: TObject);
begin
	APIOpen(ExtractFilePath(MainLogFileName));
end;

procedure TOb.Sounds1Click(Sender: TObject);
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
		M.OnClick := Ob.Exit1Click;
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
		M.OnClick := Ob.ViewIniFile1Click;
		Options1.Add(M);

		Log1 := TMenuItem.Create(Options1);
		Log1.Name := 'Log1';
		Log1.Caption := 'Log';
		Options1.Add(Log1);

		M := TMenuItem.Create(Log1);
		M.Name := 'ViewLogFile1';
		M.Caption := 'View Log File';
		M.OnClick := Ob.ViewLogFile1Click;
		Log1.Add(M);

		M := TMenuItem.Create(Log1);
		M.Name := 'ViewAllLogFiles1';
		M.Caption := 'View All Log Files';
		M.OnClick := Ob.ViewAllLogFiles1Click;
		Log1.Add(M);

		Ob.LoggingLevel1 := TMenuItem.Create(Log1);
		Ob.LoggingLevel1.Name := 'LoggingLevel1';
		Ob.LoggingLevel1.Caption := 'Logging Level';
		Log1.Add(Ob.LoggingLevel1);

		Ob.ShowSplashScreen1 := TMenuItem.Create(Options1);
		Ob.ShowSplashScreen1.Name := 'ShowSplashScreen1';
		Ob.ShowSplashScreen1.Caption := 'Show Splash Screen';
		Ob.ShowSplashScreen1.OnClick := Ob.ShowSplashScreen1Click;
		Ob.ShowSplashScreen1.Checked := ViewSplashScreen;
		Options1.Add(Ob.ShowSplashScreen1);

		M := TMenuItem.Create(Options1);
		M.Name := 'Sounds1';
		M.Caption := 'Sounds...';
		M.OnClick := Ob.Sounds1Click;
		Options1.Add(M);

		for i := 0 to Length(MessageLevelStr) - 1 do
		begin
			M := TMenuItem.Create(Ob.LoggingLevel1);
			M.Name := ComponentName(MessageLevelStr[TMessageLevel(i)]) + '21';
			M.Caption := MessageLevelStr[TMessageLevel(i)];
			M.Tag:= i;
			M.OnClick := Ob.SetLoggingLevel1Click;
			M.RadioItem := True;
			M.Checked := SG(MainLog.LoggingLevel) = i;
			Ob.LoggingLevel1.Add(M);
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
		M.OnClick := Ob.WebHomepage1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'LocalHomepage1';
		M.Caption := 'Local Homepage';
		M.OnClick := Ob.LocalHomepage1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'Messages1';
		M.Caption := 'View Messages...';
		M.OnClick := Ob.ViewMessages1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'Parameters1';
		M.Caption := 'View Parameters...';
		M.OnClick := Ob.ViewParams1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Caption := cLineCaption;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Name := 'About';
		M.Caption := 'About' + cDialogSuffix;
		M.OnClick := Ob.About1Click;
		Help1.Add(M);
	end;
end;

initialization
	Ob := TOb.Create;
finalization
	FreeAndNil(Ob);
end.
