unit uCommon;

interface

uses
	Graphics,
	uTypes, uDForm, uOptions,
	Menus;

{
	Preconditions:
	MainMenu1 with submenus File1, Options1, Window1 and Help1

	Usability in "Project file" (*.dpr):

	begin
	Application.Title := '...';
	Application.Initialize;
	CommonCreate;
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

function GetBackgroundWindowTexture: BG;
function GetBackgroundWindowColor: TColor;

var
	ForceClose: BG;

type
	TGlobalOption = (
    goLanguage,
    goStartMenuIcon, goDesktopIcon, goQuickLaunchIcon, goRunAfterStartUp,
		goShowSplashScreenWhenApplicationStarts,
		goWindowBackgroundTexture,
		goWindowBackgroundColor,
		goAutomaticallyCheckForUpdate,
		goCheckForUpdateDaysPeriod);

var
	GlobalOptions: array [TGlobalOption] of TOption = (
		(Typ: vsCombo),
		(Typ: vsCheck; Default: 1),
    (Typ: vsCheck; Default: 1),
    (Typ: vsCheck; Default: 1),
		(Typ: vsCheck; Default: 0),
    (Typ: vsCheck; Default: 1),
		(Typ: vsCheck; Default: 1),
		(Typ: vsColor; Default: clBtnFace; Minimum: 0; Maximum: MaxInt),
		(Typ: vsCheck; Default: 1),
		(Typ: vsSpin; Default: 14; Minimum: 0; Maximum: 365));

var
	GlobalParams: array [TGlobalOption] of TParam;

implementation

uses
	uDIniFile, uSplash, uMenus, uMultiIns, uFiles, uAbout, uLog, uSounds, uFileExt, uParams, uAPI, uNewThread,
	uMsgDlg, uMsg, uStart, ufOptions, uReg, uProjectInfo, uLink,
	uStrings, uWebUpdate, uStartup, uDictionary,
	Classes, Windows, ExtCtrls, Forms, SysUtils;

// Executable file is smaller
{$SETPEFLAGS IMAGE_FILE_RELOCS_STRIPPED}

type
	TCommonMenu = class(TObject)
	private
		CheckForUpdate1: TMenuItem;
		LoggingLevel1: TMenuItem;

		procedure OptionChanged(const OptionIndex: SG);

		procedure Exit1Click(Sender: TObject);
		procedure LocalHomepage1Click(Sender: TObject);
		procedure WebHomepage1Click(Sender: TObject);
		procedure ViewMessages1Click(Sender: TObject);
		procedure ViewParams1Click(Sender: TObject);
		procedure CheckForUpdate1Click(Sender: TObject);
		procedure About1Click(Sender: TObject);
		procedure ViewIniFile1Click(Sender: TObject);
		procedure ViewLogFile1Click(Sender: TObject);
		procedure ViewAllLogFiles1Click(Sender: TObject);
		procedure Sounds1Click(Sender: TObject);
		procedure SetLoggingLevel1Click(Sender: TObject);
		procedure ShowOptions(Sender: TObject);
	public
		procedure RWCommon(const Save: BG);
	end;

var
	CommonMenu: TCommonMenu;

function GetBackgroundWindowTexture: BG;
begin
	Result := GlobalParams[goWindowBackgroundTexture].Bool;
end;

function GetBackgroundWindowColor: TColor;
begin
	Result := GlobalParams[goWindowBackgroundColor].Num;
end;

var
	LastUpdate: TDateTime;

procedure CommonCreate(const Special: BG = False);
begin
	if not Special then
	begin
		if not InitInstance then
			Halt(1);
		InitializeLog;
	end;
	MainIni := TDIniFile.Create(MainIniFileName);
	LocalMainIni := TDIniFile.Create(LocalIniFileName);
	Dictionary := TDictionary.Create;
	MainIni.RegisterRW(CommonMenu.RWCommon);
	if not Special then
	begin
		if GlobalParams[goShowSplashScreenWhenApplicationStarts].Bool and
			(Pos('-Minimized', GetCommandLine) = 0) then
		begin
			ShowSplashScreen;
		end;
		if GlobalParams[goAutomaticallyCheckForUpdate].Bool and (Now - LastUpdate > GlobalParams[goCheckForUpdateDaysPeriod].Num) then
		begin
			CheckForUpdate(False);
			LastUpdate := Now;
		end;
	end;
end;

function LinkChange(const GlobalOption: TGlobalOption; const ObjectChange: TObjectChange): BG;
var
	LinkFileName, LinkFileName2: TFileName;
	Dir: string;
begin
	Result := False;
	case GlobalOption of
	goStartMenuIcon:
		begin
{			Dir := ShellFolder('Common Start Menu', True) + 'Programs' + PathDelim + GetProjectInfo
				(piProductName) + PathDelim; // Permision Denied if limited user! }
			Dir := ShellFolder('Start Menu', False) + 'Programs' + PathDelim + GetProjectInfo
				(piProductName) + PathDelim;
			LinkFileName := Dir + GetProjectInfo(piProductName) + '.lnk';
		end;
	goDesktopIcon:
		LinkFileName := ShellFolder('Common Desktop', True) + GetProjectInfo(piProductName) + '.lnk';
	goQuickLaunchIcon:
	begin
		LinkFileName := CommonAppDataDir + PathDelim + 'Microsoft' + PathDelim + 'Internet Explorer' +
			PathDelim + 'Quick Launch' + PathDelim;
		LinkFileName2 := LinkFileName + 'User Pinned\TaskBar\';
		if DirectoryExists(LinkFileName2) then
			LinkFileName := LinkFileName2;
		LinkFileName := LinkFileName + GetProjectInfo(piProductName) + '.lnk';
	end;
	end;
	case ObjectChange of
	ocTest:
		Result := FileExists(LinkFileName);
	ocCreate:
		begin
			CreateLink(LinkFileName, ExeFileName, '', WorkDir, 0, GetProjectInfo(piFileDescription),
				ExeFileName, 0);
		end;
	ocRemove:
		begin
			if FileExists(LinkFileName) then
				DeleteFile(LinkFileName);
			case GlobalOption of
			goStartMenuIcon:
				RemoveDir(Dir);
			end;
		end;
	end;
end;

procedure CommonForm(const Form: TDForm);
var
	i: SG;
	Menu: TMenu;
begin
	AddCommonParams;
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
				// IconsResize(TPanel(Form.Components[i]));
				Break;
			end;
		end;
	end;

	if not Installed then
	begin
		LinkChange(goStartMenuIcon, ocCreate);
	end;

	GlobalParams[goStartMenuIcon].Bool := LinkChange(goStartMenuIcon, ocTest);
	GlobalParams[goDesktopIcon].Bool := LinkChange(goDesktopIcon, ocTest);
	GlobalParams[goQuickLaunchIcon].Bool := LinkChange(goQuickLaunchIcon, ocTest);
	GlobalParams[goRunAfterStartUp].Bool := IsRegisteredStartup;

	Dictionary.TranslateForm(Form);
	if not Installed then
	begin
		CommonMenu.ShowOptions(Form);
	end;

	HideSplashScreen;
end;

procedure CommonFree;
begin
	if MainIni <> nil then
	begin
		MainIni.UnregisterRW(CommonMenu.RWCommon);
//		MainIni.UnregisterRW(Dictionary.RWLanguage);
	end;
	FreeSounds;
	FreeFileExt;
	Application.MainForm.Free; // Do not use FreeAndNil
	FreeAndNil(Dictionary);
	FreeAndNil(MainIni);
	FreeAndNil(LocalMainIni);
	FreeAndNil(MainLog);
end;

{ TCommonMenu }

procedure TCommonMenu.RWCommon(const Save: BG);
const
	Section = 'Options';
begin
	// Compatibility
	if Save = False then
	begin
		if MainIni.ValueExists(Section, 'ViewSplashScreen') then
		begin
			GlobalOptions[goShowSplashScreenWhenApplicationStarts].Default := MainIni.ReadNum
				(Section, 'ViewSplashScreen', 1);
		end;
		if MainIni.ValueExists(Section, 'AutomaticallyCheckForUpdate') then
		begin
			GlobalOptions[goAutomaticallyCheckForUpdate].Default := MainIni.ReadNum
				(Section, 'AutomaticallyCheckForUpdate', 1);
		end;
	end
	else
	begin
		MainIni.DeleteValue(Section, 'ViewSplashScreen');
		MainIni.DeleteValue(Section, 'AutomaticallyCheckForUpdate');
	end;

	RWStart(MainIni, Save);

	uOptions.RWOptions(POptions(@GlobalOptions), Length(GlobalOptions), PParams(@GlobalParams),
		MainIni, 'Global Options', Save);

{	if Save = False then
		AutomaticallyCheckForUpdate := True;
	MainIni.RWBool(Section, 'AutomaticallyCheckForUpdate', AutomaticallyCheckForUpdate, Save);}
	MainIni.RWDateTime(Section, 'LastUpdate', LastUpdate, Save);
end;

procedure TCommonMenu.Exit1Click(Sender: TObject);
begin
	if Assigned(Application.MainForm) then
	begin
		ForceClose := True;
		Application.MainForm.Close;
		ForceClose := False;
	end;
end;

procedure TCommonMenu.WebHomepage1Click(Sender: TObject);
begin
	OpenWebHomepage;
end;

procedure TCommonMenu.LocalHomepage1Click(Sender: TObject);
begin
	OpenLocalHomepage;
end;

procedure SetBackgroundColor(const AComponent: TComponent);
var
	i: SG;
begin
	for i := 0 to AComponent.ComponentCount - 1 do
	begin
		if AComponent.Components[i] is TForm then
		begin
			TForm(AComponent.Components[i]).Color := GetBackgroundWindowColor;
			SetBackgroundColor(AComponent.Components[i]);
		end;
	end;
end;

procedure SetBackgroundInvalidate(const AComponent: TComponent);
var
	i: SG;
begin
	for i := 0 to AComponent.ComponentCount - 1 do
	begin
		if AComponent.Components[i] is TForm then
		begin
			TForm(AComponent.Components[i]).Invalidate;
			SetBackgroundColor(AComponent.Components[i]);
		end;
	end;
end;

procedure TCommonMenu.OptionChanged(const OptionIndex: SG);
begin
	case TGlobalOption(OptionIndex) of
  goLanguage:
    Dictionary.ChangeLanguage;
	goStartMenuIcon, goDesktopIcon, goQuickLaunchIcon:
		begin
			if GlobalParams[TGlobalOption(OptionIndex)].Bool then
				LinkChange(TGlobalOption(OptionIndex), ocCreate)
			else
				LinkChange(TGlobalOption(OptionIndex), ocRemove);
		end;
	goRunAfterStartUp:
		begin
			if GlobalParams[TGlobalOption(OptionIndex)].Bool then
				RegisterStartup
			else
				UnregisterStartup;
		end;
	goShowSplashScreenWhenApplicationStarts:
		begin
			if GlobalParams[TGlobalOption(OptionIndex)].Bool then
//				ShowSplashScreen(False) Runtime 216 if application closed before splash hide
			else
				HideSplashScreen(True);
		end;
	goWindowBackgroundTexture:
		begin
			SetBackgroundInvalidate(Application);
	end;
	goWindowBackgroundColor:
		begin
			SetBackgroundColor(Application);
		end;
	end;
end;

procedure TCommonMenu.ViewMessages1Click(Sender: TObject);
begin
	ShowMessages;
end;

procedure TCommonMenu.ViewParams1Click(Sender: TObject);
begin
	HelpParams;
end;

procedure MenuCheckForUpdate(AThread: TThread);
begin
	CommonMenu.CheckForUpdate1.Enabled := False;
	try
		{$ifopt d+}
		Sleep(5000);
		{$endif}
		CheckForUpdate;
	finally
		CommonMenu.CheckForUpdate1.Enabled := True;
	end;
end;

procedure TCommonMenu.CheckForUpdate1Click(Sender: TObject);
begin
	LastUpdate := Now;
	RunInNewThread(MenuCheckForUpdate);
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

procedure TCommonMenu.ShowOptions(Sender: TObject);
begin
	ufOptions.ShowOptions('Global Options', POptions(@GlobalOptions), Length(GlobalParams), PParams
			(@GlobalParams), OptionChanged);
end;

procedure TCommonMenu.ViewIniFile1Click(Sender: TObject);
begin
	APIOpen(MainIniFileName);
	APIOpen(LocalIniFileName);
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
		M.Name := 'GlobalOptions1';
		M.Caption := 'Global Options...';
		M.OnClick := CommonMenu.ShowOptions;
		Options1.Add(M);

		M := TMenuItem.Create(Options1);
		M.Name := 'Sounds1';
		M.Caption := 'Sounds...';
		M.OnClick := CommonMenu.Sounds1Click;
		Options1.Add(M);

		M := TMenuItem.Create(Options1);
		M.Name := 'ViewIniFile1';
		M.Caption := 'View Options File';
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

		M := TMenuItem.Create(Log1);
		M.Name := 'LoggingLevel1';
		M.Caption := 'Logging Level';
		Log1.Add(M);
		CommonMenu.LoggingLevel1 := M;

		for i := 0 to Length(MessageLevelStr) - 1 do
		begin
			M := TMenuItem.Create(CommonMenu.LoggingLevel1);
			M.Name := ComponentName(MessageLevelStr[TMessageLevel(i)]) + '21';
			M.Caption := MessageLevelStr[TMessageLevel(i)];
			M.Tag := i;
			M.OnClick := CommonMenu.SetLoggingLevel1Click;
			M.RadioItem := True;
			M.Checked := Assigned(MainLog) and (SG(MainLog.LoggingLevel) = i);
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

		if FileExists(GetLocalHomepage) then
		begin
			M := TMenuItem.Create(Help1);
			M.Name := 'LocalHomepage1';
			M.Caption := 'Local Homepage';
			M.OnClick := CommonMenu.LocalHomepage1Click;
			Help1.Add(M);
		end;

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
		CommonMenu.CheckForUpdate1 := M;

		M := TMenuItem.Create(Help1);
		M.Name := 'About';
		M.Caption := 'About' + cDialogSuffix;
		M.OnClick := CommonMenu.About1Click;
		Help1.Add(M);
	end;
end;

initialization

InitOptionNames(TypeInfo(TGlobalOption), GlobalOptions);
DefaultOptions(POptions(@GlobalOptions), Length(GlobalOptions), PParams(@GlobalParams));

CommonMenu := TCommonMenu.Create;

finalization

FreeAndNil(CommonMenu);

end.
