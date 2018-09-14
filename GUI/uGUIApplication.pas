{
	Usability in "Project file" (*.dpr):

type
  TMain = class(TGUIApplication)
  private
    ...: TDirectoryArgument;
    ...: TSwitchArgument;
    ...
  protected
    procedure AddArguments; override;
  end;

var
  Main: TMain;
begin
  Main := TMain.Create;
  try
    Application.CreateForm(TfMain, fMain);
    Main.Run;
  finally
    Main.Free;
  end;
end.

}

unit uGUIApplication;

interface

uses
  uTypes,
  uCommonApplication,
  uSwitchArgument,
  Menus,
  Forms;

type
  TGUIApplication = class(TCommonApplication)
  private
    FAllowMultipleInstance: TSwitchArgument;
    procedure RWCommon(const Save: BG);
    procedure OptionChanged(const OptionIndex: SG);
  protected
    procedure AddArguments; override;
    procedure OnRun; override;
    procedure Initialize; override;
    procedure Finalize; override;

    function GetMainMenuOrPopupMenu(const Form: TForm): TMenu;
    procedure CommonForm(const Form: TForm);
  public
    procedure Terminate; override;
  end;

var
  GUIApplication: TGUIApplication;

implementation

uses
  ExtCtrls,
  SysUtils,
  uMultiIns,
  uMenus,
  uFiles,
  uDIniFile,
  uGlobalOptions,
  uOptions,
  ufOptions,
  uStartup,
  uSplash,
  uWebUpdate,
  uProjectInfo,
  uCommonMenu,
  uCustomArgument;

{ TGUIApplication }

procedure TGUIApplication.AddArguments;
begin
  inherited;

  FAllowMultipleInstance := TSwitchArgument.Create;
  FAllowMultipleInstance.Shortcut := 'multiinst';
  FAllowMultipleInstance.Description := 'Allow multi-instance run.';
  FAllowMultipleInstance.RequireCheck := rcOptional;
  Arguments.Add(FAllowMultipleInstance);
end;

procedure TGUIApplication.CommonForm(const Form: TForm);
var
	i: SG;
	Menu: TMenu;
begin
  Menu := GetMainMenuOrPopupMenu(Form);

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
end;

procedure TGUIApplication.Finalize;
begin
  try
    if MainIni <> nil then
    begin
  		MainIni.UnregisterRW(RWCommon);
  //		MainIni.UnregisterRW(Dictionary.RWLanguage);
    end;

    Application.MainForm.Free; // Do not use FreeAndNil
  finally
    inherited;
  end;
end;

function TGUIApplication.GetMainMenuOrPopupMenu(const Form: TForm): TMenu;
var
	i: SG;
begin
	Result := nil;
	if Form <> nil then
	begin
		for i := 0 to Form.ComponentCount - 1 do
		begin
			if Form.Components[i] is TMainMenu then
			begin
				Result := TMainMenu(Form.Components[i]);
				Break;
			end;
		end;
		if Result = nil then
			for i := 0 to Form.ComponentCount - 1 do
			begin
				if Form.Components[i] is TPopupMenu then
				begin
					Result := TMainMenu(Form.Components[i]);
					Break;
				end;
			end;
	end;
end;

procedure TGUIApplication.Initialize;
begin
  inherited;

  if not uMultiIns.InitInstance(FAllowMultipleInstance.Exists) then
    raise EAbort.Create('Another instance found.');

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);

	MainIni.RegisterRW(RWCommon);

  if GlobalParams[goShowSplashScreenWhenApplicationStarts].Bool then
  begin
    ShowSplashScreen;
  end;
  if GlobalParams[goAutomaticallyCheckForUpdate].Bool and (Now - LastUpdate > GlobalParams[goCheckForUpdateDaysPeriod].Num) then
  begin
    CheckForUpdate(False);
    LastUpdate := Now;
  end;

	if not Installed then
	begin
		LinkChange(goStartMenuIcon, ocCreate);
	end;

	GlobalParams[goStartMenuIcon].Bool := LinkChange(goStartMenuIcon, ocTest);
	GlobalParams[goDesktopIcon].Bool := LinkChange(goDesktopIcon, ocTest);
	GlobalParams[goQuickLaunchIcon].Bool := LinkChange(goQuickLaunchIcon, ocTest);
	GlobalParams[goRunAfterStartUp].Bool := IsRegisteredStartup;

//	Dictionary.TranslateForm(Form); TODO :
	if not Installed then
	begin
    ufOptions.ShowOptions('Global Options', POptions(@GlobalOptions), Length(GlobalParams), PParams
        (@GlobalParams), OptionChanged);
	end;
end;

procedure TGUIApplication.OnRun;
begin
  inherited;

  CommonForm(Application.MainForm);

	HideSplashScreen;
	Application.Run; // Blocking
end;

procedure TGUIApplication.OptionChanged(const OptionIndex: SG);
begin
  uGlobalOptions.OptionChanged(OptionIndex);
end;

procedure TGUIApplication.RWCommon(const Save: BG);
begin
  uGlobalOptions.RWCommon(Save);
end;

procedure TGUIApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.
