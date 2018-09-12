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
  Menus,
  Forms;

type
  TGUIApplication = class(TCommonApplication)
  private
    FShowSplashScreen: BG;
    procedure SetShowSplashScreen(const Value: BG);
  protected
    procedure AddArguments; override;
    procedure OnRun; override;
    procedure Initialize; override;

    function GetMainMenuOrPopupMenu(const Form: TForm): TMenu;
    procedure CommonForm(const Form: TForm);
  public

    property ShowSplashScreen: BG read FShowSplashScreen write SetShowSplashScreen;
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
  uLog,
  uWebUpdate,
  uProjectInfo;

{ TGUIApplication }

procedure TGUIApplication.AddArguments;
begin
  inherited;

  // TODO Multiinst
end;

procedure TGUIApplication.CommonForm(const Form: TForm);
var
	i: SG;
	Menu: TMenu;
begin
  Menu := GetMainMenuOrPopupMenu(Form);

	if Menu <> nil then
	begin
//		CommonFileMenu(Menu); TODO
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

{ TODO
procedure AllowMultiInstanceProc(const Value: string);
begin
  // Handled earlier or unhandled
end;
}

function FoundMultiInstanceParam: BG;
var
  i: SG;
  Param: string;
begin
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i)) ;
    if (Param = '-multiinst') or (Param = '/multiinst') then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TGUIApplication.Initialize;
begin
  inherited;

  // RegisterParam('multiinst', 'Allow multi-instance run.', AllowMultiInstanceProc); TODO :
  if not uMultiIns.InitInstance(FoundMultiInstanceParam) then
    Halt(1);

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);

//	MainIni.RegisterRW(CommonMenu.RWCommon); TODO :

  if GlobalParams[goShowSplashScreenWhenApplicationStarts].Bool and
    FShowSplashScreen then
  begin
    uSplash.ShowSplashScreen;
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
        (@GlobalParams), nil); // TODO : OptionChanged
	end;
end;

procedure TGUIApplication.OnRun;
begin
  inherited;

  CommonForm(Application.MainForm);

	HideSplashScreen;
	Application.Run; // Blocking

	if MainIni <> nil then
	begin
//		MainIni.UnregisterRW(GlobalOptions.RWCommon); TODO :
//		MainIni.UnregisterRW(Dictionary.RWLanguage);
	end;
	Application.MainForm.Free; // Do not use FreeAndNil
end;

procedure TGUIApplication.SetShowSplashScreen(const Value: BG);
begin
  FShowSplashScreen := Value;
end;

procedure TGUIApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.
