(*
Example of use:

MyProgram.dpr:

program MyProgram;

uses
  uGUIApplication,
  uMain in 'uMain.pas' {fMain};

{$R *.RES}

var
  GUIApplication: TGUIApplication;
begin
  GUIApplication := TGUIApplication.Create;
  try
    GUIApplication.CreateForm(TfMain, fMain);
    GUIApplication.Run;
  finally
    GUIApplication.Free;
  end;
end.

*)

unit uGUIApplication;

interface

uses
  uUIApplication,
  uTypes,
  uSwitchArgument,

  Menus,
  Forms,
  Classes;

type
  TGUIApplication = class(TUIApplication)
  private
    FAllowMultipleInstance: TSwitchArgument;
    FMinimizeToTrayIcon: BG;
    procedure RWCommon(const Save: BG);
    procedure OptionChanged(const OptionIndex: SG);
    procedure SetMinimizeToTrayIcon(const Value: BG);
  protected
    procedure AddArguments; override;
    procedure OnRun; override;
    procedure Initialize; override;
    procedure Finalize; override;

    function GetMainMenuOrPopupMenu(const Form: TForm): TMenu;
    procedure CommonForm(const Form: TForm);
  public
    destructor Destroy; override;

    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure Terminate; override;

    property MinimizeToTrayIcon: BG read FMinimizeToTrayIcon write SetMinimizeToTrayIcon;
  end;

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
  uCustomArgument,
  uPictureFactory,
  uCommonOutput,
  uGUIOutputInfo;

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

procedure TGUIApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  if Initialized then
    Application.CreateForm(InstanceClass, Reference);
end;

destructor TGUIApplication.Destroy;
begin
  try
    CommonOutput := nil; // Interface
  finally
    inherited;
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
    FreeAndNil(FAllowMultipleInstance);
    FreeAndNil(PictureFactory);
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

  CommonOutput := TGUIOutputInfo.Create;

  PictureFactory := TPictureFactory.Create;
  PictureFactory.Path := GraphDir;

  if not uMultiIns.InitInstance(FAllowMultipleInstance.Exists) then
    raise EAbort.Create('Another instance found.');

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);

	MainIni.RegisterRW(RWCommon);

  if GlobalParams[goShowSplashScreenWhenApplicationStarts].Bool and (not FMinimizedArgument.Exists) then
  begin
    ShowSplashScreen;
  end;
  if GlobalParams[goAutomaticallyCheckForUpdate].Bool and (Now - LastUpdate > GlobalParams[goCheckForUpdateDaysPeriod].Num) then
  begin
    CheckForUpdate(False);
    LastUpdate := Now;
  end;

	if Statistics.RunFirstTime then
	begin
		LinkChange(goStartMenuIcon, ocCreate);
	end;

	GlobalParams[goStartMenuIcon].Bool := LinkChange(goStartMenuIcon, ocTest);
	GlobalParams[goDesktopIcon].Bool := LinkChange(goDesktopIcon, ocTest);
	GlobalParams[goQuickLaunchIcon].Bool := LinkChange(goQuickLaunchIcon, ocTest);
	GlobalParams[goRunAfterStartUp].Bool := IsRegisteredStartup;

//	Dictionary.TranslateForm(Form); TODO :
	if Statistics.RunFirstTime then
	begin
    ufOptions.ShowOptions('Global Options', POptions(@GlobalOptions), Length(GlobalParams), PParams
        (@GlobalParams), OptionChanged);
	end;
end;

procedure TGUIApplication.OnRun;
begin
  inherited;

  if FMinimizedArgument.Exists then
  begin
    if MinimizeToTrayIcon then
    begin
      Application.ShowMainForm := False
    end
    else
    begin
      if Assigned(Application.MainForm) then
        Application.MainForm.WindowState := wsMinimized;
    end;
  end;
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

procedure TGUIApplication.SetMinimizeToTrayIcon(const Value: BG);
begin
  FMinimizeToTrayIcon := Value;
end;

procedure TGUIApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.
