(*
Example of use:

MyProgram.dpr:

program MyProgram;

uses
  uGUIApplication,
  uMain in 'uMain.pas' {fMain};

{$R *.RES}

var
  VclGuiApplication: TVclGuiApplication;
begin
  VclGuiApplication := TVclGuiApplication.Create;
  try
    VclGuiApplication.CreateForm(TfMain, fMain);
    VclGuiApplication.CreateForm(TfAnyForm, fAnyForm);
    VclGuiApplication.Run;
  finally
    VclGuiApplication.Free;
  end;
end.

*)

unit uVCLGUIApplication;

interface

uses
  uGUIApplication,
  uTypes,
  uSwitchArgument,

  Vcl.Menus,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Classes;

type
  TVclGuiApplication = class(TGUIApplication)
  private
    FMinimizeToTrayIcon: BG;
    FUseCommonMenu: BG;
    procedure RWCommon(const Save: BG);
    procedure SetMinimizeToTrayIcon(const Value: BG);
    procedure SetUseCommonMenu(const Value: BG);
  protected
    FAllowMultipleInstance: TSwitchArgument;
    procedure AddArguments; override;
    procedure OnRun; override;
    procedure Initialize; override;
    procedure Finalize; override;

    function FindPanelTool(const AForm: TForm): TPanel;
    function GetMainMenuOrPopupMenu(const AForm: TForm): TMenu;
    procedure CommonForm(const AForm: TForm);
  public
    procedure CreateForm(InstanceClass: TComponentClass; var Reference);
    procedure Terminate; override;

    property MinimizeToTrayIcon: BG read FMinimizeToTrayIcon write SetMinimizeToTrayIcon;
    property UseCommonMenu: BG read FUseCommonMenu write SetUseCommonMenu;
  end;

implementation

uses
  SysUtils,

  uMultiIns,
  uMenus,
  uSystemPaths,
  uMainCfg,
  uGlobalOptions,
  uStartup,
  uProjectInfo,
  uCommonMenu,
  uCustomArgument,
  uPictureFactory,
  uMainLog;

{ TVclGuiApplication }

procedure TVclGuiApplication.AddArguments;
begin
  inherited;

  FAllowMultipleInstance := TSwitchArgument.Create;
  FAllowMultipleInstance.Shortcut := 'multiinst';
  FAllowMultipleInstance.Description := 'Allow multi-instance run.';
  FAllowMultipleInstance.RequireCheck := rcOptional;
  Arguments.Add(FAllowMultipleInstance);
end;

procedure TVclGuiApplication.CommonForm(const AForm: TForm);
var
	Menu: TMenu;
  PanelTool: TPanel;
begin
  Menu := GetMainMenuOrPopupMenu(AForm);

	if Menu <> nil then
	begin
		TCommonMenu.CreateItems(Menu);
		MenuSet(Menu);
    PanelTool := FindPanelTool(AForm);
    if PanelTool <> nil then
      IconsFromMenu(Menu, PanelTool);
	end;
end;

procedure TVclGuiApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
begin
  Application.CreateForm(InstanceClass, Reference);
  if FUseCommonMenu then
  begin
    Assert(Application.MainForm <> nil);
//      Assert(InstanceClass = Application.MainForm);
    CommonForm({InstanceClass as TForm}Application.MainForm);
    FUseCommonMenu := False;
  end;
end;

procedure TVclGuiApplication.Finalize;
begin
  try
    if Assigned(MainCfg) then
      MainCfg.UnregisterRW(RWCommon);

    if Assigned(Application) and Assigned(Application.MainForm) then
      Application.MainForm.Free; // Do not use FreeAndNil

    FreeAndNil(FAllowMultipleInstance);
    FreeAndNil(PictureFactory);
  finally
    inherited;
  end;
end;

function TVclGuiApplication.FindPanelTool(const AForm: TForm): TPanel;
var
  i: SG;
begin
  for i := 0 to AForm.ComponentCount - 1 do
  begin
    if (AForm.Components[i] is TPanel) and (AForm.Components[i].Name = 'PanelTool') then
    begin
      Result := TPanel(AForm.Components[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TVclGuiApplication.GetMainMenuOrPopupMenu(const AForm: TForm): TMenu;
var
	i: SG;
begin
	Result := nil;
	if AForm <> nil then
	begin
		for i := 0 to AForm.ComponentCount - 1 do
		begin
			if AForm.Components[i] is TMainMenu then
			begin
				Result := TMainMenu(AForm.Components[i]);
				Break;
			end;
		end;
		if Result = nil then
			for i := 0 to AForm.ComponentCount - 1 do
			begin
				if AForm.Components[i] is TPopupMenu then
				begin
					Result := TMainMenu(AForm.Components[i]);
					Break;
				end;
			end;
	end;
end;

procedure TVclGuiApplication.Initialize;
begin
  inherited;

  PictureFactory := TPictureFactory.Create;
  PictureFactory.Path := SystemPaths.GraphDir;

  if not uMultiIns.InitInstance(FAllowMultipleInstance.Value) then
  begin
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('Another instance found, aborting start.', mlDebug);

    raise EAbort.Create('Another instance found.');
  end;

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);

	MainCfg.RegisterRW(RWCommon);

	if Statistics.RunFirstTime then
	begin
		LinkChange(goStartMenuIcon, ocCreate);
	end;

	GlobalParams[goStartMenuIcon].Bool := LinkChange(goStartMenuIcon, ocTest);
	GlobalParams[goDesktopIcon].Bool := LinkChange(goDesktopIcon, ocTest);
	GlobalParams[goQuickLaunchIcon].Bool := LinkChange(goQuickLaunchIcon, ocTest);
	GlobalParams[goRunAfterStartUp].Bool := IsRegisteredStartup;
end;

procedure TVclGuiApplication.OnRun;
begin
  inherited;

  if FMinimizedArgument.Value then
  begin
    if FMinimizeToTrayIcon then
    begin
      Application.ShowMainForm := False;
    end
    else
    begin
      if Assigned(Application.MainForm) then
        Application.MainForm.WindowState := wsMinimized;
    end;
  end;

	Application.Run; // Blocking
end;

procedure TVclGuiApplication.RWCommon(const Save: BG);
begin
  uGlobalOptions.RWCommon(Save);
end;

procedure TVclGuiApplication.SetMinimizeToTrayIcon(const Value: BG);
begin
  FMinimizeToTrayIcon := Value;
end;

procedure TVclGuiApplication.SetUseCommonMenu(const Value: BG);
begin
  FUseCommonMenu := Value;
end;

procedure TVclGuiApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.
