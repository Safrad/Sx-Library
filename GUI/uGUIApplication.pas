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
    GUIApplication.CreateForm(TfAnyForm, fAnyForm);
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

  Vcl.Menus,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Classes;

type
  TGUIApplication = class(TUIApplication)
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
    destructor Destroy; override;

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
  uFiles,
  uDIniFile,
  uGlobalOptions,
  uStartup,
  uProjectInfo,
  uCommonMenu,
  uCustomArgument,
  uPictureFactory,
  uCommonOutput,
  uGUIOutputInfo,
  uMainLog;

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

procedure TGUIApplication.CommonForm(const AForm: TForm);
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

procedure TGUIApplication.CreateForm(InstanceClass: TComponentClass; var Reference);
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
    if Assigned(MainIni) then
      MainIni.UnregisterRW(RWCommon);

    if Assigned(Application) and Assigned(Application.MainForm) then
      Application.MainForm.Free; // Do not use FreeAndNil

    FreeAndNil(FAllowMultipleInstance);
    FreeAndNil(PictureFactory);
  finally
    inherited;
  end;
end;

function TGUIApplication.FindPanelTool(const AForm: TForm): TPanel;
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

function TGUIApplication.GetMainMenuOrPopupMenu(const AForm: TForm): TMenu;
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

procedure TGUIApplication.Initialize;
begin
  CommonOutput := TGUIOutputInfo.Create;

  inherited;

  PictureFactory := TPictureFactory.Create;
  PictureFactory.Path := GraphDir;

  if not uMultiIns.InitInstance(FAllowMultipleInstance.Value) then
  begin
    if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('Another instance found, aborting start.', mlDebug);

    raise EAbort.Create('Another instance found.');
  end;

  Application.Initialize;
	Application.Title := GetProjectInfo(piProductName);

	MainIni.RegisterRW(RWCommon);

	if Statistics.RunFirstTime then
	begin
		LinkChange(goStartMenuIcon, ocCreate);
	end;

	GlobalParams[goStartMenuIcon].Bool := LinkChange(goStartMenuIcon, ocTest);
	GlobalParams[goDesktopIcon].Bool := LinkChange(goDesktopIcon, ocTest);
	GlobalParams[goQuickLaunchIcon].Bool := LinkChange(goQuickLaunchIcon, ocTest);
	GlobalParams[goRunAfterStartUp].Bool := IsRegisteredStartup;
end;

procedure TGUIApplication.OnRun;
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

procedure TGUIApplication.RWCommon(const Save: BG);
begin
  uGlobalOptions.RWCommon(Save);
end;

procedure TGUIApplication.SetMinimizeToTrayIcon(const Value: BG);
begin
  FMinimizeToTrayIcon := Value;
end;

procedure TGUIApplication.SetUseCommonMenu(const Value: BG);
begin
  FUseCommonMenu := Value;
end;

procedure TGUIApplication.Terminate;
begin
  inherited;

  Application.Terminate;
end;

end.
