unit uCommonMenu;

interface

uses
  uTypes,
  Menus;

type
	TCommonMenu = class(TObject)
	private
		class procedure Restart1Click(Sender: TObject);
		class procedure Exit1Click(Sender: TObject);
		class procedure LocalHomepage1Click(Sender: TObject);
		class procedure WebHomepage1Click(Sender: TObject);
		class procedure ViewParams1Click(Sender: TObject);
		class procedure About1Click(Sender: TObject);
		class procedure ViewIniFile1Click(Sender: TObject);
		class procedure ViewLogFile1Click(Sender: TObject);
		class procedure ViewAllLogFiles1Click(Sender: TObject);
		class procedure Sounds1Click(Sender: TObject);
		class procedure SetLoggingLevel1Click(Sender: TObject);
		class procedure ShowOptions(Sender: TObject);
    class procedure OptionChanged(const OptionIndex: SG);
  public
    class procedure CreateMenuItemLineSeparator(const AOwner: TMenuItem);
    class procedure CreateExitMenuItem(const AOwner: TMenuItem);
    class procedure CreateAboutMenuItem(const AOwner: TMenuItem);
    class procedure CreateGlobalOptionsMenuItem(const AOwner: TMenuItem);
    class procedure CreateSoundsMenuItem(const AOwner: TMenuItem);
    class procedure CreateViewIniFileMenuItem(const AOwner: TMenuItem);
    class function CreateLogMenuItem(const AOwner: TMenuItem): TMenuItem;
    class procedure CreateViewLogFileMenuItem(const AOwner: TMenuItem);
    class procedure CreateRestartMenuItem(const AOwner: TMenuItem);
    class procedure CreateViewAllLogFilesMenuItem(const AOwner: TMenuItem);
    class procedure CreateViewParamsMenuItem(const AOwner: TMenuItem);
    class procedure CreateLocalHomepageMenuItem(const AOwner: TMenuItem);
    class procedure CreateWebHomepageMenuItem(const AOwner: TMenuItem);
    class function CreateLoggingLevelMenuItem(const AOwner: TMenuItem): TMenuItem;
    class procedure CreateLoggingLevelSubMenu(const AOwner: TMenuItem);
    class procedure CreateLogSubMenu(const AOwner: TMenuItem);

    class function CanAddToMenu(const AMenuItem: TMenuItem): BG;
    class procedure CreateItems(const AMenu: TMenu);
	end;


implementation

uses
  Forms,
  Classes,
  SysUtils,
  Winapi.Windows,
  uAbout,
  uNewThread,
  uLog,
  uAPI,
  ufOptions,
  uOptions,
  uDIniFile,
  uFiles,
  uStrings,
  uMsg,
  uSounds,
  uProjectInfo,
  uGlobalOptions,
  uCommonApplication,
  uExitCommand,
  uRestartCommand;

{ TCommonMenu }

class procedure TCommonMenu.CreateMenuItemLineSeparator(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Caption := cLineCaption;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateExitMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'Exit1';
  M.Caption := 'Exit';
  M.ShortCut := ShortCut(VK_F4, [ssAlt]);
  M.OnClick := Exit1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateAboutMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'About';
  M.Caption := 'About' + cDialogSuffix;
  M.OnClick := About1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateGlobalOptionsMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'GlobalOptions1';
  M.Caption := 'Global Options...';
  M.OnClick := ShowOptions;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateSoundsMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'Sounds1';
  M.Caption := 'Sounds...';
  M.OnClick := Sounds1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateViewIniFileMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'ViewIniFile1';
  M.Caption := 'View Options Files';
  M.OnClick := ViewIniFile1Click;
  AOwner.Add(M);
end;

class function TCommonMenu.CreateLogMenuItem(const AOwner: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  Result.Name := 'Log1';
  Result.Caption := 'Log';
  AOwner.Add(Result);
end;

class procedure TCommonMenu.CreateViewLogFileMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'ViewLogFile1';
  M.Caption := 'View Log File';
  M.OnClick := ViewLogFile1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateRestartMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'Restart1';
  M.Caption := 'Restart Application';
  M.OnClick := Restart1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateViewAllLogFilesMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'ViewAllLogFiles1';
  M.Caption := 'View All Log Files';
  M.OnClick := ViewAllLogFiles1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateViewParamsMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  M := TMenuItem.Create(AOwner);
  M.Name := 'Parameters1';
  M.Caption := 'View Command-Line Parameters...';
  M.OnClick := ViewParams1Click;
  AOwner.Add(M);
end;

class procedure TCommonMenu.CreateLocalHomepageMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  if FileExists(GetLocalHomepage) then
  begin
    M := TMenuItem.Create(AOwner);
    M.Name := 'LocalHomepage1';
    M.Caption := 'Local Homepage';
    M.OnClick := LocalHomepage1Click;
    AOwner.Add(M);
  end;
end;

class procedure TCommonMenu.CreateWebHomepageMenuItem(const AOwner: TMenuItem);
var
  M: TMenuItem;
begin
  if GetProjectInfo(piWeb) <> '' then
  begin
    M := TMenuItem.Create(AOwner);
    M.Name := 'WebHomepage1';
    M.Caption := 'Web Homepage';
    M.OnClick := WebHomepage1Click;
    AOwner.Add(M);
  end;
end;

class function TCommonMenu.CreateLoggingLevelMenuItem(const AOwner: TMenuItem): TMenuItem;
begin
  Result := TMenuItem.Create(AOwner);
  Result.Name := 'LoggingLevel1';
  Result.Caption := 'Logging Level';
  AOwner.Add(Result);
end;

class procedure TCommonMenu.CreateLoggingLevelSubMenu(const AOwner: TMenuItem);
var
  i: SG;
  M: TMenuItem;
  LoggingLevel: TMenuItem;
begin
  LoggingLevel := CreateLoggingLevelMenuItem(AOwner);
  for i := 0 to Length(MessageLevelStr) - 1 do
  begin
    M := TMenuItem.Create(LoggingLevel);
    M.Name := ComponentName(MessageLevelStr[TMessageLevel(i)]) + '21';
    M.Caption := MessageLevelStr[TMessageLevel(i)];
    M.Tag := i;
    M.OnClick := SetLoggingLevel1Click;
    M.RadioItem := True;
    M.Checked := Assigned(MainLog) and (SG(MainLog.LoggingLevel) = i);
    LoggingLevel.Add(M);
  end;
end;

class procedure TCommonMenu.CreateLogSubMenu(const AOwner: TMenuItem);
var
  Log1: TMenuItem;
begin
  Log1 := CreateLogMenuItem(AOwner);
  CreateViewLogFileMenuItem(Log1);
  CreateViewAllLogFilesMenuItem(Log1);
  CreateLoggingLevelSubMenu(Log1);
end;

class function TCommonMenu.CanAddToMenu(const AMenuItem: TMenuItem): BG;
begin
  if Assigned(AMenuItem) then
  begin
    Result := True;
    if AMenuItem.Count > 0 then
      CreateMenuItemLineSeparator(AMenuItem);
  end
  else
    Result := False;
end;

class procedure TCommonMenu.CreateItems(const AMenu: TMenu);
var
	File1, Options1, Help1: TMenuItem;
  i: SG;
begin
	File1 := nil;
	Options1 := nil;
	Help1 := nil;
	for i := 0 to AMenu.Items.Count - 1 do
	begin
		if AMenu.Items[i].Name = 'File1' then
			File1 := AMenu.Items[i];
		if AMenu.Items[i].Name = 'Options1' then
			Options1 := AMenu.Items[i];
		if AMenu.Items[i].Name = 'Help1' then
			Help1 := AMenu.Items[i];
	end;

	if CanAddToMenu(File1) then
	begin
    CreateRestartMenuItem(File1);
    CreateExitMenuItem(File1);
	end;

	if CanAddToMenu(Options1) then
	begin
    CreateGlobalOptionsMenuItem(Options1);
    CreateSoundsMenuItem(Options1);
    CreateViewIniFileMenuItem(Options1);
    CreateLogSubMenu(Options1);
	end;

	if CanAddToMenu(Help1) then
	begin
    CreateWebHomepageMenuItem(Help1);
    CreateLocalHomepageMenuItem(Help1);
    CreateViewParamsMenuItem(Help1);
    CreateMenuItemLineSeparator(Help1);
    CreateAboutMenuItem(Help1);
	end;
end;

class procedure TCommonMenu.Restart1Click(Sender: TObject);
var
  RestartCommand: TRestartCommand;
begin
  RestartCommand := TRestartCommand.Create;
  try
    RestartCommand.ExecuteNoParam;
  finally
    RestartCommand.Free;
  end;
end;

class procedure TCommonMenu.Exit1Click(Sender: TObject);
var
  ExitCommand: TExitCommand;
begin
  ExitCommand := TExitCommand.Create;
  try
    ExitCommand.ExecuteNoParam;
  finally
    ExitCommand.Free;
  end;
end;

class procedure TCommonMenu.WebHomepage1Click(Sender: TObject);
begin
	OpenWebHomepage;
end;

class procedure TCommonMenu.LocalHomepage1Click(Sender: TObject);
begin
	OpenLocalHomepage;
end;

class procedure TCommonMenu.OptionChanged(const OptionIndex: SG);
begin
  uGlobalOptions.OptionChanged(OptionIndex);
end;

class procedure TCommonMenu.ViewParams1Click(Sender: TObject);
begin
	CommonApplication.Arguments.WriteToCommonOutput;
end;

class procedure TCommonMenu.About1Click(Sender: TObject);
begin
	ExecuteAbout(Application.MainForm, False);
end;

class procedure TCommonMenu.SetLoggingLevel1Click(Sender: TObject);
begin
	MainLog.LoggingLevel := TMessageLevel(TMenuItem(Sender).Tag);
	TMenuItem(Sender).Checked := True;
end;

class procedure TCommonMenu.ShowOptions(Sender: TObject);
begin
	ufOptions.ShowOptions('Global Options', POptions(@GlobalOptions), Length(GlobalParams), PParams
		(@GlobalParams), OptionChanged);
end;

class procedure TCommonMenu.ViewIniFile1Click(Sender: TObject);
begin
	APIOpen(MainIni.FileName);
	APIOpen(LocalMainIni.FileName);
end;

class procedure TCommonMenu.ViewLogFile1Click(Sender: TObject);
begin
	APIOpen(MainLog.FileName);
end;

class procedure TCommonMenu.ViewAllLogFiles1Click(Sender: TObject);
begin
	APIOpen(ExtractFilePath(MainLog.FileName));
end;

class procedure TCommonMenu.Sounds1Click(Sender: TObject);
begin
	FormSounds;
end;

end.
