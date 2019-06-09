unit uCommonMenu;

interface

uses
  uTypes,
  Menus;

type
	TCommonMenu = class(TObject)
	private
		CheckForUpdate1: TMenuItem;
		LoggingLevel1: TMenuItem;

		procedure Restart1Click(Sender: TObject);
		procedure Exit1Click(Sender: TObject);
		procedure LocalHomepage1Click(Sender: TObject);
		procedure WebHomepage1Click(Sender: TObject);
		procedure ViewParams1Click(Sender: TObject);
		procedure CheckForUpdate1Click(Sender: TObject);
		procedure About1Click(Sender: TObject);
		procedure ViewIniFile1Click(Sender: TObject);
		procedure ViewLogFile1Click(Sender: TObject);
		procedure ViewAllLogFiles1Click(Sender: TObject);
		procedure Sounds1Click(Sender: TObject);
		procedure SetLoggingLevel1Click(Sender: TObject);
		procedure ShowOptions(Sender: TObject);
    procedure OptionChanged(const OptionIndex: SG);
	end;

procedure CommonFileMenu(const Menu: TMenu);

var
  CommonMenu: TCommonMenu;

implementation

uses
  Forms,
  Classes,
  SysUtils,
  Windows,
  uWebUpdate,
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

procedure TCommonMenu.Restart1Click(Sender: TObject);
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

procedure TCommonMenu.Exit1Click(Sender: TObject);
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

procedure TCommonMenu.WebHomepage1Click(Sender: TObject);
begin
	OpenWebHomepage;
end;

procedure TCommonMenu.LocalHomepage1Click(Sender: TObject);
begin
	OpenLocalHomepage;
end;

procedure TCommonMenu.OptionChanged(const OptionIndex: SG);
begin
  uGlobalOptions.OptionChanged(OptionIndex);
end;

procedure TCommonMenu.ViewParams1Click(Sender: TObject);
begin
	CommonApplication.Arguments.WriteToCommonOutput;
end;

procedure MenuCheckForUpdate(AThread: TThread);
begin
	CommonMenu.CheckForUpdate1.Enabled := False;
	try
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
	APIOpen(MainIni.FileName);
	APIOpen(LocalMainIni.FileName);
end;

procedure TCommonMenu.ViewLogFile1Click(Sender: TObject);
begin
	APIOpen(MainLog.FileName);
end;

procedure TCommonMenu.ViewAllLogFiles1Click(Sender: TObject);
begin
	APIOpen(ExtractFilePath(MainLog.FileName));
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
		M.Name := 'Restart1';
		M.Caption := 'Restart Application';
		M.OnClick := CommonMenu.Restart1Click;
		File1.Add(M);

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
		M.Caption := 'View Options Files';
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

    if GetProjectInfo(piWeb) <> '' then
    begin
      M := TMenuItem.Create(Help1);
      M.Name := 'WebHomepage1';
      M.Caption := 'Web Homepage';
      M.OnClick := CommonMenu.WebHomepage1Click;
      Help1.Add(M);
    end;

		if FileExists(GetLocalHomepage) then
		begin
			M := TMenuItem.Create(Help1);
			M.Name := 'LocalHomepage1';
			M.Caption := 'Local Homepage';
			M.OnClick := CommonMenu.LocalHomepage1Click;
			Help1.Add(M);
		end;

		M := TMenuItem.Create(Help1);
		M.Name := 'Parameters1';
		M.Caption := 'View Parameters...';
		M.OnClick := CommonMenu.ViewParams1Click;
		Help1.Add(M);

		M := TMenuItem.Create(Help1);
		M.Caption := cLineCaption;
		Help1.Add(M);

    if GetProjectInfo(piWeb) <> '' then
    begin
      M := TMenuItem.Create(Help1);
      M.Name := 'CheckForUpdate1';
      M.Caption := 'Check For Update' + cDialogSuffix;
      M.OnClick := CommonMenu.CheckForUpdate1Click;
      Help1.Add(M);
      CommonMenu.CheckForUpdate1 := M;
    end;

		M := TMenuItem.Create(Help1);
		M.Name := 'About';
		M.Caption := 'About' + cDialogSuffix;
		M.OnClick := CommonMenu.About1Click;
		Help1.Add(M);
	end;
end;

initialization

{$IFNDEF NoInitialization}
CommonMenu := TCommonMenu.Create;
{$ENDIF NoInitialization}

finalization

{$IFNDEF NoFinalization}
FreeAndNil(CommonMenu);
{$ENDIF NoFinalization}

end.
