unit uWebUpdateApplicationModule;

interface

uses
  uTypes,
  uDelayedApplicationModule,
  uSwitchArgument,
  uTimeArgument;

type
  TWebUpdateApplicationModule = class(TDelayedApplicationModule)
  private
    FLastUpdate: TDateTime;
    FShowMessageIfSuccess: BG;
    FWebVersionURL: string;
    FWebDownloadURL: string;

    FAutomaticallyCheckForUpdate: TSwitchArgument;
    FCheckForUpdateDaysPeriod: TTimeArgument;

    procedure SetShowMessageIfSuccess(const Value: BG);
    function GetWebVersion: string;
    procedure SetWebVersionURL(const Value: string);
    procedure SetWebDownloadURL(const Value: string);
    procedure SetLastUpdate(const Value: TDateTime);
  protected
    procedure OnLoad; override;
    procedure OnUnload; override;
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property WebVersionURL: string read FWebVersionURL write SetWebVersionURL;
    property WebDownloadURL: string read FWebDownloadURL write SetWebDownloadURL;
    property ShowMessageIfSuccess: BG read FShowMessageIfSuccess write SetShowMessageIfSuccess;

    // Process
    procedure CheckForUpdate;

    // Output
    property LastUpdate: TDateTime read FLastUpdate write SetLastUpdate;
  end;

implementation

uses
  SysUtils,

  IdStack,

  uWebUpdate,
  uProjectInfo,
  uProjectVersion,
  uMsg,
  uOutputInfo,
  uAPI,
  uStrings,
  uApplicationModule,
  uLocalMainCfg;

{ TWebUpdateApplicationModule }

procedure TWebUpdateApplicationModule.CheckForUpdate;
var
	WebVersion, LocalVersion: string;
begin
  Assert(FWebVersionURL <> '');
  Assert(FWebDownloadURL <> '');

	WebVersion := GetWebVersion;
	if WebVersion = '?' then
		Exit;

  LastUpdate := Now;

	LocalVersion := GetProjectInfo(piProductVersion);
	case CompareVersion(WebVersion, LocalVersion) of
		crFirstGreater:
			begin
				if Confirmation('New version ' + WebVersion + ' is available. Your version is ' +
						LocalVersion + '. Do you want to download it?', [mbYes, mbNo]) = mbYes then
				begin
					APIOpen(FWebDownloadURL);
				end;
			end;
		crFirstLess:
			begin
				Warning('You are using newer version ' + LocalVersion + ' that version ' + WebVersion +
						' on the web!');
			end
		else
		begin
			if ShowMessageIfSuccess then
				Information('You are using the latest version ' + LocalVersion + '.');
		end;
	end;
end;

constructor TWebUpdateApplicationModule.Create;
begin
  inherited;

  StartupType := stOptional;
  FShowMessageIfSuccess := True;

  FAutomaticallyCheckForUpdate := TSwitchArgument.Create;
  FAutomaticallyCheckForUpdate.Shortcut := 'Automatically Check For Update';
  FAutomaticallyCheckForUpdate.DefaultValue := True;
//  CommonApplication.GlobalOptions.Add(FAutomaticallyCheckForUpdate);

  FCheckForUpdateDaysPeriod := TTimeArgument.Create;
  FCheckForUpdateDaysPeriod.Shortcut := 'Check For Update Days Period';
  FCheckForUpdateDaysPeriod.DefaultValue.Days := 14;
//  CommonApplication.GlobalOptions.Add(FCheckForUpdateDaysPeriod); TODO
end;

destructor TWebUpdateApplicationModule.Destroy;
begin
  try
    FAutomaticallyCheckForUpdate.Free;
    FCheckForUpdateDaysPeriod.Free;
  finally
    inherited;
  end;
end;

function TWebUpdateApplicationModule.GetWebVersion: string;
begin
	Result := '?';
	try
		Result := DownloadData(FWebVersionURL);
	except
		on E: Exception do
		begin
      if (E is EIdSocketError) and (EIdSocketError(E).LastError = 11004) then
  			Warning('No internet connection available!', [])
      else
  			ErrorMsg('%1, can not receive project version from %2!', [DelBESpaceF(E.Message), FWebVersionURL]);
		end;
	end;
end;

procedure TWebUpdateApplicationModule.OnLoad;
begin
  inherited;

  if FAutomaticallyCheckForUpdate.Value then
  begin
    LocalMainCfg.RWDateTime('Options', 'LastUpdate', FLastUpdate, False);
    if (Now - FLastUpdate > FCheckForUpdateDaysPeriod.Value.Days) then
      CheckForUpdate;
  end;
end;

procedure TWebUpdateApplicationModule.OnUnload;
begin
  inherited;

end;

procedure TWebUpdateApplicationModule.SetLastUpdate(const Value: TDateTime);
begin
  FLastUpdate := Value;
  LocalMainCfg.RWDateTime('Options', 'LastUpdate', FLastUpdate, True);
end;

procedure TWebUpdateApplicationModule.SetShowMessageIfSuccess(const Value: BG);
begin
  FShowMessageIfSuccess := Value;
end;

procedure TWebUpdateApplicationModule.SetWebDownloadURL(const Value: string);
begin
  FWebDownloadURL := Value;
end;

procedure TWebUpdateApplicationModule.SetWebVersionURL(const Value: string);
begin
  FWebVersionURL := ReplaceF(Value, 'https://', 'http://'); // Does not support SSL certificates
end;

end.
