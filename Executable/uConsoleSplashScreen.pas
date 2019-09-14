unit uConsoleSplashScreen;

interface

uses
  uTypes,
  uCustomSplashScreen;

type
  TConsoleSplashScreen = class(TCustomSplashScreen)
  private
    FShowVersionInfo: BG;
    procedure SetShowVersionInfo(const Value: BG);
  public
    constructor Create;

    procedure Show; override;
    procedure AddMessage(const AMessage: string); override;
    procedure Hide; override;
    property ShowVersionInfo: BG read FShowVersionInfo write SetShowVersionInfo;
  end;

implementation

uses
  uConsole,
  uConsoleColor,
  uProjectInfo,
  uStrings;

{ TConsoleSplashScreen }

procedure TConsoleSplashScreen.AddMessage(const AMessage: string);
begin
  inherited;

  TConsole.WriteLine(AMessage);
end;

constructor TConsoleSplashScreen.Create;
begin
  inherited;

  FShowVersionInfo := True;
end;

procedure TConsoleSplashScreen.Hide;
begin
  inherited;

  // No code
end;

procedure TConsoleSplashScreen.SetShowVersionInfo(const Value: BG);
begin
  FShowVersionInfo := Value;
end;

procedure TConsoleSplashScreen.Show;
begin
  inherited;

  if FShowVersionInfo then
  begin
    TConsole.WriteLine(GetProjectInfo(piProductName) + ' [Version ' + GetProjectInfo(piProductVersion) + ']', ccWhite);
    if GetProjectInfo(piFileDescription) <> '' then
      TConsole.WriteLine(GetProjectInfo(piFileDescription), ccLightGray);
    if GetProjectInfo(piLegalCopyright) <> '' then
      TConsole.WriteLine(GetProjectInfo(piLegalCopyright) + CharSpace + GetProjectInfo(piCompanyName), ccGray);
    TConsole.WriteLine('');
  end;
end;

end.
