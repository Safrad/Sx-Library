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
  uMsg,
  uProjectInfo,
  uStrings;

{ TConsoleSplashScreen }

procedure TConsoleSplashScreen.AddMessage(const AMessage: string);
begin
  inherited;

  Information(AMessage);
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
var
  LegalCopyrightAndCompany: string;
begin
  inherited;

  if FShowVersionInfo then
  begin
    Console.WriteLine(GetProjectInfo(piProductName) + ' [Version ' + GetProjectInfo(piProductVersion) + ']', ccWhite);
    if GetProjectInfo(piFileDescription) <> '' then
      Console.WriteLine(GetProjectInfo(piFileDescription), ccLightGray);
    LegalCopyrightAndCompany := '';
    if GetProjectInfo(piLegalCopyright) <> '' then
      AppendStrSeparator(LegalCopyrightAndCompany, GetProjectInfo(piLegalCopyright), CharSpace);
    if GetProjectInfo(piCompanyName) <> '' then
      AppendStrSeparator(LegalCopyrightAndCompany, GetProjectInfo(piCompanyName), CharSpace);
    if LegalCopyrightAndCompany <> '' then
      Console.WriteLine(LegalCopyrightAndCompany, ccGray);
    Console.WriteLine('');
  end;
end;

end.
