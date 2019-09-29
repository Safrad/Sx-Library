unit uGUISplashScreen;

interface

uses
  Forms,

  uSplash,
  uCustomSplashScreen;

type
  TGUISplashScreen = class(TCustomSplashScreen)
  private
    fSplash: TfSplash;
  public
    destructor Destroy; override;

    procedure Show; override;
    procedure AddMessage(const AText: string); override;
    procedure Hide; override;
  end;

implementation

uses
  SysUtils;

{ TGUISplashScreen }

procedure TGUISplashScreen.AddMessage(const AText: string);
begin
  inherited;

  fSplash.LabelState.Caption := AText;
  fSplash.LabelState.Update;
end;

destructor TGUISplashScreen.Destroy;
begin
  try
    FreeAndNil(fSplash);
  finally
    inherited;
  end;
end;

procedure TGUISplashScreen.Hide;
begin
  inherited;

  fSplash.WantClose;
end;

procedure TGUISplashScreen.Show;
begin
  inherited;

  fSplash := TfSplash.Create(Application.MainForm);
  fSplash.Show;
end;

end.
