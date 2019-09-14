unit uCustomSplashScreen;

interface

type
  TCustomSplashScreen = class
  public
    procedure Show; virtual; abstract;
    procedure AddMessage(const AText: string); virtual; abstract;
    procedure Hide; virtual; abstract;
  end;

implementation

end.
