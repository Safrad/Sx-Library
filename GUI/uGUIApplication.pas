
unit uGUIApplication;

interface

uses
  uUIApplication;

type
  TGUIApplication = class(TUIApplication)
  protected
    procedure Initialize; override;
  public
    destructor Destroy; override;
  end;

implementation

uses
  uCommonOutput,
  uGUIOutputInfo;

{ TGUIApplication }

destructor TGUIApplication.Destroy;
begin
  try
    CommonOutput := nil; // Interface
  finally
    inherited;
  end;
end;

procedure TGUIApplication.Initialize;
begin
  CommonOutput := TGUIOutputInfo.Create;

  inherited;
end;

end.
