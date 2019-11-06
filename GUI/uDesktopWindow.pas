unit uDesktopWindow;

interface

uses
  Winapi.Windows;

type
  TDesktopWindow = class
  private
    FDeviceContext: HDC;
    FWindowHandle: HWnd;
  public
    constructor Create;
    destructor Destroy; override;

    property WindowHandle: HWnd read FWindowHandle;
    property DeviceContext: HDC read FDeviceContext;
  end;

function DesktopWindow: TDesktopWindow;

implementation

uses
  SysUtils;

var
  GDesktopWindow: TDesktopWindow;

function DesktopWindow: TDesktopWindow;
begin
  if GDesktopWindow = nil then
    GDesktopWindow := TDesktopWindow.Create;

  Result := GDesktopWindow;
end;

{ TDesktopWindow }

constructor TDesktopWindow.Create;
begin
  inherited;

  FWindowHandle := GetDesktopWindow;
  if FWindowHandle <> INVALID_HANDLE_VALUE then
  begin
    FDeviceContext := GetDC(FWindowHandle);
    Assert(FDeviceContext <> 0);
  end
  else
    RaiseLastOSError;
end;

destructor TDesktopWindow.Destroy;
begin
  try
    if (FWindowHandle <> INVALID_HANDLE_VALUE) and (FDeviceContext <> 0) then
    begin
      Assert(ReleaseDC(FWindowHandle, FDeviceContext) = 1);
      FWindowHandle := INVALID_HANDLE_VALUE;
      FDeviceContext := 0;
    end;
  finally
    inherited;
  end;
end;

initialization

finalization
  FreeAndNil(GDesktopWindow);
end.
