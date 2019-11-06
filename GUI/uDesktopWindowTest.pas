unit uDesktopWindowTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TDesktopWindowTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  Winapi.Windows,

  uDesktopWindow,
  uFiles;

{ TDesktopWindowTest }

procedure TDesktopWindowTest.Test;
begin
  CheckTrue(DesktopWindow.WindowHandle <> INVALID_HANDLE_VALUE);
  CheckTrue(DesktopWindow.DeviceContext <> 0);
end;

initialization
	RegisterTest('Desktop Window Test', TDesktopWindowTest.Suite);
end.
