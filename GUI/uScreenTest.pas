unit uScreenTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TScreenTest = class(TTestCase)
  published
    procedure Test;
    procedure TestFail;
    procedure TestChange;
  end;

implementation

uses
  SysUtils,

  uScreen,
  uScreenMode;

{ TScreenTest }

procedure TScreenTest.Test;
var
  ScreenMode: TScreenMode;
begin
  ScreenMode.Width := 1920;
  ScreenMode.Height := 1080;
  ScreenMode.Bits := 32;
  Screen.SetScreenMode(ScreenMode, True);
  ScreenMode.Width := 0;
  ScreenMode.Height := 0;
  ScreenMode.Bits := 32;
  Screen.SetScreenMode(ScreenMode, True);
end;

procedure TScreenTest.TestChange;
var
  ScreenMode: TScreenMode;
begin
  Exit;

  ScreenMode.Width := 1920;
  ScreenMode.Height := 1080;
  ScreenMode.Bits := 32;
  Screen.SetScreenMode(ScreenMode, False);
  try
    Sleep(5000);
    ScreenMode.Width := 1280;
    ScreenMode.Height := 720;
    ScreenMode.Bits := 32;
    Screen.SetScreenMode(ScreenMode, False);
    Sleep(5000)
  finally
    Screen.RestoreStartMode;
  end;
end;

procedure TScreenTest.TestFail;
var
  ScreenMode: TScreenMode;
begin
  StartExpectingException(EArgumentException);
  ScreenMode.Width := 4444;
  ScreenMode.Height := 3354;
  ScreenMode.Bits := 16;
  Screen.SetScreenMode(ScreenMode, True);
  StopExpectingException;
end;

initialization
	RegisterTest('Screen Test', TScreenTest.Suite);
end.
