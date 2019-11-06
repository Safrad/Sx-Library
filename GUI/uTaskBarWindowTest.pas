unit uTaskbarWindowTest;

interface

uses
  uTypes,
  TestFrameWork;

type
  TTaskbarWindowTest = class(TTestCase)
  published
    procedure Test;
  end;

implementation

uses
  Winapi.Windows,

  uTaskbarWindow,
  uFiles;

{ TTaskbarWindowTest }

procedure TTaskbarWindowTest.Test;
begin
  CheckTrue(TaskbarWindow.WindowHandle <> 0);
  CheckTrue(TaskbarWindow.WindowHandle <> INVALID_HANDLE_VALUE);
  CheckTrue(TaskbarWindow.Position = poBottom);
  TaskbarWindow.Visible := False;
  try
    Sleep(1000);
  finally
    TaskbarWindow.Visible := True;
  end;
end;

initialization
	RegisterTest('Taskbar Window Test', TTaskbarWindowTest.Suite);
end.
