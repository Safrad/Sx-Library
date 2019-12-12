unit uTaskBarWindow;

interface

uses
  uTypes,
  Winapi.Windows;

type
	TPosition = (poUnknown = -1, poLeft, poRight, poTop, poBottom);

  TTaskbarWindow = class
  private
    FWindowHandle: HWnd;
    FVisible: BG;
    FPosition: TPosition;
    procedure SetVisible(const Value: BG);
    procedure SetPosition(const Value: TPosition);
    function GetPosition: TPosition;
  public
    constructor Create;

    property WindowHandle: HWnd read FWindowHandle;
    property Visible: BG read FVisible write SetVisible;
    property Position: TPosition read GetPosition write SetPosition;
  end;

function TaskbarWindow: TTaskbarWindow;

implementation

uses
  SysUtils,
  Vcl.Forms,

  uMainLog;

var
  GTaskbarWindow: TTaskbarWindow;

function TaskbarWindow: TTaskbarWindow;
begin
  if GTaskbarWindow = nil then
    GTaskbarWindow := TTaskbarWindow.Create;

  Result := GTaskbarWindow;
end;

{ TTaskbarWindow }

constructor TTaskbarWindow.Create;
begin
  inherited;

  FWindowHandle := FindWindow('Shell_TrayWnd', nil);
  if FWindowHandle = 0 then
    RaiseLastOSError;
end;

function TTaskbarWindow.GetPosition: TPosition;
var
	RectT: TRect;
	w, h: SG;
begin
	GetWindowRect(FWindowHandle, RectT);
	w := Screen.Width;
	h := Screen.Height;

	if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Top <= 0) then
	begin
		Result := poTop;
	end
	else if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Bottom >= h) then
	begin
		Result := poBottom;
	end
	else if (RectT.Left <= 0) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
		Result := poLeft;
	end
	else if (RectT.Right >= w) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
		Result := poRight;
	end
	else
		Result := poUnknown;
end;

procedure TTaskbarWindow.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

procedure TTaskbarWindow.SetVisible(const Value: BG);
begin
  FVisible := Value;
	if FVisible then
	begin
		ShowWindow(FWindowHandle, SW_SHOWNA);
		if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('ShowTaskBar', mlDebug);
	end
	else
	begin
		ShowWindow(FWindowHandle, SW_HIDE);
		if MainLog.IsLoggerFor(mlDebug) then
      MainLog.Add('HideTaskBar', mlDebug);
	end;
end;

initialization

finalization
  FreeAndNil(GTaskbarWindow);
end.
