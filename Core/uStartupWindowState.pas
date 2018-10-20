unit uStartupWindowState;

interface

type
  THideableWindowState = (hwsHidden, hwsMinimized, hwsNormal, hwsMaximized);

  TStartupWindowState = record
    WindowState: THideableWindowState;
    Active: Boolean;

    function ToString: string;
    procedure FromString(const AValue: string);
    function ToWindowsAPIParameter: Integer;
    procedure SetActive(const AActive: Boolean);
    procedure SetHideableWindowState(const AHideableWindowState: THideableWindowState);
  end;

implementation

uses
  uTypes,
  SysUtils,
  Windows,
  uStrings;

{ TStartupWindowState }

procedure TStartupWindowState.FromString(const AValue: string);
var
  WS: string;
  InLineIndex: SG;
begin
  InLineIndex := 1;
  WS := ReadToChar(AValue, InLineIndex, ';');
  if AValue = 'Hidden' then
    WindowState := hwsHidden
  else if AValue = 'Normal' then
    WindowState := hwsNormal
  else if AValue = 'Minimized' then
    WindowState := hwsMinimized
  else if AValue = 'Maximized' then
    WindowState := hwsMaximized
  else
    raise EArgumentException.Create('Invalid value ''' + AValue + '''');
  WS := ReadToChar(AValue, InLineIndex, ';');
  if WS = 'Active' then
    Active := True
  else if WS = 'Inactive' then
    Active := False
end;

function TStartupWindowState.ToWindowsAPIParameter: Integer;
begin
  case WindowState of
    hwsHidden:
    begin
      Result := SW_HIDE
    end;
    hwsMinimized:
    begin
      if Active then
        Result := SW_SHOWMINIMIZED // Works only in console
      else
        Result := SW_SHOWMINNOACTIVE
    end;
    hwsNormal:
    begin
      if Active then
        Result := SW_SHOWNORMAL // SW_SHOWDEFAULT
      else
        Result := SW_SHOWNOACTIVATE; // SW_SHOWNA
    end;
    hwsMaximized:
    begin
      Result := SW_SHOWMAXIMIZED;
    end;
    else
      raise EArgumentException.Create('Invalid WindowState parameter.');
  end;
end;

procedure TStartupWindowState.SetActive(const AActive: Boolean);
begin
  Active := AActive;
end;

procedure TStartupWindowState.SetHideableWindowState(const AHideableWindowState: THideableWindowState);
begin
  WindowState := AHideableWindowState;
end;

function TStartupWindowState.ToString: string;
begin
  case WindowState of
    hwsHidden: Result := 'Hiden';
    hwsMinimized: Result := 'Minimized';
    hwsNormal: Result := 'Normal';
    hwsMaximized: Result := 'Maximized';
  end;

  if Active then
    Result := Result + ';Active'
  else
    Result := Result + ';Inactive';
end;

end.
