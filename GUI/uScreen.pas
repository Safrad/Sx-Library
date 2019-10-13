unit uScreen;

interface

uses
  Generics.Collections,

  uScreenMode,
  uTypes;

type
  TScreen = class
  private
    FAreScreenModesInitialized: BG;
    FScreenModes: TScreenModes;
    FStartMode: TScreenMode;
    FLastMode: TScreenMode;
    procedure RaiseExceptionIfError(const AErrorCode: SG);
    function ChangeDisplaySettingsErrorCodeToString(const AErrorCode: SG): string;
    procedure AddMode(const AWidth, AHeight, ABits: UG);
  public
    constructor Create;
    destructor Destroy; override;

    function FindNearestMode(const AScreenMode: TScreenMode; out AExactlySame: BG): TScreenMode;
    function GetNowScreenMode: TScreenMode;
    procedure ReadScreenModes;
    procedure RestoreStartMode;
    procedure RestoreLastMode;
    procedure SetNearestScreenMode(const AScreenMode: TScreenMode; const ATest: Boolean);
    procedure SetScreenMode(const AScreenMode: TScreenMode; const ATest: Boolean);
  end;

function Screen: TScreen;

implementation

uses
  SysUtils,
  Winapi.Windows,

  uOutputFormat,
  uLog;

var
  GScreen: TScreen;

function Screen: TScreen;
begin
  if GScreen = nil then
    GScreen := TScreen.Create;

  Result := GScreen;
end;

{ TScreen }

function TScreen.ChangeDisplaySettingsErrorCodeToString(const AErrorCode: SG): string;
const
  DISP_CHANGE_BADDUALVIEW = -6;
begin
  case AErrorCode of
  DISP_CHANGE_SUCCESSFUL: Result := 'The settings change was successful.';
  DISP_CHANGE_BADDUALVIEW: Result := 'The settings change was unsuccessful because the system is DualView capable.';
  DISP_CHANGE_BADFLAGS: Result := 'An invalid set of flags was passed in.';
  DISP_CHANGE_BADMODE: Result := 'The graphics mode is not supported.';
  DISP_CHANGE_BADPARAM: Result := 'An invalid parameter was passed in. This can include an invalid flag or combination of flags.';
  DISP_CHANGE_FAILED: Result := 'The display driver failed the specified graphics mode.';
  DISP_CHANGE_NOTUPDATED: Result := 'Unable to write settings to the registry.';
  DISP_CHANGE_RESTART: Result := 'The computer must be restarted for the graphics mode to work.';
  else
    Result := 'Unknown error code (' + IntToStr(AErrorCode) + ').';
  end;
end;

constructor TScreen.Create;
begin
  inherited;

  FStartMode := GetNowScreenMode;
  FScreenModes := TScreenModes.Create;
end;

destructor TScreen.Destroy;
begin
  try
    FScreenModes.Free;
  finally
    inherited;
  end;
end;

function TScreen.FindNearestMode(const AScreenMode: TScreenMode; out AExactlySame: BG): TScreenMode;
var
  i: Integer;
	NowDif, BestDif: UG;
begin
  BestDif := High(BestDif);
  for i := 0 to FScreenModes.Count - 1 do
  begin
    NowDif :=
      Sqr(SG(FScreenModes[i].Width) - SG(AScreenMode.Width)) +
      Sqr(SG(FScreenModes[i].Height) - SG(AScreenMode.Height)) +
      1000 * Sqr(SG(FScreenModes[i].Bits) - SG(AScreenMode.Bits));
    if NowDif < BestDif then
    begin
      Result := FScreenModes[i];
      if NowDif = 0 then
        AExactlySame := True;
      BestDif := NowDif;
    end;
  end;
end;

function TScreen.GetNowScreenMode: TScreenMode;
var
	DeskDC: HDC;
begin
	DeskDC := GetDC(0);
	try
		Result.Bits := GetDeviceCaps(DeskDC, BITSPIXEL);
		Result.Width := GetSystemMetrics(SM_CXSCREEN);
		Result.Height := GetSystemMetrics(SM_CYSCREEN);
	finally
		ReleaseDC(0, DeskDC);
	end;
end;

procedure TScreen.RaiseExceptionIfError(const AErrorCode: SG);
begin
  if AErrorCode <> DISP_CHANGE_SUCCESSFUL then
    raise EArgumentException.Create(ChangeDisplaySettingsErrorCodeToString(AErrorCode));
end;

procedure TScreen.ReadScreenModes;
var
	ModeNumber: Integer;
	done: Boolean;
	DeviceMode: TDeviceModeA;
begin
  FAreScreenModesInitialized := True;
	// Enumerates all available Screen modes
	ModeNumber := 0;
	FillChar(DeviceMode, SizeOf(DeviceMode), 0);
	DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
	while True do
	begin
		DeviceMode.dmBitsPerPel := 0;
		DeviceMode.dmPelsWidth := 0;
		DeviceMode.dmPelsHeight := 0;
		Done := not EnumDisplaySettingsA(nil, ModeNumber, DeviceMode); // Takes long time if is called first time
		if Done then Break;
    AddMode(DeviceMode.dmPelsWidth, DeviceMode.dmPelsHeight, DeviceMode.dmBitsPerPel);
		Inc(ModeNumber);
	end;
end;

procedure TScreen.RestoreLastMode;
begin
  if FLastMode.Width = 0 then
    raise Exception.Create('Can not restore last mode. No mode was set until now.');

	SetScreenMode(FLastMode, False);
end;

procedure TScreen.RestoreStartMode;
begin
	SetScreenMode(FStartMode, False);
end;

procedure TScreen.SetNearestScreenMode(const AScreenMode: TScreenMode; const ATest: Boolean);
var
  NearestMode: TScreenMode;
  AExactlySame: BG;
begin
  if not FAreScreenModesInitialized then
    ReadScreenModes;

	NearestMode := FindNearestMode(AScreenMode, AExactlySame);
  if not AExactlySame then
  begin
    if LogWarning then
      MainLog.Add('Screen mode ' + AScreenMode.AsString(ofIO) + ' can not be set, using ' + NearestMode.AsString(ofIO) + '.', mlWarning);
  end;
  SetScreenMode(NearestMode, ATest);
end;

procedure TScreen.SetScreenMode(const AScreenMode: TScreenMode; const ATest: Boolean);
var
  DeviceMode: TDeviceMode;
  LastMode: TScreenMode;
  ReturnCode: SG;
  Flags: DWORD;
begin
  if not FAreScreenModesInitialized then
    ReadScreenModes;

  Flags := CDS_FULLSCREEN;
	if ATest then
    Flags := Flags or CDS_TEST
  else
    LastMode := GetNowScreenMode;
  FillChar(DeviceMode, SizeOf(DeviceMode), 0);
  DeviceMode.dmSize := SizeOf(DeviceMode);
  DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
  DeviceMode.dmPelsWidth := AScreenMode.Width;
  DeviceMode.dmPelsHeight := AScreenMode.Height;
  DeviceMode.dmBitsPerPel := AScreenMode.Bits;
  ReturnCode := ChangeDisplaySettings(DeviceMode, Flags);
  RaiseExceptionIfError(ReturnCode);
	if not ATest then
    FLastMode := LastMode;
end;

procedure TScreen.AddMode(const AWidth, AHeight, ABits: UG);
var
  ScreenMode: TScreenMode;
begin
  ScreenMode.Width := AWidth;
  ScreenMode.Height := AHeight;
  ScreenMode.Bits := ABits;
  if not FScreenModes.Find(ScreenMode) then
  begin
    if LogDebug then
      MainLog.Add('Found screen mode ' + ScreenMode.AsString(ofIO), mlDebug);
    FScreenModes.Add(ScreenMode);
  end;
end;

initialization

finalization
{$IFNDEF NoFinalization}
  FreeAndNil(GScreen);
{$ENDIF NoFinalization}
end.
