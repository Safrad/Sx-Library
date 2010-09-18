unit uScreen;

interface

uses Graphics;

function ScreenModeToStr(const Width, Height, Bits: Word): string;
function SetScreenMode(const Width, Height, Bits: Word; const Test: Boolean;
	const UpdateRegistry: Boolean): Integer;

type
	TScreenMode = record // 8
		Width, // 2
		Height, // 2
		Bits: Word; // 2
		Reserved: Word; // 2
	end;

var
	ScreenModes: array of TScreenMode;
	OrigScreenModeIndex,
	ScreenModeIndex: Integer;
	ScreenModeCount: Integer; // 1 because we have a default mode
	ScreenCorectColor: TColor;

type
	TLowResMode = record // 8
		 Width, // 4
		 Height: Integer; // 4
	 end;

const
	LowResModesBits: array[0..4] of Integer = (8, 15, 16, 24, 32);

	LowResModes: array[0..13] of TLowResMode = (
		(Width: 320; Height: 200),
		(Width: 320; Height: 240),
		(Width: 320; Height: 350),
		(Width: 320; Height: 400),
		(Width: 320; Height: 480),

		(Width: 360; Height: 200),
		(Width: 360; Height: 240),
		(Width: 360; Height: 350),
		(Width: 360; Height: 400),
		(Width: 360; Height: 480),

		(Width: 400; Height: 300),
		(Width: 512; Height: 384),
		(Width: 640; Height: 350),
		(Width: 640; Height: 400));

procedure ReadScreenModes;
function RestoreDefaultMode: Boolean;
function SetFullScreenMode(const ModeIndex: Integer;
	const UpdateRegistry: Boolean): Boolean;

implementation

uses
	uError,
	Windows, Forms, SysUtils;

procedure TryToAddToList(DeviceMode: TDeviceMode);
// Adds a Screen mode to the list if it's not a duplicate and can actually be set.
var I: Integer;
begin
	// See if this is a duplicate mode (can happen because of refresh
	// rates, or because we explicitly try all the low-res modes)
	for I := 1 to ScreenModeCount - 1 do
	begin
		if ((DeviceMode.dmBitsPerPel = ScreenModes[I].Bits) and
			(DeviceMode.dmPelsWidth  = ScreenModes[I].Width) and
			(DeviceMode.dmPelsHeight = ScreenModes[I].Height)) then Exit; // it's a duplicate mode
	end;

	// do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
	if ChangeDisplaySettings(DeviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then Exit;

	// it's a new, valid mode, so add this to the list
	SetLength(ScreenModes, ScreenModeCount + 1);
	ScreenModes[ScreenModeCount].Bits := DeviceMode.dmBitsPerPel;
	ScreenModes[ScreenModeCount].Width := DeviceMode.dmPelsWidth;
	ScreenModes[ScreenModeCount].Height := DeviceMode.dmPelsHeight;
	Inc(ScreenModeCount);
end;

procedure ReadScreenModes;
var
	ModeNumber: Integer;
	done: Boolean;
	DeviceMode: TDeviceMode;
	DeskDC: HDC;
	Res, Bits: Integer;
	i: Integer;
	ScreenMode: TScreenMode;
begin
	ScreenModeCount := 0;
	SetLength(ScreenModes, 0);

	// low-res modes don't always enumerate, ask about them explicitly
	DeviceMode.dmBitsPerPel := 8;
	DeviceMode.dmPelsWidth := 42;
	DeviceMode.dmPelsHeight := 37;
	DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
	// make sure the driver doesn't just answer yes to all tests
	if ChangeDisplaySettings(DeviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then
	begin
		for Bits := Low(LowResModesBits) to High(LowResModesBits) do
		for Res := Low(LowResModes) to High(LowResModes) do
		begin
			DeviceMode.dmSize := SizeOf(DeviceMode);
			DeviceMode.dmBitsPerPel := LowResModesBits[Bits];
			DeviceMode.dmPelsWidth := LowResModes[Res].Width;
			DeviceMode.dmPelsHeight := LowResModes[Res].Height;
			DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
			TryToAddToList(DeviceMode);
		end;
	end;

	// enumerate all available Screen modes
	ModeNumber := 0;
	repeat
		done := not EnumDisplaySettingsA(nil, ModeNumber, DeviceMode);
		// Windows.EnumDi
		TryToAddToList(DeviceMode);
		Inc(ModeNumber);
	until done;

	DeskDC := GetDC(0);
	try
		ScreenMode.Bits := GetDeviceCaps(DeskDC, BITSPIXEL);
		ScreenMode.Width := Screen.Width;
		ScreenMode.Height := Screen.Height;
	finally
		ReleaseDC(0, DeskDC);
	end;

	// prepare 'default' entry
	ScreenModeIndex := 0;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if (ScreenModes[i].Width = ScreenMode.Width)
		and (ScreenModes[i].Height = ScreenMode.Height)
		and (ScreenModes[i].Bits = ScreenMode.Bits)
		then ScreenModeIndex := i;
	end;
	OrigScreenModeIndex := ScreenModeIndex;
end;

function RestoreDefaultMode: Boolean;
// restores default desktop Screen mode
var DeviceMode: TDeviceMode absolute 0; // a little trick to create a nil Pointer
begin
	// Since the first parameter must be a var, we cannot use nil directly. Instead
	//  we use a variable with an absolute address of 0.
	Result := ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
	if Result then ScreenModeIndex := OrigScreenModeIndex;
end;

function SetFullScreenMode(const ModeIndex: Integer;
	const UpdateRegistry: Boolean): Boolean;
// changes to the Screen mode given by 'ModeIndex'
var
	DeviceMode: TDeviceMode;
	Flags: DWORD;
begin
	DeviceMode.dmSize := SizeOf(DeviceMode);
	DeviceMode.dmBitsPerPel := ScreenModes[ModeIndex].Bits;
	DeviceMode.dmPelsWidth := ScreenModes[ModeIndex].Width;
	DeviceMode.dmPelsHeight := ScreenModes[ModeIndex].Height;
	DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
	// if mode set failed, we'll just run in windowed mode
	Flags := CDS_FULLSCREEN;
	if UpdateRegistry then Flags := Flags or CDS_UPDATEREGISTRY;
	Result := ChangeDisplaySettings(DeviceMode, Flags) = DISP_CHANGE_SUCCESSFUL;
	if Result then ScreenModeIndex := ModeIndex;
end;

function ScreenModeToStr(const Width, Height, Bits: Word): string;
begin
	Result := IntToStr(Width) + 'x' + IntToStr(Height) + 'x' + IntToStr(Bits);
end;

function SetScreenMode(const Width, Height, Bits: Word; const Test: Boolean;
	const UpdateRegistry: Boolean): Integer;
var
	i: Integer;
	BestMode: Integer;
	BitsDif: Integer;
	NowDif, BestDif: LongWord;
begin
	BestMode := 0;
	BestDif := High(BestDif);
	for i := 1 to ScreenModeCount - 1 do
	begin
		BitsDif := ScreenModes[i].Bits - Bits;
		if BitsDif < 0 then BitsDif := -4 * BitsDif;
		NowDif :=
			Abs(ScreenModes[i].Width - Width) +
			Abs(ScreenModes[i].Height - Height) +
			BitsDif;
		if NowDif < BestDif then
		begin
			BestMode := i;
			BestDif := NowDif;
		end;
	end;

	Result := BestMode;
	if not Test then
	begin
		if (ScreenModes[BestMode].Width <> Width)
		or (ScreenModes[BestMode].Height <> Height)
		or ((ScreenModes[BestMode].Bits <> Bits)
		and ((ScreenModes[BestMode].Bits <> 32) or (Bits <> 24))
		and ((ScreenModes[BestMode].Bits <> 24) or (Bits <> 32))) then
			ErrorMessage('Screen mode ' + ScreenModeToStr(Width, Height, Bits) +
				' can not be set, using ' +
				ScreenModeToStr(ScreenModes[BestMode].Width, ScreenModes[BestMode].Height, ScreenModes[BestMode].Bits));
		if SetFullScreenMode(BestMode, UpdateRegistry) then
		begin

		end;
	end;
end;

procedure InitScreenCorectColor;
var DeskDC: HDC;
begin
	DeskDC := GetDC(0);
	try
		case GetDeviceCaps(DeskDC, BITSPIXEL) of
		1..5: ScreenCorectColor := $001f1f1f;
		6..11: ScreenCorectColor := $000f0f0f;
		12..19: ScreenCorectColor := $00070707;
		else ScreenCorectColor := $00000000;
		end;
	finally
		ReleaseDC(0, DeskDC);
	end;
end;

initialization
 InitScreenCorectColor;
end.
