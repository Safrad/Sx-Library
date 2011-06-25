//* File:     Lib\GUI\uScreen.pas
//* Created:  1999-08-01
//* Modified: 2007-08-19
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uScreen;

interface

uses
	uTypes, uMath,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	StdCtrls, uDButton, uDForm;

type
	TfScreen = class(TDForm)
		ComboBoxDriver: TComboBox;
		ButtonOk: TDButton;
		procedure FormCreate(Sender: TObject);
	private
		{ Private declarations }
	public
		{ Public declarations }
	end;

	TRefreshRateList = array of UG;
	TScreenMode = packed record // 16
		Width,
		Height, Bits: UG; // 12
		RefreshRate: UG; // 4
	end;
	TScreenModeEx = packed record // 32
		Width,
		Height, Bits: UG; // 12
		RefreshRate: UG; // 4
		RefreshRateList: TRefreshRateList; // 4
		RefreshRateListCount: Integer; // 4
		Reserved: array[0..1] of Integer; // 8
	end;

function GetVF(Height, HF: UG): UG; // Hz
function GetHF(Height, RefreshRate: UG): UG; // KHz
function GetHeight(RefreshRate, HF: UG): UG; // Pixels
function GetPixelRate(const Width, HF: UG): UG; // MHz
function GetVideoMemory(const Width, Height, Bits: UG): UG; // Bytes
function ScreenModeToStr(const Width, Height: UG): string; overload;
function ScreenModeToStr(const Width, Height, Bits: UG): string; overload;
function ScreenModeToStr(const ScreenMode: TScreenMode): string; overload;

procedure ReadScreenModes;
procedure ReadNowMode;
function RateListToStr(RefreshRateList: TRefreshRateList; RefreshRateListCount: Integer): string;
function CorrectWidth(Width: UG): UG;
function CorrectHeight(Height: UG): UG;
procedure AddLastMode(const ScreenMode: TScreenMode);


function SetScreenMode(const ScreenMode: TScreenMode;
	const Test, UpdateRegistry, Confirm, CanCreate, SaveLast: Boolean): Boolean;
procedure SetSafeMode;
function RestoreStartMode: Boolean;
procedure FillRefreshRates(Index, VF: UG);
function DeleteScreenMode(Width, Height, Bits: UG): Boolean;

var
	StartScreenMode: TScreenMode;
	NowScreenMode: TScreenMode;
	LastModes: array of TScreenMode;
	LastModeCount, LastModeIndex: Integer;

	DriverDesc, DriverDate: string;
	DriverNames: array of string;
	DriverNameCount: SG;
	ScreenModes: array of TScreenModeEx;
	EnabledBits: array of U4; // 4, 8, 15, 16, 24, 32
	EnabledBitsCount: SG;

	NotFirstTime: Boolean;

	SelfChange: Boolean;

	ScreenModeIndex: Integer;
	ScreenModeCount: Integer;
//	ScreenCorrectColor: TColor;

	ActualDriver: Integer;

	MinWidth, MinHeight: UG;
	RetraceDelay: Integer;
	MinVF, MaxVF, UserMaxVF,
	MinHF, MaxHF, UserMaxHF,
	MinPixelRate, MaxPixelRate, UserMaxPixelRate,
	MinMemory, MaxMemory, UserMaxMemory: UG;

	fScreen: TfScreen;
const
	DefaultRetraceDelay = 560;

	WorstVF = 30;
	SafeVF = 60;
	ErgoVF = 80;
	BestVF = 200;

	WorstHF = 30000;
	BestHF = 700000;

	WorstPixelRate = 1000000;
	BestPixelRate = 1000000000;

	WorstMemory = MB;
	BestMemory = 256 * MB;

	DoubleHeight = 399; // and less
	MaxScreenWidth = 4096;
	MaxScreenHeight = 3072;

implementation

{$R *.DFM}
uses
	Registry, Math, MMSystem,
	uMsg, uStrings, uWave, uFiles, uGetInt, uDIniFile, uInputFormat, uOutputFormat, uSystem;
var
	First: Boolean;

function GetVF(Height, HF: UG): UG;
begin
	if (Height = 0) or (HF = 0) then
		Result := 0
	else
	begin
		if Height <= DoubleHeight then Height := Height * 2;
//  Result := HF div Height;
//  Result := Trunc(HF / (Height + (RetraceDelay / 1000000) * HF))
		Result := (Int64(HF) * 1000000) div (Int64(Height) * 1000000 + Int64(RetraceDelay) * Int64(HF));
	end;
end;

function GetHF(Height, RefreshRate: UG): UG;
begin
	if Height <= DoubleHeight then Height := Height * 2;
//  Result := Height * RefreshRate;
	if RefreshRate = 0 then
		Result := 0
	else
//    Result := Floor(Height / ((1 / RefreshRate) - UG(RetraceDelay) / 1000000));
		Result := MaxDivS8((S8(Height) * S8(RefreshRate) * 1000000), (1000000 - S8(RetraceDelay) * S8(RefreshRate)));

end;

function GetHeight(RefreshRate, HF: UG): UG;
begin
	Result := (1000000 * S8(HF) - S8(RefreshRate) * S8(HF) * S8(RetraceDelay)) div (1000000 * S8(RefreshRate));
	if Result <= DoubleHeight then Result := Result div 2;
end;

function GetPixelRate(const Width, HF: UG): UG;
begin
	Result := Width * HF; // +
end;

function GetVideoMemory(const Width, Height, Bits: UG): UG;
begin
	Result := 4 * ((Width * Bits + 31) div 32);
	Result := Result * Height;
end;

function ScreenModeToStr(const Width, Height: UG): string; overload;
begin
	Result :=
		NToS(Width) + CharTimes + NToS(Height);
end;

function ScreenModeToStr(const Width, Height, Bits: UG): string; overload;
begin
	Result := ScreenModeToStr(Width, Height);
	if Bits <> 0 then Result := Result + CharTimes + NToS(Bits) + ' bit';
end;

function ScreenModeToStr(const ScreenMode: TScreenMode): string; overload;
begin
	Result := ScreenModeToStr(ScreenMode.Width, ScreenMode.Height, ScreenMode.Bits);
	if ScreenMode.RefreshRate <> 0 then
		Result := Result + '/' + NToS(ScreenMode.RefreshRate) + ' Hz';
end;

function ScreenModeToStr(const ScreenMode: TScreenModeEx): string; overload;
begin
	Result := ScreenModeToStr(ScreenMode.Width, ScreenMode.Height, ScreenMode.Bits);
	if ScreenMode.RefreshRate <> 0 then
		Result := Result + '/' + NToS(ScreenMode.RefreshRate) + ' Hz';
end;

procedure AddMode(Width, Height, Bits: UG);
var
	Index: Integer;
	i: Integer;
	NewSize: SG;
begin
	Index := ScreenModeCount;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if ScreenModes[i].Width * ScreenModes[i].Height >
			Width * Height then
			begin
				Index := i;
				Break;
			end;
	end;

	NewSize := ScreenModeCount + 1;
	if AllocByExp(Length(ScreenModes), NewSize) then
		SetLength(ScreenModes, NewSize);
	for i := ScreenModeCount - 1 downto Index do
	begin
		ScreenModes[i + 1] := ScreenModes[i];
	end;

	ScreenModes[Index].Width := Width;
	ScreenModes[Index].Height := Height;
	ScreenModes[Index].Bits := Bits;
//  ScreenModes[Index].Duplicit := Found;

//  s := DeviceMode.dmDeviceName;  //''
{ i := DeviceMode.dmSpecVersion;  // 5
	i := DeviceMode.dmDriverVersion; // 35778
	i := DeviceMode.dmDriverExtra;    // 0
	i := DeviceMode.dmFields;      // 1385040
	i := DeviceMode.dmDisplayFlags;    // 0
	i := DeviceMode.dmDisplayFrequency; // 0
	i := DeviceMode.dmICCManufacturer; // 6385664
	i := DeviceMode.dmICCModel; // 4883048  GeF 4884968}


	Inc(ScreenModeCount);
end;

procedure TryToAddToList(DeviceMode: TDeviceMode);
var
	I: Integer;
	Found: Boolean;
begin
	for I := 0 to ScreenModeCount - 1 do
	begin
		if ((DeviceMode.dmPelsWidth  = ScreenModes[I].Width) and
			(DeviceMode.dmPelsHeight = ScreenModes[I].Height) and
			(DeviceMode.dmBitsPerPel = ScreenModes[I].Bits)) then Exit; // it's a duplicate mode
	end;

	// do a mode set test (doesn't actually do the mode set, but reports whether it would have succeeded).
{ SelfChange := True;
	if ChangeDisplaySettings(DeviceMode, CDS_TEST or CDS_FULLSCREEN) <> DISP_CHANGE_SUCCESSFUL then Exit;
	SelfChange := False;}

	Found := False;
	for i := 0 to EnabledBitsCount - 1 do
	begin
		if EnabledBits[i] = DeviceMode.dmBitsPerPel then
		begin
			Found := True;
			Break;
		end;
	end;
	if Found = False then
	begin
		Inc(EnabledBitsCount);
		SetLength(EnabledBits, EnabledBitsCount);
		EnabledBits[EnabledBitsCount - 1] := DeviceMode.dmBitsPerPel;
	end;
{ Found := False;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if (ScreenModes[i].Width = DeviceMode.dmPelsWidth) and
			(ScreenModes[i].Height = DeviceMode.dmPelsHeight) then
			begin
				Found := True;
				Break;
			end;
	end;}

	AddMode(DeviceMode.dmPelsWidth, DeviceMode.dmPelsHeight, DeviceMode.dmBitsPerPel);
end;

function GetNowScreenMode(): TScreenMode;
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

procedure ReadScreenModes;
var
	ModeNumber: Integer;
	done: Boolean;
	DeviceMode: TDeviceMode;
	i, j: Integer;

	Reg: TRegistry;
	Key: string;
	S: string;
	HF, PixelRate: UG;
	DefVF: UG;

	f: UG;
	InLineIndex: Integer;
	Found: Boolean;
	Index: Integer;
	NewSize: SG;

	Ram: UG;
begin
	BeginLongOperation;

	if First = False then
	begin
		DriverNameCount := 0;
		SetLength(DriverNames, 0);

		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_LOCAL_MACHINE;
			for i := 0 to 15 do
			begin
				Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(i, '0000');
				if Reg.KeyExists(Key) then
				begin
					ActualDriver := i;
					if Reg.OpenKey(Key, False) then
					begin
						SetLength(DriverNames, DriverNameCount + 1);
						DriverNames[DriverNameCount] := Reg.ReadString('DriverDesc');
						Inc(DriverNameCount);
						Reg.CloseKey;
					end;

					Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(i, '0000') + '\Modes\8';
					if Reg.KeyExists(Key) then
					begin
						if Reg.OpenKey(Key + '\641,' + IntToStr(i + 480), True) then
						begin
							Reg.WriteString('', '60,75');
							Reg.WriteString('RefreshRate', '60');
							Reg.CloseKey;
						end;
					end;
				end;
			end;
		finally
			Reg.Free;
		end;
		if DriverNameCount > 1 then ActualDriver := -1;
	end;


	ScreenModeCount := 0;
	SetLength(ScreenModes, 0);
	EnabledBitsCount := 0;
	SetLength(EnabledBits, 0);

	// enumerate all available Screen modes
	ModeNumber := 0;
	FillChar(DeviceMode, SizeOf(DeviceMode), 0);
	DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
	while True do
	begin
		DeviceMode.dmBitsPerPel := 0;
		DeviceMode.dmPelsWidth := 0;
		DeviceMode.dmPelsHeight := 0;
		Done := not EnumDisplaySettingsA(nil, ModeNumber, DeviceMode); // Long time if call first time
		if Done then Break;
		if DeviceMode.dmPelsWidth = 641 then
		begin
			ActualDriver := DeviceMode.dmPelsHeight - 480;
		end
		else
		begin
			if DeviceMode.dmBitsPerPel > 4 then TryToAddToList(DeviceMode);
		end;
		Inc(ModeNumber);
	end;
	if (ActualDriver = -1) then
	begin
		if MainIni <> nil then
			ActualDriver := MainIni.RWSGF('Monitor', 'ActualDriver', ActualDriver, -1, False);
		if (ActualDriver = -1) and (DriverNameCount > 1) then
		begin
			fScreen := TfScreen.Create(nil);
			fScreen.ComboBoxDriver.Items.Clear;
			for i := 0 to DriverNameCount - 1 do
			begin
				fScreen.ComboBoxDriver.Items.Add(NToS(i) + ': ' + DriverNames[i]);
			end;
			fScreen.ComboBoxDriver.ItemIndex := 0;
			fScreen.ShowModal;
			ActualDriver := fScreen.ComboBoxDriver.ItemIndex;
			if MainIni <> nil then
				MainIni.RWSGF('Monitor', 'ActualDriver', ActualDriver, -1, True);
		end;
	end;


	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;
		for i := 0 to 15 do
		begin
			Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(i, '0000') + '\Modes\8';
			if Reg.KeyExists(Key) then
			begin
				Reg.DeleteKey(Key + '\641,' + IntToStr(i + 480));
			end;
		end;
	finally
		Reg.Free;
	end;

	// For Check Only
	NowScreenMode := GetNowScreenMode();
	DeviceMode.dmPelsWidth := NowScreenMode.Width;
	DeviceMode.dmPelsHeight := NowScreenMode.Height;
	DeviceMode.dmBitsPerPel := NowScreenMode.Bits;
	TryToAddToList(DeviceMode);

	// Read RefreshRates
	MinVF := MaxInt;
	MaxVF := 0;
	MinHF := MaxInt;
	MaxHF := 0;
	MinPixelRate := MaxInt;
	MaxPixelRate := 0;
	MinMemory := MaxInt;
	MaxMemory := 0;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;
		Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(ActualDriver, '0000');
		if Reg.OpenKey(Key, False) then
		begin
			DriverDesc := Reg.ReadString('DriverDesc');
			DriverDate := Reg.ReadString('DriverDate');
			Reg.CloseKey;
		end;
		for i := 0 to ScreenModeCount - 1 do
		begin
			Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(ActualDriver, '0000') + '\MODES\' +
				IntToStr(ScreenModes[i].Bits) + '\' + IntToStr(ScreenModes[i].Width) + ',' + IntToStr(ScreenModes[i].Height);
			if Reg.KeyExists(Key) then
			begin
				if Reg.OpenKey(Key, False) then
				begin
					s := Reg.ReadString('');
					ScreenModes[i].RefreshRateListCount := 0;
					InLineIndex := 1;
					while InLineIndex < Length(s) do
					begin
						f := StrToValI(ReadToChar(s, InLineIndex, ','), False, 0, 0, MaxInt, 1);
						if (f >= WorstVF) and (f <= BestVF) then
						begin
							Found := False;
							for j := 0 to ScreenModes[i].RefreshRateListCount - 1 do
							begin
								if ScreenModes[i].RefreshRateList[j] = f then
								begin
									Found := True;
									Break;
								end;
							end;
							if Found = False then
							begin
								Index := ScreenModes[i].RefreshRateListCount;
								for j := 0 to ScreenModes[i].RefreshRateListCount - 1 do
								begin
									if f < ScreenModes[i].RefreshRateList[j] then
									begin
										Index := j;
										Break;
									end;
								end;

								NewSize := ScreenModes[i].RefreshRateListCount + 1;
								if AllocByExp(Length(ScreenModes[i].RefreshRateList), NewSize) then
									SetLength(ScreenModes[i].RefreshRateList, NewSize);
								for j := ScreenModes[i].RefreshRateListCount - 1 downto Index do
								begin
									ScreenModes[i].RefreshRateList[j + 1] := ScreenModes[i].RefreshRateList[j];
								end;
								Inc(ScreenModes[i].RefreshRateListCount);
								ScreenModes[i].RefreshRateList[Index] := f;
							end;
						end;
					end;
				end;
				for j := 0 to ScreenModes[i].RefreshRateListCount - 1 do
				begin
					if ScreenModes[i].RefreshRateList[j] < MinVF then MinVF := ScreenModes[i].RefreshRateList[j];
					if ScreenModes[i].RefreshRateList[j] > MaxVF then MaxVF := ScreenModes[i].RefreshRateList[j];
					HF := GetHF(ScreenModes[i].Height, ScreenModes[i].RefreshRateList[j]);
					if HF < MinHF then MinHF := HF;
					if HF > MaxHF then MaxHF := HF;
					PixelRate := GetPixelRate(ScreenModes[i].Width, HF);
					if PixelRate < MinPixelRate then MinPixelRate := PixelRate;
					if PixelRate > MaxPixelRate then MaxPixelRate := PixelRate;
				end;
				if ScreenModes[i].RefreshRateListCount > 0 then
					DefVF := ScreenModes[i].RefreshRateList[0]
				else
					DefVF := SafeVF;

				if Reg.ValueExists('RefreshRate') then
					ScreenModes[i].RefreshRate := StrToValI(Reg.ReadString('RefreshRate'), False, 0, DefVF, BestVF, 1)
				else
					ScreenModes[i].RefreshRate := DefVF;
				Reg.CloseKey;
			end;
		end;
	finally
		Reg.Free;
	end;

	if Pos('GeForce', DriverDesc) <> 0 then
	begin
		MinWidth := 80;
		MinHeight := 142;
	end
	else
	begin
		MinWidth := 208;
		MinHeight := 156;
	end;

	for i := 0 to ScreenModeCount - 1 do
	begin
		Ram := GetVideoMemory(ScreenModes[i].Width, ScreenModes[i].Height, ScreenModes[i].Bits);
		if Ram < MinMemory then
			MinMemory := Ram;
		if Ram > MaxMemory then
			MaxMemory := Ram;
	end;
	ReadNowMode;
	if NotFirstTime = False then
	begin
		StartScreenMode := NowScreenMode;
		NotFirstTime := True;
	end;
	EndLongOperation(False);
	if First = False then
	begin
		First := True;
		UserMaxHF := MaxHF;
		UserMaxVF := MaxVF;
		UserMaxPixelRate := MaxPixelRate;
		UserMaxMemory := MaxMemory;
	end;
end;

procedure ReadNowMode;
var
	i: Integer;
begin
	NowScreenMode := GetNowScreenMode();

	ScreenModeIndex := 0;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if (ScreenModes[i].Width = NowScreenMode.Width)
		and (ScreenModes[i].Height = NowScreenMode.Height)
		and (ScreenModes[i].Bits = NowScreenMode.Bits)
		then
		begin
			NowScreenMode.RefreshRate := ScreenModes[i].RefreshRate;
			ScreenModeIndex := i;
		end;
	end;
end;

procedure SetSafeMode;
var
	ScreenMode: TScreenMode;
begin
	ScreenMode.Width := 800;
	ScreenMode.Height := 600;
	ScreenMode.Bits := 16;
	ScreenMode.RefreshRate := SafeVF;
	SetScreenMode(ScreenMode, False, True, False, True, True);
end;

function RestoreStartMode: Boolean;
//var DeviceMode: TDeviceMode absolute 0; // a little trick to create a nil Pointer
begin
	// Since the first parameter must be a var, we cannot use nil directly. Instead
	//  we use a variable with an absolute address of 0.
	Result := SetScreenMode(StartScreenMode, False, True, False, True, True);
	{ SelfChange := True;
	Result := ChangeDisplaySettings(DeviceMode, CDS_FULLSCREEN) = DISP_CHANGE_SUCCESSFUL;
	SelfChange := False;}
end;

{
TNT
XMax:
YMax: 1312

GeFroce
1152x864/75=960x720/90

					80x142
1600x1725 60
1280x1772 60

					MinY: 142 + 1 + 1
					800x600 158,160, ne ..165
					960x720 136,138
					1024x768 97, 99, 100, 102, 104, 105, 107, 109, 110, 112, 114, 116, 117, 118, 119, 120, 121, 123, 124, 125, 126, 128, 130 ne 131
					1152x864 100, 116, 118

					160Hz max

//    1280x1024x100: 108.4, 109.3
// 800x600 98.2 99.0

// Mon: 1152x864 118,ne 119
// GeForceMax: 1600x1200x85
// TNT2 Max: 1600x1200x75
}
function RateListToStr(RefreshRateList: TRefreshRateList; RefreshRateListCount: Integer): string;
var
	i: Integer;
	s: string;
begin
	for i := 0 to RefreshRateListCount - 1 do
	begin
		s := s + IntToStr(RefreshRateList[i]) + ',';
	end;
	if Length(s) > 0 then SetLength(s, Length(s) - 1);
	Result := s;
end;

function CorrectWidth(Width: UG): UG;
begin
	Result := 8 * (Width div 8);
	if (Result < MinWidth) then
		Result := MinWidth
	else if (Result > MaxScreenWidth) then
		Result := MaxScreenWidth;

end;

function CorrectHeight(Height: UG): UG;
begin
	Result := Height;
	if Result < MinHeight then
		Result := MinHeight
	else if (Result > MaxScreenHeight) then
		Result := MaxScreenHeight;
end;

procedure CorrectWidthHeight(var Width, Height: UG);
begin
	Width := CorrectWidth(Width);
	Height := CorrectHeight(Height);
end;

procedure AddLastMode(const ScreenMode: TScreenMode);
var NewSize: SG;
begin
	Inc(LastModeIndex);
	LastModeCount := LastModeIndex + 1;
	NewSize := LastModeCount;
	if AllocByExp(Length(LastModes), NewSize) then
		SetLength(LastModes, NewSize);
	LastModes[LastModeIndex] := ScreenMode;
end;

function SetScreenMode(const ScreenMode: TScreenMode;
	const Test, UpdateRegistry, Confirm, CanCreate, SaveLast: Boolean): Boolean;
var
	BestMode, ModeIndex: SG;
	NowDif, BestDif: UG;

	DeviceMode: TDeviceMode;
	Flags: U4;

	Reg: TRegistry;
	i, j: SG;
	Key, s: string;
	Found: BG;
	SetWidth, SetHeight, SetBits, SetRefreshRate: UG;
	VF: UG;

	NewSize: SG;

	procedure FindMode;
	var i: Integer;
	begin
		BestMode := 0;
		BestDif := High(BestDif);
		for i := 0 to ScreenModeCount - 1 do
		begin
			NowDif :=
				Abs(Integer(ScreenModes[i].Width) - Integer(SetWidth)) +
				Abs(Integer(ScreenModes[i].Height) - Integer(SetHeight)) +
				Abs(Integer(ScreenModes[i].Bits) - Integer(SetBits));
			if NowDif < BestDif then
			begin
				BestMode := i;
				BestDif := NowDif;
			end;
		end;
		ModeIndex := BestMode;
	end;
begin
	SetWidth := ScreenMode.Width;
	SetHeight := ScreenMode.Height;
	CorrectWidthHeight(SetWidth, SetHeight);

	SetBits := EnabledBits[EnabledBitsCount - 1];
	if ScreenMode.Bits <> 0 then
	begin
		BestDif := MaxInt;
		for i := 0 to EnabledBitsCount - 1 do
		begin
			if EnabledBits[i] >= ScreenMode.Bits then
				NowDif := EnabledBits[i] - ScreenMode.Bits
			else
				NowDif := 4 * (ScreenMode.Bits - EnabledBits[i]);
			if NowDif < BestDif then
			begin
				SetBits := EnabledBits[i];
				BestDif := NowDif;
			end;
		end;
	end;

	FindMode;

	VF := GetVF(SetHeight, UserMaxHF);
	if ScreenMode.RefreshRate = 0 then
		SetRefreshRate := ScreenModes[ModeIndex].RefreshRate
	else
		SetRefreshRate := ScreenMode.RefreshRate;
	if SetRefreshRate < WorstVF then
		SetRefreshRate := WorstVF
	else if SetRefreshRate > VF then
		SetRefreshRate := VF;


	if (CanCreate = True) or (BestDif = 0) then
	begin
		Reg := TRegistry.Create;
		try
			Reg.RootKey := HKEY_LOCAL_MACHINE;
			Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(ActualDriver, '0000') + '\MODES\' +
				IntToStr(SetBits) + '\' + IntToStr(SetWidth) + ',' + IntToStr(SetHeight);

			Reg.OpenKey(Key, True);
			if BestDif = 0 then
			begin
				Found := False;
				for i := 0 to ScreenModes[ModeIndex].RefreshRateListCount - 1 do
				begin
					if ScreenModes[ModeIndex].RefreshRateList[i] = SetRefreshRate then
					begin
						Found := True;
						Break;
					end;
				end;

				if Found = False then
				begin
					s := '';
					for i := 0 to ScreenModes[ModeIndex].RefreshRateListCount do
					begin
						if (i = ScreenModes[ModeIndex].RefreshRateListCount)
						or (SetRefreshRate < ScreenModes[ModeIndex].RefreshRateList[i]) then
						begin
							NewSize := ScreenModes[ModeIndex].RefreshRateListCount + 1;
							if AllocByExp(Length(ScreenModes[ModeIndex].RefreshRateList), NewSize) then
								SetLength(ScreenModes[ModeIndex].RefreshRateList, NewSize);
							for j := ScreenModes[ModeIndex].RefreshRateListCount - 1 downto i do
								ScreenModes[ModeIndex].RefreshRateList[j + 1] := ScreenModes[ModeIndex].RefreshRateList[j];

							ScreenModes[ModeIndex].RefreshRateList[i] := SetRefreshRate;

							Inc(ScreenModes[ModeIndex].RefreshRateListCount);
							Break;
						end;
					end;
					s := RateListToStr(ScreenModes[ModeIndex].RefreshRateList, ScreenModes[ModeIndex].RefreshRateListCount);
					Reg.WriteString('', s);
				end;
				if (ScreenModes[ModeIndex].RefreshRate <> SetRefreshRate) then
				begin
					Reg.WriteString('RefreshRate', IntToStr(SetRefreshRate));
					ScreenModes[ModeIndex].RefreshRate := SetRefreshRate;
				end;
			end
			else
			begin
				AddMode(SetWidth, SetHeight, SetBits);
				FindMode;
				FillRefreshRates(ModeIndex, ScreenMode.RefreshRate);
			end;
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end;

	Result := True;
	if Test = False then
	begin
		if ((ScreenMode.Width <> 0) and (ScreenModes[ModeIndex].Width <> ScreenMode.Width))
		or ((ScreenMode.Height <> 0) and (ScreenModes[ModeIndex].Height <> ScreenMode.Height))
		or ((ScreenMode.Bits <> 0) and (ScreenModes[ModeIndex].Bits <> ScreenMode.Bits))
		or ((ScreenMode.RefreshRate <> 0) and (ScreenModes[ModeIndex].RefreshRate <> ScreenMode.RefreshRate))
		and ((ScreenModes[ModeIndex].Bits <> 32) or (ScreenMode.Bits <> 24))
		and ((ScreenModes[ModeIndex].Bits <> 24) or (ScreenMode.Bits <> 32)) then
			ErrorMsg('Screen mode ' + ScreenModeToStr(ScreenMode) +
				' can not be set, using ' +
				ScreenModeToStr(ScreenModes[ModeIndex]) + '.');

		DeviceMode.dmSize := SizeOf(DeviceMode);
		DeviceMode.dmFields := DM_BITSPERPEL or DM_PELSWIDTH or DM_PELSHEIGHT;
		// if mode set failed, we'll just run in windowed mode
		Flags := CDS_FULLSCREEN;
		DeviceMode.dmPelsWidth := ScreenModes[ModeIndex].Width;
		DeviceMode.dmPelsHeight := ScreenModes[ModeIndex].Height;
		DeviceMode.dmBitsPerPel := ScreenModes[ModeIndex].Bits;
		Flags := Flags or CDS_RESET; // For VF
		if UpdateRegistry then Flags := Flags or CDS_UPDATEREGISTRY;
		SelfChange := False;
		Result := ChangeDisplaySettings(DeviceMode, Flags) = DISP_CHANGE_SUCCESSFUL;
		SelfChange := True;
		s := ScreenModeToStr(ScreenModes[ModeIndex]);
		if Result then
		begin
			if Confirm then
			begin
				PlayWinSound(wsQuestion);
				if LastModeIndex >= 0 then
				if Confirmation('Use mode ' + s + '?', [mbYes, mbNo]) <> mbYes then
				begin
					SetScreenMode(LastModes[LastModeIndex], False, UpdateRegistry, False, True, False);
					Exit;
				end;
			end;
			ScreenModeIndex := ModeIndex;
			NowScreenMode.Width := ScreenModes[ScreenModeIndex].Width;
			NowScreenMode.Height := ScreenModes[ScreenModeIndex].Height;
			NowScreenMode.Bits := ScreenModes[ScreenModeIndex].Bits;
			NowScreenMode.RefreshRate := ScreenModes[ScreenModeIndex].RefreshRate;
			if (Test = False) and SaveLast then
				AddLastMode(NowScreenMode);
		end
		else
			ErrorMsg('Can not change screen mode to %1.', [s]);
	end;
end;

procedure FillRefreshRates(Index, VF: UG);
var
	Reg: TRegistry;
	Key: string;
	D, DefD, R, k: UG;
	s: string;
	NewSize: SG;
begin
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;
		Key := 'System\CurrentControlSet\Services\Class\Display\' + NToS(ActualDriver, '0000') + '\MODES\' +
			IntToStr(ScreenModes[Index].Bits) + '\' + IntToStr(ScreenModes[Index].Width) + ',' + IntToStr(ScreenModes[Index].Height);
		Reg.OpenKey(Key, True);
		D := Max(Min(GetVF(ScreenModes[Index].Height, UserMaxHF), UserMaxVF), VF);
		D := Min(D, GetVF(ScreenModes[Index].Height, UserMaxPixelRate div ScreenModes[Index].Width));  
		DefD := D;
		S := '';
		R := Min(MinVF, D);
		k := 0;
		ScreenModes[Index].RefreshRateListCount := 0;
		SetLength(ScreenModes[Index].RefreshRateList, 0);

		while R <= D do
		begin
			if (R >= D - k) or
			(R = 85) or // Ergo
			(R = 60) or // 2
			// 24, 25, 30
			(R = 72) or(R = 75) or (R = 90) or // 3
			(R = 96) or (R = 100) or (R = 120) or // 4
				(R = 125) or (R = 150) or // 5
			(R = 144) or (R = 150) or (R = 180) // 6
			then
			begin
				S := S + IntToStr(R) + ',';
				Inc(ScreenModes[Index].RefreshRateListCount);
				NewSize := ScreenModes[Index].RefreshRateListCount + 1;
				if AllocByExp(Length(ScreenModes[Index].RefreshRateList), NewSize) then
					SetLength(ScreenModes[Index].RefreshRateList, NewSize);
				ScreenModes[Index].RefreshRateList[ScreenModes[Index].RefreshRateListCount - 1] := R;
			end;
			Inc(R);
		end;
		if Length(S) > 0 then
			SetLength(S, Length(S) - 1);

		Reg.WriteString('', S);
		Reg.WriteString('RefreshRate', IntToStr(DefD));
		ScreenModes[Index].RefreshRate := DefD;
		Reg.DeleteValue('ModeRefreshRateList');
		Reg.CloseKey;
	finally
		Reg.Free;
	end;
end;

{
function CreateScreenMode(Width, Height: UG): Boolean;
var
	i: Integer;
begin
	Result := False;
	Width := 8 * (Width div 8);
	if (Width < 80) or (Width > 2048) or (Height < 142) or (Height > 2048) then Exit;

	Result := True;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if (ScreenModes[i].Width = Width) and (ScreenModes[i].Height = Height) then
			Exit;
	end;
	Inc(ScreenModeCount);
	SetLength(ScreenModes, ScreenModeCount);
	ScreenModes[ScreenModeCount - 1].Width := Width;
	ScreenModes[ScreenModeCount - 1].Height := Height;
	ScreenModes[ScreenModeCount - 1].Bits := 32;
	FillRefreshRates(ScreenModes[ScreenModeCount - 1]);
end;
}

function StandardMode(Width, Height: UG): Boolean;
begin
	Result := True;
	if (Width = 320) and (Height = 200) then Exit;
	if (Width = 320) and (Height = 240) then Exit;
	if (Width = 400) and (Height = 300) then Exit;
	if (Width = 512) and (Height = 384) then Exit;
	if (Width = 640) and (Height = 480) then Exit;
	if (Width = 800) and (Height = 600) then Exit;
	if (Width = 1024) and (Height = 768) then Exit;
	if (Width = 1152) and (Height = 864) then Exit;
	if (Width = 1280) and (Height = 960) then Exit;
	if (Width = 1280) and (Height = 1024) then Exit; // Non 4:3
	if (Width = 1600) and (Height = 1200) then Exit;
	if (Width = 1920) and (Height = 1440) then Exit;
	if (Width = 2048) and (Height = 1536) then Exit;
	if (Width = ScreenModes[ScreenModeIndex].Width) and (Height = ScreenModes[ScreenModeIndex].Height) then Exit;
	Result := False;
end;

function DeleteScreenMode(Width, Height, Bits: UG): Boolean;
var
	i: Integer;
	Reg: TRegistry;
begin
	Result := False;
	if StandardMode(Width, Height) then Exit;
	Reg := TRegistry.Create;
	try
		Reg.RootKey := HKEY_LOCAL_MACHINE;
		i := 0;
		while i < ScreenModeCount do
		begin
			if i <> ScreenModeIndex then
			if ((Width = 0) or (ScreenModes[i].Width = Width))
			and ((Height = 0) or (ScreenModes[i].Height = Height))
			and ((Bits = 0) or (ScreenModes[i].Bits = Bits))
			and (not StandardMode(ScreenModes[i].Width, ScreenModes[i].Height)) then
			begin
				Reg.DeleteKey('System\CurrentControlSet\Services\Class\Display\' + NToS(ActualDriver, '0000') + '\MODES\' +
						IntToStr(ScreenModes[i].Bits) + '\' + IntToStr(ScreenModes[i].Width) + ',' + IntToStr(ScreenModes[i].Height));
{       for j := i + 1 to ScreenModeCount - 1 do
					ScreenModes[j - 1] := ScreenModes[j];

				if ScreenModeIndex >= i then Dec(ScreenModeIndex);
				Dec(ScreenModeCount);}
				Result := True;
			end;
			Inc(i);
		end;
	finally
		Reg.Free;
	end;
end;

{procedure InitScreenCorectColor;
begin
	NowScreenMode := GetNowScreenMode();
	case NowBits of
	1..5: ScreenCorrectColor := $001f1f1f;
	6..11: ScreenCorrectColor := $000f0f0f;
	12..19: ScreenCorrectColor := $00030303;
	else ScreenCorrectColor := $00000000;
	end;
end;}

procedure TfScreen.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

initialization
	LastModeIndex := -1;
	ActualDriver := -1;
	RetraceDelay := DefaultRetraceDelay;
finalization

end.
