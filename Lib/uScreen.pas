//* File:     Lib\uScreen.pas
//* Created:  1999-08-01
//* Modified: 2004-09-26
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uScreen;

interface

uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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

	TRefreshRateList = array of Cardinal;
	TScreenMode = packed record // 32
		Width,
		Height, Bits: Cardinal; // 12
		RefreshRate: Cardinal; // 4
		RefreshRateList: TRefreshRateList; // 4
		RefreshRateListCount: Integer; // 4
		Reserved: array[0..1] of Integer; // 8
	end;
	TScreenModeS = packed record // 16
		Width,
		Height, Bits: Cardinal; // 12
		RefreshRate: Cardinal; // 4
	end;

function GetVF(Height, HF: Cardinal): Cardinal; // Hz
function GetHF(Height, RefreshRate: Cardinal): Cardinal; // KHz
function GetHeight(RefreshRate, HF: Cardinal): Cardinal; // Pixels
function GetPixelRate(const Width, HF: Cardinal): Cardinal; // MHz
function GetVideoMemory(const Width, Height, Bits: Cardinal): Cardinal; // Bytes
function ScreenModeToStr(const Width, Height: Word): string; overload;
function ScreenModeToStr(const Width, Height, Bits: Word): string; overload;
function ScreenModeToStr(const Width, Height, Bits, VF: Word): string; overload;

procedure ReadScreenModes;
procedure ReadNowMode;
function RateListToStr(RefreshRateList: TRefreshRateList; RefreshRateListCount: Integer): string;
function CorrectWidth(Width: Cardinal): Cardinal;
function CorrectHeight(Height: Cardinal): Cardinal;
procedure AddLastMode(Width, Height, Bits, RefreshRate: Cardinal);


function SetScreenMode(Width, Height, Bits, RefreshRate: Word;
	const Test, UpdateRegistry, Confirm, CanCreate, SaveLast: Boolean): Boolean;
procedure SetSaveMode;
function RestoreStartMode: Boolean;
procedure FillRefreshRates(Index, VF: Cardinal);
function DeleteScreenMode(Width, Height, Bits: Cardinal): Boolean;
procedure InitScreenCorectColor;

{
const
	LightSpeed = 299792458; // m/s
	LightLMin = 390; // nm
	LightLMax = 760; // nm
}
var
	StartWidth, StartHeight, StartBits, StartRefreshRate: Cardinal;
	NowWidth, NowHeight, NowBits, NowRefreshRate: Cardinal;
	LastModes: array of TScreenModeS;
	LastModeCount, LastModeIndex: Integer;

	DriverDesc, DriverDate: string;
	DriverNames: array of string;
	DriverNameCount: SG;
	ScreenModes: array of TScreenMode;
	EnabledBits: array of U4; // 4, 8, 15, 16, 24, 32
	EnabledBitsCount: SG;

	NotFirstTime: Boolean;

	SelfChange: Boolean;

	ScreenModeIndex: Integer;
	ScreenModeCount: Integer;
	ScreenCorrectColor: TColor;
	ScreenBits: Cardinal;

	ActualDriver: Integer;

	MinWidth, MinHeight: Cardinal;
	RetraceDelay: Integer;
	MinVF, MaxVF, UserMaxVF,
	MinHF, MaxHF, UserMaxHF,
	MinPixelRate, MaxPixelRate, UserMaxPixelRate,
	MinMemory, MaxMemory, UserMaxMemory: Cardinal;

	fScreen: TfScreen;
const
	DefaultRetraceDelay = 560;

	WorstVF = 30;
	SaveVF = 60;
	ErgoVF = 80;
	BestVF = 200;

	WorstHF = 30000;
	BestHF = 700000;

	WorstPixelRate = 1000000;
	BestPixelRate = 1000000000;

	WorstMemory = 1024 * 1024;
	BestMemory = 256 * 1024 * 1024;

	DoubleHeight = 399; // and less
	MaxScreenWidth = 4096;
	MaxScreenHeight = 3072;

implementation

{$R *.DFM}
uses
	Registry, Math, MMSystem,
	uError, uStrings, uWave, uFiles, uGetInt, uDIni, uInput;
var
	SndBeep: PWave;
	First: Boolean;

function GetVF(Height, HF: Cardinal): Cardinal;
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

function GetHF(Height, RefreshRate: Cardinal): Cardinal;
begin
	if Height <= DoubleHeight then Height := Height * 2;
//  Result := Height * RefreshRate;
	if RefreshRate = 0 then
		Result := 0
	else
//    Result := Floor(Height / ((1 / RefreshRate) - Cardinal(RetraceDelay) / 1000000));
		Result := MaxDivS8((S8(Height) * S8(RefreshRate) * 1000000), (1000000 - S8(RetraceDelay) * S8(RefreshRate)));

end;

function GetHeight(RefreshRate, HF: Cardinal): Cardinal;
begin
	Result := (1000000 * S8(HF) - S8(RefreshRate) * S8(HF) * S8(RetraceDelay)) div (1000000 * S8(RefreshRate));
	if Result <= DoubleHeight then Result := Result div 2;
end;

function GetPixelRate(const Width, HF: Cardinal): Cardinal;
begin
	Result := Width * HF; // +
end;

function GetVideoMemory(const Width, Height, Bits: Cardinal): Cardinal;
begin
	Result := 4 * ((Width * Bits + 31) div 32);
	Result := Result * Height;
end;

function ScreenModeToStr(const Width, Height: Word): string; overload;
begin
	Result :=
		NToS(Width) + CharTimes + NToS(Height);
end;

function ScreenModeToStr(const Width, Height, Bits: Word): string; overload;
begin
	Result := ScreenModeToStr(Width, Height);
	if Bits <> 0 then Result := Result + CharTimes + NToS(Bits) + ' bit';
end;

function ScreenModeToStr(const Width, Height, Bits, VF: Word): string;
begin
	Result := ScreenModeToStr(Width, Height, Bits);
	if VF <> 0 then Result := Result + '/' + NToS(VF) + ' Hz';
end;

procedure AddMode(Width, Height, Bits: Cardinal);
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

procedure ReadScreenModes;
var
	ModeNumber: Integer;
	done: Boolean;
	DeviceMode: TDeviceMode;
	DeskDC: HDC;
	i, j: Integer;

	Reg: TRegistry;
	Key: string;
	S: string;
	HF, PixelRate: Cardinal;
	DefVF: Cardinal;

	f: Cardinal;
	InLineIndex: Integer;
	Found: Boolean;
	Index: Integer;
	NewSize: SG;

	Ram: Cardinal;
begin
	Screen.Cursor := crHourGlass;

	if First = False then
	begin
		if FileExists(SoundsDir + 'Question.wav') then
			WaveReadFromFile(SndBeep, SoundsDir + 'Question.wav');

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
		if (ActualDriver = -1) then
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
	DeskDC := GetDC(0);
	try
		NowBits := GetDeviceCaps(DeskDC, BITSPIXEL);
		NowWidth := Screen.Width;
		NowHeight := Screen.Height;
	finally
		ReleaseDC(0, DeskDC);
	end;
	DeviceMode.dmPelsWidth := NowWidth;
	DeviceMode.dmPelsHeight := NowHeight;
	DeviceMode.dmBitsPerPel := NowBits;
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
					DefVF := SaveVF;

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
		StartWidth := NowWidth;
		StartHeight := NowHeight;
		StartBits := NowBits;
		StartRefreshRate := NowRefreshRate;
		NotFirstTime := True;
	end;
	Screen.Cursor := crDefault;
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
	DeskDC: HDC;
	i: Integer;
begin
	DeskDC := GetDC(0);
	try
		NowBits := GetDeviceCaps(DeskDC, BITSPIXEL);
		NowWidth := Screen.Width;
		NowHeight := Screen.Height;
	finally
		ReleaseDC(0, DeskDC);
	end;

	ScreenModeIndex := 0;
	for i := 0 to ScreenModeCount - 1 do
	begin
		if (ScreenModes[i].Width = NowWidth)
		and (ScreenModes[i].Height = NowHeight)
		and (ScreenModes[i].Bits = NowBits)
		then
		begin
			NowRefreshRate := ScreenModes[i].RefreshRate;
			ScreenModeIndex := i;
		end;
	end;
end;

procedure SetSaveMode;
begin
	SetScreenMode(
		640,
		480,
		8,
		SaveVF,
		False, True, False, True, True);
end;

function RestoreStartMode: Boolean;
//var DeviceMode: TDeviceMode absolute 0; // a little trick to create a nil Pointer
begin
	// Since the first parameter must be a var, we cannot use nil directly. Instead
	//  we use a variable with an absolute address of 0.
	Result := SetScreenMode(StartWidth, StartHeight, StartBits, StartRefreshRate, False, True, False, True, True);
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

function CorrectWidth(Width: Cardinal): Cardinal;
begin
	Result := 8 * (Width div 8);
	if (Result < MinWidth) then
		Result := MinWidth
	else if (Result > MaxScreenWidth) then
		Result := MaxScreenWidth;

end;

function CorrectHeight(Height: Cardinal): Cardinal;
begin
	Result := Height;
	if Result < MinHeight then
		Result := MinHeight
	else if (Result > MaxScreenHeight) then
		Result := MaxScreenHeight;
end;

procedure CorrectWidthHeight(var Width, Height: Cardinal);
begin
	Width := CorrectWidth(Width);
	Height := CorrectHeight(Height);

end;

procedure AddLastMode(Width, Height, Bits, RefreshRate: Cardinal);
var NewSize: SG;
begin
	Inc(LastModeIndex);
	LastModeCount := LastModeIndex + 1;
	NewSize := LastModeCount;
	if AllocByExp(Length(LastModes), NewSize) then
		SetLength(LastModes, NewSize);
	LastModes[LastModeIndex].Width := Width;
	LastModes[LastModeIndex].Height := Height;
	LastModes[LastModeIndex].Bits := Bits;
	LastModes[LastModeIndex].RefreshRate := RefreshRate;
end;

function SetScreenMode(Width, Height, Bits, RefreshRate: Word;
	const Test, UpdateRegistry, Confirm, CanCreate, SaveLast: Boolean): Boolean;
var
	BestMode, ModeIndex: Integer;
	NowDif, BestDif: LongWord;

	DeviceMode: TDeviceMode;
	Flags: U4;

	Reg: TRegistry;
	i, j: Integer;
	Key, s: string;
	Found: Boolean;
	SetWidth, SetHeight, SetBits, SetRefreshRate: Cardinal;
	VF: Cardinal;

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
	SetWidth := Width;
	SetHeight := Height;
	CorrectWidthHeight(SetWidth, SetHeight);

	SetBits := EnabledBits[EnabledBitsCount - 1];
	if Bits <> 0 then
	begin
		BestDif := MaxInt;
		for i := 0 to EnabledBitsCount - 1 do
		begin
			if EnabledBits[i] >= Bits then
				NowDif := EnabledBits[i] - Bits
			else
				NowDif := 4 * (Bits - EnabledBits[i]);
			if NowDif < BestDif then
			begin
				SetBits := EnabledBits[i];
				BestDif := NowDif;
			end;
		end;
	end;

	FindMode;

	VF := GetVF(SetHeight, UserMaxHF);
	if RefreshRate = 0 then
		SetRefreshRate := ScreenModes[ModeIndex].RefreshRate
	else
		SetRefreshRate := RefreshRate;
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
				FillRefreshRates(ModeIndex, RefreshRate);
			end;
			Reg.CloseKey;
		finally
			Reg.Free;
		end;
	end;

	Result := True;
	if Test = False then
	begin
		if ((Width <> 0) and (ScreenModes[ModeIndex].Width <> Width))
		or ((Height <> 0) and (ScreenModes[ModeIndex].Height <> Height))
		or ((Bits <> 0) and (ScreenModes[ModeIndex].Bits <> Bits))
		or ((RefreshRate <> 0) and (ScreenModes[ModeIndex].RefreshRate <> RefreshRate))
		and ((ScreenModes[ModeIndex].Bits <> 32) or (Bits <> 24))
		and ((ScreenModes[ModeIndex].Bits <> 24) or (Bits <> 32)) then
			ErrorMessage('Screen mode ' + ScreenModeToStr(Width, Height, Bits, RefreshRate) +
				' can not be set, using ' +
				ScreenModeToStr(ScreenModes[ModeIndex].Width, ScreenModes[ModeIndex].Height, ScreenModes[ModeIndex].Bits, ScreenModes[ModeIndex].RefreshRate));

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
{   MouseX := Mouse.CursorPos.x;
		MouseY := Mouse.CursorPos.y;}
		Result := ChangeDisplaySettings(DeviceMode, Flags) = DISP_CHANGE_SUCCESSFUL;
{   mouse_event(
			MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE,
			0,  // horizontal mouse position or position change
			0,  // vertical mouse position or position change
			0,  // amount of wheel movement
			0   // 32 bits of application-defined information
		 );
		mouse_event(
			MOUSEEVENTF_MOVE,
			MouseX div 2, // horizontal mouse position or position change
			MouseY div 2, // vertical mouse position or position change
			0,  // amount of wheel movement
			0   // 32 bits of application-defined information
		 );}
		SelfChange := True;
		s := ScreenModeToStr(
			ScreenModes[ModeIndex].Width,
			ScreenModes[ModeIndex].Height,
			ScreenModes[ModeIndex].Bits,
			ScreenModes[ModeIndex].RefreshRate);
		if Result then
		begin
			if Confirm then
			begin
				PlayWave(SndBeep);
				if LastModeIndex >= 0 then
				if MessageD('Use mode ' + s + '?', mtConfirmation, [mbYes, mbNo]) <> mbYes then
				begin
{         SetScreenMode(StartWidth, StartHeight,
						StartBits, StartRefreshRate,
						False, False, False, True);
					Sleep(100);}
					SetScreenMode(LastModes[LastModeIndex].Width, LastModes[LastModeIndex].Height,
						LastModes[LastModeIndex].Bits, LastModes[LastModeIndex].RefreshRate,
						False, UpdateRegistry, False, True, False);
					Exit;
				end;
			end;
			ScreenModeIndex := ModeIndex;
			NowWidth := ScreenModes[ScreenModeIndex].Width;
			NowHeight := ScreenModes[ScreenModeIndex].Height;
			NowBits := ScreenModes[ScreenModeIndex].Bits;
			NowRefreshRate := ScreenModes[ScreenModeIndex].RefreshRate;
			if (Test = False) and SaveLast then
				AddLastMode(NowWidth, NowHeight, NowBits, NowRefreshRate);
		end
		else
			ErrorMessage('Can not change screen mode to ' + s);
	end;
end;

procedure FillRefreshRates(Index, VF: Cardinal);
var
	Reg: TRegistry;
	Key: string;
	D, DefD, R, k: Cardinal;
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
function CreateScreenMode(Width, Height: Cardinal): Boolean;
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

function StandardMode(Width, Height: Cardinal): Boolean;
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

function DeleteScreenMode(Width, Height, Bits: Cardinal): Boolean;
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

procedure InitScreenCorectColor;
var DeskDC: HDC;
begin
	DeskDC := GetDC(0);
	try
		ScreenBits := GetDeviceCaps(DeskDC, BITSPIXEL);
		case ScreenBits of
		1..5: ScreenCorrectColor := $001f1f1f;
		6..11: ScreenCorrectColor := $000f0f0f;
		12..19: ScreenCorrectColor := $00030303;
		else ScreenCorrectColor := $00000000;
		end;
	finally
		ReleaseDC(0, DeskDC);
	end;
end;

procedure TfScreen.FormCreate(Sender: TObject);
begin
	Background := baGradient;
end;

initialization
	LastModeIndex := -1;
	ActualDriver := -1;
	RetraceDelay := DefaultRetraceDelay;
	InitScreenCorectColor;
finalization
	if Assigned(SndBeep) then
	begin
		FreeMem(SndBeep); SndBeep := nil;
	end;
end.           
