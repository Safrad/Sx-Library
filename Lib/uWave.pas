//* File:     Lib\uWave.pas
//* Created:  1999-07-01
//* Modified: 2005-05-11
//* Version:  X.X.34.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@centrum.cz
//* Web:      http://safrad.webzdarma.cz

unit uWave;

interface

uses
	MMSystem, SysUtils, Windows,
	uDTimer, uTypes, uData, uMath;

{
procedure NoSound;
procedure Sound(const Hz: Word);
}
{
	Supported Wave:
	Format: PCM only
	Channels: 1..2 (Mono, Stereo)
	BitsPerSample: 8 or 16 bits
	SampleRate: 1..512 * 1024 * 1024

	8 bits
			Byte Hex SortInt
	Max: 255  $ff -1

	Cen: 128  $80 -128
			 127  $7f 127

	Min:   0  $00 0


	16 bits
			 Word   Hex SmallInt
	Max: 32767 $7fff  32767

	Cen:     0 $0000      0
			 65535 $ffff     -1

	Min: 32768 $8000 -32768
}
const
	WaveHead = 44;
	MaxVolume = 1024;
type
	TBLR = record
		L, R: Byte;
	end;
	TWLR = record
		L, R: SmallInt;
	end;
	TDLR = record
		L, R: LongInt;
	end;
	PWaveData = ^TWaveData;
	TWaveData = packed record
		case Integer of
		0: (B: array[0..1024 * 1024 * 1024 - 1] of Byte);
		1: (W: array[0..512 * 1024 * 1024 - 1] of SmallInt);
		2: (D: array[0..256 * 1024 * 1024 - 1] of LongInt);
		3: (BLR: array[0..512 * 1024 * 1024 - 1] of TBLR);
		4: (WLR: array[0..256 * 1024 * 1024 - 1] of TWLR);
		5: (DLR: array[0..128 * 1024 * 1024 - 1] of TDLR);
	end;

	TWave = packed record // 44
		Marker1:         array[0..3] of Char; // 4
		BytesFollowing:  LongInt; // 4; FileSize - 8
		Marker2:         array[0..3] of Char; // 4
		Marker3:         array[0..3] of Char; // 4
		BlockAlign:      LongInt; // 4; 16
		FormatTag:       Word; // 2; 1
		Channels:        Word; // 2; 2: stereo, 1: mono
		SampleRate:      LongInt; // 4; 11025, 22050, 44100
		BytesPerSecond:  LongInt; // 4; BytesPerSample * SampleRate
		BytesPerSample:  Word; // 2; 1, 2, 4; 4: 16 bit stereo, 2: 8 bit stereo
		BitsPerSample:   Word; // 2; 16: 16 bit mono/stereo, 8: 8 bit mono/stereo
		Marker4:         array[0..3] of Char; // 4
		DataBytes:       LongInt; // 4; <= (FileSize - 44)
		Data:            TWaveData; // X
	end;
	PWave = ^TWave;

type
	TWinSound = (
		wsAsterisk, // Done
		wsCloseProgram,
		wsCriticalStop, // uError
		wsDefaultSound, // Beep
		wsExclamation,
		wsExitWindows,
		wsMaximize,
		wsMenuCommand,
		wsMenuPopup,
		wsMinimize,
		wsOpenProgram,
		wsProgramError,
		wsQuestion, // uError
		wsRestoreDown,
		wsRestoreUp,
//		wsRingIn,
//		wsRingout,
//		wsSelect,
//		wsShowToolbarBand,
		wsStartWindows
//		wsSystemDefault
		);
const
	WinSoundNames: array[TWinSound] of string = (
		'SystemAsterisk',
		'Close',
		'SystemHand',
		'.Default',
		'SystemExclamation',
		'SystemExit',
		'Maximize',
		'MenuCommand',
		'MenuPopup',
		'Minimize',
		'Open',
		'AppGPFault',
		'SystemQuestion',
		'RestoreDown',
		'RestoreUp',
//		'RingIn',
//		'RingOut',
//		'CCSelect',
//		'ShowBand',
		'SystemStart'
//		'SystemDefault'
		);

const
	ConvertShr = 16;
	ConvertPre = 1 shl ConvertShr;
procedure SoundLR(var Left, Right: Integer; const NowPos, MaxPos: Integer);
// For screen Width 800 is NowPos 0..799, MaxPos 799

function GetBufferSize(wBitsPerSample, nChannels, BufferOutSamples: SG): SG;
function GetBufferSample(wBitsPerSample, BufferOutSize: SG): SG;

function WaveErrorText(ErrorCode: U4): string;

procedure WaveReadFromFile(var Wave: PWave; FName: TFileName);
procedure WaveWriteToFile(var Wave: PWave; FName: TFileName);

procedure WaveCreate(var Wave: PWave;
	const Channels: Word; // 1, 2
	const BitsPerSample: Word; // 8, 16
	const SampleRate: LongInt; // 11025, 22050, 44100, 48000...
	const TotalSamples: LongInt); // 1 sec = SampleRate
procedure WaveFree(var Wave: PWave);

// Left, Right (0..ConvertPre)
procedure ConvertChannels(const WaveS: PWave; var WaveD: PWave;
	const NewChannels: Word; const Left, Right: Integer);
procedure ConvertBitsPerSample(const WaveS: PWave; var WaveD: PWave;
	const NewBitsPerSample: Integer);
procedure ConvertSampleRate(const WaveD: PWave; const SampleRate: LongInt);
procedure ConvertWave(const WaveS: PWave; var WaveD: PWave;
	const Channels: Word;
	const BitsPerSample: Word;
	const SampleRate: LongInt);

procedure PlayWave(Wave: PWave);
procedure PlayWaveFile(WaveName: TFileName);
procedure PlayWinSound(WinSound: TWinSound);
procedure Beep;

type
	PPlayItem = ^TPlayItem;
	TPlayItem = packed record // 64
		Wave: PWave; // 4
		SampleCount: U4;
		SamplePos: U4;
		Frequency: U4;
		VolumeLeft: S4;
		VolumeRight: S4;
		Speed: U4;
		Offset: U4;
		PlayAs: (paWave, paTone, paNoise); // 1
		Reserved: array[0..30] of B1;
	end;


	TWavePlayer = class
	public
		// IO
		HWaveOut: HWaveOut;
		WaveFormat: TWaveFormatEx;

		// Player status
		FError: MMResult;
		Initialized: Boolean;
		CloseInvoked: Boolean;

		// Output buffer
		OutCount: SG;
//		BufferOut: PWaveData;
		BufferOutSize: SG;
		BufferOutSamples: SG;

		// Items to play
		PlayItems: TData;

		VolumeLeft: SG;
		VolumeRight: SG;
		Speed: SG;

		// Options
		Sound16bits: Boolean;
		Frequency: SG;
		SoundStereo: Boolean;
		BufferTime: UG;

		procedure MMError(s: string);
		procedure SendBuffer;
		constructor Create;
		destructor Destroy; override;

		procedure Init;
		procedure Close;

		procedure Pause;
		procedure Resume;
		procedure Stop;

		procedure Play(Wave: PWave);
		procedure Beep;
		procedure Tone(Frequency, Tim: UG);
		procedure Noise(Tim: UG);

		property Running: Boolean read Initialized;
	end;

var
	WavePlayer: TWavePlayer;

implementation

uses
	Registry, Dialogs,
	uFiles, uError, uStrings, uFormat;

(*
procedure NoSound;
asm
	{$ifopt O+}
	push ax
	{$endif}
	in al, 61h
	and al, 00fch
	out 61h, al
	{$ifopt O+}
	pop ax
	{$endif}
end;

procedure Sound(const Hz: Word);
asm
	{$ifopt O+}
	pushad
	{$endif}
	mov bx, Hz
	mov ax, 34DDh
	mov dx, 0012h
	cmp dx, bx
	jnb @ExitProc
	div bx
	{ ax := dx&ax div bx
		dx := dx&ax mod bx
		dx&ax := 1193181
		f := 1092Hz
	}
	mov bx, ax
	in al, 61h
	test al, 03h
	jne @SoundIsOn
		or al, 03h
		out 61h, al
		mov al, 0B6h
		out 43h, al
	@SoundIsOn:
	mov al, bl
	out 42h, al
	mov al, bh
	out 42h, al
	@ExitProc:
	{$ifopt O+}
	popad
	{$endif}
end;
*)

function WaveErrorText(ErrorCode: U4): string;
begin
	SetLength(Result, MAXERRORLENGTH);
	if waveOutGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR then
		Result := ''
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' ' + 'Unknown error';
end;

procedure SoundLR(var Left, Right: Integer; const NowPos, MaxPos: Integer);
begin
	Left := ConvertPre * (MaxPos - NowPos) div MaxPos;
	if Left < 0 then
		Left := 0
	else if Left > ConvertPre then
		Left := ConvertPre;
	Right := ConvertPre * NowPos div MaxPos;
	if Right < 0 then
		Right := 0
	else if Right > ConvertPre then
		Right := ConvertPre;
end;

function CheckWave(Wave: PWave): Boolean;
begin
	Result := False;
	if Wave <> nil then
	begin
		if ((Wave.BitsPerSample = 8) or (Wave.BitsPerSample = 16)) then
		begin
			Result := True;
		end
		else
			ErrorMessage('Invalid wave format');
	end
	else
		ErrorMessage('Wave is empty');
end;

procedure WaveReadFromFile(var Wave: PWave; FName: TFileName);
label LRetry, LFin;
var
	F: TFile;
begin
	if Wave <> nil then
	begin
//		MessageD('Destination Wave Must Be Nil', mtError, [mbOk]);
		WaveFree(Wave);
	end;
	F := TFile.Create;
	LRetry:
	if F.Open(FName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if F.FileSize < WaveHead then
		begin
			IOErrorMessage(FName, 'File truncated');
			goto LFin;
		end;
		GetMem(Wave, F.FileSize);
		if not F.BlockRead(Wave^, F.FileSize) then goto LFin;
		if (Wave.Marker1 <> 'RIFF') or (Wave.Marker2 <> 'WAVE') or
			(Wave.Marker3 <> 'fmt ') {or
			(Wave.BytesFollowing <> F.FileSize - 8) }then
		begin
			IOErrorMessage(FName, 'File is not wave');
			WaveFree(Wave);
		end
		else
		begin
			if Wave.BytesFollowing <> F.FileSize - 8 then
			begin
				IOErrorMessage(FName, 'Wave bytes following repaired');
				Wave.BytesFollowing := F.FileSize - 8;
			end;
			if (Wave.BitsPerSample = 4) {Microsoft ADPCM} or (Wave.BitsPerSample = 0) {GSM 6.10} then goto LFin;
			if ((Wave.BitsPerSample <> 8) and (Wave.BitsPerSample <> 16)) then
			begin
				IOErrorMessage(FName, 'Wave format not supported');
				WaveFree(Wave);
				goto LFin;
			end;
	{		if Wave.DataBytes > F.FileSize - 44 then
			begin
				IOErrorMessage(FName, 'Wave data bytes repaired');
				Wave.DataBytes := F.FileSize - 44;
			end;}
		end;
		LFin:
		F.Close;
	end;
	F.Free;
end;

procedure WaveWriteToFile(var Wave: PWave; FName: TFileName);
label LRetry;
var
	F: TFile;
begin
	if Wave = nil then Exit;
	F := TFile.Create;
	LRetry:
	if F.Open(FName, fmReadOnly, FILE_FLAG_SEQUENTIAL_SCAN, False) then
	begin
		if not F.BlockWrite(Wave^, Wave^.BytesFollowing + 8) then goto LRetry;
		F.Truncate;
		F.Close;
	end;
	F.Free;
end;

function BitsToByte(const Bits: Int64): LongInt;
begin
	Result := (Bits + 7) shr 3;
end;

function GetTotalSamples(const BytesPerSample: Word;
	const DataBytes: LongInt): LongInt;
begin
	Result := DataBytes div BytesPerSample;
end;

procedure WaveCreate(var Wave: PWave;
	const Channels: Word;
	const BitsPerSample: Word;
	const SampleRate: LongInt;
	const TotalSamples: LongInt);
var
	BitsPerSamples: Word;
	DataBytes: LongInt;
begin
	BitsPerSamples := Channels * BitsPerSample;
	DataBytes := BitsToByte(BitsPerSamples * TotalSamples);

	if Wave <> nil then
	begin
//		MessageD('Destination Wave Must Be Nil', mtError, [mbOk]);
		if LongInt(Pointer(LongInt(Wave) - 4)^) - 6 < WaveHead + DataBytes then
		begin
			WaveFree(Wave);
//			FreeMem(Wave);
		end;
	end;
	GetMem(Wave, WaveHead + DataBytes);
	Wave.Marker1 := 'RIFF';
	Wave.BytesFollowing := DataBytes + WaveHead - 8;
	Wave.Marker2 := 'WAVE';
	Wave.Marker3 := 'fmt ';
	Wave.BlockAlign := 16;
	Wave.FormatTag := 1;
	Wave.Channels := Channels;
	Wave.SampleRate := SampleRate;
	Wave.BytesPerSecond := BitsToByte(BitsPerSamples * SampleRate);
	Wave.BytesPerSample := BitsToByte(BitsPerSamples); // 256 for 4 bit !!!
	Wave.BitsPerSample := BitsPerSample;
	Wave.Marker4 := 'data';
	Wave.DataBytes := DataBytes;

{ case BitsPerSample of
	8: FillChar(Wave.Data.B, Wave.DataBytes, 128);
	16: FillChar(Wave.Data.W, Wave.DataBytes div 2, 0);
	end;}
end;

procedure WaveFree(var Wave: PWave);
begin
	FreeMem(Wave); Wave := nil;
end;

procedure ConvertChannels(const WaveS: PWave; var WaveD: PWave;
	const NewChannels: Word; const Left, Right: Integer);
var
	i: Integer;
//  WaveData: Integer;
	TotalSamples: Integer;
begin
	if (WaveS = nil) or (WaveS = WaveD) then Exit;
	TotalSamples :=
		GetTotalSamples(WaveS.BytesPerSample, WaveS.DataBytes);

{ if WaveD = nil then
	begin}
		WaveCreate(WaveD, NewChannels, WaveS.BitsPerSample,
		WaveS.SampleRate, TotalSamples);
{ end
	else
	begin
		WaveData := 2 * WaveS.DataBytes;
		Move(WaveS^, WaveD^, WaveHead);
		WaveD.Channels := 2;
		WaveD.BytesPerSecond := WaveD.BytesPerSecond * 2;
		WaveD.BytesPerSample := WaveD.BytesPerSample * 2;
		WaveD.DataBytes := WaveData;
		WaveD.BytesFollowing :=  WaveHead + WaveData - 8;
	end;}

	case WaveS.BitsPerSample of
	8:
		case WaveS.Channels of
		1:
			case WaveD.Channels of
			1:
				for i := 0 to WaveD.DataBytes - 1 do
				begin
					WaveD.Data.B[i] := WaveS.Data.B[i];
				end;
			2:
			begin
{       for i := 0 to WaveD.DataBytes div 2 - 1 do
				begin
					WaveD.Data.B[i shl 1 + 0] := Left * ShortInt(WaveS.Data.B[i] - 128) div ConvertPre + 128;
					WaveD.Data.B[i shl 1 + 1] := Right * ShortInt(WaveS.Data.B[i] - 128) div ConvertPre + 128;
				end;}
				asm
				pushad
				mov eax, WaveD
				mov edi, [eax]
				add edi, WaveHead
				mov esi, WaveS
				mov ecx, [esi + WaveHead - 4]
				add esi, WaveHead
				add ecx, esi
				@Loop:
					xor ebx, ebx
					mov bl, byte ptr [esi] // 3
					sub ebx, 128

					mov eax, Left
					imul eax, ebx // 10
					sar eax, ConvertShr
					add al, 128
					mov [edi], al
					inc edi

					mov eax, Right
					imul eax, ebx // 10
					sar eax, ConvertShr
					add al, 128
					mov [edi], al
					inc edi

					inc esi
				cmp esi, ecx
				jne @Loop
				popad
				end;
			end;
			end;
		2:
			case WaveD.Channels of
			1:
				for i := 0 to WaveD.DataBytes - 1 do
				begin
					WaveD.Data.B[i] :=
						(Left * ShortInt(WaveS.Data.B[i] - 128) div ConvertPre +
						Right * ShortInt(WaveS.Data.B[i] - 128) div ConvertPre) div 2 + 128;
				end;
			2:
				for i := 0 to WaveD.DataBytes div 2 - 1 do
				begin
					WaveD.Data.B[i shl 1 + 0] := Left * ShortInt(WaveS.Data.B[i shl 1 + 0] - 128) div ConvertPre + 128;
					WaveD.Data.B[i shl 1 + 1] := Right * ShortInt(WaveS.Data.B[i shl 1 + 1] - 128) div ConvertPre + 128;
				end;
			end;
		end;
	16:
		case WaveS.Channels of
		1:
			case WaveD.Channels of
			1:
				for i := 0 to WaveD.DataBytes div 2 - 1 do
				begin
					WaveD.Data.W[i] := WaveS.Data.W[i];
				end;
			2:
			begin
{       for i := 0 to WaveD.DataBytes div 4 - 1 do
				begin
					WaveD.Data.W[i shl 1 + 0] := Left * WaveS.Data.W[i] div ConvertPre;
					WaveD.Data.W[i shl 1 + 1] := Right * WaveS.Data.W[i] div ConvertPre;
				end;}
				asm
				pushad
				mov eax, WaveD
				mov edi, [eax]
				add edi, WaveHead
				mov esi, WaveS
				mov ecx, [esi + WaveHead - 4]
				add esi, WaveHead
				add ecx, esi
				@Loop:
					movsx ebx, SmallInt ptr [esi] // 3

					mov eax, Left
					imul eax, ebx // 10
					sar eax, ConvertShr
					mov [edi], ax
					add edi, 2

					mov eax, Right
					imul eax, ebx // 10
					sar eax, ConvertShr
					mov [edi], ax
					add edi, 2

					add esi, 2
				cmp esi, ecx
				jne @Loop
				popad
				end;
			end;
			end;
		2:
			case WaveD.Channels of
			1:
				for i := 0 to WaveD.DataBytes div 2 - 1 do
				begin
					WaveD.Data.W[i] :=
						(Left * WaveS.Data.W[i] div ConvertPre +
						Right * WaveS.Data.W[i] div ConvertPre) div 2;
				end;
			2:
				for i := 0 to WaveD.DataBytes div 4 - 1 do
				begin
					WaveD.Data.W[i shl 1 + 0] := Left * WaveS.Data.W[i shl 1 + 0] div ConvertPre;
					WaveD.Data.W[i shl 1 + 1] := Right * WaveS.Data.W[i shl 1 + 1] div ConvertPre;
				end;
			end;
		end;
	end;
end;

procedure ConvertBitsPerSample(const WaveS: PWave; var WaveD: PWave;
	const NewBitsPerSample: Integer);
var
	i: Integer;
	TotalSamples: LongInt;
begin
	if (WaveS = nil) or (WaveS = WaveD) then Exit;
	TotalSamples :=
		GetTotalSamples(WaveS.BytesPerSample, WaveS.DataBytes);

{ if WaveD = nil then
	begin}
		WaveCreate(WaveD, WaveS.Channels, NewBitsPerSample,
		WaveS.SampleRate, TotalSamples);
{ end
	else
	begin
		WaveData := 2 * WaveS.DataBytes;
		Move(WaveS^, WaveD^, WaveHead);
		WaveD.Channels := 2;
		WaveD.BytesPerSecond := WaveD.BytesPerSecond * 2;
		WaveD.BytesPerSample := WaveD.BytesPerSample * 2;
		WaveD.DataBytes := WaveData;
		WaveD.BytesFollowing :=  WaveHead + WaveData - 8;
	end;}

	case WaveS.BitsPerSample of
	8:
	begin
		case WaveD.BitsPerSample of
		8:
			for i := 0 to WaveD.DataBytes - 1 do
			begin
				WaveD.Data.B[i] := WaveS.Data.B[i];
			end;
		16:
			for i := 0 to WaveD.DataBytes div 2 - 1 do
			begin
				WaveD.Data.W[i] := 256 * (WaveS.Data.B[i] - 128);
			end;
		end;
	end;
	16:
	begin
		case WaveD.BitsPerSample of
		8:
			for i := 0 to WaveD.DataBytes - 1 do
			begin
				WaveD.Data.B[i] := (WaveS.Data.W[i] div 256) + 128;
			end;
		16:
			for i := 0 to WaveD.DataBytes div 2 - 1 do
			begin
				WaveD.Data.W[i] := WaveS.Data.W[i];
			end;
		end;
	end;
	end;
end;

procedure ConvertSampleRate(const WaveD: PWave; const SampleRate: LongInt);
begin
	WaveD.SampleRate := SampleRate;
	WaveD.BytesPerSecond := WaveD.BytesPerSample * WaveD.SampleRate;
//    BitsToByte(WaveD.Channels * WaveD.BitsPerSample * Int64(SampleRate));
end;

procedure ConvertWave(const WaveS: PWave; var WaveD: PWave;
	const Channels: Word;
	const BitsPerSample: Word;
	const SampleRate: LongInt);
begin
{ ConvertBitsPerSample(WaveS, WaveD, BitsPerSample);
	ConvertChannels(WaveS, WaveD, Channels, ConvertPre, ConvertPre);
	ConvertSampleRate(WaveD, SampleRate);}
end;

procedure PlayWave(Wave: PWave);
begin
	if Wave <> nil then
		PlaySound(PChar(Wave), 0, SND_ASYNC or SND_MEMORY or SND_NODEFAULT)
end;

procedure PlayWaveFile(WaveName: TFileName);
//var Wave: PWave;
begin
{	WaveReadFromFile(Wave, WaveName);
	PlayWave(Wave);
	FreeMem(Wave);}
	if WaveName <> '' then
		PlaySound(PChar(WaveName), 0, SND_ASYNC {and SND_FILENAME});
end;

procedure PlayWinSound(WinSound: TWinSound);
var
	Reg: TRegistry;
	Key: string;
	SndName: TFileName;
begin
	Reg := TRegistry.Create;
	Reg.RootKey := HKEY_CURRENT_USER;
	Key := 'AppEvents\Schemes\Apps\.Default\' + WinSoundNames[WinSound] + '\.Current';
	if Reg.OpenKey(Key, False) then
	begin
		if Reg.ValueExists('') then
		begin
			SndName := ReplaceF(Reg.ReadString(''), '%SystemRoot%', WinDir);
			PlayWaveFile(SndName);
		end;
		Reg.CloseKey;
	end;
	Reg.Free;
end;

// TWavePlayer

const
	SpeedDiv = 1024;

constructor TWavePlayer.Create;
begin
	inherited Create;
	HWaveOut := 0;
	PlayItems := TData.Create(True);
	PlayItems.ItemSize := SizeOf(TPlayItem);
	Initialized := False;
	CloseInvoked := False;
	VolumeLeft := MaxVolume;
	VolumeRight := MaxVolume;
	Speed := SpeedDiv;

	Sound16bits := True;
	Frequency := 44100;
	SoundStereo := True;
	BufferTime := 200;
end;

destructor TWavePlayer.Destroy;
begin
	if Initialized then Close;
	HWaveOut := 0;
	FreeAndNil(PlayItems);
	inherited Destroy;
end;

procedure TWavePlayer.MMError(s: string);
begin
	if FError <> 0 then ErrorMessage(s + ' (' + WaveErrorText(FError) + ')');
end;

procedure MMOutDone(
	wo: HWAVEOUT;
	Msg: UINT;
	Instance: U4;
	Param1: U4;
	Param2: U4); stdcall;
var
	Header: PWaveHdr;
	BufferOut: PWaveData;
//	BufferOutSize: SG;
	WavePlayer: TWavePlayer;
begin
	if Msg = WOM_DONE then
	begin
		WavePlayer := TWavePlayer(Instance);
		Header := PWaveHdr(Param1);
		BufferOut := Pointer(Header^.lpData);
//		BufferOutSize := Header^.dwBufferLength;
{		GetMem(Buffer, BufferSize);
		Move(BufferOut^, Buffer^, BufferSize);}

		WavePlayer.FError := waveOutUnPrepareHeader(WavePlayer.HWaveOut, Header, SizeOf(TWaveHdr));
		WavePlayer.MMError('WaveOutUnPrepare');
		FreeMem(BufferOut); //BufferOut := nil;
		Dispose(Header);

		if WavePlayer.Initialized = False then Exit;
		Dec(WavePlayer.OutCount);
		if WavePlayer.CloseInvoked = False then
				WavePlayer.SendBuffer
		else
		if WavePlayer.OutCount = 0 then
		begin
			WavePlayer.FError := WaveOutClose(WavePlayer.HWaveOut);
			WavePlayer.HWaveOut := 0;
			WavePlayer.MMError('WaveOutClose');
			if WavePlayer.FError = 0 then
				WavePlayer.Initialized := False;
		end;

	end;
end;

function GetBufferSize(wBitsPerSample, nChannels, BufferOutSamples: SG): SG;
begin
	Result := BufferOutSamples * ((wBitsPerSample * nChannels + 7) div 8);
end;

function GetBufferSample(wBitsPerSample, BufferOutSize: SG): SG;
begin
	case wBitsPerSample of
	16: Result := BufferOutSize div 2;
	else Result := BufferOutSize;
	end;
end;

procedure TWavePlayer.Init;
const BufferCount = 8;
var i: SG;
begin
	if Initialized then Close;
	CloseInvoked := False;
	WaveFormat.WFormatTag := WAVE_FORMAT_PCM; // PCM format - the only option
	if SoundStereo then
		WaveFormat.nChannels := 2
	else
		WaveFormat.nChannels := 1;
	WaveFormat.nSamplesPerSec := Frequency;
	if Sound16bits then
		WaveFormat.wBitsPerSample := 16
	else
		WaveFormat.wBitsPerSample := 8;
	WaveFormat.nAvgBytesPerSec := WaveFormat.wBitsPerSample * WaveFormat.nSamplesPerSec div 8;
	WaveFormat.nBlockAlign := (WaveFormat.wBitsPerSample * WaveFormat.nChannels) div 8;
	WaveFormat.cbSize := SizeOf(WaveFormat);

	FError := waveOutOpen(nil, 0, @WaveFormat, 0, 0, WAVE_FORMAT_QUERY);
	MMError('Play format not supported');
	if FError <> 0 then Exit;

	FError := waveOutOpen(@HWaveOut, 0, @WaveFormat, Cardinal(@MMOutDone), Cardinal(Self), CALLBACK_FUNCTION);
	MMError('WaveOutOpen');
	if FError <> 0 then Exit;

	OutCount := 0;

	BufferOutSamples := RoundDiv(WaveFormat.nSamplesPerSec * BufferTime, BufferCount * 1000);
	BufferOutSize := GetBufferSize(WaveFormat.wBitsPerSample, WaveFormat.nChannels, BufferOutSamples);
{	BufferOutSize := BufferSize;
	BufferOutSamples := GetBufferSample(WaveFormat.wBitsPerSample, WaveFormat.nChannels, BufferOutSize);}

	Initialized := True;
	for i := 0 to BufferCount - 1 do
		SendBuffer;
end;

procedure TWavePlayer.Close;
begin
	CloseInvoked := True;
//	Stop; // Lag problem
	while OutCount > 0 do
		Sleep(40);
end;

procedure TWavePlayer.Pause;
begin
	FError := waveOutPause(HWaveOut);
	MMError('WaveOutOpen');
end;

procedure TWavePlayer.Resume;
begin
	FError := waveOutRestart(HWaveOut);
	MMError('WaveOutOpen');
end;

procedure TWavePlayer.Stop;
begin
	FError := waveOutReset(HWaveOut);
	MMError('WaveOutOpen');
end;


procedure TWavePlayer.SendBuffer;
var
	Header: PWaveHdr;
	i, j: SG;
	SampleP: UG;
	ValueL, ValueR, ValueLS, ValueRS, Value: SG;
	PlayItem: PPlayItem;
	BufferOut: PWaveData;

	procedure WriteValue;
	begin
		case WaveFormat.wBitsPerSample of
		8:
		begin
			Value := (Value + 32768) div 256;
			if Value < 0 then
				Value := 0
			else if Value > 255 then
				Value := 255;
			BufferOut.B[i] := Value;
		end;
		16:
		begin
			if Value < -32768 then
				Value := -32768
			else if Value > 32767 then
				Value := 32767;
			BufferOut.W[i] := Value;
		end;
		end;
		Inc(i);
	end;

begin
	j := 0;
	while j < SG(PlayItems.Count) do
	begin
		PlayItem := PlayItems.Get(j);
		case PlayItem.PlayAs of
		paNoise: PlayItem.Offset := PlayItem.Speed;
		paTone: PlayItem.Offset := PlayItem.Speed;
		else PlayItem.Offset := PlayItem.Wave.Channels * PlayItem.Speed * UG(PlayItem.Wave.SampleRate) div UG(WaveFormat.nSamplesPerSec);
		end;
		Inc(j);
	end;
	// 16bit Mixer
	GetMem(BufferOut, BufferOutSize);
	FillChar(BufferOut^, BufferOutSize, 0);
	i := 0;
	while i < BufferOutSamples * WaveFormat.nChannels do
	begin
		ValueLS := 0;
		ValueRS := 0;
		j := 0;
		while j < SG(PlayItems.Count) do
		begin
			ValueL := 0;
			ValueR := 0;
			PlayItem := PlayItems.Get(j);
			SampleP := (PlayItem.SamplePos div SpeedDiv);
			if SampleP >= PlayItem.SampleCount then
			begin
				PlayItems.Delete(j);
			end
			else
			begin
				Inc(j);
				case PlayItem.PlayAs of
				paNoise:
				begin
					ValueL := Random2(32768);
					ValueR := Random2(32768);
				end;
				paTone:
				begin
					SampleP := (Int64(PlayItem.SamplePos));
					ValueL := 32768 *
						Sins[(SampleP) and (AngleCount - 1)] div SinDiv;
					ValueR := ValueL;
				end;
				else // paWave
				begin
					if PlayItem.Wave.Channels = 2 then
						SampleP := SampleP and $fffffffe;
					case PlayItem.Wave.BitsPerSample of
					16: ValueL := PlayItem.Wave.Data.W[SampleP];
					else ValueL := 256 * (SG(PlayItem.Wave.Data.B[SampleP]) - 128);
					end;
					if PlayItem.Wave.Channels = 2 then
					begin
						case PlayItem.Wave.BitsPerSample of
						16: ValueR := PlayItem.Wave.Data.W[SampleP + 1];
						else ValueR := 256 * (SG(PlayItem.Wave.Data.B[SampleP + 1]) - 128);
						end;
//						Inc(PlayItem.SamplePos, PlayItem.Offset);
					end
					else
					begin
						ValueR := ValueL
					end;
				end;
				end;
				Inc(PlayItem.SamplePos, PlayItem.Offset);
			end;

			if (PlayItem.VolumeLeft <> MaxVolume) then
				ValueL := ValueL * PlayItem.VolumeLeft div MaxVolume;
			if (PlayItem.VolumeRight <> MaxVolume) then
				ValueR := ValueR * PlayItem.VolumeRight div MaxVolume;
			Inc(ValueLS, ValueL);
			Inc(ValueRS, ValueR);
		end;

		// 16bit stereo here

		if WaveFormat.nChannels = 2 then
		begin
			Value := ValueLS;
			WriteValue;
			Value := ValueRS;
			WriteValue;
		end
		else
		begin
			Value := (ValueLS + ValueRS) div 2;
			WriteValue;
		end;
	end;

	Header := New(PWaveHdr);
	Header^.lpData := Pointer(BufferOut);
	Header^.dwBufferLength := BufferOutSize;
	Header^.dwBytesRecorded := 0;
	Header^.dwUser := 0;
	Header^.dwFlags := 0;
	Header^.dwLoops := 0;
	Header^.lpNext := nil;
	Header^.reserved := 0;

	FError := waveOutPrepareHeader(HWaveOut, Header, SizeOf(TWaveHdr));
	MMError('WaveOutPrepare');
	if FError <> 0 then Exit;

	FError := waveOutWrite(HWaveOut, Header, SizeOf(TWaveHdr));
	MMError('WaveOutWrite');
	if FError <> 0 then Exit;

	Inc(OutCount);
end;

procedure TWavePlayer.Play(Wave: PWave);
var PlayItem: PPlayItem;
begin
	if CheckWave(Wave) then
	begin
		PlayItem := PlayItems.Add;
		PlayItem.PlayAs := paWave;
		PlayItem.Wave := Wave;
		PlayItem.Frequency := 0;
		PlayItem.SampleCount := 8 * Wave.DataBytes div Wave.BitsPerSample;
		PlayItem.SamplePos := 0;
		PlayItem.VolumeLeft := VolumeLeft;
		PlayItem.VolumeRight := VolumeRight;
		PlayItem.Speed := Speed;
	end;
end;

procedure TWavePlayer.Beep;
begin
	Tone(1000, 1000);
end;

procedure TWavePlayer.Tone(Frequency, Tim: UG);
var
	PlayItem: PPlayItem;
begin
	PlayItem := PlayItems.Add;
	PlayItem.PlayAs := paTone;
	PlayItem.Wave := nil;
	PlayItem.Frequency := Frequency;
	PlayItem.SampleCount := WaveFormat.nSamplesPerSec * Tim div 1000;
	PlayItem.SamplePos := 0;
	PlayItem.VolumeLeft := VolumeLeft;
	PlayItem.VolumeRight := VolumeRight;
	PlayItem.Speed := Speed;
end;

procedure TWavePlayer.Noise(Tim: UG);
var
	PlayItem: PPlayItem;
begin
	PlayItem := PlayItems.Add;
	PlayItem.PlayAs := paNoise;
	PlayItem.Wave := nil;
	PlayItem.Frequency := 0;
	PlayItem.SampleCount := WaveFormat.nSamplesPerSec * Tim div 1000;
	PlayItem.SamplePos := 0;
	PlayItem.VolumeLeft := VolumeLeft;
	PlayItem.VolumeRight := VolumeRight;
	PlayItem.Speed := Speed;
end;

procedure Beep;
begin
	PlayWinSound(wsDefaultSound);
end;

initialization

finalization
	if Assigned(WavePlayer) then
	begin
		FreeAndNil(WavePlayer);
	end;
end.
