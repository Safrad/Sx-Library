// Build: 07/1999-01/2000 Author: Safranek David

unit uWave;

interface

uses MMSystem, SysUtils;

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
		BytesFollowing:  LongInt; // 4
		Marker2:         array[0..3] of Char; // 4
		Marker3:         array[0..3] of Char; // 4
		BlockAlign:      LongInt; // 4
		FormatTag:       Word; // 2
		Channels:        Word; // 2
		SampleRate:      LongInt; // 4
		BytesPerSecond:  LongInt; // 4
		BytesPerSample:  Word; // 2 ; 1, 2, 4; 4: 16 bit stereo, 2: 8 bit stereo
		BitsPerSample:   Word; // 2 16: 16 bit stereo, 8: 8 bit stereo
		Marker4:         array[0..3] of Char; // 4
		DataBytes:       LongInt; // 4
		Data:            TWaveData; // X
	end;
	PWave = ^TWave;

const
	ConvertShr = 16;
	ConvertPre = 1 shl ConvertShr;
procedure SoundLR(var Left, Right: Integer; const NowPos, MaxPos: Integer);
// For screen Width 800 is NowPos 0..799, MaxPos 799


procedure WaveReadFromFile(var Wave: PWave; FName: TFileName);
procedure WaveWriteToFile(var Wave: PWave; FName: TFileName);

procedure WaveCreate(var Wave: PWave;
	const Channels: Word; // 1, 2
	const BitsPerSample: Word; // 8, 16
	const SampleRate: LongInt; // 11025, 22050, 44100 ...
	const TotalSamples: LongInt); // 1 sec = SampleRate

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

procedure WaveOpen(const Channels: Word; const SampleRate: Cardinal;
	const BytesPerSecond: Cardinal; const BitsPerSample: Word);
procedure WavePrepareHeader(var Header: PWaveHdr; Wave: PWave);
procedure WaveUnprepareHeader(var Header: PWaveHdr);
procedure WavePlay(Header: PWaveHdr);
procedure WaveClose;

var
	HW: PHWAVEOUT;

implementation

uses uFiles, uError;

(*
procedure NoSound; assembler;
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
	push ax
	push bx
	push dx
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
	pop dx
	pop bx
	pop ax
	{$endif}
end;
*)

(*
	MMSYSERR_NOERROR      = 0;                  { no error }
	MMSYSERR_ERROR        = MMSYSERR_BASE + 1;  { unspecified error }
	MMSYSERR_BADDEVICEID  = MMSYSERR_BASE + 2;  { device ID out of range }
	MMSYSERR_NOTENABLED   = MMSYSERR_BASE + 3;  { driver failed enable }
	MMSYSERR_ALLOCATED    = MMSYSERR_BASE + 4;  { device already allocated }
	MMSYSERR_INVALHANDLE  = MMSYSERR_BASE + 5;  { device handle is invalid }
	MMSYSERR_NODRIVER     = MMSYSERR_BASE + 6;  { no device driver present }
	MMSYSERR_NOMEM        = MMSYSERR_BASE + 7;  { memory allocation error }
	MMSYSERR_NOTSUPPORTED = MMSYSERR_BASE + 8;  { function isn't supported }
	MMSYSERR_BADERRNUM    = MMSYSERR_BASE + 9;  { error value out of range }
	MMSYSERR_INVALFLAG    = MMSYSERR_BASE + 10; { invalid flag passed }
	MMSYSERR_INVALPARAM   = MMSYSERR_BASE + 11; { invalid parameter passed }
	MMSYSERR_HANDLEBUSY   = MMSYSERR_BASE + 12; { handle being used
																								simultaneously on another
																								thread (eg callback) }
	MMSYSERR_INVALIDALIAS = MMSYSERR_BASE + 13; { specified alias not found }
	MMSYSERR_BADDB        = MMSYSERR_BASE + 14; { bad registry database }
	MMSYSERR_KEYNOTFOUND  = MMSYSERR_BASE + 15; { registry key not found }
	MMSYSERR_READERROR    = MMSYSERR_BASE + 16; { registry read error }
	MMSYSERR_WRITEERROR   = MMSYSERR_BASE + 17; { registry write error }
	MMSYSERR_DELETEERROR  = MMSYSERR_BASE + 18; { registry delete error }
	MMSYSERR_VALNOTFOUND  = MMSYSERR_BASE + 19; { registry value not found }
	MMSYSERR_NODRIVERCB   = MMSYSERR_BASE + 20; { driver does not call DriverCallback }
	MMSYSERR_LASTERROR    = MMSYSERR_BASE + 20; { last error in range }
*)
function WaveErrorText(ErrorCode: Integer): string;
var P: PChar;
begin
	P := '';
	waveOutGetErrorText(ErrorCode, P, 255);
	Result := P;
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

procedure WaveReadFromFile(var Wave: PWave; FName: TFileName);
label LRetry, LFin;
var
	FSize: LongInt;
	F: file;
	ErrorCode: Integer;
begin
	LRetry:
	AssignFile(F, FName);
	FileMode := 0; Reset(F, 1);
	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FName, ErrorCode) then goto LRetry;
	end
	else
	begin
		FSize := FileSize(F);
		GetMem(Wave, FSize);
		BlockRead(F, Wave^, FSize);
		ErrorCode := IOResult; if ErrorCode <> 0 then goto LFin;
		if (Wave.Marker1 <> 'RIFF') or (Wave.Marker2 <> 'WAVE') or
			(Wave.Marker3 <> 'fmt ') then
		begin
			IOErrorMessage(FName, 'is not wave');
		end;
		LFin:
		CloseFile(F);
		IOResult;
		if ErrorCode <> 0 then
		begin
			if IOErrorRetry(FName, ErrorCode) then goto LRetry;
		end;
	end;
end;

procedure WaveWriteToFile(var Wave: PWave; FName: TFileName);
label LRetry;
var
	F: file;
	ErrorCode: Integer;
begin
	if Wave = nil then Exit;
	LRetry:
	AssignFile(F, FName);
	if FileExists(FName) then
	begin
		FileMode := 1; Reset(F, 1);
	end
	else
		Rewrite(F, 1);

	ErrorCode := IOResult;
	if ErrorCode <> 0 then
	begin
		if IOErrorRetry(FName, ErrorCode) then goto LRetry;
	end
	else
	begin
		BlockWrite(F, Wave^, Wave^.BytesFollowing + 8);
		Truncate(F);
		ErrorCode := IOResult;
		CloseFile(F);
		IOResult;
		if ErrorCode <> 0 then
		begin
			if IOErrorRetry(FName, ErrorCode) then goto LRetry;
		end;
	end;
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

	if Wave = nil then
	begin
		GetMem(Wave, WaveHead + DataBytes);
	end
	else
	begin
		if LongInt(Pointer(LongInt(Wave) - 4)^) - 6 < WaveHead + DataBytes then
		begin
			FreeMem(Wave);
			GetMem(Wave, WaveHead + DataBytes);
		end;
	end;
	Wave.Marker1 := 'RIFF';
	Wave.BytesFollowing := DataBytes + WaveHead - 8;
	Wave.Marker2 := 'WAVE';
	Wave.Marker3 := 'fmt ';
	Wave.BlockAlign := 16;
	Wave.FormatTag := 1;
	Wave.Channels := Channels;
	Wave.SampleRate := SampleRate;
	Wave.BytesPerSecond := BitsToByte(BitsPerSamples * SampleRate);
	Wave.BytesPerSample := BitsToByte(BitsPerSamples);  // 256 for 4 bit !!!
	Wave.BitsPerSample := BitsPerSample;
	Wave.Marker4 := 'data';
	Wave.DataBytes := DataBytes;
{ case BitsPerSample of
	8: FillChar(Wave.Data.B, Wave.DataBytes, 128);
	16: FillChar(Wave.Data.W, Wave.DataBytes div 2, 0);
	end;}
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


procedure WaveOpen(const Channels: Word; const SampleRate: Cardinal;
	const BytesPerSecond: Cardinal; const BitsPerSample: Word);
var
	OpenParm: TMCI_Open_Parms;
	FFlags: LongInt;
	FError: Integer;
	WF: PWaveFormatEx;
begin
	FillChar(OpenParm, SizeOf(TMCI_Open_Parms), 0);

	OpenParm.dwCallback := 0;
	OpenParm.lpstrDeviceType := 'WaveAudio';
	OpenParm.lpstrElementName := PChar('');

	FFlags := 0;

{ if FUseWait then
	begin
		if FWait then FFlags := mci_Wait;
		FUseWait := False;
	end
	else
		FFlags := mci_Wait;

	if FUseNotify then
	begin
		if FNotify then FFlags := FFlags or mci_Notify;
		FUseNotify := False;
	end;

	if FDeviceType <> dtAutoSelect then}
		FFlags := FFlags or mci_Open_Type;
{ else;
		FFlags := FFlags or MCI_OPEN_ELEMENT;}

{ if FShareable then
		FFlags := FFlags or mci_Open_Shareable;
	OpenParm.dwCallback := Handle;}


	FError := mciSendCommand(0, mci_Open, FFlags, LongInt(@OpenParm));
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));

	New(WF);
	WF.WFormatTag := WAVE_FORMAT_PCM; {PCM format - the only option!}
	WF.NChannels := 1;//Channels;
	WF.NSamplesPerSec := 22050;//SampleRate;
	WF.NAvgBytesPerSec := 22050;//BytesPerSecond;
	WF.wBitsPerSample := 8;//BitsPerSample;
	WF.NBlockAlign := 1; {only one Byte in each sample}
	WF.cbSize := 0;
	// , WAVE_MAPPER      DeviceId
//MPlayer                              dtAVIVideo
{ i := waveOutOpen(nil, 1,
		WF, 0, 0, WAVE_FORMAT_QUERY);}

	HW := New(PHWAVEOUT);
	FError := waveOutOpen(HW, OpenParm.wDeviceID,
		WF, 0, 0, CALLBACK_NULL{WAVE_MAPPED{WAVE_ALLOWSYNC});
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));

	Dispose(WF);
end;

procedure WavePrepareHeader(var Header: PWaveHdr; Wave: PWave);
var FError: Integer;
begin
	New(Header);
	FillChar(Header^, SizeOf(Header^), 0);
	Header.lpData := Addr(Wave.Data);
	Header.dwBufferLength := Wave.DataBytes; //SizeOf(PMemBlock^);
	Header.dwUser := Wave.DataBytes; // SizeOf(PMemBlock^);
	Header.dwFlags := WHDR_DONE; //0 + WHDR_PREPARED  + WHDR_INQUEUE;
	Header.lpNext := nil;//Header;
	Header.dwLoops := 1;

	FError := waveOutPrepareHeader(HW^, Header, SizeOf(TWaveHdr));
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));
end;

procedure WaveUnprepareHeader(var Header: PWaveHdr);
var FError: Integer;
begin
	FError := waveOutUnprepareHeader(HW^, Header, SizeOf(TWaveHdr));
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));
	Dispose(Header); Header := nil;
end;


procedure WavePlay(Header: PWaveHdr);
var
	FError: Integer;
begin
	FError := waveOutReset(HW^);
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));

	FError := waveOutWrite(HW^, Header, SizeOf(TWaveHdr));
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));


{ i := waveOutRestart(HW^);}
end;

procedure WaveClose;
var
	FError: Integer;
begin
	FError := waveOutClose(HW^);
	if FError <> 0 then ErrorMessage(WaveErrorText(FError));

	if FError = 0 then
	begin
		Dispose(HW); HW := nil;
	end;
end;

end.
