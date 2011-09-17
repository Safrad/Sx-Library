unit uWavePlayer;

interface

uses
	MMSystem, SysUtils, Windows,
	uTypes, uDTimer, uWave, uMath, uFiles, uData;

function GetBufferSize(wBitsPerSample, nChannels, BufferOutSamples: SG): SG;
function GetBufferSample(wBitsPerSample, BufferOutSize: SG): SG;

const
	MaxVolume = 1024;
	SpeedDiv = 1024;

type
	TVolume = packed record
		Left: S4;
		Right: S4;
	end;

	PPlayItem = ^TPlayItem;
	TPlayItem = packed record // 32
		PlayAs: (paNone, paWave, paTone, paNoise, paSilent); // 1
		StartDelay: S4;
		SampleCount: U4;
		SamplePos: U4;
		Speed: U4;
		Offset: U4; // Calculated from Speed
		Volume: TVolume;
		Wave: TWave; // 4
	end;

	TWaveCommon = class
	private
		FHWave: HWave;
		FWaveFormat: TWaveFormatEx;

		FError: MMResult;
		FActive: Boolean;
		FCloseInvoked: Boolean;

		FBufferOutSize: SG;
		FBufferOutSamples: SG;
		FOutCount: SG;

		procedure SendBuffer;
		procedure FillBuffer(Buffer: PWaveSample); virtual; abstract; // WavePlayerOnly
	public
		// Options
		Volume: TVolume;

		Bits: SG;
		Frequency: SG;
		Channels: 1..2;

		BufferTime: UG;
		property WaveFormat: TWaveFormatEx read FWaveFormat;

		function WaveErrorText(const ErrorCode: U4): string;
		procedure MMError(s: string);

		constructor Create;
		destructor Destroy; override;

		procedure Open;
		procedure Close;

		procedure Pause;
		procedure Resume;
		procedure Stop; virtual;

		property BufferSize: SG read FBufferOutSize;
		property BufferSamples: SG read FBufferOutSamples;
		property BufferOutCount: SG read FOutCount;
		property Active: Boolean read FActive;
	end;

	TWavePlayer = class(TWaveCommon)
	private
		// Items to play
		PlayItems: TData;

		procedure WriteValue(Value: SG; var Buffer: PWaveSample);
		procedure FillBuffer(Buffer: PWaveSample); override;
	public
		MaxItems: SG;

		constructor Create;
		destructor Destroy; override;

		procedure Play(const Wave: TWave; const Volume: TVolume; const Speed: UG = SpeedDiv; const StartDelay: SG = 0);
		procedure Beep;
		procedure Silent(const Tim: UG; const StartDelay: SG = 0);
		procedure Tone(const Frequency, Tim: UG);
		procedure Noise(const Tim: UG);
		procedure Stop; override;
	end;

	TOnReciveBuffrerEvent = procedure(Sender: TObject; Buffer: PWaveSample) of object;

	TWaveRecorder = class(TWaveCommon)
	private
		FOnReciveBuffrer: TOnReciveBuffrerEvent;
	public
		property OnReciveBuffrer: TOnReciveBuffrerEvent read FOnReciveBuffrer write FOnReciveBuffrer;
	end;

// Midi
function MidiMCICallBack: Boolean;
procedure MidiMCIOpen(FileName: TFileName);
procedure MidiMCISeek(const SeekTo: U4);
function MidiMCIGetPos: U4;
procedure MidiMCIPause;
procedure MidiMCIStop;
procedure MidiMCIResume;
procedure MidiMCIPlay;
procedure MidiMCIClose;

var
	MidiHandle: HWnd;
	MidiPlaying: Boolean;
	MidiOpened: Boolean;

implementation

uses uOutputFormat, uMsg;

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

// TPlayerCommon

constructor TWaveCommon.Create;
begin
	inherited Create;
	FHWave := 0;
	FActive := False;
	FCloseInvoked := False;
	Volume.Left := MaxVolume;
	Volume.Right := MaxVolume;

	Bits := 16;
	Frequency := 44100;
	if Self is TWavePlayer then
		Channels := 2
	else
		Channels := 1;
	BufferTime := 200;
end;

destructor TWaveCommon.Destroy;
begin
	if FActive then Close;
	FHWave := 0;
	inherited Destroy;
end;

function TWaveCommon.WaveErrorText(const ErrorCode: U4): string;
var B: BG;
begin
	SetLength(Result, MAXERRORLENGTH);
	if Self is TWavePlayer then
		B:= waveOutGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR
	else
		B:= waveInGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR;
	if B then
	begin
		if Result <> '' then
			if Result[Length(Result)] = #0 then SetLength(Result, Length(Result) - 1);
//		Replace(Result, #0, ' ')
	end
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' ' + 'Unknown error';
end;

procedure TWaveCommon.MMError(s: string);
begin
	if Self is TWavePlayer then
		s := 'WaveOut: ' + s
	else
		s := 'WaveIn: ' + s;
	if FError <> 0 then ErrorMsg(s + ' (' + WaveErrorText(FError) + ')');
end;

procedure MMOutDone(
	wo: HWAVEOUT;
	Msg: UINT;
	Instance: U4;
	Param1: U4;
	Param2: U4); stdcall;
var
	Header: PWaveHdr;
	BufferOut: PWaveSample;
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

		WavePlayer.FError := waveOutUnPrepareHeader(WavePlayer.FHWave, Header, SizeOf(TWaveHdr));
		WavePlayer.MMError('UnPrepare');
		FreeMem(BufferOut); //BufferOut := nil;
		Dispose(Header);

		if WavePlayer.FActive = False then Exit;
		Dec(WavePlayer.FOutCount);
		if WavePlayer.FCloseInvoked = False then
			WavePlayer.SendBuffer
		else
		if WavePlayer.FOutCount = 0 then
		begin
			WavePlayer.FError := WaveOutClose(WavePlayer.FHWave);
			WavePlayer.FHWave := 0;
			WavePlayer.MMError('Close');
			if WavePlayer.FError = 0 then
				WavePlayer.FActive := False;
		end;
	end;
end;

procedure MMInDone(
	wi: HWAVEIN;
	Msg: UINT;
	Instance: U4;
	Param1: U4;
	Param2: U4); stdcall;
var
	Header: PWaveHdr;
{	NewSize: SG;
	WaveSample: PWaveSample;}

	BufferIn: PWaveSample;
	WaveRecorder: TWaveRecorder;
begin
	if Msg = WIM_DATA then
	begin
		WaveRecorder := TWaveRecorder(Instance);

		Dec(WaveRecorder.FOutCount);
		Header := PWaveHdr(Param1);
		BufferIn := Pointer(Header^.lpData);

{		if Volume <> 100 then
		begin
			WaveSample := BufferIn;
			while UG(WaveSample) < UG(BufferIn) + Header^.dwBytesRecorded do
			begin
				case WaveFormat^.wBitsPerSample of
				16:
				begin
					WaveSample.W := Range(-32768, Volume * SG(WaveSample.W) div 100, 32767);
					Inc(SG(WaveSample), 2);
				end
				else
				begin
					WaveSample.B := ((WaveSample.B - Zero1) * Volume div 100) + Zero1;
					Inc(SG(WaveSample), 1);
				end;
				end;
			end;
		end; }
		if Assigned(WaveRecorder.FOnReciveBuffrer) then
			WaveRecorder.FOnReciveBuffrer(WaveRecorder, BufferIn);

		WaveRecorder.FError := waveInUnPrepareHeader(WaveRecorder.FHWave, Header, SizeOf(TWaveHdr));
		WaveRecorder.MMError('UnPrepare');
		FreeMem(BufferIn);
		Dispose(Header);

		if WaveRecorder.FActive = False then Exit;
		Dec(WaveRecorder.FOutCount);
		if WaveRecorder.FCloseInvoked = False then
			WaveRecorder.SendBuffer
		else
		if WaveRecorder.FOutCount = 0 then
		begin
			WaveRecorder.FError := WaveInClose(WaveRecorder.FHWave);
			WaveRecorder.FHWave := 0;
			WaveRecorder.MMError('Close');
			if WaveRecorder.FError = 0 then
				WaveRecorder.FActive := False;
		end;
	end;
end;

procedure TWaveCommon.Open;
const BufferCount = 8;
var i: SG;
begin
	if FActive then Close;
	FActive := False;
	FCloseInvoked := False;
	FWaveFormat.WFormatTag := WAVE_FORMAT_PCM; // PCM format - the only option
	FWaveFormat.nChannels := Channels;
	FWaveFormat.nSamplesPerSec := Frequency;
	FWaveFormat.wBitsPerSample := Bits;
	FWaveFormat.nAvgBytesPerSec := FWaveFormat.wBitsPerSample * FWaveFormat.nSamplesPerSec div 8;
	FWaveFormat.nBlockAlign := (FWaveFormat.wBitsPerSample * FWaveFormat.nChannels) div 8;
	FWaveFormat.cbSize := SizeOf(FWaveFormat);

	if Self is TWavePlayer then
		FError := waveOutOpen(nil, 0, @FWaveFormat, 0, 0, WAVE_FORMAT_QUERY)
	else
		FError := waveInOpen(nil, 0, @FWaveFormat, 0, 0, WAVE_FORMAT_QUERY);
	MMError('WaveOpen');
	if FError <> 0 then Exit;

	if Self is TWavePlayer then
		FError := waveOutOpen(@FHWave, 0, @FWaveFormat, UG(@MMOutDone), UG(Self), CALLBACK_FUNCTION)
	else
		FError := waveInOpen(@FHWave, 0, @FWaveFormat, UG(@MMInDone), UG(Self), CALLBACK_FUNCTION);
	MMError('WaveOpen');
	if FError <> 0 then
	begin
		FHWave := 0;
		Exit;
	end;

	FOutCount := 0;

	FBufferOutSamples := RoundDiv(FWaveFormat.nSamplesPerSec * BufferTime, BufferCount * 1000);
	FBufferOutSize := GetBufferSize(FWaveFormat.wBitsPerSample, FWaveFormat.nChannels, FBufferOutSamples);
{	BufferOutSize := BufferSize;
	BufferOutSamples := GetBufferSample(WaveFormat.wBitsPerSample, WaveFormat.nChannels, BufferOutSize);}

	if not (Self is TWavePlayer) then
	begin
		FError := waveInStart(FHWave);
		MMError('Start');
	end;
	FActive := True;
	for i := 0 to BufferCount - 1 do
		SendBuffer;
end;

procedure TWaveCommon.Close;
begin
	if FActive then
	begin
		FCloseInvoked := True;
{	if Self is TWavePlayer then
		waveOutBreakLoop(FHWave);}
		while FOutCount > 0 do
			Sleep(LoopSleepTime);
		FActive := False;
		FCloseInvoked := False;
	end;
end;

procedure TWaveCommon.Pause;
begin
	if Self is TWavePlayer then
	begin
		FError := waveOutPause(FHWave);
		MMError('Pause');
	end
	else
	begin
		MMError('Pause not supported by WinAPI');
	end;
end;

procedure TWaveCommon.Resume;
begin
	if Self is TWavePlayer then
	begin
		FError := waveOutRestart(FHWave);
		MMError('Restart');
	end
	else
	begin
		FError := waveInReset(FHWave);
		MMError('Reset');
		FError := waveInStart(FHWave);
		MMError('Start');
	end;
end;

procedure TWaveCommon.Stop;
begin
	Close;
	Open;
{
	CloseInvoked := True;
	if Self is TWavePlayer then
		FError := waveOutReset(HWave)
	else
		FError := waveInReset(HWave);
	MMError('Reset');
}
end;

procedure TWaveCommon.SendBuffer;
var
	Header: PWaveHdr;
	Buffer: PWaveSample;
begin
	GetMem(Buffer, FBufferOutSize);
	if Self is TWavePlayer then
		FillBuffer(Buffer);

	Header := New(PWaveHdr);
	Header^.lpData := Pointer(Buffer);
	Header^.dwBufferLength := FBufferOutSize;
	Header^.dwBytesRecorded := 0;
	Header^.dwUser := 0;
	Header^.dwFlags := 0;
	Header^.dwLoops := 0;
	Header^.lpNext := nil;
	Header^.reserved := 0;

	if Self is TWavePlayer then
		FError := waveOutPrepareHeader(FHWave, Header, SizeOf(TWaveHdr))
	else
		FError := waveInPrepareHeader(FHWave, Header, SizeOf(TWaveHdr));
	MMError('Prepare');
	if FError <> 0 then Exit;

	if Self is TWavePlayer then
		FError := waveOutWrite(FHWave, Header, SizeOf(TWaveHdr))
	else
		FError := waveInAddBuffer(FHWave, Header, SizeOf(TWaveHdr));
	MMError('AddBuffer');
	if FError <> 0 then Exit;

	Inc(FOutCount);
end;

// TWavePlayer

const
	AngleCount = 4096;
{type
	PSinTable = ^TSinTable;
	TSinTable = array[0..AngleCount - 1] of TAngle;}
var
	Sins: PSinTable;

procedure TWavePlayer.WriteValue(Value: SG; var Buffer: PWaveSample);
begin
	case FWaveFormat.wBitsPerSample of
	8:
	begin
		Value := (Value + 32768) div 256;
		if Value < 0 then
			Value := 0
		else if Value > 255 then
			Value := 255;
		Buffer.B := Value;
		Inc(PByte(Buffer), 1);
	end;
	16:
	begin
		if Value < -32768 then
			Value := -32768
		else if Value > 32767 then
			Value := 32767;
		Buffer.W := Value;
		Inc(PByte(Buffer), 2);
	end;
	end;
end;

procedure TWavePlayer.FillBuffer(Buffer: PWaveSample);
var
	i, j: SG;
	SampleP: UG;
	ValueL, ValueR, ValueLS, ValueRS: SG;
	PlayItem: PPlayItem;
begin
	if PlayItems <> nil then
	begin
		j := 0;
		while j < SG(PlayItems.Count) do
		begin
			if j >= MaxItems then Break;
			PlayItem := PlayItems.Get(j);
			case PlayItem.PlayAs of
			paWave:
			begin
				if PlayItem.Wave <> nil then
					PlayItem.Offset := {PlayItem.Wave.Format.Channels *} U8(PlayItem.Speed) * UG(PlayItem.Wave.Format.SampleRate) div UG(FWaveFormat.nSamplesPerSec);
			end
			else
			begin
				PlayItem.Offset := SpeedDiv;
			end
			end;
			Inc(j);
		end;

		// 16-bit Mixer
		FillChar(Buffer^, FBufferOutSize, 0);
		i := 0;
		while i < FBufferOutSamples * FWaveFormat.nChannels do
		begin
			ValueLS := 0;
			ValueRS := 0;
			j := 0;
			ValueL := 0;
			ValueR := 0;
			while j < SG(PlayItems.Count) do
			begin
				if j >= MaxItems then Break;
				PlayItem := PlayItems.Get(j);
				if PlayItem.StartDelay > 0 then
				begin
					Dec(PlayItem.StartDelay);
					Inc(j);
					Continue;
				end;
				SampleP := PlayItem.SamplePos div SpeedDiv;
				if SampleP >= PlayItem.SampleCount then
				begin
					PlayItems.Delete(j);
					Continue;
				end
				else
				begin
					Inc(j);
					case PlayItem.PlayAs of
					paNoise:
					begin
						ValueL := Random2(32767);
						ValueR := Random2(32767);
					end;
					paTone:
					begin
						ValueL := Sins[((U8(AngleCount) * U8(SampleP) * U8(PlayItem.Speed) div FWaveFormat.nSamplesPerSec)) and (AngleCount - 1)];
						ValueR := ValueL;
					end;
					paSilent:
					begin
						ValueL := 0;
						ValueR := 0;
					end
					else // paWave
					begin
{						if PlayItem.Wave.Format.Channels = 2 then
							SampleP := SampleP and $fffffffe;}
						case PlayItem.Wave.Format.BitsPerSample of
						16: ValueL := PlayItem.Wave.Sample(SampleP, 0);
						else ValueL := 256 * (SG(PlayItem.Wave.Sample(SampleP, 0)) - 128);
						end;
						if PlayItem.Wave.Format.Channels = 2 then
						begin
							case PlayItem.Wave.Format.BitsPerSample of
							16: ValueR := PlayItem.Wave.Sample(SampleP, 1);
							else ValueR := 256 * (SG(PlayItem.Wave.Sample(SampleP, 1)) - 128);
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

				if (PlayItem.Volume.Left <> MaxVolume) then
					ValueL := ValueL * PlayItem.Volume.Left div MaxVolume;
				if (PlayItem.Volume.Right <> MaxVolume) then
					ValueR := ValueR * PlayItem.Volume.Right div MaxVolume;
				Inc(ValueLS, ValueL);
				Inc(ValueRS, ValueR);
			end;

			if FWaveFormat.nChannels = 2 then
			begin
				WriteValue(ValueLS * Volume.Left div MaxVolume, Buffer);
				WriteValue(ValueRS * Volume.Right div MaxVolume, Buffer);
				Inc(i, 2);
			end
			else
			begin
				WriteValue((Volume.Left + Volume.Right) * (ValueLS + ValueRS) div (2 * 2 * MaxVolume), Buffer);
				Inc(i);
			end;
		end;
	end;
end;

procedure TWavePlayer.Play(const Wave: TWave; const Volume: TVolume; const Speed: UG = SpeedDiv; const StartDelay: SG = 0);
var
	PlayItem: PPlayItem;
begin
{	PlayItem := PlayItems.GetFirst;
	while PlayItem <> nil do
	begin
		if PlayItem.Wave = Wave then Exit;
		PlayItems.Next(Pointer(PlayItem));
	end;}
	PlayItem := PlayItems.Add;
	PlayItem.PlayAs := paWave;
	PlayItem.Wave := Wave;
	PlayItem.SampleCount := Wave.SampleCount;
	PlayItem.SamplePos := 0;
	PlayItem.Volume := Volume;
	PlayItem.Speed := Speed;
	PlayItem.StartDelay := StartDelay;
end;

procedure TWavePlayer.Beep;
begin
	Tone(1000, 1000);
end;

procedure TWavePlayer.Silent(const Tim: UG; const StartDelay: SG = 0);
var
	PlayItem: PPlayItem;
begin
	PlayItem := PlayItems.Add;
	PlayItem.PlayAs := paSilent;
	PlayItem.Wave := nil;
	PlayItem.SampleCount := FWaveFormat.nSamplesPerSec * Tim div 1000;
	PlayItem.SamplePos := 0;
	PlayItem.Volume := Volume;
	PlayItem.Speed := SpeedDiv;
	PlayItem.StartDelay := StartDelay;
end;

procedure TWavePlayer.Tone(const Frequency, Tim: UG);
var
	PlayItem: PPlayItem;
	T: U4;
begin
	if Frequency > 0 then
	begin
		T := FWaveFormat.nSamplesPerSec * Tim div 1000;
		if T > 0 then
		begin
			PlayItem := PlayItems.Add;
			PlayItem.PlayAs := paTone;
			if Sins = nil then
			begin
				GetMem(Sins, SizeOf(TAngle) * AngleCount);
				FillSinTable(Sins, AngleCount, SinDiv);
			end;
			PlayItem.Wave := nil;
			PlayItem.SampleCount := T;
			PlayItem.SamplePos := 0;
			PlayItem.Volume := Volume;
			PlayItem.Speed := Frequency;
		end;
	end;
end;

procedure TWavePlayer.Noise(const Tim: UG);
var
	PlayItem: PPlayItem;
begin
	PlayItem := PlayItems.Add;
	PlayItem.PlayAs := paNoise;
	PlayItem.Wave := nil;
	PlayItem.SampleCount := FWaveFormat.nSamplesPerSec * Tim div 1000;
	PlayItem.SamplePos := 0;
	PlayItem.Volume := Volume;
	PlayItem.Speed := SpeedDiv;
end;

constructor TWavePlayer.Create;
begin
	inherited Create;
	PlayItems := TData.Create(True);
	PlayItems.ItemSize := SizeOf(TPlayItem);
	MaxItems := 16;
end;

destructor TWavePlayer.Destroy;
begin
	FreeAndNil(PlayItems);
	inherited Destroy;
end;

// Midi
var
	OpenParm: TMCI_Open_Parms;
	PlayParm: TMCI_Play_Parms;
	GenParm: TMCI_Generic_Parms;
	SeekParm: TMCI_Seek_Parms;

function MCIErrorToStr(const ErrorCode: U4): string;
begin
	SetLength(Result, 255);
	if mciGetErrorString(ErrorCode, PChar(Result), 255) then
		Result := PChar(Result)
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' Unknown error';
end;

function MCIError(const ErrorCode: SG): Boolean;
begin
	if ErrorCode <> 0 then
	begin
		Result := ErrorRetry(MCIErrorToStr(ErrorCode));
	end
	else
		Result := False;
end;

function MidiMCICallBack: Boolean;
begin
	Result := MidiPlaying;
end;

procedure MidiMCIOpen(FileName: TFileName);
var
	FFlags: U4;
	FError: SG;
	F: file;
	ErrorCode: SG;
begin
	MidiPlaying := False;
	while True do
	begin
		AssignFile(F, FileName);
		FileMode := 0; Reset(F, 1);
		ErrorCode := IOResult;
		CloseFile(F);
		IOResult;
		if ErrorCode <> 0 then
		begin
			if IOErrorRetry(FileName, ErrorCode) then Continue;
			Exit;
		end;
		Break;
	end;
	while True do
	begin
		FillChar(OpenParm, SizeOf(TMCI_Open_Parms), 0);

		OpenParm.dwCallback := 0;
		OpenParm.lpstrDeviceType := 'WaveAudio';
		OpenParm.lpstrElementName := PChar(FileName);
		FFlags := MCI_OPEN_ELEMENT or MCI_NOTIFY;

		FError := mciSendCommand(0, mci_Open, FFlags, U4(@OpenParm));
		MidiOpened := FError = 0;
		if MCIError(FError) then Continue;
		Break;
	end;
end;

procedure MidiMCISeek(const SeekTo: U4);
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	SeekParm.dwTo := SeekTo;
	SeekParm.dwCallback := 0;
	FFlags := 0;
	FFlags := FFlags or mci_To;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Seek, FFlags, U4(@SeekParm));
	if MCIError(FError) then goto LRetrySend;
end;

function MidiMCIGetPos: U4;
label LRetrySeek;
var
	FFlags: U4;
	FError: SG;
	StatusParm: TMCI_Status_Parms;
begin
	Result := 0;
	if MidiOpened = False then Exit;
	LRetrySeek:
	StatusParm.dwItem := mci_Status_Position;
	StatusParm.dwTrack := 0;
	StatusParm.dwReturn := 0;
	FFlags := mci_Wait or mci_Status_Item;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Status, FFlags, U4(@StatusParm));
	if MCIError(FError) then goto LRetrySeek;
	Result := StatusParm.dwReturn;
end;

procedure MidiMCIPause;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Pause, FFlags, U4(@GenParm));
	if MCIError(FError) then goto LRetrySend;
	MidiPlaying := False;
end;

procedure MidiMCIStop;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	MidiPlaying := False;
	FFlags := 0;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Stop, FFlags, U4(@GenParm));
	if MCIError(FError) then goto LRetrySend;
end;

procedure MidiMCIResume;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	GenParm.dwCallback := MidiHandle;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Resume, FFlags, U4(@GenParm));
	if MCIError(FError) then goto LRetrySend;
	MidiPlaying := False;
	MidiMCIPlay; // Need for Callback
end;

procedure MidiMCIPlay;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	if MidiPlaying = True then Exit;
	LRetrySend:
	MidiPlaying := True;
	FFlags := mci_Notify;
	PlayParm.dwCallback := MidiHandle;

	FError := mciSendCommand(OpenParm.wDeviceID, mci_Play, FFlags, U4(@PlayParm));
	if MCIError(FError) then goto LRetrySend;
end;

procedure MidiMCIClose;
label LRetrySend;
var
	FFlags: U4;
	FError: SG;
begin
	if MidiOpened = False then Exit;
	LRetrySend:
	FFlags := 0;
	PlayParm.dwCallback := OpenParm.dwCallback;
	GenParm.dwCallback := 0;
	FError := mciSendCommand(OpenParm.wDeviceID, mci_Close, FFlags, U4(@GenParm));
	if FError = 0 then
	begin
		MidiOpened := False;
		MidiPlaying := False;
	end;
	if MCIError(FError) then goto LRetrySend;
end;

procedure TWavePlayer.Stop;
begin
	Close;
	PlayItems.Clear;
	Open;
end;

initialization

finalization
	if Sins <> nil then
	begin
		FreeMem(Sins);
		Sins := nil;
	end;
end.
