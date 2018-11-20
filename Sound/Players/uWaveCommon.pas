unit uWaveCommon;

interface

uses
  uTypes,
  uWave,
  uChannels,
  Windows,
  MMSystem;

const
  WAVE_FORMAT_EXTENSIBLE = $FFFE;
  KSDATAFORMAT_SUBTYPE_PCM = '{00000001-0000-0010-8000-00aa00389b71}';
  KSDATAFORMAT_SUBTYPE_IEEE_FLOAT = '{00000003-0000-0010-8000-00AA00389B71}';

type
  TSamples = packed record
    case word of
      0: (wValidBitsPerSample: word); // bits of precision
      1: (wSamplesPerBlock: word); // valid if wBitsPerSample = 0
      2: (wReserved: word); // If neither applies, set to zero.
  end;

  PWAVEFORMATEXTENSIBLE = ^tWAVEFORMATEXTENSIBLE;
  tWAVEFORMATEXTENSIBLE = packed record
    Format: tWAVEFORMATEX;
    Samples: TSamples;
    dwChannelMask: longword; // which channels are present in stream
    SubFormat: TGUID;
  end;

	TWaveCommon = class
	private
		FHWave: HWave;
		FWaveFormat: TWaveFormatExtensible;

		FActive: Boolean;
		FCloseInvoked: Boolean;

		FBufferOutCount: S4;
    FFrequency: SG;
    FBits: SG;
    FChannels: TChannels;

    procedure MMError(const FError: MMResult; const AMessagePrefix: string);
    procedure SetBits(const Value: SG);
    procedure SetFrequency(const Value: SG);
    procedure SetChannels(const Value: TChannels);
  protected
    IsWavePlayer: BG;
		procedure FillBuffer(Buffer: PWaveSample); virtual; abstract;
	public
		property WaveFormat: TWaveFormatExtensible read FWaveFormat;

		function WaveErrorText(const ErrorCode: U4): string;

		constructor Create;
		destructor Destroy; override;

		procedure Open;
		procedure Close;
    procedure Restart;

		procedure Pause;
		procedure Resume;
		procedure Stop; virtual;
    procedure PrepareHeader(const Header: PWaveHdr);
    procedure UnprepareHeader(const Header: PWaveHdr);
    procedure WriteBuffer(const Header: PWaveHdr);

		property BufferOutCount: S4 read FBufferOutCount;
		property Active: Boolean read FActive;

		property Bits: SG read FBits write SetBits;
		property Frequency: SG read FFrequency write SetFrequency;
		property Channels: TChannels read FChannels write SetChannels;
	end;

implementation

uses
  SysUtils,
  uMsg,
  uMath,
  uOutputFormat;

(*	Header: PWaveHdr;
	BufferOut: PWaveSample;
		Header := PWaveHdr(Param1);
		BufferOut := Pointer(Header^.lpData);
//		BufferOutSize := Header^.dwBufferLength;
{		GetMem(Buffer, BufferSize);
		Move(BufferOut^, Buffer^, BufferSize);}

		WavePlayer.FError := waveOutUnPrepareHeader(WavePlayer.FHWave, Header, SizeOf(TWaveHdr));
		WavePlayer.MMError('UnPrepare');
		FreeMem(BufferOut); // BufferOut := nil;
		Dispose(Header);

		if WavePlayer.FActive = False then Exit;
		Dec(WavePlayer.FBufferOutCount);
		if WavePlayer.FCloseInvoked = False then
			WavePlayer.SendBuffer
		else if WavePlayer.FBufferOutCount = 0 then
		begin
			WavePlayer.FError := WaveOutClose(WavePlayer.FHWave);
			WavePlayer.FHWave := 0;
			WavePlayer.MMError('Close');
			if WavePlayer.FError = 0 then
				WavePlayer.FActive := False;
		end;
	end;
*)
// Called from different thread
procedure MMOutDone(
	wo: HWAVEOUT;
	Msg: UINT;
	Instance: U4;
	Param1: U4;
	Param2: U4); stdcall;
var
	WaveCommon: TWaveCommon;
begin
	if Msg = WOM_DONE then
	begin
		WaveCommon := TWaveCommon(Instance);
    InterlockedDecrement(WaveCommon.FBufferOutCount);
  end;
end;

// Called from different thread
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
	WaveCommon: TWaveCommon;
begin
(*	if Msg = WIM_DATA then
	begin
		WaveCommon := TWaveCommon(Instance);

		Dec(WaveRecorder.FBufferOutCount);
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
		Dec(WaveRecorder.FBufferOutCount);
		if WaveRecorder.FCloseInvoked = False then
			WaveRecorder.SendBuffer
		else
		if WaveRecorder.FBufferOutCount = 0 then
		begin
			WaveRecorder.FError := WaveInClose(WaveRecorder.FHWave);
			WaveRecorder.FHWave := 0;
			WaveRecorder.MMError('Close');
			if WaveRecorder.FError = 0 then
				WaveRecorder.FActive := False;
		end;
	end; TODO *)
end;


{ TWaveCommon }

constructor TWaveCommon.Create;
begin
	inherited Create;
	FHWave := 0;
	FActive := False;
	FCloseInvoked := False;

	Bits := BitsPerByte * SizeOf(F4);
	Frequency := 44100;
end;

destructor TWaveCommon.Destroy;
begin
	if FActive then
    Close;
	FHWave := 0;

	inherited Destroy;
end;

function TWaveCommon.WaveErrorText(const ErrorCode: U4): string;
var B: BG;
begin
	SetLength(Result, MAXERRORLENGTH);
	if IsWavePlayer then
		B:= waveOutGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR
	else
		B:= waveInGetErrorText(ErrorCode, @Result[1], MAXERRORLENGTH) = MMSYSERR_NOERROR;
	if B then
	begin
		if Result <> '' then
			if Result[Length(Result)] = #0 then SetLength(Result, Length(Result) - 1);
	end
	else
		Result := 'MMSYSTEM' + NToS(ErrorCode) + ' ' + 'Unknown error';
end;

procedure TWaveCommon.MMError(const FError: MMResult; const AMessagePrefix: string);
begin
	if FError <> 0 then
  begin
    ErrorMsg(ClassName + ': ' + AMessagePrefix + ' (' + WaveErrorText(FError) + ')');
  end;
end;

procedure TWaveCommon.Open;
var
  FError: MMResult;
begin
	if FActive then Close;
	FActive := False;
	FCloseInvoked := False;

  FillChar(FWaveFormat, SizeOf(FWaveFormat), 0);

	FWaveFormat.Format.WFormatTag := WAVE_FORMAT_EXTENSIBLE;
	FWaveFormat.Format.nChannels := Channels.Count;
	FWaveFormat.Format.nSamplesPerSec := Frequency;
	FWaveFormat.Format.wBitsPerSample := Bits;
	FWaveFormat.Format.nBlockAlign := (FWaveFormat.Format.wBitsPerSample shr 3) * FWaveFormat.Format.nChannels;
	FWaveFormat.Format.nAvgBytesPerSec := FWaveFormat.Format.nSamplesPerSec * FWaveFormat.Format.nBlockAlign;
  FWaveFormat.Format.cbSize := SizeOf(TWaveFormatExtensible) - SizeOf(TWaveFormatEx);

  FWaveFormat.Samples.wValidBitsPerSample := Bits;

  FWaveFormat.dwChannelMask := Channels.Mask;
  FWaveFormat.SubFormat := StringToGUID(KSDATAFORMAT_SUBTYPE_IEEE_FLOAT);

	if IsWavePlayer then
		FError := waveOutOpen(nil, WAVE_MAPPER, @FWaveFormat.Format, 0, 0, WAVE_FORMAT_QUERY)
	else
		FError := waveInOpen(nil, WAVE_MAPPER, @FWaveFormat.Format, 0, 0, WAVE_FORMAT_QUERY);
	MMError(FError, 'WaveOpen');
	if FError <> 0 then Exit;

	if IsWavePlayer then
		FError := waveOutOpen(@FHWave, WAVE_MAPPER, @FWaveFormat.Format, UG(@MMOutDone), UG(Self), CALLBACK_FUNCTION)
	else
		FError := waveInOpen(@FHWave, WAVE_MAPPER, @FWaveFormat.Format, UG(@MMInDone), UG(Self), CALLBACK_FUNCTION);
	MMError(FError, 'WaveOpen');
	if FError <> 0 then
	begin
		FHWave := 0;
		Exit;
	end;

	FBufferOutCount := 0;


	if not IsWavePlayer then
	begin
		FError := waveInStart(FHWave);
		MMError(FError, 'Start');
	end;

	FActive := True;
end;

procedure TWaveCommon.Close;
begin
	if FActive then
	begin
		FCloseInvoked := True;
{		while FBufferOutCount > 0 do
			Sleep(LoopSleepTime);}
		FActive := False;
		FCloseInvoked := False;
	end;
end;

procedure TWaveCommon.Pause;
var
  FError: MMResult;
begin
	if IsWavePlayer then
	begin
		FError := waveOutPause(FHWave);
		MMError(FError, 'Pause');
	end
	else
	begin
		MMError(0, 'Pause not supported by WinAPI');
	end;
end;

procedure TWaveCommon.Resume;
var
  FError: MMResult;
begin
	if IsWavePlayer then
	begin
		FError := waveOutRestart(FHWave);
		MMError(FError, 'Restart');
	end
	else
	begin
		FError := waveInReset(FHWave);
		MMError(FError, 'Reset');
		FError := waveInStart(FHWave);
		MMError(FError, 'Start');
	end;
end;

procedure TWaveCommon.Stop;
begin
	Close;
	Open;
{ TODO Test
	CloseInvoked := True;
	if Self is TWavePlayer then
		FError := waveOutReset(HWave)
	else
		FError := waveInReset(HWave);
	MMError('Reset');
}
end;

procedure TWaveCommon.WriteBuffer(const Header: PWaveHdr);
var
  FError: MMResult;
begin
  if IsWavePlayer then
    FError := waveOutWrite(FHWave, Header, SizeOf(TWaveHdr))
  else
    FError := waveInAddBuffer(FHWave, Header, SizeOf(TWaveHdr));
  MMError(FError, 'AddBuffer');
  if FError <> 0 then Exit;

  InterlockedIncrement(FBufferOutCount);
end;

// Set Header.dwFlags to WHDR_PREPARED
procedure TWaveCommon.PrepareHeader(const Header: PWaveHdr);
var
  FError: MMResult;
begin
  if IsWavePlayer then
    FError := waveOutPrepareHeader(FHWave, Header, SizeOf(TWaveHdr))
  else
    FError := waveInPrepareHeader(FHWave, Header, SizeOf(TWaveHdr));
  MMError(FError, 'Prepare');
  if FError <> 0 then Exit;
  Assert(Header.dwFlags = WHDR_PREPARED);
end;

// Set Header.dwFlags to WHDR_DONE
procedure TWaveCommon.UnprepareHeader(const Header: PWaveHdr);
var
  FError: MMResult;
begin
	FError := waveOutUnPrepareHeader(FHWave, Header, SizeOf(TWaveHdr));
	MMError(FError, 'UnPrepare');
end;

procedure TWaveCommon.SetBits(const Value: SG);
begin
  FBits := Value;
  if FActive then
  begin
    Restart;
  end;
end;

procedure TWaveCommon.SetChannels(const Value: TChannels);
begin
  FChannels := Value;
  if FActive then
  begin
    Restart;
  end;
end;

procedure TWaveCommon.SetFrequency(const Value: SG);
begin
  FFrequency := Value;
  if FActive then
  begin
    Restart;
  end;
end;

procedure TWaveCommon.Restart;
begin
  Close;
  Open;
end;

end.
