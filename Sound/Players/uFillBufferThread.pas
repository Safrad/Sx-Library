unit uFillBufferThread;

interface

uses
  uTypes,
  MMSystem,
  uSoundMixer,
  uWavePlayer,
  uSxThread;

type
  TFillBufferThread = class(TSxThread)
  private
    FHeaders: array of TWaveHdr;
    FBufferCount: SG;
    FBufferSize: SG;
    FMixer: TSoundMixer;
    FWavePlayer: TWavePlayer;
    procedure SetBufferCount(const Value: SG);
    procedure FillHeader(var WaveHdr: TWaveHdr);
    procedure UnfillHeader(var WaveHdr: TWaveHdr);
    procedure SetBufferSize(const Value: SG);
    procedure SetMixer(const Value: TSoundMixer);
    procedure SetWavePlayer(const Value: TWavePlayer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

		property BufferSize: SG read FBufferSize write SetBufferSize;
    property BufferCount: SG read FBufferCount write SetBufferCount;
    property Mixer: TSoundMixer read FMixer write SetMixer;
    property WavePlayer: TWavePlayer read FWavePlayer write SetWavePlayer;
  end;


implementation

uses
  uMath,
  uMainLog,
  uFiles,
  uOutputFormat,
  Windows,
  Classes,
  SysUtils, uWaveCommon;

function GetBufferSize(wBitsPerSample, nChannels, BufferOutSamples: SG): SG;
begin
	Result := BufferOutSamples * ((wBitsPerSample * nChannels + 7) div 8);
end;

{ TFillBufferThread }

procedure TFillBufferThread.Execute;
const
  BufferTime = 33; // ms
var
  i: SG;
  FillCount: SG;
  PCMWaveFormat: TPCMWaveFormat;
begin
  inherited;

  PCMWaveFormat.wBitsPerSample := WavePlayer.WaveFormat.Format.wBitsPerSample;
  PCMWaveFormat.wf.wFormatTag := WAVE_FORMAT_PCM;
  PCMWaveFormat.wf.nChannels := WavePlayer.WaveFormat.Format.nChannels;
  PCMWaveFormat.wf.nSamplesPerSec := WavePlayer.WaveFormat.Format.nSamplesPerSec;
  PCMWaveFormat.wf.nAvgBytesPerSec := WavePlayer.WaveFormat.Format.nAvgBytesPerSec;
  PCMWaveFormat.wf.nBlockAlign := WavePlayer.WaveFormat.Format.nBlockAlign;

  // 1 * x gap
  // 2 * >=26 ok
  // 3 * >=15 ok
  // 4 * >=7 ok, fastest response 28 ms
  // 5 * >=6 ok
  // 6 * >=5 ok
  // 30 * >=2 ok

  FMixer.BufferSingleChannelSampleCount := RoundDiv(FWavePlayer.WaveFormat.Format.nSamplesPerSec * BufferTime, 1000);
  FBufferSize := GetBufferSize(FWavePlayer.WaveFormat.Format.wBitsPerSample, FWavePlayer.WaveFormat.Format.nChannels, FMixer.BufferSingleChannelSampleCount);
  BufferCount := 4; // minimum is 2

  while not Terminated do
  begin
    FillCount := 0;
    for i := 0 to FBufferCount - 1 do
    begin
      if (FHeaders[i].dwFlags = WHDR_DONE or WHDR_PREPARED) then
        WavePlayer.UnprepareHeader(@FHeaders[i]);

      if FHeaders[i].dwFlags in [WHDR_DONE, 0] then
      begin
        Mixer.FillBuffer(Pointer(FHeaders[i].lpData));

        WavePlayer.PrepareHeader(@FHeaders[i]);

        WavePlayer.WriteBuffer(@FHeaders[i]);
        MainLog.Add('Write buffer ' + IntToStr(i) + ' - ' + BToStr(FHeaders[i].dwBufferLength), mlDebug);
        Inc(FillCount);
      end;
    end;
    if FillCount > 1 then
        MainLog.Add('Write ' + IntToStr(FillCount) + ' buffers.', mlWarning);

    if WavePlayer.BufferOutCount >= BufferCount then
      Sleep(BufferTime div 4);
    if (WavePlayer.BufferOutCount = 0) and (BufferCount < 10) then
    begin
        MainLog.Add('Audio gap', mlError);
        BufferCount := BufferCount + 1;
    end;
  end;
  BufferCount := 0; // Free memory
end;

procedure TFillBufferThread.FillHeader(var WaveHdr: TWaveHdr);
begin
  WaveHdr := Default(TWaveHdr);
  GetMem(WaveHdr.lpData, FBufferSize);
  MainLog.Add('GetMem ' + IntToStr(Integer(WaveHdr.lpData)), mlDebug);
  WaveHdr.dwBufferLength := FBufferSize;
end;

procedure TFillBufferThread.SetBufferCount(const Value: SG);
var
  i: SG;
begin
  if FBufferCount <> Value then
  begin
    if Value < FBufferCount then
    begin
      for i := Value to FBufferCount - 1 do
      begin
        MainLog.Add('UnfillHeader ' + IntToStr(i), mlDebug);
        UnfillHeader(FHeaders[i]);
      end;
    end;

    SetLength(FHeaders, Value);

    if Value > FBufferCount then
    begin
      for i := FBufferCount to Value - 1 do
      begin
        MainLog.Add('FillHeader ' + IntToStr(i), mlDebug);
        FillHeader(FHeaders[i]);
      end;
    end;

    FBufferCount := Value;
  end;
end;

procedure TFillBufferThread.UnfillHeader(var WaveHdr: TWaveHdr);
begin
  MainLog.Add('FreeMem ' + IntToStr(Integer(WaveHdr.lpData)), mlDebug);
  FreeMem(WaveHdr.lpData);
  WaveHdr := Default(TWaveHdr);
end;

procedure TFillBufferThread.SetBufferSize(const Value: SG);
begin
  FBufferSize := Value;
end;

procedure TFillBufferThread.SetMixer(const Value: TSoundMixer);
begin
  FMixer := Value;
end;

procedure TFillBufferThread.SetWavePlayer(const Value: TWavePlayer);
begin
  FWavePlayer := Value;
end;

constructor TFillBufferThread.Create;
begin
  inherited;

  Priority := tpTimeCritical;
end;

destructor TFillBufferThread.Destroy;
begin

  inherited;
end;

end.

