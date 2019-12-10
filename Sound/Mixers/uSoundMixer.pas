unit uSoundMixer;

interface

uses
  uTypes,
  uSoundItem,
  uISoundItem,
  uChannels;

type
  PSamplesF4 = ^TSamplesF4;
  TSamplesF4 = array[0..512 * MB - 2] of TSampleF4;

  TSampleS2 = S2;

  PSamplesS2 = ^TSamplesS2;
  TSamplesS2 = array[0..1024 * MB - 2] of TSampleS2;

  TSoundMixer = class
  private
    FSoundItems: TSoundItems;
    FBufferSamplesF4: PSamplesF4;
    FBufferSingleChannelSampleCount: SG;
    FSampleRate: SG;
    FMaxVolume: FG;
    FActualVolume: FG;
    FChannels: TChannels;

    procedure SetSoundItems(const Value: TSoundItems);
    procedure SetBufferSingleChannelSampleCount(const Value: SG);
    function GetBufferSize: SG;
    function GetBufferAllChannelsSampleCount: SG;
    procedure SetSampleRate(const Value: SG);
    procedure SetChannels(const Value: TChannels);
  public
    destructor Destroy; override;

    procedure Add(const ASoundItem: TSoundItem);
    procedure FillBuffer(const ASamplesF4: PSamplesF4); virtual; abstract;
    procedure ConvertF4ToS2(const ASamplesF4: PSamplesF4; const ASamplesS2: PSamplesS2);
    procedure Normalize(const ASamplesF4: PSamplesF4;
      const ATargetF4: PSamplesF4);

    property BufferSamplesF4: PSamplesF4 read FBufferSamplesF4;
    property BufferSingleChannelSampleCount: SG read FBufferSingleChannelSampleCount write SetBufferSingleChannelSampleCount;
    property BufferSize: SG read GetBufferSize;
    property SampleRate: SG read FSampleRate write SetSampleRate;
    property Channels: TChannels read FChannels write SetChannels;
    property SoundItems: TSoundItems read FSoundItems write SetSoundItems;

    property MaxVolume: FG read FMaxVolume;
    property ActualVolume: FG read FActualVolume;
  end;

implementation

uses
  uMainLog,
  SysUtils,
  Math;

{ TSoundMixer }

procedure TSoundMixer.Add(const ASoundItem: TSoundItem);
begin
  SetLength(FSoundItems, Length(FSoundItems) + 1);
  FSoundItems[Length(FSoundItems) - 1] := ASoundItem;
  MainLog.Add('AddSound ' + ASoundItem.ClassName, mlDebug);
  MainLog.Add('Number of sounds: ' + IntToStr(Length(FSoundItems)), mlDebug);
end;

procedure TSoundMixer.Normalize(const ASamplesF4, ATargetF4: PSamplesF4);
var
  i: SG;
  Ratio, MaxValue, MinValue: F4;
begin
  MinValue := 0;
  MaxValue := 0;
  for i := 0 to GetBufferAllChannelsSampleCount - 1 do
  begin
    if ASamplesF4[i] > MaxValue then
    begin
      MaxValue := ASamplesF4[i];
    end;
    if ASamplesF4[i] < MinValue then
    begin
      MinValue := ASamplesF4[i];
    end;
  end;
  Ratio := 0.5;
  MaxValue := Max(MaxValue, -MinValue);
  if MaxValue * Ratio > 1 then
    Ratio := 1 / MaxValue;

  FActualVolume := MaxValue;
  if FActualVolume > FMaxVolume then
  begin
    FMaxVolume := FActualVolume;
  end;

  for i := 0 to GetBufferAllChannelsSampleCount - 1 do
  begin
    ATargetF4[i] := Ratio * ASamplesF4[i];
  end;
end;

procedure TSoundMixer.ConvertF4ToS2(const ASamplesF4: PSamplesF4;
  const ASamplesS2: PSamplesS2);
var
  i: SG;
  Ratio, MaxValue, MinValue: F4;
begin
  MinValue := 0;
  MaxValue := 0;
  for i := 0 to GetBufferAllChannelsSampleCount - 1 do
  begin
    if ASamplesF4[i] > MaxValue then
    begin
      MaxValue := ASamplesF4[i];
    end;
    if ASamplesF4[i] < MinValue then
    begin
      MinValue := ASamplesF4[i];
    end;
  end;
  Ratio := 8191;
  MaxValue := Max(MaxValue, -MinValue);
  if MaxValue * Ratio > 32767 then
    Ratio := 32767 / MaxValue;

  FActualVolume := MaxValue;
  if FActualVolume > FMaxVolume then
  begin
    FMaxVolume := FActualVolume;
  end;

  for i := 0 to GetBufferAllChannelsSampleCount - 1 do
  begin
    ASamplesS2[i] := Round(Ratio * ASamplesF4[i]);
  end;
end;

destructor TSoundMixer.Destroy;
begin
  FreeMem(FBufferSamplesF4);

  inherited;
end;

function TSoundMixer.GetBufferAllChannelsSampleCount: SG;
begin
  Result := FBufferSingleChannelSampleCount * FChannels.Count;
end;

function TSoundMixer.GetBufferSize: SG;
begin
  Result := FBufferSingleChannelSampleCount * FChannels.Count * SizeOf(TSampleF4);
end;

procedure TSoundMixer.SetBufferSingleChannelSampleCount(const Value: SG);
begin
  if FBufferSingleChannelSampleCount <> Value then
  begin
    FreeMem(FBufferSamplesF4);
    FBufferSingleChannelSampleCount := Value;
    GetMem(FBufferSamplesF4, BufferSize);
  end;
end;

procedure TSoundMixer.SetChannels(const Value: TChannels);
begin
  FChannels := Value;
end;

procedure TSoundMixer.SetSampleRate(const Value: SG);
begin
  FSampleRate := Value;
end;

procedure TSoundMixer.SetSoundItems(const Value: TSoundItems);
begin
  FSoundItems := Value;
end;

end.

