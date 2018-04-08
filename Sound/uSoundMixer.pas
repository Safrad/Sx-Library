unit uSoundMixer;

interface

uses
  uTypes,
  Windows,
  uSoundItem,
  uISoundItem;

// TODO : Multithreading

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
    FBufferSampleCount: SG;
    FSampleRate: SG;
    FMaxVolume: FG;
    FActualVolume: FG;

    procedure SetSoundItems(const Value: TSoundItems);
    procedure SetBufferSampleCount(const Value: SG);
    function GetBufferSize: SG;
    procedure SetSampleRate(const Value: SG);
  public
    procedure Add(const ASoundItem: TSoundItem);
    procedure FillBuffer(const ASamplesS2: PSamplesS2); virtual; abstract;
    procedure ConvertF4ToS2(const ASamplesF4: PSamplesF4; const ASamplesS2: PSamplesS2);

    property BufferSamplesF4: PSamplesF4 read FBufferSamplesF4;
    property BufferSampleCount: SG read FBufferSampleCount write SetBufferSampleCount;
    property BufferSize: SG read GetBufferSize;
    property SampleRate: SG read FSampleRate write SetSampleRate;
    property SoundItems: TSoundItems read FSoundItems write SetSoundItems;

    property MaxVolume: FG read FMaxVolume;
    property ActualVolume: FG read FActualVolume;
  end;

implementation

uses
  uLog,
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

procedure TSoundMixer.ConvertF4ToS2(const ASamplesF4: PSamplesF4;
  const ASamplesS2: PSamplesS2);
var
  i: SG;
  Ratio, MaxValue, MinValue: F4;
begin
  MinValue := 0;
  MaxValue := 0;
  for i := 0 to FBufferSampleCount - 1 do
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
  Ratio := 8191; //32767;
  MaxValue := Max(MaxValue, -MinValue);
  if MaxValue * Ratio > 32767 then
    Ratio := 32767 / MaxValue;
//  Ratio := 8000;

  FActualVolume := MaxValue;
  if FActualVolume > FMaxVolume then
  begin
    FMaxVolume := FActualVolume;
  end;

  for i := 0 to FBufferSampleCount - 1 do
  begin
    ASamplesS2[2 * i] := Round(Ratio * ASamplesF4[i]);
    ASamplesS2[2 * i + 1] := ASamplesS2[2 * i];
  end;
end;

function TSoundMixer.GetBufferSize: SG;
begin
  Result := FBufferSampleCount * SizeOf(TSampleF4);
end;

procedure TSoundMixer.SetBufferSampleCount(const Value: SG);
begin
  FBufferSampleCount := Value;
  GetMem(FBufferSamplesF4, BufferSize);
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

