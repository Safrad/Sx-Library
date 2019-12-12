unit uSoundMixer0D;

interface

uses
  uTypes,
  uSoundMixer;

type
  TSoundMixer0D = class(TSoundMixer)
  public
    procedure FillBuffer(const ASamplesF4: PSamplesF4); override;
  end;

implementation

uses
  uMath,
  uSoundItem,
  uISoundItem;

{ TSoundMixer0D }

procedure TSoundMixer0D.FillBuffer(const ASamplesF4: PSamplesF4);
var
  SampleIndex, SoundItemIndex: SG;
  SoundItem: TSoundItem;
  BufferSample: ^TSampleF4;
  Value: F4;
begin
  ClearMemory(BufferSamplesF4^, BufferSize);

  SoundItemIndex := 0;
  while SoundItemIndex < Length(SoundItems) do
  begin
    SoundItem := SoundItems[SoundItemIndex];
    if SoundItem.Amplitude > 0.001 then
    begin
      BufferSample := @BufferSamplesF4[0];
      for SampleIndex := 0 to BufferSingleChannelSampleCount - 1 do
      begin
        Value := SoundItem.Amplitude * SoundItem.GetSample;
        Increment(BufferSample^, Value);
        Inc(BufferSample);
      end;
    end;

    Inc(SoundItemIndex);
  end;

  Normalize(BufferSamplesF4, ASamplesF4);
end;

end.
