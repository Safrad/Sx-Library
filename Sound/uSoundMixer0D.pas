unit uSoundMixer0D;

interface

uses
  uTypes,
  Windows,
  uSoundMixer;

type
  TSoundMixer0D = class(TSoundMixer)
  public
    procedure FillBuffer(const ASamplesS2: PSamplesS2); override;
  end;

implementation

uses
  uMath,
  uSoundItem,
  uISoundItem;

{ TSoundMixer0D }

procedure TSoundMixer0D.FillBuffer(const ASamplesS2: PSamplesS2);
var
  SampleIndex, SoundItemIndex: SG;
  SoundItem: TSoundItem;
  ABufferSample: ^TSampleF4;
  Value: F4;
begin
  FillChar(BufferSamplesF4^, BufferSize, 0);

  SoundItemIndex := 0;
  while SoundItemIndex < Length(SoundItems) do
  begin
    SoundItem := SoundItems[SoundItemIndex];
    if SoundItem.Amplitude > 0.001 then
    begin
      ABufferSample := @BufferSamplesF4[0];
      for SampleIndex := 0 to BufferSampleCount - 1 do
      begin
        Value := SoundItem.Amplitude * SoundItem.GetSample;
        Increment(ABufferSample^, Value);
        Inc(ABufferSample);
      end;
    end;

    Inc(SoundItemIndex);
  end;

  ConvertF4ToS2(BufferSamplesF4, ASamplesS2);
end;

end.
