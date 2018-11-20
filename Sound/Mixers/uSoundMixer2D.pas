unit uSoundMixer2D;

interface

uses
  uTypes,
  Windows,
  uSoundMixer;

type
  TSoundMixer2D = class(TSoundMixer)
  public
    procedure FillBuffer(const ASamplesF4: PSamplesF4); override;
  end;

implementation

uses
  Math,
  uMath,
  uSoundItem2D,
  uISoundItem,
  uChannel,
  uGeometry2D;

{ TSoundMixer2D }

procedure TSoundMixer2D.FillBuffer(const ASamplesF4: PSamplesF4);
var
  c: SG;
  SampleIndex, SoundItemIndex: SG;
  SoundItem: TSoundItem2D;
  BufferSample: ^TSampleF4;
  Value, Amplitude, Sample, Distance: F4;
  Channel: TChannel;
begin
  FillChar(BufferSamplesF4^, BufferSize, 0);

  SoundItemIndex := 0;
  while SoundItemIndex < Length(SoundItems) do
  begin
    SoundItem := TSoundItem2D(SoundItems[SoundItemIndex]);
    if SoundItem.Amplitude > 0.001 then
    begin
      // TODO : Doppler effect

      BufferSample := @BufferSamplesF4[0];
      for SampleIndex := 0 to BufferSingleChannelSampleCount - 1 do
      begin
        Sample := SoundItem.GetSample;
        for c := 0 to Channels.Count - 1 do
        begin
          Channel := Channels.Get(c);

          Distance := GetDistance2D(SoundItem.Position, Channel.Position);
          Amplitude := SoundItem.Amplitude / Max(0.1, Distance); // A = A0 / R, I = I0 / R^2
          Value := Amplitude * Sample;
          Increment(BufferSample^, Value);
          Inc(BufferSample);
        end;
      end;
    end;

    Inc(SoundItemIndex);
  end;

  Normalize(BufferSamplesF4, ASamplesF4);
end;

end.
