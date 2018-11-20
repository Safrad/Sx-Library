unit uSpeakerConfiguration;

interface

uses
  uTypes;

type
  TSpeakerConfiguration = (scMono, scStereo, scQuad, scSurround, sc5Point0, sc5Point1, sc7Point1);

function CreateChannelsClass(const ASpeakerConfiguration: TSpeakerConfiguration): TObject;

implementation

uses
  uMonoChannels,
  uStereoChannels,
  uQuadChannels,
  uSurroundChannels,
  u5Point0Channels,
  u5Point1Channels,
  u7Point1Channels;

function CreateChannelsClass(const ASpeakerConfiguration: TSpeakerConfiguration): TObject;
begin
  case ASpeakerConfiguration of
  scMono:
    Result := TMonoChannels.Create;
  scStereo:
    Result := TStereoChannels.Create;
  scQuad:
    Result := TQuadChannels.Create;
  scSurround:
    Result := TSurroundChannels.Create;
  sc5Point0:
    Result := T5Point0Channels.Create;
  sc5Point1:
    Result := T5Point1Channels.Create;
  else // sc7Point1
    Result := T7Point1Channels.Create;
  end;
end;

end.
