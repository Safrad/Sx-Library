unit uChannels;

interface

uses
  uTypes;

const
  SPEAKER_FRONT_LEFT = $1;
  SPEAKER_FRONT_RIGHT = $2;
  SPEAKER_FRONT_CENTER = $4;
  SPEAKER_LOW_FREQUENCY = $8;
  SPEAKER_BACK_LEFT = $10;
  SPEAKER_BACK_RIGHT = $20;
  SPEAKER_FRONT_LEFT_OF_CENTER = $40;
  SPEAKER_FRONT_RIGHT_OF_CENTER = $80;
  SPEAKER_BACK_CENTER = $100;
  SPEAKER_SIDE_LEFT = $200;
  SPEAKER_SIDE_RIGHT = $400;
  SPEAKER_TOP_CENTER = $800;
  SPEAKER_TOP_FRONT_LEFT = $1000;
  SPEAKER_TOP_FRONT_CENTER = $2000;
  SPEAKER_TOP_FRONT_RIGHT = $4000;
  SPEAKER_TOP_BACK_LEFT = $8000;
  SPEAKER_TOP_BACK_CENTER = $10000;
  SPEAKER_TOP_BACK_RIGHT = $20000;

  KSAUDIO_SPEAKER_MONO            = (SPEAKER_FRONT_CENTER);
  KSAUDIO_SPEAKER_STEREO          = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT);
  KSAUDIO_SPEAKER_QUAD            = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                     SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);
  KSAUDIO_SPEAKER_SURROUND        = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                     SPEAKER_FRONT_CENTER or SPEAKER_BACK_CENTER);
  KSAUDIO_SPEAKER_5POINT1         = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                     SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or
                                     SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);
  KSAUDIO_SPEAKER_7POINT1         = (SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
                                     SPEAKER_FRONT_CENTER or SPEAKER_LOW_FREQUENCY or
                                     SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT or
                                     SPEAKER_FRONT_LEFT_OF_CENTER or SPEAKER_FRONT_RIGHT_OF_CENTER);
  KSAUDIO_SPEAKER_NOLOW = (
    SPEAKER_FRONT_LEFT or SPEAKER_FRONT_RIGHT or
    SPEAKER_FRONT_CENTER or
    SPEAKER_BACK_LEFT or SPEAKER_BACK_RIGHT);


type
  TSpeakerConfiguration = (
    scMono = KSAUDIO_SPEAKER_MONO,
    scStereo = KSAUDIO_SPEAKER_STEREO,
    scQuad = KSAUDIO_SPEAKER_QUAD,
    scSurround = KSAUDIO_SPEAKER_SURROUND,
    sc5Point1 = KSAUDIO_SPEAKER_5POINT1,
    sc7Point1 = KSAUDIO_SPEAKER_7POINT1);

function GetMaskForNumberOfChannels(const m_Channels: SG): SG;

implementation

function GetMaskForNumberOfChannels(const m_Channels: SG): SG;
begin
  Result := 0;
  if m_Channels >= 1 then
    Result := Result or SPEAKER_FRONT_LEFT;
  if m_Channels >= 2 then
    Result := Result or SPEAKER_FRONT_RIGHT;
  if m_Channels >= 3 then
    Result := Result or SPEAKER_FRONT_CENTER;
  if m_Channels >= 4 then
    Result := Result or SPEAKER_LOW_FREQUENCY;
  if m_Channels >= 5 then
    Result := Result or SPEAKER_BACK_LEFT;
  if m_Channels >= 6 then
    Result := Result or SPEAKER_BACK_RIGHT;
  if m_Channels >= 7 then
    Result := Result or SPEAKER_SIDE_LEFT;
  if m_Channels >= 8 then
    Result := Result or SPEAKER_SIDE_RIGHT;
end;

end.
