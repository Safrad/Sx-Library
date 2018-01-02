unit uSoundMixer;

interface

uses
  uTypes,
  Windows,
  uSoundItem;

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


type
  TSpeakerConfiguration = (
    scMono = KSAUDIO_SPEAKER_MONO,
    scStereo = KSAUDIO_SPEAKER_STEREO,
    scQuad = KSAUDIO_SPEAKER_QUAD,
    scSurround = KSAUDIO_SPEAKER_SURROUND,
    sc5Point1 = KSAUDIO_SPEAKER_5POINT1,
    sc7Point1 = KSAUDIO_SPEAKER_7POINT1);

  tWAVEFORMATEX = packed record
    wFormatTag: Word; { format type }
    nChannels: Word; { number of channels (i.e. mono, stereo, etc.) }
    nSamplesPerSec: DWORD; { sample rate }
    nAvgBytesPerSec: DWORD; { for buffer estimation }
    nBlockAlign: Word; { block size of data }
    wBitsPerSample: Word; { number of bits per sample of mono data }
    cbSize: Word; { the count in bytes of the size of }
  end;

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

  
  1/sqrt(2)

  TSoundMixer = class
  private
    FSounds: array of TSoundItem;

  end;

implementation

end.
