unit uSampleRates;

interface

const
  // Speach
  SampleRateTelephone = 8000;
  SampleRateVoIP = 16000;
  SampleRateMiniDV = 32000;

  // CD / PC
  SampleRateQuaterCD = 11025;
  SampleRateHalfCD = 22100;
  SampleRateCD = 44100; // Most used
  SampleRateDoubleCD = 88200;
  SampleRateHDCD = 176400;
  SampleRateDXD = 352800; // Digital eXtreme Definition

  // DVD
  SampleRateHalfDVD = 48000;
  SampleRateDVD = 96000;
  SampleRateHDDVD = 192000;

  // Special
  SampleRateCDXA = 37800;
  SampleRateNTSC = 44056;
  SampleRateNipponColumbia = 47250;
  SampleRate3MAndSoundStream = 50000;
  SampleRateMitsubishiX80 = 50400;

  // Direct Stream Digital
  SampleRateDSD = 2822400;
  SampleRateDoubleDSD = 5644800;
  SampleRateQuadDSD = 11289600;
  SampleRateOctupleDSD = 22579200;

  DefaultSampleRate = SampleRateDVD;

implementation

end.
