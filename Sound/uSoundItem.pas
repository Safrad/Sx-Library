unit uSoundItem;

interface

uses
  uTypes,
  uTimeSpan,
  uISoundItem;

const
  Amplitude8 = High(S1); // 8 bit
  Amplitude16 = High(S2); // 16 bit
  Amplitude24 = 16 * MB; // 24 bit
  Amplitude32 = High(S4); // 32 bit

  Amplitude = Amplitude24; // Used apmlitude

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

type
  TSampleRate = S4;

  TSoundItem = class(TInterfacedObject, ISoundItem)
  private
    FTime: TTimeSpan;
    FSampleRate: TSampleRate;
    FAmplitude: FG;
    FSampleId: U8;
    procedure SetSampleRate(const Value: TSampleRate);
    procedure SetAmplitude(const Value: FG);
    procedure SetSampleId(const Value: U8);
    function GetTime: TTimeSpan;
  protected
    function GetSample(const ASampleIndex: SG): TSample; virtual;
  public
    constructor Create;
    destructor Destroy; override;

    property SampleRate: TSampleRate read FSampleRate write SetSampleRate;
    property Amplitude: FG read FAmplitude write SetAmplitude;
    property SampleId: U8 read FSampleId write SetSampleId;
    property Time: TTimeSpan read GetTime;
  end;

implementation

uses
  SysUtils,
  uMath;

{ TSoundItem }

constructor TSoundItem.Create;
begin
  FTime := TTimeSpan.Create;

  FSampleRate := DefaultSampleRate;
end;

destructor TSoundItem.Destroy;
begin
  FreeAndNil(FTime);

  inherited;
end;

function TSoundItem.GetSample(const ASampleIndex: SG): TSample;
begin
  Result := 0;
end;

function TSoundItem.GetTime: TTimeSpan;
begin
  FTime.Seconds := FSampleId / FSampleRate;

  Result := FTime;
end;

procedure TSoundItem.SetAmplitude(const Value: FG);
begin
  FAmplitude := Value;
end;

procedure TSoundItem.SetSampleId(const Value: U8);
begin
  FSampleId := Value;
end;

procedure TSoundItem.SetSampleRate(const Value: TSampleRate);
begin
  if FSampleRate <> Value then
  begin
    FSampleId := RoundDivU8(Value * FSampleId, FSampleRate);

    FSampleRate := Value;
  end;
end;

end.
