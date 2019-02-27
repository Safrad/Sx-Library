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

type
  TSampleRate = U4;

  TSoundItem = class(TInterfacedObject, ISoundItem)
  private
    FTime: TTimeSpan;
    FSampleRate: TSampleRate;
    FAmplitude: FG;
    FSampleId: U8;
    FAmplitudeFade: FG;
    procedure SetSampleRate(const Value: TSampleRate);
    procedure SetAmplitude(const Value: FG);
    procedure SetSampleId(const Value: U8);
    function GetTime: TTimeSpan;
    procedure SetAmplitudeFade(const Value: FG);
  protected
    FSampleIndex: SG;
  public
    constructor Create;

    procedure Reset; virtual;

    function GetSample: TSampleF4; virtual;
    property SampleRate: TSampleRate read FSampleRate write SetSampleRate;
    property Amplitude: FG read FAmplitude write SetAmplitude;
    property AmplitudeFade: FG read FAmplitudeFade write SetAmplitudeFade;
    property SampleId: U8 read FSampleId write SetSampleId;
    property Time: TTimeSpan read GetTime;
  end;

  TSoundItems = array of TSoundItem;

implementation

uses
  SysUtils,
  uSampleRates,
  uMath;

{ TSoundItem }

constructor TSoundItem.Create;
begin
  inherited;

  FSampleRate := DefaultSampleRate;
  Reset;
end;

function TSoundItem.GetSample: TSampleF4;
begin
  Result := 0;
  FAmplitude := FAmplitude * (1 - FAmplitudeFade / FSampleRate);
end;

function TSoundItem.GetTime: TTimeSpan;
begin
  FTime.Seconds := FSampleId / FSampleRate;

  Result := FTime;
end;

procedure TSoundItem.Reset;
begin
  Amplitude := 1;
  FSampleIndex := 0;
end;

procedure TSoundItem.SetAmplitude(const Value: FG);
begin
  FAmplitude := Value;
end;

procedure TSoundItem.SetAmplitudeFade(const Value: FG);
begin
  FAmplitudeFade := Value;
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
