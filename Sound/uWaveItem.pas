unit uWaveItem;

interface

uses
  uTypes,
  SysUtils,
  uWave,
  uISoundItem,
  uSoundItem;

type
  TWaveItem = class(TSoundItem)
  private
    FWave: TWave;
    FFrequency: FG;
    procedure SetFrequency(const Value: FG);
  public
    destructor Destroy; override;

    procedure ReadFromFile(const AFileName: TFileName);
    function GetSample: TSampleF4; override;

    property Frequency: FG read FFrequency write SetFrequency;
  end;

implementation

{ TWaveItem }

destructor TWaveItem.Destroy;
begin
  FreeAndNil(FWave);

  inherited;
end;

function TWaveItem.GetSample: TSampleF4;
begin
  Result := FWave.Sample(2 * FSampleIndex) / 32768;
  Amplitude := Amplitude * (1 - AmplitudeFade / SampleRate);

  Inc(FSampleIndex);
  if FSampleIndex >= FWave.SampleCount div 2 {Stereo} then
  begin
    FSampleIndex := 0;
    Amplitude := 0;
  end;
end;

procedure TWaveItem.ReadFromFile(const AFileName: TFileName);
begin
  FWave := TWave.Create;
  FWave.ReadFromFile(AFileName);
//  FWave.ConvertSampleRate(SampleRate);
end;

procedure TWaveItem.SetFrequency(const Value: FG);
begin
  FFrequency := Value;
end;

end.
