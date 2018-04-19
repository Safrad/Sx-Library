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
    procedure SetWave(const Value: TWave);
  public
    destructor Destroy; override;

    procedure ReadFromFile(const AFileName: TFileName);
    function GetSample: TSampleF4; override;
    property Wave: TWave read FWave write SetWave;
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
  if FSampleIndex >= SG(FWave.SampleCount div 2) {Stereo} then
  begin
    FSampleIndex := 0;
    Amplitude := 0;
  end;
end;

procedure TWaveItem.ReadFromFile(const AFileName: TFileName);
var
  TmpWave: TWave;
begin
  TmpWave := TWave.Create;
  try
    TmpWave.ReadFromFile(AFileName);
    FWave := TmpWave.ConvertSampleRate(SampleRate);
  finally
    TmpWave.Free;
  end;
end;

procedure TWaveItem.SetWave(const Value: TWave);
begin
  FWave := Value;
end;

end.
