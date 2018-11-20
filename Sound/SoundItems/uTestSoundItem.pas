unit uTestSoundItem;

interface

uses
  uTypes,
  uISoundItem,
  uSoundItem2D;

type
  TTestSoundItem = class(TSoundItem2D)
  private
    FFrequency: FG;
    procedure SetFrequency(const Value: FG);
  public
    function GetSample: TSampleF4; override;
    property Frequency: FG read FFrequency write SetFrequency;
  end;

implementation

uses
  uGeometry2D;

{ TTestSoundItem }

function TTestSoundItem.GetSample: TSampleF4;
begin
  Result := Sin(2 * pi * Frequency * FSampleIndex / SampleRate);
  Amplitude := Amplitude * (1 - AmplitudeFade / SampleRate);
  Position := CreatePoint2D(2 * Sin(FSampleIndex / SampleRate), -2 * Cos(FSampleIndex / SampleRate));

  Inc(FSampleIndex);
end;

procedure TTestSoundItem.SetFrequency(const Value: FG);
begin
  FFrequency := Value;
end;

end.
