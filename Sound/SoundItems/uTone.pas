unit uTone;

interface

uses
  uTypes,
  uISoundItem,
  uSoundItem2D;

type
  TTone = class(TSoundItem2D)
  private
    FFrequency: FG;
    procedure SetFrequency(const Value: FG);

  public
    function GetSample: TSampleF4; override;
    property Frequency: FG read FFrequency write SetFrequency;
  end;

implementation

{ TTone }

function TTone.GetSample: TSampleF4;
begin
  Result := Sin(2 * pi * Frequency * FSampleIndex / SampleRate);
  Amplitude := Amplitude * (1 - AmplitudeFade / SampleRate);

  Inc(FSampleIndex);
end;

procedure TTone.SetFrequency(const Value: FG);
begin
  FFrequency := Value;
end;

end.
