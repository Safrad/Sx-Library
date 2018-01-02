unit uTone;

interface

uses
  uTypes,
  uISoundItem,
  uSoundItem;

type
  TTone = class(TSoundItem)
  private
    FFrequency: FG;
    procedure SetFrequency(const Value: FG);

  protected
    function GetSample(const ASampleIndex: SG): TSample; override;
  public
    property Frequency: FG read FFrequency write SetFrequency;
  end;

implementation

{ TTone }

function TTone.GetSample(const ASampleIndex: SG): TSample;
begin
  Result := Round(Amplitude * Sin(2 * pi * Frequency * ASampleIndex / SampleRate));
end;

procedure TTone.SetFrequency(const Value: FG);
begin
  FFrequency := Value;
end;

end.
