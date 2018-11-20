unit uWhiteNoise;

interface

uses
  uTypes,
  uISoundItem,
  uSoundItem;

type
  TNoise = class(TInterfacedObject, ISoundItem)
  private
    t: FG;
  public
    function GetSample(const ASampleIndex: SG): TSample;
  end;

implementation

uses
  uMath;

const
  SampleRate = 44100;

{ TNoise }

function TNoise.GetSample(const ASampleIndex: SG): TSample;
var
  f: FG;
  Count: UG;
begin
  Result := 0;
  Count := 0;
  t := 0;
  f := 20;
  while f < 20000 do
  begin
//    f := f + 0.1;
    f := f * 1.1;
    Result := Result + Sin(2 * pi * f + t);
    Inc(Count);
  end;
  Increment(t, 1 / SampleRate);
  Increment(Result, 1 / Count);

//  Result := Round(Amplitude * Sin(2 * pi * Frequency * ASampleIndex / SampleRate));
end;

end.
