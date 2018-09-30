unit uSxRandomGenerator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TSxRandomGenerator = class(TLinearCongruentialGenerator)
  public
    constructor Create;
  end;

implementation

{ TSxRandomGenerator }

constructor TSxRandomGenerator.Create;
begin
  inherited;

  // MMIX by Donald Knuth
  Multiplicand := 6364136223846793005;
  Addition := 1442695040888963407;
end;

end.
