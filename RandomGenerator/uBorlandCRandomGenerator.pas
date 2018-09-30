unit uBorlandCRandomGenerator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TBorlandCRandomGenerator = class(TLinearCongruentialGenerator)
  public
    constructor Create;
  end;

implementation

uses
  uMath;

{ TBorlandCRandomGenerator }

constructor TBorlandCRandomGenerator.Create;
begin
  inherited;

  Multiplicand := 22695477;
  Addition := 1;
end;

end.

