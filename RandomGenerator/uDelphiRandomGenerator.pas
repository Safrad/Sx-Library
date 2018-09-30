// The same since Turbo Pascal

unit uDelphiRandomGenerator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TDelphiRandomGenerator = class(TLinearCongruentialGenerator)
  public
    constructor Create;
  end;

implementation

uses
  uMath;

{ TDelphiRandomGenerator }

constructor TDelphiRandomGenerator.Create;
begin
  inherited;

// Random integer, implemented as a deterministic linear congruential generator
// with 134775813 as a and 1 as c.

// Original
//  Multiplicand := $08088405;
//  Addition := 1;

// Expanded to 64 bit, compatibility is kept, the same as twice call of 32 bit version (a2 = a^2, c2 = c * a + c)
  Multiplicand := $D465281908088405;
  Addition := $0808840600000001;
end;

end.
