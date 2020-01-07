// Used in Fortran and MATLAB
// Supports only 134456 range
// The lowest bit oscillates at each step

unit uRandom0Generator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TRandom0Generator = class(TLinearCongruentialGenerator)
  public
    constructor Create;

    function RandomU4: U4; override;
    function RandomU8: U8; override;
  end;

implementation

{ TRandom0Generator }

constructor TRandom0Generator.Create;
begin
  inherited;

  Multiplicand := 8121;
  Addition := 28411;
end;

{$ifopt Q+} {$Q-} {$define OverflowCheck} {$endif}
{$ifopt R+} {$R-} {$define RangeCheck} {$endif}
function TRandom0Generator.RandomU4: U4;
const
  Modulo = 134456;
var
  ResultOffset: U1;
begin
  Seed := Seed * Multiplicand + Addition;
  ResultOffset := (Seed mod (32 * Modulo)) div Modulo;
  Result := ((Seed shr ResultOffset) mod Modulo) * 31943 {2^32 div Modulo};
end;

function TRandom0Generator.RandomU8: U8;
const
  Modulo = 134456;
var
  ResultOffset: U1;
begin
  Seed := Seed * Multiplicand + Addition;
  ResultOffset := (Seed mod (32 * Modulo)) div Modulo;
  Result := ((Seed shr ResultOffset) mod Modulo) * U8(137195395324192) {2^64 div Modulo};
end;
{$ifdef OverflowCheck} {$Q+} {$undef OverflowCheck} {$endif}
{$ifdef RangeCheck} {$R+} {$undef RangeCheck} {$endif}

end.
