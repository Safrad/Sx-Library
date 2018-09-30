unit uGNUCRandomGenerator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TGNUCRandomGenerator = class(TLinearCongruentialGenerator)
  public
    constructor Create;
  end;

implementation

uses
  uMath;

{ TGNUCRandomGenerator }

constructor TGNUCRandomGenerator.Create;
begin
  inherited;

  Multiplicand := 1103515245;
  Addition := 12345;
end;

end.

