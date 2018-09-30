// https://en.wikipedia.org/wiki/Numerical_Recipes

unit uNumericalRecepiesRandomGenerator;

interface

uses
  uTypes,
  uLinearCongruentialGenerator;

type
  TNumericalRecepiesRandomGenerator = class(TLinearCongruentialGenerator)
  public
    constructor Create;
  end;

implementation

uses
  uMath;

{ TNumericalRecepiesRandomGenerator }

constructor TNumericalRecepiesRandomGenerator.Create;
begin
  inherited;

  Multiplicand := 1664525;
  Addition := 1013904223;
end;

end.
