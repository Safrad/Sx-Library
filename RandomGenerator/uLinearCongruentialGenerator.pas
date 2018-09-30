// https://en.wikipedia.org/wiki/Linear_congruential_generator
unit uLinearCongruentialGenerator;

{$R-}
{$Q-}

interface

uses
  uTypes,
  uRandomGenerator;

type
  TLinearCongruentialGenerator = class(TRandomGenerator)
  private
    FSeed: U8;
    FMultiplicand: U8;
    FAddition: U8;
    procedure SetAddition(const Value: U8);
    procedure SetMultiplicand(const Value: U8);
    procedure SetSeed(const Value: U8);
  public
    constructor Create;

    procedure Randomize;

    function RandomU4: U4; override;
    function RandomU8: U8; override;

    property Seed: U8 read FSeed write SetSeed;
    property Multiplicand: U8 read FMultiplicand write SetMultiplicand;
    property Addition: U8 read FAddition write SetAddition;
  end;

implementation

uses
  uMath;

{ TLinearCongruentialGenerator }

procedure TLinearCongruentialGenerator.Randomize;
begin
  FSeed := PerformanceCounter;
end;

function TLinearCongruentialGenerator.RandomU4: U4;
begin
  FSeed := FSeed * FMultiplicand + FAddition;
  Result := FSeed;
end;

function TLinearCongruentialGenerator.RandomU8: U8;
begin
  FSeed := FSeed * FMultiplicand + FAddition;
  Result := FSeed;
end;

procedure TLinearCongruentialGenerator.SetAddition(const Value: U8);
begin
  FAddition := Value;
end;

procedure TLinearCongruentialGenerator.SetMultiplicand(const Value: U8);
begin
  FMultiplicand := Value;
end;

procedure TLinearCongruentialGenerator.SetSeed(const Value: U8);
begin
  FSeed := Value;
end;

constructor TLinearCongruentialGenerator.Create;
begin
  inherited;

  Randomize;
end;

end.

