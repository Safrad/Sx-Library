unit uRandomGenerator;

interface

uses
  uTypes;

type
  TRandomGenerator = class
  public
    function RandomU1: U1;
    function RandomU2: U2;
    function RandomU4: U4; virtual; abstract;
    function RandomU8: U8; virtual; abstract;

    function RangeU4(const AMaxValue: U4): U4; overload;
    function RangeU4(const AMinimalValue, AMaximalValue: U4): U4; overload;

    function RangeU8(const AMaxValue: U8): U8; overload;
    function RangeU8(const AMinimalValue, AMaximalValue: U8): U8; overload;

    function RandomZeroDistance(const AMaximalDistance: U4): S4;
  end;

implementation

uses
  uMath;

{ TRandomGenerator }

function TRandomGenerator.RangeU4(const AMaxValue: U4): U4;
begin
  Result := RandomU4;
  Result := (U8(AMaxValue + 1) * U8(Result)) shr 32;
end;

function TRandomGenerator.RangeU8(const AMaxValue: U8): U8;
begin
  Result := RandomU8;
  if AMaxValue <> $FFFFFFFFFFFFFFFF then
    Result := MultiplyAndReturnMostSignificantHalf(Result, AMaxValue + 1);
end;

function TRandomGenerator.RandomU1: U1;
begin
	Result := RangeU4(256);
end;

function TRandomGenerator.RandomU2: U2;
begin
  Result := RangeU4(65536);
end;

function TRandomGenerator.RandomZeroDistance(const AMaximalDistance: U4): S4;
begin
	Result := S4(RangeU4(2 * AMaximalDistance)) - S4(AMaximalDistance);
end;

function TRandomGenerator.RangeU4(const AMinimalValue, AMaximalValue: U4): U4;
begin
  Result := RangeU4(AMaximalValue - AMinimalValue) + AMinimalValue;
end;

function TRandomGenerator.RangeU8(const AMinimalValue, AMaximalValue: U8): U8;
begin
  Result := RangeU8(AMaximalValue - AMinimalValue) + AMinimalValue;
end;

end.
