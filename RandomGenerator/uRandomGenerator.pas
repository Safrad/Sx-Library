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

    procedure Random(out AData: U1); overload;
    procedure Random(out AData: U2); overload;
    procedure Random(out AData: U4); overload;
    procedure Random(out AData: U8); overload;
    procedure Random(out AData: PByte; const ASize: SG); overload;

    function RangeU4(const AMaxValue: U4): U4; overload;
    function RangeU4(const AMinimalValue, AMaximalValue: U4): U4; overload;

    function RangeU8(const AMaxValue: U8): U8; overload;
    function RangeU8(const AMinimalValue, AMaximalValue: U8): U8; overload;

    function RangeExactDistributionU4(const AMaxValue: U4): U4; overload;
    function RangeExactDistributionU4(const AMinimalValue, AMaximalValue: U4): U4; overload;

    function RangeExactDistributionU8(const AMaxValue: U8): U8; overload;
    function RangeExactDistributionU8(const AMinimalValue, AMaximalValue: U8): U8; overload;

    function GetDistrigutionMax(const AMaxValue: U4): U4; overload;
    function GetDistrigutionMax(const AMaxValue: U8): U8; overload;

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
  if AMaxValue <> High(AMaxValue) then
    Result := MultiplyAndReturnMostSignificantHalf(Result, AMaxValue + 1);
end;

function TRandomGenerator.RandomU1: U1;
var
  X: TU4;
begin
  X.A := RandomU4;
	Result := X.B0;
end;

function TRandomGenerator.RandomU2: U2;
var
  X: TU4;
begin
  X.A := RandomU4;
  Result := X.W0;
end;

procedure TRandomGenerator.Random(out AData: U1);
begin
  AData := RandomU1;
end;

procedure TRandomGenerator.Random(out AData: U2);
begin
  AData := RandomU2;
end;

procedure TRandomGenerator.Random(out AData: U4);
begin
  AData := RandomU4;
end;

procedure TRandomGenerator.Random(out AData: U8);
begin
  AData := RandomU8;
end;

function TRandomGenerator.GetDistrigutionMax(const AMaxValue: U8): U8;
begin
  if AMaxValue > High(AMaxValue) div 2 then
    Result := AMaxValue
  else
    Result := (High(AMaxValue) - ((High(AMaxValue) div 2) + 1) mod (AMaxValue + 1));
end;

function TRandomGenerator.GetDistrigutionMax(const AMaxValue: U4): U4;
begin
  Result := U4(High(AMaxValue) - (U8(High(AMaxValue)) + 1) mod U8(AMaxValue + 1));
end;

procedure TRandomGenerator.Random(out AData: PByte; const ASize: SG);
var
  i: SG;
  Data: PU4;
  Remain: UG;
  R: TU4;
begin
  Data := PU4(AData);
  for i := 0 to ASize div 4 - 1 do
  begin
    Data^ := RandomU4;
    Inc(Data);
  end;
  Remain := ASize mod 4;
  if Remain <> 0 then
  begin
    R.A := RandomU4;
    case Remain of
    1: PU1(Data)^ := R.B0;
    2: PU2(Data)^ := R.W0;
    3:
    begin
      PU2(Data)^ := R.W0;
      Inc(PU2(Data));
      PU1(Data)^ := R.B2;
    end;
    end;
  end;
end;

function TRandomGenerator.RandomZeroDistance(const AMaximalDistance: U4): S4;
begin
	Result := S4(RangeU4(2 * AMaximalDistance)) - S4(AMaximalDistance);
end;

function TRandomGenerator.RangeExactDistributionU4(const AMaxValue: U4): U4;
var
  DistributionMax: U4;
begin
  DistributionMax := GetDistrigutionMax(AMaxValue);
  while True do
  begin
    Result := RangeU4(AMaxValue);
    if Result <= DistributionMax then
      Break;
  end;
end;

function TRandomGenerator.RangeExactDistributionU4(const AMinimalValue, AMaximalValue: U4): U4;
begin
  Result := RangeExactDistributionU4(AMaximalValue - AMinimalValue) + AMinimalValue;
end;

function TRandomGenerator.RangeExactDistributionU8(const AMaxValue: U8): U8;
var
  DistributionMax: U8;
begin
  DistributionMax := GetDistrigutionMax(AMaxValue);
  while True do
  begin
    Result := RangeU8(AMaxValue);
    if Result <= DistributionMax then
      Break;
  end;
end;

function TRandomGenerator.RangeExactDistributionU8(const AMinimalValue, AMaximalValue: U8): U8;
begin
  Result := RangeExactDistributionU8(AMaximalValue - AMinimalValue) + AMinimalValue;
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
