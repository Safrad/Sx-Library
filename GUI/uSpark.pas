unit uSpark;

interface

uses
  uTypes,
  uColor,
  uRandomGenerator;

type
	TSpark = class
  private
    FColor: TRGBA;
    FX: S4;
    FY: S4;
    FPower: S4;
    FRandomGenerator: TRandomGenerator;
    procedure SetColor(const Value: TRGBA);
    procedure SetPower(const Value: S4);
    procedure SetX(const Value: S4);
    procedure SetY(const Value: S4);
    procedure SetRandomGenerator(const Value: TRandomGenerator);
  public
		property X: S4 read FX write SetX;
    property Y: S4 read FY write SetY;
		property Power: S4 read FPower write SetPower;
		property Color: TRGBA read FColor write SetColor;
    property RandomGenerator: TRandomGenerator read FRandomGenerator write SetRandomGenerator;

    procedure Update;
	end;

implementation

{ TSpark }

procedure TSpark.SetColor(const Value: TRGBA);
begin
  FColor := Value;
end;

procedure TSpark.SetPower(const Value: S4);
begin
  FPower := Value;
end;

procedure TSpark.SetRandomGenerator(const Value: TRandomGenerator);
begin
  FRandomGenerator := Value;
end;

procedure TSpark.SetX(const Value: S4);
begin
  FX := Value;
end;

procedure TSpark.SetY(const Value: S4);
begin
  FY := Value;
end;

procedure TSpark.Update;
begin
  Dec(FPower, 10);
  Inc(FY, 2);
  Inc(FX, FRandomGenerator.RandomZeroDistance(2));
end;

end.
