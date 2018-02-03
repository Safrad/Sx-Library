unit uRatioValue;

interface

uses
  uTypes;

type
  TRatioValue = class
  private
    FUsed: S8;
    FTotal: S8;
    procedure SetRemain(const Value: S8);
    procedure SetRemainPercent(const Value: FG);
    procedure SetTotal(const Value: S8);
    procedure SetUsed(const Value: S8);
    procedure SetUsedPercent(const Value: FG);
    function GetRemainPercent: FG;
    function GetUsedPercent: FG;
    function GetRemain: S8;
  public
    property Used: S8 read FUsed write SetUsed;
    property UsedPercent: FG read GetUsedPercent write SetUsedPercent;
    property Remain: S8 read GetRemain write SetRemain;
    property RemainPercent: FG read GetRemainPercent write SetRemainPercent;
    property Total: S8 read FTotal write SetTotal;
  end;

implementation

{ TRatioValue }

function TRatioValue.GetRemain: S8;
begin
  Result := FTotal - FUsed;
end;

function TRatioValue.GetRemainPercent: FG;
begin
  Result := 100 * Remain / FTotal;
end;

function TRatioValue.GetUsedPercent: FG;
begin
  Result := 100 * FUsed / FTotal;
end;

procedure TRatioValue.SetRemain(const Value: S8);
begin
  FUsed := FTotal - Value;
end;

procedure TRatioValue.SetRemainPercent(const Value: FG);
begin
  Remain := Round(Value / 100 * FTotal);
end;

procedure TRatioValue.SetTotal(const Value: S8);
begin
  FTotal := Value;
end;

procedure TRatioValue.SetUsed(const Value: S8);
begin
  FUsed := Value;
end;

procedure TRatioValue.SetUsedPercent(const Value: FG);
begin
  FUsed := Round(Value / 100 * FTotal);
end;

end.
