unit uTransient;

interface

uses
  uTypes;

type
  TTransient = class
  private
    FActualValue: FG;
    FTargetValue: FG;
    FSpeed: FG;
    procedure SetActualValue(const Value: FG);
    procedure SetTargetValue(const Value: FG);
    procedure SetSpeed(const Value: FG);
  public
    procedure Step;

    property ActualValue: FG read FActualValue write SetActualValue;
    property TargetValue: FG read FTargetValue write SetTargetValue;
    property Speed: FG read FSpeed write SetSpeed;
  end;

implementation

{ TTransient }

procedure TTransient.SetActualValue(const Value: FG);
begin
  FActualValue := Value;
end;

procedure TTransient.SetSpeed(const Value: FG);
begin
  FSpeed := Value;
end;

procedure TTransient.SetTargetValue(const Value: FG);
begin
  FTargetValue := Value;
end;

procedure TTransient.Step;
begin
  FActualValue := (FActualValue + FTargetValue * FSpeed) / (1 + FSpeed);
end;

end.
