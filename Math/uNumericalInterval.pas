unit uNumericalInterval;

interface

uses
  uTypes,
  uNumericalSet;

type
  TNumericalInterval = class(TNumericalSet)
  private
    FMinimalValue: S8;
    FMaximalValue: S8;
    procedure SetMinimalValue(const Value: S8);
    procedure SetMaximalValue(const Value: S8);
  public
    constructor Create;
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
    property MinimalValue: S8 read FMinimalValue write SetMinimalValue;
    property MaximalValue: S8 read FMaximalValue write SetMaximalValue;
  end;


implementation

uses
  uMath,
  uOutputFormat;

{ TNumericalInterval }

function TNumericalInterval.Contains(const ANumber: S8): BG;
begin
  Result := IsInRange(FMinimalValue, ANumber, FMaximalValue);
end;

constructor TNumericalInterval.Create;
begin
  inherited;

  FMinimalValue := Low(FMinimalValue);
  FMaximalValue := High(FMaximalValue);
end;

function TNumericalInterval.Description: string;
begin
  Result := 'Interval [' + NToS(FMinimalValue) + '..' + NToS(FMaximalValue) + ']';
end;

procedure TNumericalInterval.SetMaximalValue(const Value: S8);
begin
  FMaximalValue := Value;
end;

procedure TNumericalInterval.SetMinimalValue(const Value: S8);
begin
  FMinimalValue := Value;
end;

end.
