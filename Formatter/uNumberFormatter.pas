unit uNumberFormatter;

interface

uses
  uTypes,
  uFormatter;

type
  TNumberFormatter = class(TInterfacedObject, IFormatter)
  private
    FPrecision: SG;
    procedure SetPrecision(const Value: SG);
  public
    constructor Create;
    
    function Format(const AValue: S8): string; overload;
    function Format(const AValue: FG): string; overload;

    property Precision: SG read FPrecision write SetPrecision;
  end;

implementation

uses
  SysUtils,
  uOutputFormat;

{ TNumberFormatter }

constructor TNumberFormatter.Create;
begin
  inherited;

  FPrecision := 3;
end;

function TNumberFormatter.Format(const AValue: FG): string;
begin
  Result := FloatToStrF(AValue, ffGeneral, FPrecision, 20);
end;

function TNumberFormatter.Format(const AValue: S8): string;
begin
  Result := NToS(AValue, FPrecision);
end;

procedure TNumberFormatter.SetPrecision(const Value: SG);
begin
  FPrecision := Value;
end;

end.
