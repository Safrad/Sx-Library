unit uDistanceFormater;

interface

uses
  uUnitFormatter;

type
  TDistanceUnits = (duSI, duImperial);

  TDistanceFormater = class(TUnitFormatter)
  private
    FDistanceUnits: TDistanceUnits;
    procedure SetDistanceUnits(const Value: TDistanceUnits);
  public
    property DistanceUnits: TDistanceUnits read FDistanceUnits write SetDistanceUnits;
  end;

implementation

{ TDistanceFormater }

procedure TDistanceFormater.SetDistanceUnits(const Value: TDistanceUnits);
begin
  FDistanceUnits := Value;
end;

end.
