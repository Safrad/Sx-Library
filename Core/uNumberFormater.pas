unit uNumberFormatter;

interface

uses
  uNumberFormatter;

type
  TNumberFormatter = class(TFormatter)
  public
    function Format(const AValue: FG): string;
    function Format(const AValue: SG): string;
  end;

implementation

uses
  uOutputFormat;

{ TNumberFormatter }

function TNumberFormatter.Format(const AValue: FG): string;
begin
  Result := FToS(AValue);
end;

function TNumberFormatter.Format(const AValue: SG): string;
begin
  Result := NToS(AValue);
end;

end.
