unit uUnitPrefix;

interface

uses
  uTypes;

type
  TPrefixedValue = record
    Value: FG;
    Prefix: string;
  end;

  TUnitPrefix = class
  protected
    class function GetCount: SG; virtual; abstract;
    class function GetMultiplicator(const AIndex: SG): FG; virtual; abstract;
    class function GetString(const AIndex: SG): string; virtual; abstract;
  public
    class function PrefixedValue(const AValue: FG): TPrefixedValue;
  end;

implementation

{ TUnitPrefix }

class function TUnitPrefix.PrefixedValue(const AValue: FG): TPrefixedValue;
var
  i: SG;
  NewValue: FG;
  MaxPrefix: SG;
begin
  Result.Value := AValue;
  Result.Prefix := '';
  if AValue <> 0 then
  begin
    MaxPrefix := GetCount - 1;
    for i := 0 to MaxPrefix do
    begin
      NewValue := AValue * GetMultiplicator(i);

      if (NewValue >= 1) or (i = MaxPrefix) then
      begin
        Result.Value := NewValue;
        Result.Prefix := GetString(i);
        Break;
      end;
    end;
  end;
end;

end.
