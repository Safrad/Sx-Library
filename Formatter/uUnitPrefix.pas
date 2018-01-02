unit uUnitPrefix;

interface

uses
  uTypes;

type
  TPrefixType = (ptMetric, ptBinary);

  TPrefixedResult = record
    Value: FG;
    Prefix: string;
  end;

function PrefixedValue(const AValue: FG; const PrefixType: TPrefixType): TPrefixedResult;

implementation

uses
  uStrings;

const
  MinPrefix = -7;
  MaxPrefix = 8;
  PrefixStr: array[MinPrefix..MaxPrefix] of string = ('Y', 'Z', 'E', 'P', 'T', 'G', 'M', 'K', '', 'm', 'µ', 'n', 'p', 'f', 'a', 'z');
  PrefixMult: array[TPrefixType, MinPrefix..MaxPrefix] of FG = (
    (1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18, 1e21),
    (1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-9, 1/1024/1024, 1/1024, 1, 1024, 1024 * 1024, 1e9, 1e12, 1e15, 1e18, 1e21)); // TODO

function PrefixedValue(const AValue: FG; const PrefixType: TPrefixType): TPrefixedResult;
var
  i: SG;
  NewValue: FG;
begin
  Result.Value := AValue;
  Result.Prefix := '';
  if AValue <> 0 then
  for i := MinPrefix to MaxPrefix do
  begin
    NewValue := AValue * PrefixMult[PrefixType, i];
    if (NewValue >= 1) or (i = MaxPrefix) then
    begin
      Result.Value := NewValue;
      Result.Prefix := PrefixStr[i];
      if PrefixType = ptBinary then
        Result.Prefix := Result.Prefix + 'i';
      Break;
    end;
  end;
end;

end.
