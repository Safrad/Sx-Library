unit uVariant;

interface

uses
  uTypes,
  uOutputFormat;

function IsVariantIntegerNumber(const AValue: Variant): BG;
function IsVariantFloatNumber(const AValue: Variant): BG;
function IsVariantNumber(const AValue: Variant): BG;
function VariantToString(const AValue: Variant; const AOutputFormat: TOutputFormat): string;

implementation

uses
  Variants;

function IsVariantIntegerNumber(const AValue: Variant): BG;
begin
  Result := VarType(AValue) in [varSmallint, varInteger, varInt64, varUInt64, varShortInt, varByte, varWord, varLongWord];
end;

function IsVariantFloatNumber(const AValue: Variant): BG;
begin
  Result := VarType(AValue) in [varSingle, varDouble];
end;

function IsVariantNumber(const AValue: Variant): BG;
begin
  Result := IsVariantIntegerNumber(AValue) or IsVariantFloatNumber(AValue);
end;

function VariantToString(const AValue: Variant; const AOutputFormat: TOutputFormat): string;
begin
  if AOutputFormat = ofIO then
    Result := VarToStr(AValue)
  else
  begin
    if IsVariantIntegerNumber(AValue) then
      Result := NToS(AValue, AOutputFormat)
    else if IsVariantFloatNumber(AValue) then
      Result := FToS(AValue, AOutputFormat)
    else
      Result := VarToStr(AValue);
  end;
end;


end.
