unit uSortVariant;

interface

uses
  uTypes;

procedure SortVariant(const AIndex: PArraySG; const AVariant: array of Variant; const Count: SG; const Reverse: BG = False);

implementation

uses
  Variants,
  Math,
  uSorts;

procedure SortVariant(const AIndex: PArraySG; const AVariant: array of Variant; const Count: SG; const Reverse: BG = False);
var
	VarTyp: TVarType;
	AStr: array of string;
	AInteger: array of S4;
	AFloat: array of {$ifndef CPUX64}FA{$else}F8{$endif};
	AInt64: array of S8;
  i: SG;
begin
  if Count = 0 then Exit; // No Data

	VarTyp := VarType(AVariant[0]);
  case VarTyp of
  varSmallint, varInteger, varBoolean, varShortInt, varByte, varWord, varLongWord:
  begin
    SetLength(AInteger, Count);
    for i := 0 to Count - 1 do
    begin
      if VarType(AVariant[i]) = VarTyp then
      begin
        AInteger[i] := AVariant[i];
      end
      else
        AInteger[i] := MaxInt;
    end;
    SortS4(True, Reverse, AIndex, PArrayS4(AInteger), Count);
  end;
  varInt64, varUInt64:
  begin
    SetLength(AInt64, Count);
    for i := 0 to Count - 1 do
  		AInt64[i] := AVariant[i];
		SortS8(True, Reverse, AIndex, PArrayS8(AInt64), Count);
  end;
  varSingle, varDouble, varCurrency, varDate:
  begin
    SetLength(AFloat, Count);
    for i := 0 to Count - 1 do
    begin
      if VarType(AVariant[i]) = VarTyp then
      begin
  			AFloat[i] := AVariant[i];
      end
      else
  			AFloat[i] := {$ifndef CPUX64}MaxExtended{$else}MaxDouble{$endif};
    end;
    {$ifndef CPUX64}
		SortFA(True, Reverse, AIndex, PArrayFA(AFloat), Count);
    {$else}
		SortF8(True, Reverse, AIndex, PArrayF8(AFloat), Count);
    {$endif}
  end;
  varOleStr, varString{$ifdef UniCode}, varUString{$endif}:
  begin
    SetLength(AStr, Count);
    for i := 0 to Count - 1 do
  		AStr[i] := AVariant[i];
  	SortStr(AIndex, PArrayString(AStr), Count, Reverse);
  end;
  end;
end;

end.
