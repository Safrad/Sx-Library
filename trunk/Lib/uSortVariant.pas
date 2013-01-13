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
	AInteger: array of SG;
	AFloat: array of FA;
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
  varInt64:
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
  			AFloat[i] := MaxExtended;
    end;
		SortFA(True, Reverse, AIndex, PArrayFA(AFloat), Count);
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
