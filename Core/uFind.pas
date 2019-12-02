unit uFind;

interface

uses
  uTypes;

// AValue is Sorted Array
function FindS2(AValue: PArrayS2; var FromV, ToV: SG; const Value: S2; FindGroup: BG): Boolean;
function FindU2(AValue: PArrayU2; var FromV, ToV: SG; const Value: U2; FindGroup: BG): Boolean;
function FindS4(AValue: PArrayS4; var FromV, ToV: SG; const Value: S4; FindGroup: BG): Boolean;

function FindIS(AIndex: array of SG; AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
function FindS(AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;

implementation

function FindS2(AValue: PArrayS2; var FromV, ToV: SG; const Value: S2; FindGroup: BG): Boolean;
{$I Find.inc}

function FindU2(AValue: PArrayU2; var FromV, ToV: SG; const Value: U2; FindGroup: BG): Boolean;
{$I Find.inc}

function FindS4(AValue: PArrayS4; var FromV, ToV: SG; const Value: S4; FindGroup: BG): Boolean;
{$I Find.inc}

const
	MinIndex = 0;

procedure CheckIndexedValueOrder(const AIndex: array of SG; const AValue: array of string);
var
	i: SG;
begin
  for i := MinIndex to Length(AIndex) - 1 do
  begin
    Assert(AValue[AIndex[i]] <= AValue[AIndex[i]]);
  end;
end;

function FindIS(AIndex: array of SG; AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
type
	TIndex = SG;
var
	L, R, M: TIndex;
	MaxIndex: TIndex;
begin
	MaxIndex := Length(AValue) - 1;
	if MaxIndex < 0 then
	begin
		Result := False;
		FromV := -1;
		ToV := -1;
		Exit;
	end;

	if IsDebug then
    CheckIndexedValueOrder(AIndex, AValue);

	L := MinIndex;
	R := MaxIndex;
	while L < R do
	begin
		M := (L + R) div 2;

		if Value <= AValue[AIndex[M]] then
      R := M
    else
      L := M + 1;
	end;
	Result := Value = AValue[AIndex[L]];
	FromV := L;
	ToV := R;
end;

procedure CheckValueOrder(AValue: array of string);
var
  i: SG;
begin
  for i := MinIndex to Length(AValue) - 2 do
  begin
    Assert(AValue[i] <= AValue[i + 1]);
  end;
end;

function FindS(AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
type
	TIndex = SG;
var
	L, R, M: TIndex;
	MaxIndex: TIndex;
begin
	MaxIndex := Length(AValue) - 1;
	if MaxIndex < 0 then
	begin
		Result := False;
		FromV := -1;
		ToV := -1;
		Exit;
	end;

	if IsDebug then
    CheckValueOrder(AValue);

	L := MinIndex;
	R := MaxIndex;
	while L < R do
	begin
		M := (L + R) div 2;

		if Value <= AValue[M] then
      R := M
    else
      L := M + 1;
	end;
	Result := Value = AValue[L];
	FromV := L;
	ToV := R;
end;

end.
