unit uFind;

interface

uses uTypes;
// AValue is Sorted Array
function FindS2(AValue: PArrayS2; var FromV, ToV: SG; const Value: S2; FindGroup: BG): Boolean;
function FindU2(AValue: PArrayU2; var FromV, ToV: SG; const Value: U2; FindGroup: BG): Boolean;
function FindS4(AValue: PArrayS4; var FromV, ToV: SG; const Value: S4; FindGroup: BG): Boolean;

function FindIS(AIndex: array of SG; AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
function FindS(AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;

(*
function FindBM(SubStr, Str: string): SG;
function FindKMP(SubStr, Str: string): SG;
*)
function SearchHamming(const Pattern, Text: AnsiString; ErrorLen: SG = 0): SG; // 10x slower that Pos();
function LevenshteinDistance(const A1, A2: string; const ACostInsert: integer = 1; const ACostDel: integer = 1; const ACostRep: integer = 1): integer;

implementation

uses
  Math,
	uStrings, SysUtils, uCharset;

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

	if IsDebug then
    CheckIndexedValueOrder(AIndex, AValue);

	L := MinIndex;
	R := MaxIndex;
	while L < R do
	begin
		M := (L + R) div 2;

		if Value <= AValue[AIndex[M]] then R := M else L := M + 1;
	end;
	Result := Value = AValue[AIndex[L]];
	FromV := L;
	ToV := R;
end;

procedure CheckValueOrder(AValue: array of string);
var
  i: SG;
begin
  for i := MinIndex to Length(AValue) - 1 do
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

		if Value <= AValue[M] then R := M else L := M + 1;
	end;
	Result := Value = AValue[L];
	FromV := L;
	ToV := R;
end;



(*
// Knuth, Morris, Pratt (KMP)
function FindKMP(SubStr, Str: string): SG;
label LNFound;
var
	i, j, k: SG;
	M, N: SG;
	Fails: array of SG;

begin
	M := Length(SubStr);
	N := Length(Str);

	SetLength(Fails, M + 1);
	for i := 1 to M do
	begin
		if i <= 1 then Fails[i] := 0
		else
		begin
			k := 0;
			while SubStr[k + 1] = SubStr[i - k] do
			begin
				if k >= M then Break;
				Inc(k);
			end;
			if k > 0 then
			begin
				if SubStr[i] <> SubStr[k + 1] then
					Fails[i] := k + 1
				else
					Fails[i] := Fails[k + 1];
			end
			else
			begin
				if i = 1 then
					Fails[i] := 0
				else
					Fails[i] := 1;

			end;
		end;
	end;

	i := 1; j := 1; Result := 0;
	while (i <= M) and (j <= N) do
	begin
		if SubStr[i] = Str[j] then
			if i = M then
			begin
				Result := j - M;
				Exit;
			end
			else
			begin
				Inc(i);
				Inc(j);
			end
		else
			if Fails[i] = 0 then
			begin
				Inc(i);
				Inc(j);
			end
			else
				i := Fails[i];
	end;
	Result := 0;
	i := 0;
	while i <= Length(Str) - Length(SubStr) do
	begin
		for j := 0 to Length(SubStr) - 1 do
			if Str[i + j + 1] <> SubStr[j + 1] then goto LNFound;
		Result := i + 1;
		Exit;
		LNFound:
		i := i + 1 + j;
	end;

	SetLength(Fails, 0);
end;

// Boyer-Moore (BM)
var
	Fail1: array[0..255] of SG;
	Fail2, F: array of SG;
procedure FillFail(P: string; M: SG);
var i, j, t, k, tp: SG;
begin
	for i := 0 to 255 do
		Fail1[i] := M;
	SetLength(Fail2, 0);
	SetLength(Fail2, M + 2);
	SetLength(F, M + 1);
	for j := M downto 1 do
	begin
		if Fail1[Ord(P[j])] = M then Fail1[Ord(P[j])] := M - j;
		Fail2[j] := 2 * M - j;
	end;
	j := M; t := M + 1;
	while j > 0 do
	begin
		F[j] := t;
		while (t <= M) and (P[j] <> P[t]) do
		begin
			Fail2[t] := Min(Fail2[t], M - j);
			t := F[t];
		end;
		Dec(j);
		Dec(t);
	end;
	for k := 1 to t do Fail2[k] := Min(Fail2[k], M + t - k);
	tp := F[t];
	while (t <= tp) do
	begin
		Fail2[t] := Min(Fail2[t], tp - t + M);
		Inc(t);
	end;
	SetLength(F, 0);
end;

function FindBM(SubStr, Str: string): SG;
var
	M, N: SG;
	i, j: SG;
begin
	Result := 0;
	M := Length(SubStr);
	N := Length(Str);
	FillFail(SubStr, M);

	j := M;
	while j <= N do
	begin
		i := M;
		while (i > 0) and (Str[j] = SubStr[i]) do
		begin
			Dec(i);
			Dec(j);
		end;
		if i = 0 then
		begin
			if Result = 0 then
			begin
				Result := j + 1;
			end;
			j := j + M;
//			Exit;
		end
		else
			j := j + Max(Fail1[Ord(Str[j])], Fail2[i]);
	end;
end;
*)

function SearchHamming(const Pattern, Text: AnsiString; ErrorLen: SG = 0): SG;
const
	Empty = U8($FFFFFFFFFFFFFFFF);
var
	R: array[0..7] of U8;
	D: array[AnsiChar] of U8;
	L: U8;
	i, j: SG;
	c: AnsiChar;
begin
	Result := 0;
	// bit paralelism version Shift-Or
	if ErrorLen > Length(R) then ErrorLen := Length(R);
	if Length(Pattern) <= 0 then Exit;
	if Length(Pattern) >= 64 then Exit;

	// Precalculate
	for c := Low(c) to High(c) do
	begin
		D[c] := Empty;
		for i := Length(Pattern) downto 1 do
		begin
			D[c] := D[c] shl 1;
			if c <> Pattern[i] then
				D[c] := D[c] or 1;
		end;
	end;

	L := 1 shl (Length(Pattern) - 1);

	R[0] := Empty;
	for j := 1 to ErrorLen do
		R[j] := Empty;
	for i := 1 to Length(Text) do
	begin
		for j := ErrorLen downto 1 do
		begin
			R[j] :=
				((R[j] shl 1) or D[Text[i]]) and // Hamming
				//((R[j - 1] and LR[j - 1])shl 1) and // Levenshtein
				(R[j - 1] shl 1 {or V});
		end;
		R[0] := (R[0] shl 1) or D[Text[i]];
		if (R[0] and L) = 0 then
		begin
			Result := i - Length(Pattern) + 1;
			Exit;
		end;
		for j := 1 to ErrorLen do
			if (R[j] and L) = 0 then
			begin
				Result := i - Length(Pattern) + 1;
				Exit;
			end;

	end;
end;

function LevenshteinDistance(const A1, A2: string; const ACostInsert: integer = 1; const ACostDel: integer = 1; const ACostRep: integer = 1): integer;
var
  ari1, ari2, aris: PIntegerArray;
  i1: integer;
  i2: integer;
  iC0: integer;
  iC1: integer;
  iCount: integer;
  iCount2: integer;
begin
  i1 := length(A1);
  i2 := length(A2);
  if i1 = 0 then
  begin
    Result := i2 * ACostInsert;
    exit;
  end;
  if i2 = 0 then
  begin
    Result := i1 * ACostDel;
    exit;
  end;

  GetMem(ari1, sizeof(integer) * (i2 + 1));
  try
    for iCount := 0 to i2 do
      ari1[iCount] := iCount * ACostInsert;

    GetMem(ari2, sizeof(integer) * (i2 + 1));
    try
      for iCount := 0 to i1-1 do
      begin
        ari2[0] := ari1[0] + ACostDel;
        for iCount2 := 0 to i2-1 do
        begin
          iC0 := ari1[iCount2];
          if A1[iCount + 1] <> A2[iCount2 + 1] then
            iC0 := iC0 + ACostRep;
          iC1 := ari1[iCount2 + 1] + ACostDel;
          if iC1 < iC0 then
            iC0 := iC1;
          iC1 := ari2[iCount2] + ACostInsert;
          if iC1 < iC0 then
            iC0 := iC1;
          ari2[iCount2 + 1] := iC0;
        end;
        aris := ari1;
        ari1 := ari2;
        ari2 := aris;
      end;
    finally
      FreeMem(ari2);
    end;
    Result := ari1[i2];
  finally
    FreeMem(ari1);
  end;
end;

end.
