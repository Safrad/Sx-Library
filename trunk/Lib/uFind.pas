//* File:     Lib\uFind.pas
//* Created:  1999-05-01
//* Modified: 2007-05-11
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

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
function Search(Pattern, Text: string; ErrorLen: SG = 0): SG; // 10x slower that Pos();
*)

implementation

function FindS2(AValue: PArrayS2; var FromV, ToV: SG; const Value: S2; FindGroup: BG): Boolean;
{$I Find.inc}

function FindU2(AValue: PArrayU2; var FromV, ToV: SG; const Value: U2; FindGroup: BG): Boolean;
{$I Find.inc}

function FindS4(AValue: PArrayS4; var FromV, ToV: SG; const Value: S4; FindGroup: BG): Boolean;
{$I Find.inc}

function FindIS(AIndex: array of SG; AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
const
	MinIndex = 0;
type
	TIndex = SG;
var
	L, R, M: TIndex;
	MaxIndex: TIndex;
	{$ifopt d+}
	i: SG;
	{$endif}
begin
	MaxIndex := Length(AValue) - 1;

	{$ifopt d+}
	for i := MinIndex to MaxIndex - 1 do
	begin
		Assert(AValue[AIndex[i]] <= AValue[AIndex[i]]);
	end;
	{$endif}

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

function FindS(AValue: array of string;
	const Value: string; out FromV, ToV: SG): Boolean;
const
	MinIndex = 0;
type
	TIndex = SG;
var
	L, R, M: TIndex;
	MaxIndex: TIndex;
	{$ifopt d+}
	i: SG;
	{$endif}
begin
	MaxIndex := Length(AValue) - 1;
	if MaxIndex < 0 then
	begin
		Result := False;
		FromV := -1;
		ToV := -1;
		Exit;
	end;

	{$ifopt d+}
	for i := MinIndex to MaxIndex - 1 do
	begin
		Assert(AValue[i] <= AValue[i + 1]);
	end;
	{$endif}

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

function Search(Pattern, Text: string; ErrorLen: SG = 0): SG;
const
	Empty = $FFFFFFFFFFFFFFFF;
var
	R: array[0..7] of U8;
	D: array[Char] of U8;
	L: U8;
	i, j: SG;
	c: Char;
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
				// ((R[j - 1] and LR[j - 1])shl 1) and // Levenshtein
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
*)

end.
