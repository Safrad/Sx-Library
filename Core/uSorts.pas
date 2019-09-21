unit uSorts;

interface

uses uTypes;

type
	TSortType = (
		//                                                  For
		//                                                  Sorted
		//              CPU         Mem         Stability   Array
		//------------------------------------------------------------------------
		stSelect,    // n ^ 2       n           N           N
		stInsertion, // n ^ 2       n           Y           Y       +
		stBubble,    // n ^ 2       n           Y           Y
		stExchange,  // n ^ 2       n           N           N
		stShell,     // n ^ 1.5     n           N           Y

		stHeap,      // n * log n   n           N           N
		stQuick,     // n * log n   n+c*log n   N           N       +
		stPartialQuick, // 2 times faster, but sort only some first half
		stMerge,     // n * log n   2 * n       Y           N       +
		stRadix,     // n           n * 8 !     Y           N       +
//		stCounting,  // n           n * v !     Y           N       +
		stAuto);

//  TAIndex = array of SG;
var
	SortType: TSortType = stAuto;
// Statistics
	SortCompared, SortSwaped: U8;
	SortMaxDepth: UG;

procedure SortS1(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS1; Count: UG);
procedure SortU1(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU1; Count: UG);
procedure SortS2(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS2; Count: UG);
procedure SortU2(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU2; Count: UG);
procedure SortS4(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS4; Count: UG);
procedure SortU4(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU4; Count: UG);
procedure SortS8(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS8; Count: UG);

procedure SortF8(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayF8; Count: UG);
{$ifndef CPUX64}
procedure SortFA(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayFA; Count: UG);
{$endif}

procedure SortS(const Reverse: Boolean; AIndex: PArraySG; var AValue: array of AnsiString); deprecated;
procedure SortWS(const Reverse: Boolean; AIndex: PArraySG; var AValue: array of UnicodeString); deprecated;

type TCompare = function(const Index0, Index1: SG): TCompareResult;

procedure SortStrBinary(const AIndex: PArraySG; const AString: PArrayString; const Count: SG; const Reverse: BG = False);
procedure SortStr(const AIndex: PArraySG; const AString: PArrayString; const Count: SG; const Reverse: BG = False);
// Stable Megre sort used for strings (few comparison)
procedure Sort(const AIndex: PArraySG; const Count: SG; const Compare: TCompare; const Reverse: BG = False);

function LocaleCompareText(const S1, S2: string): SG;

implementation

uses
  SysUtils,
	uMath,
  uStrings;

const
	MinIndex = 0;
type
	TIndex = SG;
var
	MaxIndex: TIndex;
	Depth: UG;

procedure SortS1;
type
	TValue = S1;
	PArray = PArrayS1;
{$I Sort.inc}

procedure SortU1;
type
	TValue = U1;
	PArray = PArrayU1;
{$I Sort.inc}

procedure SortS2;
type
	TValue = S2;
	PArray = PArrayS2;
{$I Sort.inc}

procedure SortU2;
type
	TValue = U2;
	PArray = PArrayU2;
{$I Sort.inc}

procedure SortS4;
type
	TValue = S4;
	PArray = PArrayS4;
{$I Sort.inc}

procedure SortU4;
type
	TValue = U4;
	PArray = PArrayU4;
{$I Sort.inc}

procedure SortS8;
type
	TValue = S8;
	PArray = PArrayS8;
{$I Sort.inc}

procedure SortF8;
type
	TValue = F8;
	PArray = PArrayF8;
{$define F}
{$I Sort.inc}
{$undef F}

{$ifndef CPUX64}
procedure SortFA;
type
	TValue = FA;
	PArray = PArrayFA;
{$define F}
{$I Sort.inc}
{$undef F}
{$endif}

procedure SortS;
type
	TValue = AnsiString;
	TValue1 = U1;
{$I SortS.inc}

procedure SortWS;
type
	TValue = UnicodeString;
	TValue1 = U2;
{$I SortS.inc}

var
	MeI: array of TIndex;

procedure Merge(const AIndex: PArraySG; const I1F, I1T, I2F, I2T: SG; const Compare: TCompare);
var i, j, M, c: SG;
begin
	i := I1F;
	j := I2F;
	M := 0;
	while True do
	begin
    if IsDebug then
			Inc(SortCompared);
		if Compare(AIndex[i], AIndex[j]) <> crFirstGreater then
		begin
			MeI[M] := AIndex[i];
			Inc(M);

			Inc(i);
			if i > I1T then
			begin
				if j <> I2T then
				begin
					c := I2T- j + 1;
					Move(AIndex[j], MeI[M], SizeOf(TIndex) * c);
					Inc(M, c);
				end
				else
				begin
					MeI[M] := AIndex[j];
					Inc(M);
				end;

{				while j <= I2T do
				begin
					MeI[M] := AIndex[j];
					Inc(M);
					Inc(j);
				end;}
				Break;
			end;
		end
		else
		begin
			MeI[M] := AIndex[j];
			Inc(M);

			Inc(j);
			if j > I2T then
			begin
				if i <> I1T then
				begin
					c := I1T- i + 1;
					Move(AIndex[i], MeI[M], SizeOf(TIndex) * c);
					Inc(M, c);
				end
				else
				begin
					MeI[M] := AIndex[i];
					Inc(M);
				end;

			{				while i <= I1T do
				begin
					MeI[M] := AIndex[i];
					Inc(M);
					Inc(i);
				end;}
				Break;
			end;
		end;
	end;
  if IsDebug then
		Inc(SortSwaped, M);
(*	for i := 0 to M - 1 do
	begin
    if IsDebug then
			Inc(SortSwaped);
		AIndex[i + I1F] := MeI[i];
	end; *)
	if M = 2 then
	begin
		AIndex[I1F] := MeI[0];
		AIndex[I1F + 1] := MeI[1];
	end
	else
		Move(MeI[0], AIndex[I1F], SizeOf(TIndex) * M);
end;

procedure RSort(const AIndex: PArraySG; const F, T: SG; const Compare: TCompare);
var
	I1F, I1T, I2F, I2T: SG;
begin
	Assert(F < T);
	I1F := F;
	I1T := F + (T - F) div 2; //(F + T) div 2;
	I2F := I1T + 1;
	I2T := T;

	if I1F < I1T then RSort(AIndex, I1F, I1T, Compare);
	if I2F < I2T then RSort(AIndex, I2F, I2T, Compare);
	Merge(AIndex, I1F, I1T, I2F, I2T, Compare);
end;

procedure MergeSort(const AIndex: PArraySG; const MaxIndex: TIndex; const Compare: TCompare);
begin
	if Length(MeI) < MaxIndex - MinIndex + 1 then
		SetLength(MeI, MaxIndex - MinIndex + 1);
	RSort(AIndex, MinIndex, MaxIndex, Compare);
	if MaxIndex - MinIndex + 1 > 256 * KB then
		SetLength(MeI, 0);
end;

function LocaleCompareText(const S1, S2: string): SG;
begin
	Result := AnsiCompareStr(S1, S2);
end;

var
	AStr: PArrayString;

function BinaryCompareText(const Index0, Index1: SG): TCompareResult;
begin
  if AStr[Index0] > AStr[Index1] then
    Result := crFirstGreater
  else if AStr[Index0] < AStr[Index1] then
    Result := crFirstLess
  else
    Result := crBothSame;
end;

function Compare(const Index0, Index1: SG): TCompareResult;
begin
	Result := CompareStringLogical(AStr[Index0], AStr[Index1]);
{	Result := TCompareResult(CompareString(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
		PChar(AStr[Index0]), Length(AStr[Index0]),
		PChar(AStr[Index1]), Length(AStr[Index1])) - 2);}
end;

procedure SortStr(const AIndex: PArraySG; const AString: PArrayString; const Count: SG; const Reverse: BG = False);
begin
	if Count > 1 then
	begin
		AStr := AString;
		Sort(AIndex, Count, Compare, Reverse);
	end;
end;

procedure SortStrBinary(const AIndex: PArraySG; const AString: PArrayString; const Count: SG; const Reverse: BG = False);
begin
	if Count > 1 then
	begin
		AStr := AString;
		Sort(AIndex, Count, BinaryCompareText, Reverse);
	end;
end;

procedure Sort(const AIndex: PArraySG; const Count: SG; const Compare: TCompare; const Reverse: BG = False);
begin
	if Count > 1 then
	begin
		MergeSort(AIndex, Count - 1, Compare);
		if Reverse then
			ReverseG(AIndex[0], Count);
	end;
end;

initialization

finalization
{$IFNDEF NoFinalization}
	SetLength(MeI, 0);
{$ENDIF NoFinalization}
end.
