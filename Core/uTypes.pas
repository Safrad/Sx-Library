unit uTypes;

interface

const
  IsDebug = {$ifopt d+}True{$else}False{$endif};
  IsRelease = not IsDebug;

	MinInt = Low(Integer);

	BitsPerByte = 8;
	KB = 1024;
	MB = 1024 * KB;
	GB = 1024 * MB;

type
{$ifdef CPUX64}
	SG = NativeInt;
	UG = NativeUInt;
{$else}
	SG = Integer;
	UG = Cardinal;
{$endif}
	S1 = ShortInt;
	U1 = Byte;
	S2 = SmallInt;
	U2 = Word;
	S4 = LongInt;
	U4 = LongWord;
	S8 = Int64;
	U8 = {$if CompilerVersion >= 23}UInt64{$else}Int64{$ifend};
  TNative = {$ifdef CPUX64}U8{$else}U4{$endif};

	PSG = ^SG;
	PUG = ^UG;
	PS1 = ^S1;
	PU1 = ^U1;
	PS2 = ^S2;
	PU2 = ^U2;
	PS4 = ^S4;
	PU4 = ^U4;
	PS8 = ^S8;
	PU8 = ^U8;

	TS2 = record
		case Integer of
		0: (
				B0: U1;
				B1: S1);
		1: (
				A: S2);
	end;
	TU2 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1);
		1: (
				A: U2);
	end;
	TS4 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: S1);
		1: (
				W0: U2;
				W1: S2);
		2: (
			A: S4);
	end;
	TU4 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1);
		1: (
				W0: U2;
				W1: U2);
		2: (
			A: U4);
	end;
	TS8 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: S1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: S2);
		2: (
			D0: U4;
			D1: S4);
		3: (
			A: U8);
	end;
	TU8 = record
		case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: U1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: U2);
		2: (
			D0: U4;
			D1: U4);
		3: (
			A: U8);
	end;
	TS16 = record
	case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: U1;
				B8: U1;
				B9: U1;
				B10: U1;
				B11: U1;
				B12: U1;
				B13: U1;
				B14: U1;
				B15: S1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: U2;
				W4: U2;
				W5: U2;
				W6: U2;
				W7: S2);
		2: (
			D0: U4;
			D1: U4;
			D2: U4;
			D3: S4);
		3: (
			Q0: U8;
			Q1: S8);
	end;
	TU16 = record
	case Integer of
		0: (
				B0: U1;
				B1: U1;
				B2: U1;
				B3: U1;
				B4: U1;
				B5: U1;
				B6: U1;
				B7: U1;
				B8: U1;
				B9: U1;
				B10: U1;
				B11: U1;
				B12: U1;
				B13: U1;
				B14: U1;
				B15: U1);
		1: (
				W0: U2;
				W1: U2;
				W2: U2;
				W3: U2;
				W4: U2;
				W5: U2;
				W6: U2;
				W7: U2);
		2: (
			D0: U4;
			D1: U4;
			D2: U4;
			D3: U4);
		3: (
			Q0: U8;
			Q1: U8);
	end;

	F4 = Single;
//	F6 = Real48;
	F8 = Double;
{$ifndef CPUX64}
  FA = Extended;
  FM = Extended;
{$else}
  Extended = Double deprecated; // same as Double for 64 bit
  FM = Double;
{$endif}
  FG = F8;


{ CG = Char;
	C1 = AnsiChar;
	C2 = WideChar;

	TG = string;
	TA1 = ShortString;
	T1 = AnsiString;
	T2 = UnicodeString;}

	BG = Boolean; // LongBool;
	// Boolean // $00 / $01
	B1 = ByteBool; // $00 / $FF
	B2 = WordBool; // $0000 / $FFFF
	B4 = LongBool; // $0000 / $FFFFFFFF

	TIndex = SG;

 	PStringPair = ^TStringPair;
	TStringPair = record
		Name: string; // or Key
		Value: string;
	end;

	PFloPoint = ^TFloPoint;
	TFloPoint = packed record
		X, Y: F8;
	end;

	PFloRect = ^TFloRect;
	TFloRect = packed record
		case Integer of
			0: (Left, Top, Right, Bottom: F8);
			1: (TopLeft, BottomRight: TFloPoint);
	end;

	PRange = ^TRange;
	TRange = record
		F, T: SG;
	end;
	TRangeArray = array of TRange;

	// Dynamic Arrays
	TArrayOfBG = array of BG;
	TArrayOfS1 = array of S1;
	TArrayOfU1 = array of U1;
	TArrayOfS2 = array of S2;
	TArrayOfU2 = array of U2;
	TArrayOfS4 = array of S4;
	TArrayOfU4 = array of U4;
	TArrayOfS8 = array of S8;
	TArrayOfU8 = array of U8;
	TArrayOfSG = array of SG;
	TArrayOfUG = array of UG;
	TArrayOfFG = array of FG;
	TArrayOfString = array of string;
	TArrayOfChar = array of Char;

	// Static Arrays
	TArrayS1 = array[0..GB - 1] of S1;
	PArrayS1 = ^TArrayS1;
	TArrayU1 = array[0..GB - 1] of U1;
	PArrayU1 = ^TArrayU1;
	TArrayS2 = array[0..GB - 2] of S2;
	PArrayS2 = ^TArrayS2;
	TArrayU2 = array[0..GB - 2] of U2;
	PArrayU2 = ^TArrayU2;
	TArrayS4 = array[0..512 * MB - 2] of S4;
	PArrayS4 = ^TArrayS4;
	TArrayU4 = array[0..512 * MB - 2] of U4;
	PArrayU4 = ^TArrayU4;
	TArrayS8 = array[0..256 * MB - 2] of S8;
	PArrayS8 = ^TArrayS8;
	TArrayU8 = array[0..256 * MB - 2] of U8;
	PArrayU8 = ^TArrayU8;
	TArraySG = array[0..256 * MB - 2] of SG;
	PArraySG = ^TArraySG;

	TArrayF4 = array[0..512 * MB - 2] of F4;
	PArrayF4 = ^TArrayF4;
	TArrayF8 = array[0..256 * MB - 2] of F8;
	PArrayF8 = ^TArrayF8;
{$ifndef CPUX64}
	TArrayFA = array[0..128 * MB - 2] of FA;
	PArrayFA = ^TArrayFA;
{$endif}

	TArrayChar = array[0..512 * MB - 1] of Char;
	PArrayChar = ^TArrayChar;

	TArrayString = array[0..{$ifdef CPUX64}256{$else}512{$endif} * MB - 2] of string;
	PArrayString = ^TArrayString;

{
Value	Meaning
-1 The string pointed to by the Index0 parameter is less in lexical value than the string pointed to by the Index1 parameter.
0  The string pointed to by Index0 is equal in lexical value to the string pointed to by Index1.
+1 The string pointed to by Index0 is greater in lexical value than the string pointed to by Index1.
}
  TCompareResult = (crFirstLess = -1, crBothSame = 0, crFirstGreater = 1);

	TMessageLevel = (
		mlConfirmation,
		mlDebug, // Debug-level messages (Opening file)
		mlInformation, // Informational (Started, Finished)
//		mlNotice, // (ltHint) Normal but significant condition
		mlWarning, // Warning conditions (File already opened)
		mlError, // Error conditions (File not found)
		mlFatalError, // (ltFatalError) Critical conditions
		// lmAlert, // Action must be taken immediately
		// lmEmerg, // Emergencies - system is unusable
		mlNone);

{$ifdef ANDROID}
  AnsiChar = U1;
{$endif}

const
	Second = 1000;
	Minute = 60 * Second;
	Hour = 60 * Minute;
	Day = 24 * Hour;
	DaysInWeek = 7;
	MonthsInYear = 12;
	MSecsPerWeek = DaysInWeek * Day;
	MSecsPerYear = 365 * U8(Day);

	LoopSleepTime = 40; // [ms], 25 interrupts per second.

var
  MinU8: U8 = 0;
  MaxU8: U8 = 18446744073709551615;

procedure AssertEqual(const ActualValue: SG; const ReferentialValue :SG);
procedure AssertRange(const ActualValue: SG; const MinValue, MaxValue :SG);

function CreateRange(const AF, AT: SG): TRange;

implementation

procedure AssertEqual(const ActualValue: SG; const ReferentialValue :SG);
begin
	Assert(ActualValue = ReferentialValue);
end;

procedure AssertRange(const ActualValue: SG; const MinValue, MaxValue :SG);
begin
	Assert((ActualValue >= MinValue) and (ActualValue <= MaxValue));
end;

function CreateRange(const AF, AT: SG): TRange;
begin
  Result.F := AF;
  Result.T := AT;
end;

end.
