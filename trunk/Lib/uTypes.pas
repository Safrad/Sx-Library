// * File:     Lib\uTypes.pas
// * Created:  1998-01-01
// * Modified: 2009-12-29
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uTypes;

interface

uses TypInfo;

const
{$ifdef VER130} // Delphi = 5
	NaN         =  0.0 / 0.0;
	Infinity    =  1.0 / 0.0;
	NegInfinity = -1.0 / 0.0;
	SwitchChars = ['-', '/'];
{$endif}
	MinInt = Low(Integer);

	BitsPerByte = 8;
	KB = 1024;
	MB = 1024 * KB;
	GB = 1024 * MB;

type
	SG = Integer; // LongInt for 32bit Delphi
	UG = Cardinal; // LongWord for 32bit Delphi
	S1 = ShortInt;
	U1 = Byte;
	S2 = SmallInt;
	U2 = Word;
	S4 = LongInt;
	U4 = LongWord;
	S8 = Int64;
	U8 = Int64; // Car64 for 64bit Delphi?

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

	FG = Real; // Double
	F4 = Single;
//	F6 = Real48;
	F8 = Double;
	FA = Extended;

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

	TStringPair = record
		Name: string; // or Key
		Value: string;
	end;

	PFloPoint = ^TFloPoint;
	TFloPoint = packed record
		X, Y: Double;
	end;

	PFloRect = ^TFloRect;
	TFloRect = packed record
		case Integer of
			0: (Left, Top, Right, Bottom: Double);
			1: (TopLeft, BottomRight: TFloPoint);
	end;

	// Dynamic Arrays
	TArrayOfBG = array of BG;
	TArrayOfSG = array of SG;
	TArrayOfString = array of string;

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
	TArraySG = array[0..256 * MB - 2] of SG;
	PArraySG = ^TArraySG;

	TArrayF8 = array[0..256 * MB - 2] of F8;
	PArrayF8 = ^TArrayF8;
	TArrayFA = array[0..128 * MB - 2] of FA;
	PArrayFA = ^TArrayFA;

	TArrayChar = array[0..512 * MB - 1] of Char;
	PArrayChar = ^TArrayChar;

	TArrayString = array[0..512 * MB - 2] of string;
	PArrayString = ^TArrayString;

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

const
	MiliSecond = 1;
	Second = 1000;
	Minute = 60 * Second;
	Hour = 60 * Minute;
	Day = 24 * Hour;
	DaysInWeek = 7;
	MonthsInYear = 12;
	MSecsPerWeek = DaysInWeek * Day;
	MSecsPerYear = 365 * DaysInWeek * U8(Day);

// System
	LoopSleepTime = {$ifopt d-}40{$else}40{$endif}; // [ms], 25 interrupts per second.
	MouseTolerance = 4; // 0=1 pixel..6=15 pixels, 3 pixels: Delphi panel, 13 pixels: Delphi Table

	{$EXTERNALSYM WM_XBUTTONDOWN}
	WM_XBUTTONDOWN      = $020B;
	{$EXTERNALSYM WM_XBUTTONUP}
	WM_XBUTTONUP        = $020C;
	{$EXTERNALSYM WM_XBUTTONDBLCLK}
	WM_XBUTTONDBLCLK    = $020D;

	const
	HTMLExt = '.html'; // Could be also ".htm", ".php", ".php3", ".php4".
	IndexFile = 'index' + HTMLExt;
	nbsp = '&nbsp;'; // Non-dividable Blank SPace.

procedure AssertEqual(const ActualValue: SG; const ReferentialValue :SG);
procedure AssertRange(const ActualValue: SG; const MinValue, MaxValue :SG);

{$ifdef VER150}
procedure CopyArray(const Dest: Pointer; const Source: Pointer; const T: PTypeInfo; const Count: SG);
{$endif}

{$ifndef UNICODE}
type
	UnicodeString = WideString;

{ Standard Character set type }

	TSysCharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload;
function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean; overload;
{$endif}

implementation

procedure AssertEqual(const ActualValue: SG; const ReferentialValue :SG);
begin
	Assert(ActualValue = ReferentialValue);
end;

procedure AssertRange(const ActualValue: SG; const MinValue, MaxValue :SG);
begin
	Assert((ActualValue >= MinValue) and (ActualValue <= MaxValue));
end;

{$ifndef UNICODE}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
	Result := C in CharSet;
end;

function CharInSet(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
	Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;
{$endif}

{$ifdef VER150}
procedure CopyArray(const Dest: Pointer; const Source: Pointer; const T: PTypeInfo; const Count: SG);
begin
	Move(Source^, Dest^, GetTypeData(T).elSize * Count);
end;
{$endif}

initialization
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}

	{$ifopt d+}
	{$ifdef UNICODE}
	ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
	{$endif}
	{$endif}
end.
