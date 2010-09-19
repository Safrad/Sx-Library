//* File:     Lib\uTypes.pas
//* Created:  1998-01-01
//* Modified: 2006-01-25
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uTypes;

interface

const
	MyName = 'David Safranek (Safrad)';
	MyEMail = 'safrad at email.cz';
	MyWeb = 'http://safrad.webzdarma.cz';

{$ifdef VER130} // Delphi = 5
	NaN         =  0.0 / 0.0;
	Infinity    =  1.0 / 0.0;
	NegInfinity = -1.0 / 0.0;
{$endif}

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

	FG = Real; // Double for Delphi 6
	F4 = Single;
//	F6 = Real48;
	F8 = Double;
	FA = Extended;

{ CG = Char; // AnsiChar for Delphi 6
	C1 = AnsiChar;
	C2 = WideChar;

	TG = string; // AnsiString for Delphi 6
	TA1 = ShortString;
	T1 = AnsiString;
	T2 = WideString;}

	BG = Boolean; // LongBool;
	// Boolean // $00 / $01
	B1 = ByteBool; // $00 / $FF
	B2 = WordBool; // $0000 / $FFFF
	B4 = LongBool; // $0000 / $FFFFFFFF
const
	KB = 1024;
	MB = 1024 * KB;
	GB = 1024 * MB;
type
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

	TArrayChar = array[0..GB - 1] of AnsiChar;
	PArrayChar = ^TArrayChar;

	TArrayString = array[0..512 * MB - 2] of string;
	PArrayString = ^TArrayString;

	TIndex = SG;

// Time
const
	MiliSecond = 1;
	Second = 1000;
	Minute = 60 * Second;
	Hour = 60 * Minute;
	Day = 24 * Hour;

	LoopSleepTime = 40;

const
	MinInt = Low(Integer);

	{$EXTERNALSYM WM_XBUTTONDOWN}
	WM_XBUTTONDOWN      = $020B;
	{$EXTERNALSYM WM_XBUTTONUP}
	WM_XBUTTONUP        = $020C;
	{$EXTERNALSYM WM_XBUTTONDBLCLK}
	WM_XBUTTONDBLCLK    = $020D;

implementation

initialization
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}
end.
