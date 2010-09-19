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

// Delphi <= 5
	NaN         =  0.0 / 0.0;
	Infinity    =  1.0 / 0.0;
	NegInfinity = -1.0 / 0.0;
//

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

	TArrayS1 = array[0..1024 * 1024 * 1024 - 1] of S1;
	PArrayS1 = ^TArrayS1;
	TArrayU1 = array[0..1024 * 1024 * 1024 - 1] of U1;
	PArrayU1 = ^TArrayU1;
	TArrayS2 = array[0..1024 * 1024 * 1024 - 2] of S2;
	PArrayS2 = ^TArrayS2;
	TArrayU2 = array[0..1024 * 1024 * 1024 - 2] of U2;
	PArrayU2 = ^TArrayU2;
	TArrayS4 = array[0..512 * 1024 * 1024 - 2] of S4;
	PArrayS4 = ^TArrayS4;
	TArrayU4 = array[0..512 * 1024 * 1024 - 2] of U4;
	PArrayU4 = ^TArrayU4;
	TArrayS8 = array[0..256 * 1024 * 1024 - 2] of S8;
	PArrayS8 = ^TArrayS8;
	TArraySG = array[0..256 * 1024 * 1024 - 2] of SG;
	PArraySG = ^TArraySG;

	TArrayF8 = array[0..256 * 1024 * 1024 - 2] of F8;
	PArrayF8 = ^TArrayF8;
	TArrayFA = array[0..128 * 1024 * 1024 - 2] of FA;
	PArrayFA = ^TArrayFA;

	TArrayChar = array[0..1024 * 1024 * 1024 - 1] of AnsiChar;
	PArrayChar = ^TArrayChar;

	TIndex = SG;

// Time
const
	Second = 1000;
	Minute = 60 * Second;
	Hour = 60 * Minute;
	Day = 24 * Hour;

// Graphics
const
	MaxSpectrum = 1529;
type
	PRGB = ^TRGB;
	TRGB = packed record // RGB=3
		case Integer of
		0: (R, G, B: U1);
		1: (I: array[0..2] of U1);
		2: (RG: U2);
	end;
	PRGBA = ^TRGBA;
	TRGBA = packed record // RGBA=4
		case Integer of
		0: (R, G, B, A: U1);
		1: (L: S4);
		2: (I: array[0..3] of U1);
		3: (RG, BA: U2);
	end;

	THLSColor = packed record // 4
{		case Integer of
		0:
		(}
		H: -1..MaxSpectrum; // 2
		L: 0..255; // 1
		S: 0..255; // 1
(*		);
		1: (
		Hue: -1..MaxSpectrum; // 2
		Lightness{Lum(inary, inous)}: 0..255; // 1
		Saturation{Sat(iety)}: 0..255; // 1
		);*)
	end;

const
	MinInt = Low(Integer);

implementation

end.
