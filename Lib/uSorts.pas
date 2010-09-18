//* File:     Lib\uSorts.pas
//* Created:  2000-08-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uSorts;

interface

uses uAdd;
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
		stShell,     // n ^ 1.2     n           N           Y

		stHeap,      // n * log n   n           N           N
		stQuick,     // n * log n   n+c*log n   N           N       +
		stMerge,     // n * log n   2 * n       Y           N       +
		stRadix,     // n           n * 8 !     Y           N       +
		stCounting,  // n           n * v !     Y           N       +
		stAuto);

//  TAIndex = array of SG;
{$ifopt d+}
// Statistics
var
	Compared, Swaped: U8;
	MaxDepth: UG;
{$endif}

procedure SortS1(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S1);
procedure SortU1(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U1);
procedure SortS2(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S2);
procedure SortU2(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U2);
procedure SortS4(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S4);
procedure SortU4(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U4);
procedure SortS8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S8);

procedure SortFA(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of FA);

procedure SortS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of string);
procedure SortWS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of WideString);

implementation

uses uError, Math;
const
	MinIndex = 0;
type
	TIndex = type SG;
var
	MaxIndex: TIndex;
	Depth: UG;

procedure SortS1(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S1);
type
	TValue = type S1;
{$I Sort.inc}

procedure SortU1(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U1);
type
	TValue = type U1;
{$I Sort.inc}

procedure SortS2(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S2);
type
	TValue = type S2;
{$I Sort.inc}

procedure SortU2(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U2);
type
	TValue = type U2;
{$I Sort.inc}

procedure SortS4(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S4);
type
	TValue = type S4;
{$I Sort.inc}

procedure SortU4(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U4);
type
	TValue = type U4;
{$I Sort.inc}

procedure SortS8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S8);
type
	TValue = type S8;
{$I Sort.inc}

procedure SortFA(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of FA);
type
	TValue = type FA;
{$define F}
{$I Sort.inc}
{$undef F}

procedure SortS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of string);
type
	TValue = type string;
	TValue1 = U8;
{$I SortS.inc}

procedure SortWS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of WideString);
type
	TValue = type WideString;
	TValue1 = U2;
{$I SortS.inc}

end.
