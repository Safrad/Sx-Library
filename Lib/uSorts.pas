//* File:     Lib\uSorts.pas
//* Created:  2000-08-01
//* Modified: 2005-10-08
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

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
		stShell,     // n ^ 1.2     n           N           Y

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
{$ifopt d+}
// Statistics
	SortCompared, SortSwaped: U8;
	SortMaxDepth: UG;
{$endif}

procedure SortS1(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS1; Count: UG);
procedure SortU1(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU1; Count: UG);
procedure SortS2(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS2; Count: UG);
procedure SortU2(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU2; Count: UG);
procedure SortS4(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS4; Count: UG);
procedure SortU4(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayU4; Count: UG);
procedure SortS8(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayS8; Count: UG);

procedure SortF8(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayF8; Count: UG);
procedure SortFA(const Stability: Boolean; const Reverse: Boolean; AIndex: PArraySG; AValue: PArrayFA; Count: UG);

procedure SortS(const Reverse: Boolean; AIndex: PArraySG; var AValue: array of string);
procedure SortWS(const Reverse: Boolean; AIndex: PArraySG; var AValue: array of WideString);

{
Sort(TCompare
function Compare(const Left, Right: SG;
begin

end;
}

implementation

uses uMath, uMem;
const
	MinIndex = 0;
type
	TIndex = type SG;
var
	MaxIndex: TIndex;
{$ifopt d+}
	Depth: UG;
{$endif}

procedure SortS1;
type
	TValue = type S1;
{$I Sort.inc}

procedure SortU1;
type
	TValue = type U1;
{$I Sort.inc}

procedure SortS2;
type
	TValue = type S2;
{$I Sort.inc}

procedure SortU2;
type
	TValue = type U2;
{$I Sort.inc}

procedure SortS4;
type
	TValue = type S4;
{$I Sort.inc}

procedure SortU4;
type
	TValue = type U4;
{$I Sort.inc}

procedure SortS8;
type
	TValue = type S8;
{$I Sort.inc}

procedure SortF8;
type
	TValue = type F8;
{$define F}
{$I Sort.inc}
{$undef F}

procedure SortFA;
type
	TValue = type FA;
{$define F}
{$I Sort.inc}
{$undef F}

procedure SortS;
type
	TValue = type string;
	TValue1 = U1;
{$I SortS.inc}

procedure SortWS;
type
	TValue = type WideString;
	TValue1 = U2;
{$I SortS.inc}

end.
