// Build: 08/2000-04/2001 Author: Safranek David

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
	Compared, Swaped: U64;
	MaxDepth: UG;
{$endif}

procedure SortS8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S8);
procedure SortU8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U8);
procedure SortS16(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S16);
procedure SortU16(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U16);
procedure SortS32(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S32);
procedure SortU32(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U32);
procedure SortS64(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S64);
procedure SortS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of string);
procedure SortWS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of WideString);

implementation

const
	MinIndex = 0;
type
	TIndex = type SG;
var
	MaxIndex: TIndex;
	Depth: UG;

procedure SortS8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S8);
type
	TValue = type S8;
{$I Sort.inc}

procedure SortU8(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U8);
type
	TValue = type U8;
{$I Sort.inc}

procedure SortS16(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S16);
type
	TValue = type S16;
{$I Sort.inc}

procedure SortU16(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U16);
type
	TValue = type U16;
{$I Sort.inc}

procedure SortS32(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S32);
type
	TValue = type S32;
{$I Sort.inc}

procedure SortU32(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of U32);
type
	TValue = type U32;
{$I Sort.inc}

procedure SortS64(const SortType: TSortType; const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of S64);
type
	TValue = type S64;
{$I Sort.inc}

procedure SortS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of string);
type
	TValue = type string;
	TValue1 = U8;
{$I SortS.inc}

procedure SortWS(const Stability: Boolean; const Reverse: Boolean; var AIndex: array of SG; var AValue: array of WideString);
type
	TValue = type WideString;
	TValue1 = U16;
{$I SortS.inc}

end.
