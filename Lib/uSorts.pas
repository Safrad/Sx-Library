// Build: 08/2000-04/2001 Author: Safranek David

unit uSorts;

interface

uses uAdd;
type
	TSortType = (
		//              CPU         Mem         Stability   For Sorted Array
		stInsertion, // n ^ 2       n           Y           Y
		stBubble,    // n ^ 2       n           Y           Y
		stSelection, // n ^ 2       n           Y           Y
		stSelectionS,// n ^ 2       n           N           Y
		stExchange,  // n ^ 2       n           Y           Y
		stShell,     // n ^ 2       n           N           Y
		stHeap,      // n * log n   n + n       N           N
		stMerge,     // n * log n   n + log n   Y           N
		stQuick,     // n * log n   n + log n   N           N
		stRadix);    //                         Y           N

{$ifopt d+}
var
	Compared, Swaped: U64;
	MaxDepth: UG;
{$endif}

procedure SortS8(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S8);
procedure SortU8(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U8);
procedure SortS16(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S16);
procedure SortU16(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U16);
procedure SortS32(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S32);
procedure SortU32(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U32);
procedure SortS64(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S64);
procedure SortS(const SortType: TSortType; var AIndex: array of SG; var AValue: array of string);
procedure SortWS(const SortType: TSortType; var AIndex: array of SG; var AValue: array of WideString);

implementation

const
	MinIndex = 0;
type
	TIndex = type SG;
var
	MaxIndex: TIndex;
	Depth: UG;

procedure SortS8(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S8);
type
	TValue = type S8;
{$I Sort.inc}
procedure SortU8(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U8);
type
	TValue = type U8;
{$I Sort.inc}
procedure SortS16(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S16);
type
	TValue = type S16;
{$I Sort.inc}
procedure SortU16(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U16);
type
	TValue = type U16;
{$I Sort.inc}
procedure SortS32(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S32);
type
	TValue = type S32;
{$I Sort.inc}
procedure SortU32(const SortType: TSortType; var AIndex: array of SG; var AValue: array of U32);
type
	TValue = type U32;
{$I Sort.inc}
procedure SortS64(const SortType: TSortType; var AIndex: array of SG; var AValue: array of S64);
type
	TValue = type S64;
{$I Sort.inc}
procedure SortS(const SortType: TSortType; var AIndex: array of SG; var AValue: array of string);
type
	TValue = type string;
{$I Sort.inc}
procedure SortWS(const SortType: TSortType; var AIndex: array of SG; var AValue: array of WideString);
type
	TValue = type WideString;
{$I Sort.inc}

end.
