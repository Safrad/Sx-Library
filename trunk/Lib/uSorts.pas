// Build: 08/2000-08/2000 Author: Safranek David

unit uSorts;

interface

procedure QuickSortI(var AIndex: array of Integer; var AValue: array of Integer);
procedure QuickSortS(var AIndex: array of Integer; var AValue: array of string);
procedure QuickSortWS(var AIndex: array of Integer; var AValue: array of WideString);
procedure ExchangeSortI(var AIndex: array of Integer; var AValue: array of Integer);
procedure ExchangeSortS(var AIndex: array of Integer; var AValue: array of string);
procedure ExchangeSortWS(var AIndex: array of Integer; var AValue: array of WideString);

implementation

procedure QuickSortI(var AIndex: array of Integer; var AValue: array of Integer);
type
	TValue = Integer;
{$I QuickSort.inc}

procedure QuickSortS(var AIndex: array of Integer; var AValue: array of string);
type
	TValue = string;
{$I QuickSort.inc}

procedure QuickSortWS(var AIndex: array of Integer; var AValue: array of WideString);
type
	TValue = WideString;
{$I QuickSort.inc}

procedure ExchangeSortI(var AIndex: array of Integer; var AValue: array of Integer);
type
	TValue = Integer;
{$I ExchangeSort.inc}

procedure ExchangeSortS(var AIndex: array of Integer; var AValue: array of string);
type
	TValue = string;
{$I ExchangeSort.inc}

procedure ExchangeSortWS(var AIndex: array of Integer; var AValue: array of WideString);
type
	TValue = WideString;
{$I ExchangeSort.inc}

end.
