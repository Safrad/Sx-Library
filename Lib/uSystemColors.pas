//* File:     Lib\uSystemColors.pas
//* Created:  2000-05-01
//* Modified: 2007-05-08
//* Version:  1.1.40.9
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uSystemColors;

interface

uses
	uTypes,
	Windows, Graphics;

procedure SetSystemColors(const Names: array of SG; const Colors: array of TColor);
procedure RestoreSystemColors;

var
	StartColors: array[0..COLOR_ENDCOLORS] of TColor; // Read Only

implementation

uses
	uMath;

procedure SetSystemColors(const Names: array of SG; const Colors: array of TColor);
begin
	Assert(Length(Names) = Length(Colors));
	SetSysColors(Length(Names), Names, Colors);
end;

procedure StoreSystemColors;
var i: SG;
begin
	for i := 0 to Length(StartColors) - 1 do
		StartColors[i] := GetSysColor(i);
end;

procedure RestoreSystemColors;
var
	StartNames: array[0..COLOR_ENDCOLORS] of SG;
begin
	FillOrderU4(StartNames, Length(StartNames));
	SetSysColors(Length(StartColors), StartNames, StartColors);
end;

initialization
	StoreSystemColors;
end.
