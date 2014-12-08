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
	FillOrderUG(StartNames, Length(StartNames));
	SetSysColors(Length(StartColors), StartNames, StartColors);
end;

initialization
{$IFNDEF NoInitialization}
	StoreSystemColors;
{$ENDIF NoInitialization}
end.
