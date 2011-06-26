unit uLayout;

interface

uses
	uTypes,
	Controls;

procedure LayoutControls(const Controls: array of TControl; const Width, Height: SG);

implementation

uses uDForm;

procedure LayoutControls(const Controls: array of TControl; const Width, Height: SG);
var
	i: SG;
	Control: TControl;
	X, Y, W, H: SG;
begin
	X := Width;
	Y := Height - FormBorder;
	for i := Length(Controls) - 1 downto 0 do
	begin
		Control := Controls[i];
		if Control.Visible then
		begin
			W := 76;
			H := 24;
			Dec(X, W + FormBorder);
			Control.SetBounds(X, Y - H, W, H);
		end;
	end;
end;


end.
