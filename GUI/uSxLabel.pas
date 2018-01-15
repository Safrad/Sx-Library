unit uSxLabel;

interface

uses
	SysUtils, Classes, Controls, StdCtrls, Messages;

type
	TSxLabel = class(TLabel)
	private
    procedure WMClick(var Message: TWMMouse); message WM_LBUTTONDOWN;
		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
	end;

implementation

uses Types, Graphics, uTypes;

procedure TSxLabel.WMClick(var Message: TWMMouse);
begin
  inherited;
  if FocusControl <> nil then
    FocusControl.SetFocus;
end;

procedure TSxLabel.WMPaint(var Message: TWMPaint);
var
	PenStyle : TPenStyle;
	BrushColor : TColor ;
begin
	inherited;
	if csDesigning in ComponentState then
	begin
		PenStyle := Canvas.Pen.Style;
		BrushColor := Canvas.Brush.Color;
    try
      Canvas.Pen.Style := psDot;
      Canvas.Brush.Color := Color;
      Canvas.PolyLine([Point(0, 0), Point(Width - 1, 0), Point(Width - 1, Height - 1),Point(0, Height - 1), Point(0, 0)]);
    finally
  		Canvas.Pen.Style := PenStyle;
  		Canvas.Brush.Color := BrushColor;
    end;
	end;
end;

end.
