unit uSxLabel;

interface

uses
	SysUtils, Classes, Controls, StdCtrls, Messages;

type
	TSxLabel = class(TLabel)
	private
		{ Private declarations }
	protected
		{ Protected declarations }
		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
  public
    { Public declarations }
  published
    { Published declarations }
  end;

procedure Register;

implementation

uses Graphics;

procedure TSxLabel.WMPaint(var Message: TWMPaint);
var
	ps : TPenStyle;
  bc : TColor ;
begin
  inherited ;
  if csDesigning in ComponentState then
  begin
    ps := Canvas.Pen.Style ;
    bc := Canvas.Brush.Color ;
    Canvas.Pen.Style := psDot;
    Canvas.Brush.Color := Color;
    Canvas.PolyLine([Point(0, 0),
                     Point(Width - 1, 0),
                     Point(Width - 1, Height - 1),
                     Point(0, Height - 1),
                     Point(0, 0)]);
    Canvas.Pen.Style := ps ;
    Canvas.Brush.Color := bc ;
  end;
end;

procedure Register;
begin
  RegisterComponents(ComponentPageName, [TSxLabel]);
end;

end.
