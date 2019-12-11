unit uDPanel deprecated;

interface

uses
	uTypes, uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls;

type
	TDPanel = class(TPanel)
	private
		{ Private declarations }
		FLayout: TTextLayout;
		FFontShadow: SG;
		FOnPaint: TNotifyEvent;
		procedure SetLayout(Value: TTextLayout);
		procedure SetFontShadow(Value: SG);

		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
		procedure Paint; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		property Canvas;
	published
		{ Published declarations }
		property Caption;
		property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
		property FontShadow: SG read FFontShadow write SetFontShadow default 0;
		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

implementation

uses uGraph, uColor;

procedure TDPanel.SetLayout(Value: TTextLayout);
begin
	if FLayout <> Value then
	begin
		FLayout := Value;
		Invalidate;
	end;
end;

procedure TDPanel.SetFontShadow(Value: SG);
begin
	if FFontShadow <> Value then
	begin
		FFontShadow := Value;
		Invalidate;
	end;
end;

constructor TDPanel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FLayout := tlCenter;
end;

procedure TDPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	Message.Result := 1;
end;

procedure TDPanel.Paint;
var
	Recta: TRect;
	TopColor, BottomColor: TColor;
begin
	Recta := GetClientRect;
	if BevelOuter <> bvNone then
	begin
		if BevelOuter = bvLowered then
		begin
			TopColor := clDepth[1];
			BottomColor := clDepth[3];
		end
		else
		begin
			TopColor := clDepth[3];
			BottomColor := clDepth[1];
		end;
		Border(Canvas, Recta, TopColor, BottomColor, BevelWidth);
		InflateRect(Recta, -BevelWidth, -BevelWidth);
	end;
	if Color <> clNone then
	begin
		Rec(Canvas, Recta, Color, BorderWidth);
		InflateRect(Recta, -BorderWidth, -BorderWidth);
	end;
	if BevelInner <> bvNone then
	begin
		if BevelInner = bvLowered then
		begin
			TopColor := clDepth[1];
			BottomColor := clDepth[3];
		end
		else
		begin
			TopColor := clDepth[3];
			BottomColor := clDepth[1];
		end;
		Border(Canvas, Recta, TopColor, BottomColor, BevelWidth);
		InflateRect(Recta, -BevelWidth, -BevelWidth);
	end;
	if Color <> clNone then
	begin
		Canvas.Brush.Color := Color;
		Canvas.FillRect(Recta);
	end;
	if Font.Color <> clNone then
	begin
		Canvas.Brush.Style := bsClear;
		Canvas.Font := Font;
		DrawCuttedText(Canvas, Recta, Alignment, Layout, Caption, True, FFontShadow);
	end;

	if Assigned(FOnPaint) then FOnPaint(Self);
end;

end.
