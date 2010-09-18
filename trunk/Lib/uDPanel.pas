//* File:     Lib\uDPanel.pas
//* Created:  1999-08-01
//* Modified: 2004-09-26
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDPanel;

interface

{$R *.RES}
uses
	uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
	TDPanel = class(TPanel)
	private
		{ Private declarations }
//		FBmpOut: TDBitmap;

		FLayout: TTextLayout;
		FFontShadow: ShortInt;
		FOnPaint: TNotifyEvent;
		procedure SetLayout(Value: TTextLayout);
		procedure SetFontShadow(Value: ShortInt);

		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
//		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
	protected
		{ Protected declarations }
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure Paint; override;
		property Canvas;
	published
		{ Published declarations }
		property Caption;
		property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
		property FontShadow: ShortInt read FFontShadow write SetFontShadow default 0;
		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

procedure Register;

implementation

uses uGraph;

procedure TDPanel.SetLayout(Value: TTextLayout);
begin
	if FLayout <> Value then
	begin
		FLayout := Value;
		Invalidate;
	end;
end;

procedure TDPanel.SetFontShadow(Value: ShortInt);
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
	DefaultHandler(Message);
end;

//procedure TDPanel.WMPaint(var Message: TWMPaint);
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
			TopColor := DepthColor(1);
			BottomColor := DepthColor(3);
		end
		else
		begin
			TopColor := DepthColor(3);
			BottomColor := DepthColor(1);
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
			TopColor := DepthColor(1);
			BottomColor := DepthColor(3);
		end
		else
		begin
			TopColor := DepthColor(3);
			BottomColor := DepthColor(1);
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
		DrawCutedText(Canvas, Recta, Alignment, Layout, Caption, True, FFontShadow);
	end;
{	FBmpOut.SetSize(Width, Height);
	FBmpOut.Bar(clRed,0
	BitBlt(Message.DC, 0, 0, FBmpOut.Width, FBmpOut.Height,
		FBmpOut.Canvas.Handle,
		0, 0,
		SRCCOPY);}

	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDPanel]);
end;

end.
