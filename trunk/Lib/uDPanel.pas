// Build: 08/1999-08/1999 Author: Safranek David

unit uDPanel;

interface

{$R *.RES}
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
	TDPanel = class(TPanel)
	private
		{ private declarations }
		FLayout: TTextLayout;
		FFontShadow: ShortInt;
		FOnPaint: TNotifyEvent;
		procedure SetLayout(Value: TTextLayout);
		procedure SetFontShadow(Value: ShortInt);
	protected
		{ Protected declarations }
	public
		{ public declarations }
		constructor Create(AOwner: TComponent); override;
		procedure Paint; override;
		property Canvas;
	published
		{ published declarations }
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

procedure TDPanel.Paint;
var
	Recta: TRect;
	TopColor, BottomColor: TColor;
	i: Integer;
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
		if FFontShadow <> 0 then
		begin
			Canvas.Font.Color := ShadowColor(Font.Color);
			i := FFontShadow;
			repeat
				OffsetRect(Recta, i, i);
				DrawCutedText(Canvas, Recta, Alignment, Layout, Caption);
				OffsetRect(Recta, -i, -i);
				if FontShadow > 0 then Dec(i) else Inc(i);
			until i = 0;
			Canvas.Font.Color := Font.Color;
		end;
		DrawCutedText(Canvas, Recta, Alignment, Layout, Caption);
	end;
	// if FOnPaint<>nil then call OnPaint(Sender);
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDPanel]);
end;

end.
