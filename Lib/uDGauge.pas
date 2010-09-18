//* File:     Lib\uDGauge.pas
//* Created:  1999-08-01
//* Modified: 2004-08-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDGauge;

interface

{$R *.RES}
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uDBitmap, uDispl;

type
	TBuffer = (bfDynamic, bfStatic);
	TGaugeKind = (gkNormal, gkSpectrum, gkStandard);

	TDGauge = class(TLabel)
	private
		{ Private declarations }
		FBmpOut: TDBitmap;

		FBackEffect: TEffect;
		FBackPaint: Boolean;
		FFontShadow: ShortInt;
		FDispl: TDispl;

		FKind: TGaugeKind;

		FMin: Integer;
		FPosition: Integer;
		FMax: Integer;

		FBevelInner: TPanelBevel;
		FBevelOuter: TPanelBevel;
		FBevelWidth: TBevelWidth;
		FBorderWidth: TBorderWidth;
		FBorderStyle: TBorderStyle;

		procedure SetBackEffect(Value: TEffect);
		procedure SetFontShadow(Value: ShortInt);
		procedure DisplChanged(ADispl: TObject);
		procedure SetDispl(Value: TDispl);

		procedure SetKind(Value: TGaugeKind);

		procedure SetMin(Value: Integer);
		procedure SetPosition(Value: Integer);
		procedure SetMax(Value: Integer);

		procedure SetBevelInner(Value: TPanelBevel);
		procedure SetBevelOuter(Value: TPanelBevel);
		procedure SetBevelWidth(Value: TBevelWidth);
		procedure SetBorderWidth(Value: TBorderWidth);
		procedure SetBorderStyle(Value: TBorderStyle);
	protected
		{ Protected declarations }
		procedure Paint; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		{ Published declarations }
		property BackPaint: Boolean read FBackPaint write FBackPaint default False;
		property BackEffect: TEffect read FBackEffect write SetBackEffect default ef16;
		property FontShadow: ShortInt read FFontShadow write SetFontShadow default 0;
		property Displ: TDispl read FDispl write SetDispl;

		property Kind: TGaugeKind read FKind write SetKind default gkNormal;

		property Min: Integer read FMin write SetMin default 0;
		property Position: Integer read FPosition write SetPosition default 8;
		property Max: Integer read FMax write SetMax default 16;

		property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvLowered;
		property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
		property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 2;
		property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
		property Color;
		property Enabled;
		property Visible;
	end;

procedure Register;

implementation

uses uGraph, uScreen;

{function EffectToPenMode(Effect: TEffect): TPenMode;
begin
		case Effect of
		ef00..ef03:
			Result:=pmNop;
		ef04..ef12:
			Result:=pmMerge;
		ef13..ef16:
			Result:=pmCopy;
		efNeg:
			Result:=pmNot;
		efXor:
			Result:=pmXor;
		efAdd:
			Result:=pmWhite;
		efSub:
			Result:=pmBlack;
		else
			Result:=pmMask;
		end;
end;}

constructor TDGauge.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FBmpOut := nil;
	FDispl := TDispl.Create;
	FDispl.Enabled := False;
	FDispl.Format := '88';
	FDispl.SizeT := 1;
	FDispl.SizeX := 4;
	FDispl.SizeY := 4;
	FDispl.SpaceSX := 2;
	FDispl.SpaceSY := 2;
	FDispl.Spacing := 0;
	FDispl.ColorA := clRed;
	FDispl.ColorD := clMaroon;
	FDispl.OnChange := DisplChanged;

	FBackEffect := ef16;
	FMin := 0;
	FPosition := 0;
	FMax := 100;
	FBevelInner := bvLowered;
	FBevelOuter := bvRaised;
	FBevelWidth := 2;
	FBorderWidth := 0;

	AutoSize := False;
	Alignment := taCenter;
	Layout := tlCenter;
	Width := 128;
	Height := 32;
end;

destructor TDGauge.Destroy;
begin
	FDispl.Free;
	if Assigned(FBmpOut) then
	begin
		FBmpOut.Free;
		FBmpOut := nil;
	end;
	inherited Destroy;
end;

procedure TDGauge.SetBackEffect(Value: TEffect);
begin
	if FBackEffect <> Value then
	begin
		FBackEffect := Value;
		Invalidate;
	end;
end;

procedure TDGauge.SetFontShadow(Value: ShortInt);
begin
	if FFontShadow <> Value then
	begin
		FFontShadow := Value;
		Invalidate;
	end;
end;

procedure TDGauge.DisplChanged(ADispl: TObject);
begin
	Invalidate;
end;

procedure TDGauge.SetDispl(Value: TDispl);
begin
	FDispl.Assign(Value);
end;

procedure TDGauge.SetKind(Value: TGaugeKind);
begin
	if FKind <> Value then
	begin
		FKind := Value;
		Invalidate;
	end;
end;

procedure TDGauge.SetMin(Value: Integer);
begin
	if FMin <> Value then
	begin
		FMin := Value;
		Invalidate;
	end;
end;

procedure TDGauge.SetPosition(Value: Integer);
begin
	if FPosition <> Value then
	begin
		FPosition := Value;
		Invalidate;
	end;
end;

procedure TDGauge.SetMax(Value: Integer);
begin
	if FMax <> Value then
	begin
		FMax := Value;
		Invalidate;
	end;
end;

procedure TDGauge.SetBevelInner(Value: TPanelBevel);
begin
	FBevelInner := Value;
	Invalidate;
end;

procedure TDGauge.SetBevelOuter(Value: TPanelBevel);
begin
	FBevelOuter := Value;
	Invalidate;
end;

procedure TDGauge.SetBevelWidth(Value: TBevelWidth);
begin
	FBevelWidth := Value;
	Invalidate;
end;

procedure TDGauge.SetBorderWidth(Value: TBorderWidth);
begin
	FBorderWidth := Value;
	Invalidate;
end;

procedure TDGauge.SetBorderStyle(Value: TBorderStyle);
begin
	if FBorderStyle <> Value then
	begin
		FBorderStyle := Value;
		Invalidate;
	end;
end;

procedure TDGauge.Paint;
var
	Recta, RectaS: TRect;
	TopColor, BottomColor: TColor;
	X: Integer;
	C: TColor;
	i: Integer;
	Posit, MaxPosit: Integer;
	Co: array[0..3] of TColor;
begin
//  Recta:=GetClientRect;
	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Width;
	Recta.Bottom := Height;
	if (not Assigned(FBmpOut)) then
	begin
		FBmpOut := TDBitmap.Create;
	end;
	FBmpOut.SetSize(Recta.Right - Recta.Left, Recta.Bottom - Recta.Top);

	// Background
	if (Transparent = False) and (BackPaint = True) then
	begin
		FBmpOut.Canvas.Brush := Parent.Brush;
		FBmpOut.Canvas.FillRect(Recta);
	end
	else
	begin
		FBmpOut.Canvas.CopyRect(Rect(0, 0, FBmpOut.Width, FBmpOut.Height),
			Canvas, Recta);
	end;

// Border
	if (FBorderStyle <> bsNone) then
	begin
		FBmpOut.Border(clBtnShadow, clBtnHighlight, 1, BackEffect);
		FBmpOut.Border(1, 1, FBmpOut.Width - 2, FBmpOut.Height - 2,
			cl3DDkShadow, cl3DLight, 1, BackEffect);
		InflateRect(Recta, -2, -2);
	end;
	if FBevelOuter <> bvNone then
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
		FBmpOut.Border(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;
	if Color <> clNone then
	begin
		for i := 0 to FBorderWidth - 1 do
			FBmpOut.Rec(Recta.Left + i, Recta.Top + i,
				Recta.Right - i - 1, Recta.Bottom - i - 1,
				Color, BackEffect);
		InflateRect(Recta, -FBorderWidth, -FBorderWidth);
	end;
	RectaS := Recta;
	if FBevelInner <> bvNone then
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
		FBmpOut.Border(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -Integer(FBevelWidth) div 2, -Integer(FBevelWidth) div 2);
		InflateRect(RectaS, -FBevelWidth, -FBevelWidth);
	end;

// Status
	Posit := FPosition - FMin;
	MaxPosit := FMax - FMin;
	if Posit > MaxPosit then Posit := MaxPosit;
	if MaxPosit = 0 then
	begin
		X := Recta.Left;
		C := SpectrumColor((X - 1) shl 1);
	end
	else
	begin
		X := Recta.Left + (Recta.Right - Recta.Left) * Posit div MaxPosit;
		if X > Recta.Right then X := Recta.Right;
		C := SpectrumColor(512 * Posit div MaxPosit);
	end;

	if X > Recta.Left then
	begin
		case FKind of
		gkNormal:
		begin
			FBmpOut.Bar(clNone, Recta.Left, Recta.Top, X - 1, Recta.Bottom - 1,
				C, FBackEffect);
		end;
		gkSpectrum:
		begin
			for i := Recta.Left to X - 1 do
			begin
				FBmpOut.Line(i, Recta.Top, i, Recta.Bottom - 1,
					SpectrumColor(512 * i div (Recta.Right - Recta.Left)), FBackEffect);
			end;
		end;
		gkStandard:
		begin
			Co[0] := LighterColor(clBtnFace);
			Co[1] := DarkerColor(clBtnFace);
			Co[2] := Co[0];
			Co[3] := Co[1];
			FBmpOut.GenerateRGB(Recta.Left, Recta.Top, X - 1, Recta.Bottom - 1,
				clNone, gfFade2x, Co, ScreenCorrectColor, ef16, 0, nil);
		end;
		end;
	end;
	if X < RectaS.Left then X := RectaS.Left;
	if (X < RectaS.Right) then
	begin
		FBmpOut.Bar(clNone, X, RectaS.Top, RectaS.Right - 1, RectaS.Bottom - 1,
			Color, FBackEffect);
	end;

// Caption
	if (Caption <> '') {and (FFontEffect<>ef00)} then
	begin
		FBmpOut.Canvas.Brush.Style := bsClear;
		FBmpOut.Canvas.Font := Font;
		if Displ.Enabled then
		begin
			DisplDrawRect(FBmpOut, Caption, FDispl, Recta, Alignment, Layout, ef16);
		end
		else
		begin
			DrawCutedText(FBmpOut.Canvas, Recta, Alignment, Layout, Caption, True, FFontShadow);
		end;
	end;

// Draw
	Canvas.Draw(0, 0, FBmpOut);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDGauge]);
end;

end.
