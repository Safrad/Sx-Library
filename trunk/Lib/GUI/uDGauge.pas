//* File:     Lib\GUI\uDGauge.pas
//* Created:  1999-08-01
//* Modified: 2008-05-11
//* Version:  1.1.41.12
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDGauge;

interface

{$R *.RES}
uses
	uTypes, uDImage,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uDBitmap, uDispl;

type
	TDGauge = class(TDImage)
	private
		{ Private declarations }
		FBackEffect: TEffect;
		FBackPaint: Boolean;
		FFontShadow: SG;
		FDispl: TDispl;

		FMin: Integer;
		FPosition: Integer;
		FMax: Integer;
		FPercentage: SG;

		FBevelInner: TPanelBevel;
		FBevelOuter: TPanelBevel;
		FBevelWidth: TBevelWidth;
		FBorderWidth: TBorderWidth;
		FBorderStyle: TBorderStyle;

		procedure SetBackEffect(Value: TEffect);
		procedure SetFontShadow(Value: SG);
		procedure DisplChanged(ADispl: TObject);
		procedure SetDispl(Value: TDispl);

		procedure SetMin(Value: Integer);
		procedure SetPosition(Value: Integer);
		procedure SetMax(Value: Integer);

		procedure SetBevelInner(Value: TPanelBevel);
		procedure SetBevelOuter(Value: TPanelBevel);
		procedure SetBevelWidth(Value: TBevelWidth);
		procedure SetBorderWidth(Value: TBorderWidth);
		procedure SetBorderStyle(Value: TBorderStyle);

		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		function InitPercentage: BG;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		{ Published declarations }
		property BackPaint: Boolean read FBackPaint write FBackPaint default False;
		property BackEffect: TEffect read FBackEffect write SetBackEffect default ef16;
		property FontShadow: SG read FFontShadow write SetFontShadow default 0;
		property Displ: TDispl read FDispl write SetDispl;

		property Min: Integer read FMin write SetMin default 0;
		property Position: Integer read FPosition write SetPosition default 0;
		property Max: Integer read FMax write SetMax default 100;

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

uses uGraph, uScreen, uOutputFormat, uColor;

constructor TDGauge.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
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
	Width := 128;
	Height := 32;
end;

destructor TDGauge.Destroy;
begin
	FDispl.Free;
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

procedure TDGauge.SetFontShadow(Value: SG);
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

function TDGauge.InitPercentage: BG;
var FPerc: SG;
begin
	if FMax <= FMin then
		FPerc := 0
	else
		FPerc:= 1000 * (FPosition - FMin) div (FMax - FMin);
	if FPercentage <> FPerc then
	begin
		Result := True;
		FPercentage := FPerc;
	end
	else
		Result := False;
end;


procedure TDGauge.SetMin(Value: Integer);
begin
	if FMin <> Value then
	begin
		FMin := Value;
		if InitPercentage then
			Invalidate;
	end;
end;

procedure TDGauge.SetPosition(Value: Integer);
begin
	if FPosition <> Value then
	begin
		FPosition := Value;
		if InitPercentage then
			Invalidate;
	end;
end;

procedure TDGauge.SetMax(Value: Integer);
begin
	if FMax <> Value then
	begin
		FMax := Value;
		if InitPercentage then
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

procedure TDGauge.WMPaint(var Message: TWMPaint);
var
	Recta, RectaS: TRect;
	TopColor, BottomColor: TColor;
	X: Integer;
//	C: TColor;
	i: Integer;
	Posit, MaxPosit: Integer;
	Co: array[0..3] of TColor;
begin
	inherited;

	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Width;
	Recta.Bottom := Height;

	// Background
	Bitmap.Canvas.Brush := Parent.Brush;
	Bitmap.Canvas.FillRect(Recta);

// Border
	if (FBorderStyle <> bsNone) then
	begin
		Bitmap.Border(clBtnShadow, clBtnHighlight, 1, BackEffect);
		Bitmap.Border(1, 1, Bitmap.Width - 2, Bitmap.Height - 2,
			cl3DDkShadow, cl3DLight, 1, BackEffect);
		InflateRect(Recta, -2, -2);
	end;
	if FBevelOuter <> bvNone then
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
		Bitmap.Border(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;
	if Color <> clNone then
	begin
		for i := 0 to FBorderWidth - 1 do
			Bitmap.Rec(Recta.Left + i, Recta.Top + i,
				Recta.Right - i - 1, Recta.Bottom - i - 1,
				Color, BackEffect);
		InflateRect(Recta, -FBorderWidth, -FBorderWidth);
	end;
	RectaS := Recta;
	if FBevelInner <> bvNone then
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
		Bitmap.Border(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
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
//		C := SpectrumColor((X - 1) shl 1);
	end
	else
	begin
		X := Recta.Left + (Recta.Right - Recta.Left) * Posit div MaxPosit;
		if X > Recta.Right then X := Recta.Right;
//		C := SpectrumColor(512 * Posit div MaxPosit);
	end;

	if X > Recta.Left then
	begin
{		case FKind of
		gkNormal:
		begin
			Bitmap.Bar(Recta.Left, Recta.Top, X - 1, Recta.Bottom - 1,
				C, FBackEffect);
		end;
		gkSpectrum:
		begin
			for i := Recta.Left to X - 1 do
			begin
				Bitmap.Line(i, Recta.Top, i, Recta.Bottom - 1,
					SpectrumColor(512 * i div (Recta.Right - Recta.Left)), FBackEffect);
			end;
		end;
		gkStandard:
		begin}
			Co[0] := LighterColor(clBtnFace);
			Co[1] := DarkerColor(clBtnFace);
			Co[2] := Co[0];
			Co[3] := Co[1];
			Bitmap.GenerateRGBEx(Recta.Left, Recta.Top, X - 1, Recta.Bottom - 1,
				gfFade2x, Co, ef16, 0, nil);
{		end;
		end;}
	end;
	if X < RectaS.Left then X := RectaS.Left;
	if (X < RectaS.Right) then
	begin
		Bitmap.Bar(X, RectaS.Top, RectaS.Right - 1, RectaS.Bottom - 1,
			Color, FBackEffect);
	end;

	Caption := NToS(FPercentage, 1) + '%';
	if (Caption <> '') {and (FFontEffect<>ef00)} then
	begin
		Bitmap.Canvas.Brush.Color := Color;
		Bitmap.Canvas.Brush.Style := bsClear;
		Bitmap.Canvas.Font := Font;
		if Displ.Enabled then
		begin
			DisplDraw(Bitmap, Caption, FDispl, Recta, taCenter, tlCenter, ef16);
		end
		else
		begin
			DrawCutedText(Bitmap.Canvas, Recta, taCenter, tlCenter, Caption, True, FFontShadow);
		end;
	end;

	Bitmap.DrawToDC(Canvas.Handle, 0, 0);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDGauge]);
end;

end.
