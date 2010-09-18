// Build: 08/1999-08/1999 Author: Safranek David

unit uDLabel;

interface

{$R *.RES}
uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uGraph, uDBitmap, uDispl;

type
	TBuffer = (bfDynamic, bfStatic);

	TDLabel = class(TLabel)
	private
		{ Private declarations }
		FBmpOut: TDBitmap;
		FBmpText: TDBitmap;
		FBuffer: TBuffer;

		FBackEffect: TEffect;

		FFontEffect: TEffect;
		FFontAngle: TAngle;
		FFontShadow: ShortInt;
		FDispl: TDispl;

		FBevelInner: TPanelBevel;
		FBevelOuter: TPanelBevel;
		FBevelWidth: TBevelWidth;
		FBorderWidth: TBorderWidth;
		FBorderStyle: TBorderStyle;

		FOnPaint: TNotifyEvent;

		procedure SetBuffer(Value: TBuffer);

		procedure SetBackEffect(Value: TEffect);

		procedure SetFontShadow(Value: ShortInt);
		procedure SetFontAngle(Value: TAngle);
		procedure SetFontEffect(Value: TEffect);
		procedure DisplChanged(ADispl: TObject);
		procedure SetDispl(Value: TDispl);

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
		property Canvas;
	published
		{ Published declarations }
		property Buffer: TBuffer read FBuffer write SetBuffer default bfStatic;

		property BackEffect: TEffect read FBackEffect write SetBackEffect default ef16;

		property FontEffect: TEffect read FFontEffect write SetFontEffect default ef16;
		property FontAngle: TAngle read FFontAngle write SetFontAngle default 0;
		property FontShadow: ShortInt read FFontShadow write SetFontShadow default 0;
		property Displ: TDispl read FDispl write SetDispl;

		property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
		property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
		property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
		property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;

		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

procedure Register;

implementation

uses uStrings;

constructor TDLabel.Create(AOwner: TComponent);
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

	FBmpOut := nil;
	FBmpText := nil;
	FBuffer := bfStatic;

	FBackEffect := ef16;
	FBevelOuter := bvRaised;
	FBevelWidth := 1;

	FFontEffect := ef16;

	Width := 64;
	Height := 64;
//  Alignment := taCenter;
	Layout := tlCenter;
	AutoSize := False;
end;

destructor TDLabel.Destroy;
begin
	FDispl.Free;
	if Assigned(FBmpOut) then
	begin
		FBmpOut.Free;
		FBmpOut := nil;
	end;
	if Assigned(FBmpText) then
	begin
		FBmpText.Free;
		FBmpText := nil;
	end;
	inherited Destroy;
end;

procedure TDLabel.SetBuffer(Value: TBuffer);
begin
	if FBuffer <> Value then
	begin
		if Value <> bfStatic then
		begin
			if Assigned(FBmpOut) then
			begin
				FBmpOut.Free;
				FBmpOut := nil;
			end;
			if Assigned(FBmpText) then
			begin
				FBmpText.Free;
				FBmpText := nil;
			end;
		end
		else
		begin
			if not Assigned(FBmpOut) then
			begin
				FBmpOut := TDBitmap.Create;
			end;
			if not Assigned(FBmpText) then
			begin
				FBmpText := TDBitmap.Create;
			end;
		end;
		FBuffer := Value;
	end;
end;

procedure TDLabel.DisplChanged(ADispl: TObject);
begin
	Invalidate;
end;

procedure TDLabel.SetDispl(Value: TDispl);
begin
	FDispl.Assign(Value);
end;

procedure TDLabel.SetBackEffect(Value: TEffect);
begin
	if FBackEffect <> Value then
	begin
		FBackEffect := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetFontShadow(Value: ShortInt);
begin
	if FFontShadow <> Value then
	begin
		FFontShadow := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetFontAngle(Value: TAngle);
begin
	if FFontAngle <> Value then
	begin
		FFontAngle := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetFontEffect(Value: TEffect);
begin
	if FFontEffect <> Value then
	begin
		FFontEffect := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetBevelInner(Value: TPanelBevel);
begin
	FBevelInner := Value;
	Invalidate;
end;

procedure TDLabel.SetBevelOuter(Value: TPanelBevel);
begin
	FBevelOuter := Value;
	Invalidate;
end;

procedure TDLabel.SetBevelWidth(Value: TBevelWidth);
begin
	FBevelWidth := Value;
	Invalidate;
end;

procedure TDLabel.SetBorderWidth(Value: TBorderWidth);
begin
	FBorderWidth := Value;
	Invalidate;
end;

procedure TDLabel.SetBorderStyle(Value: TBorderStyle);
begin
	if FBorderStyle <> Value then
	begin
		FBorderStyle := Value;
		Invalidate;
	end;
end;

procedure TDLabel.Paint;
var
	Recta: TRect;
	TopColor, BottomColor: TColor;
	i: Integer;
begin
//  Recta := GetClientRect;
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
	if FBackEffect <> ef16 then
	begin
		if (Transparent = False) then
		begin
			FBmpOut.Canvas.Brush := Parent.Brush;
			FBmpOut.Canvas.FillRect(Recta);
		end
		else
		begin
			FBmpOut.Canvas.CopyRect(Rect(0, 0, FBmpOut.Width, FBmpOut.Height),
				Canvas, Recta);
		end;
	end;

// Border
	if (FBorderStyle <> bsNone) then
	begin
		FBmpOut.BorderE24(clBtnShadow, clBtnHighlight, 1, BackEffect);
		FBmpOut.Border24(1, 1, FBmpOut.Width - 2, FBmpOut.Height - 2,
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
		FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;
	if (Color <> clNone) then
	begin
		for i := 0 to FBorderWidth - 1 do
			FBmpOut.Rec24(Recta.Left + i, Recta.Top + i,
				Recta.Right - i - 1, Recta.Bottom - i - 1,
				Color, BackEffect);
		InflateRect(Recta, -FBorderWidth, -FBorderWidth);
	end;
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
		FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;

// Background
	if Color <> clNone then
	begin
		if (FBackEffect <> ef16) then
		begin
			FBmpOut.Bar24(clNone, Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
				Color, FBackEffect);
		end
		else
		begin
			FBmpOut.Canvas.Brush.Color := Color;
			FBmpOut.Canvas.FillRect(Recta);
		end;
	end;

//Caption
	if (Caption <> '') and (FFontEffect <> ef00) then
	begin
		if (not Assigned(FBmpText)) then
		begin
			FBmpText := TDBitmap.Create;
		end;

		FBmpText.SetSize(FBmpOut.Width, FBmpOut.Height);
		FBmpText.Bar24(clNone, 0, 0, FBmpText.Width - 1, FBmpText.Height - 1,
			Color, ef16);

		FBmpText.Canvas.Brush.Style := bsClear;
		FBmpText.Canvas.Font := Font;
		if FFontShadow <> 0 then
		begin
			FBmpText.Canvas.Font.Color := ShadowColor(Font.Color);
			TopColor := FDispl.ColorA;
			BottomColor := FDispl.ColorD;
			FDispl.ColorA := ShadowColor(FDispl.ColorA);
			FDispl.ColorD := ShadowColor(FDispl.ColorD);
			i := FFontShadow;
			repeat
				OffsetRect(Recta, i, i);
				if Displ.Enabled then
				begin
					DisplDrawRect(FBmpText, DelCharsF(Caption, '&'), FDispl, Recta, Alignment, Layout,
						ef16);
				end
				else
				begin
					DrawCutedText(FBmpText.Canvas, Recta, Alignment, Layout, Caption);
				end;
				OffsetRect(Recta, -i, -i);
				if FontShadow > 0 then Dec(i) else Inc(i);
			until i = 0;
			FDispl.ColorA := TopColor;
			FDispl.ColorD := BottomColor;
			FBmpText.Canvas.Font.Color := Font.Color;
		end;
		if Displ.Enabled then
		begin
			DisplDrawRect(FBmpText, DelCharsF(Caption, '&'), FDispl, Recta, Alignment, Layout,
			ef16);
		end
		else
		begin
			DrawCutedText(FBmpText.Canvas, Recta, Alignment, Layout, Caption);
		end;

		if FFontAngle = 0 then
		begin
			FBmpOut.BmpE24(0, 0, FBmpText, Color{NegColor(Font.Color)}, FFontEffect);
		end
		else
		begin
			RotateDefE24(FBmpOut, FBmpText, 0, FFontAngle, Color{NegColor(Font.Color)}, FFontEffect);
		end;
		if (Assigned(FBmpText)) and (FBuffer <> bfStatic) then
		begin
			FBmpText.Free;
			FBmpText := nil;
		end;
	end;

// Draw
	Canvas.Draw(0, 0, FBmpOut);
// Free
	if (Assigned(FBmpOut)) and (FBuffer <> bfStatic) then
	begin
		FBmpOut.Free;
		FBmpOut := nil;
	end;
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDLabel]);
end;

end.
