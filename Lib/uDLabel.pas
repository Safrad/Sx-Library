//* File:     Lib\uDLabel.pas
//* Created:  1999-08-01
//* Modified: 2004-12-30
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDLabel;

interface

{$R *.RES}
uses
	uAdd,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls, uGraph, uDBitmap, uDispl;

type
	TDLabel = class(TWinControl)
	private
		{ Private declarations }
		FBmpOut: TDBitmap;
		FBmpText: TDBitmap;

		// Properties
		FAlignment: TAlignment;
{		FAlphaBlend: Boolean;
		FAlphaBlendValue: Byte;}
		FAutoSize: BG;
		FBackEffect: TEffect;
		FBevelInner: TPanelBevel;
		FBevelOuter: TPanelBevel;
		FBevelWidth: TBevelWidth;
		FBorderWidth: TBorderWidth;
		FBorderStyle: TBorderStyle;
		FFocusControl: TWinControl;
		FCaption: string;
//		FColor: TColor;
//		FFont: TFont;
		FFontEffect: TEffect;
		FFontAngle: TAngle;
		FFontShadow: ShortInt;
		FDispl: TDispl;
		FLayout: TTextLayout;

		FTransparent: BG;
		FTransparentColor: Boolean;
		FTransparentColorValue: TColor;
		FWordWrap: BG;

		// Events
		FOnPaint: TNotifyEvent;

		procedure SetBackEffect(Value: TEffect);
//		procedure SetColor(Value: TColor);
		procedure SetCaption(Value: string);

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

		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMShow(var Message: TWMShowWindow); message WM_SHOWWINDOW;

//		procedure SetLayeredAttribs;
{		procedure SetAlphaBlend(const Value: Boolean);
		procedure SetAlphaBlendValue(const Value: Byte);}
		procedure SetTransparentColor(const Value: Boolean);
		procedure SetTransparentColorValue(const Value: TColor);
		procedure SetWordWrap(const Value: Boolean);
//		procedure InitAlphaBlending(var Params: TCreateParams);

	protected
		{ Protected declarations }
		procedure CreateParams(var Params: TCreateParams); override;
//		procedure Paint; override;
	public
		{ Public declarations }

		procedure Invalidate; override;
		procedure Fill;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
//		property Canvas;
	published
		{ Published declarations }
{		property AlphaBlend: Boolean read FAlphaBlend write SetAlphaBlend;
		property AlphaBlendValue: Byte read FAlphaBlendValue write SetAlphaBlendValue;}

		property AutoSize: BG read FAutoSize write FAutoSize;
		property Alignment: TAlignment read FAlignment write FAlignment;
		property Caption: string read FCaption write SetCaption;
		property Color; //: TColor read FColor write SetColor;
		property BackEffect: TEffect read FBackEffect write SetBackEffect default ef16;

		property FocusControl: TWinControl read FFocusControl write FFocusControl;
//		property FocusControl; //: TFont read FFont write SetFont;
		property Font; //: TFont read FFont write SetFont;
		property FontEffect: TEffect read FFontEffect write SetFontEffect default ef16;
		property FontAngle: TAngle read FFontAngle write SetFontAngle default 0;
		property FontShadow: ShortInt read FFontShadow write SetFontShadow default 0;
		property Displ: TDispl read FDispl write SetDispl;

		property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
		property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
		property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
		property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;

		property Layout: TTextLayout read FLayout write FLayout;
		property ParentColor; //: TColor read FColor write SetColor;
		property Hint;
		property ShowHint;
		property ParentShowHint;
		property ParentFont;
		property PopupMenu;

		property Transparent: Boolean read FTransparent write FTransparent;
		property TransparentColor: Boolean read FTransparentColor write SetTransparentColor;
		property TransparentColorValue: TColor read FTransparentColorValue write SetTransparentColorValue;

		property WordWrap: Boolean read FWordWrap write SetWordWrap;

		property OnClick;
		property OnDblClick;
{		property OnMouseEnter;
		property OnMouseLeave;}
		property OnMouseMove;
		property OnMouseDown;
		property OnMouseUp;

		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

procedure Register;

implementation

uses uStrings, uScreen;

type
  TSetLayeredWindowAttributes = function (Hwnd: THandle; crKey: COLORREF; bAlpha: Byte; dwFlags: U4): Boolean; stdcall;

var
	SetLayeredWindowAttributes: TSetLayeredWindowAttributes = nil;

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
//	FBmpBack := nil;
	FBmpText := nil;
//	FBuffer := bfStatic;

	FBackEffect := ef16;
	FBevelOuter := bvRaised;
	FBevelWidth := 1;
//	Color := clWindow;

	FFontEffect := ef16;

	Width := 64;
	Height := 64;
//  Alignment := taCenter;
	FLayout := tlCenter;
	FAutoSize := False;
//	FFont := TFont.Create;
end;

procedure TDLabel.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
//	InitAlphaBlending(Params);
//  SetLayeredAttribs;
end;

destructor TDLabel.Destroy;
begin
	FDispl.Free;
	if Assigned(FBmpOut) then
	begin
		FreeAndNil(FBmpOut);
	end;
{	if Assigned(FBmpBack) then
	begin
		FreeAndNil(FBmpBack);
	end;}
	if Assigned(FBmpText) then
	begin
		FreeAndNil(FBmpText);
	end;
//	FreeAndNil(FFont);
	inherited Destroy;
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

procedure TDLabel.SetWordWrap(const Value: Boolean);
begin
	if FWordWrap <> Value then
	begin
		FWordWrap := Value;
		Invalidate;
	end;
end;


{procedure TDLabel.SetColor(Value: TColor);
begin
	if Value <> FColor then
	begin
		FColor := Value;
		Invalidate;
	end;
end;}

procedure TDLabel.SetCaption(Value: string);
begin
	if Value <> FCaption then
	begin
		FCaption := Value;
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

procedure TDLabel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
//	Message.Result := 0;
	DefaultHandler(Message);
end;

procedure TDLabel.Invalidate;
var Message: TWMPaint;
begin
	inherited Invalidate;
	if (Assigned(FBmpOut)) then
	begin
		Fill;
		Message.Msg := 0;
		Message.DC := 0;
		Message.Unused := 0;
		Message.Result := 0;
		WMPaint(Message);
	end;
end;

procedure TDLabel.WMPaint;
var
	Recta: TRect;
	DC: HDC;
begin
	if Message.Msg <> 0 then
		DefaultHandler(Message);

	DC := GetDC(Handle);

	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Width;
	Recta.Bottom := Height;
	if (not Assigned(FBmpOut)) then
	begin
		FBmpOut := TDBitmap.Create;
		FBmpOut.SetSize(Recta.Right - Recta.Left, Recta.Bottom - Recta.Top);
		Fill;
	end;

	if Assigned(FBmpOut) then
	begin
		BitBlt(DC, 0, 0, FBmpOut.Width, FBmpOut.Height,
			FBmpOut.Canvas.Handle,
			0, 0,
			SRCCOPY);
	end
	else
		PatBlt(DC, 0, 0, Width, Height, 10);
	if Assigned(FOnPaint) then FOnPaint(Self);
	ReleaseDC(Handle, DC);
end;

procedure TDLabel.WMShow(var Message: TWMShowWindow);
begin
	Fill;
	inherited;
end;

procedure TDLabel.WMSize(var Message: TWMSize);
begin
	if Visible = False then Exit;
	if (Message.Width = 0) or (Message.Height = 0) then Exit;
	Fill;
end;

//procedure TDLabel.WMPaint(var Message: TWMPaint);
procedure TDLabel.Fill;
var
	Recta: TRect;
	TopColor, BottomColor: TColor;
	i: Integer;
	Co: array[0..3] of TColor;
begin

//	Recta := GetClientRect;
	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Width;
	Recta.Bottom := Height;
	if (not Assigned(FBmpOut)) then
	begin
		Exit;
//		FBmpOut := TDBitmap.Create;
	end;
	FBmpOut.SetSize(Recta.Right - Recta.Left, Recta.Bottom - Recta.Top);

// Background
	if FBackEffect <> ef16 then
	begin
//		if (Transparent = False) then
		begin
{			FBmpOut.Canvas.Brush := Parent.Brush;
			FBmpOut.Canvas.FillRect(Recta);}
			FBmpOut.FormBitmap(Color);
		end;
{		else
		begin}
{	if FBackEffect <> ef16 then
	begin
		if (Transparent = True) then
		begin}
{			if FBmpBack = nil then
			begin
				FBmpBack := TDBitmap.Create;
				FBmpBack.SetSize(FBmpOut.Width, FBmpOut.Height);
				FBmpBack.Canvas.CopyRect(Rect(Left, Top, FBmpOut.Width, FBmpOut.Height),
					Canvas, Recta);
			end;}
{		end;
	end;}
//			FBmpOut.CopyBitmap(FBmpBack);
{			FBmpOut.Canvas.CopyRect(Rect(0, 0, FBmpOut.Width, FBmpOut.Height),
				Canvas, Recta);}
//		end;
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
	if (Color <> clNone) then
	begin
		for i := 0 to FBorderWidth - 1 do
			FBmpOut.Rec(Recta.Left + i, Recta.Top + i,
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
		FBmpOut.Border(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			TopColor, BottomColor, FBevelWidth, BackEffect);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;

// Background
	if Color <> clNone then
	begin
{		if (FBackEffect <> ef16) then
		begin
			FBmpOut.Bar(clNone, Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
				ColorToRGB(Color), FBackEffect);
		end
		else
		begin}
			Co[0] := LighterColor(ColorToRGB(Color));
			Co[1] := DarkerColor(ColorToRGB(Color));
			Co[2] := Co[0];
			Co[3] := Co[1]; 
			FBmpOut.GenerateRGBEx(Recta.Left, Recta.Top, Recta.Right - 1, SG(Recta.Bottom) - 1,
				gfFade2x, Co, ScreenCorrectColor, FBackEffect, 0, nil);
//			FBmpOut.FormBitmap(Color);
{			FBmpOut.Canvas.Brush.Color := Color;
			FBmpOut.Canvas.FillRect(Recta);}
//		end;
	end;

//Caption
	if (Caption <> '') and (FFontEffect <> ef00) then
	begin
		if (not Assigned(FBmpText)) then
		begin
			FBmpText := TDBitmap.Create;
		end;

		FBmpText.SetSize(FBmpOut.Width, FBmpOut.Height);
		FBmpText.Bar(0, 0, FBmpText.Width - 1, FBmpText.Height - 1,
			Color, ef16);
		FBmpText.TransparentColor := Color;

		FBmpText.Canvas.Brush.Style := bsClear;
		FBmpText.Canvas.Font := Font;
		FBmpOut.Canvas.Font := Font;
//		if
(*		if FFontShadow <> 0 then
		begin
			FBmpText.Canvas.Font.Color := ShadowColor(Font.Color);
			FBmpOut.Canvas.Font.Color := ShadowColor(Font.Color);
{			TopColor := ShadowColor(FDispl.ColorA);
			BottomColor := ShadowColor(FDispl.ColorD);}
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
					if (FFontAngle = 0) and (FFontEffect = ef16) then
					begin
						FBmpOut.Canvas.Brush.Style := bsClear;
						DrawCutedText(FBmpOut.Canvas, Recta, Alignment, Layout, Caption, WordWrap);
					end
					else
					begin
						DrawCutedText(FBmpText.Canvas, Recta, Alignment, Layout, Caption, WordWrap);
					end;
				end;
				OffsetRect(Recta, -i, -i);
				if FFontShadow > 0 then Dec(i) else Inc(i);
			until i = 0;
			FBmpText.Canvas.Font.Color := Font.Color;
		end; *)
		FBmpText.Canvas.Font := Font;
		FBmpOut.Canvas.Font := Font;
		if Displ.Enabled then
		begin
			if FFontEffect <> ef16 then
				DisplDrawRect(FBmpText, DelCharsF(Caption, '&'), FDispl, Recta, FAlignment, FLayout,
					ef16)
			else
				DisplDrawRect(FBmpOut, DelCharsF(Caption, '&'), FDispl, Recta, FAlignment, FLayout,
					ef16);
		end
		else
		begin
			if (FFontAngle = 0) and (FFontEffect = ef16) then
			begin
				FBmpOut.Canvas.Brush.Color := Color;
				FBmpOut.Canvas.Brush.Style := bsClear;
				DrawCutedText(FBmpOut.Canvas, Recta, FAlignment, FLayout, Caption, FWordWrap, FFontShadow);
			end
			else
			begin
				DrawCutedText(FBmpText.Canvas, Recta, FAlignment, FLayout, Caption, FWordWrap, FFontShadow)
			end;
		end;

		if FFontAngle = 0 then
		begin
			if FFontEffect <> ef16 then
				FBmpOut.Bmp(0, 0, FBmpText, FFontEffect);
		end
		else
		begin
			RotateDef(FBmpOut, FBmpText, 0, FFontAngle, FFontEffect);
		end;
{		if (Assigned(FBmpText)) then
		begin
			FreeAndNil(FBmpText);
		end;}
	end;

// Draw
//	Canvas.Draw(0, 0, FBmpOut);
{	if (Assigned(FBmpOut)) then
	begin
		FreeAndNil(FBmpOut);
	end;}
end;
{
procedure TDLabel.SetLayeredAttribs;
const
	cUseAlpha: array [Boolean] of Integer = (0, LWA_ALPHA);
	cUseColorKey: array [Boolean] of Integer = (0, LWA_COLORKEY);
var
	AStyle: Integer;
begin
	if not (csDesigning in ComponentState) and
		(Assigned(SetLayeredWindowAttributes)) and HandleAllocated then
	begin
		AStyle := GetWindowLong(Handle, GWL_EXSTYLE);
		if FAlphaBlend or FTransparentColor then
		begin
			if (AStyle and WS_EX_LAYERED) = 0 then
				SetWindowLong(Handle, GWL_EXSTYLE, AStyle or WS_EX_LAYERED);
			SetLayeredWindowAttributes(Handle, FTransparentColorValue, FAlphaBlendValue,
				cUseAlpha[FAlphaBlend] or cUseColorKey[FTransparentColor]);
		end
		else
		begin
			SetWindowLong(Handle, GWL_EXSTYLE, AStyle and not WS_EX_LAYERED);
			RedrawWindow(Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
		end;
	end;
end;

procedure TDLabel.SetAlphaBlend(const Value: Boolean);
begin
	if FAlphaBlend <> Value then
	begin
		FAlphaBlend := Value;
		SetLayeredAttribs;
	end;
end;

procedure TDLabel.SetAlphaBlendValue(const Value: Byte);
begin
	if FAlphaBlendValue <> Value then
	begin
		FAlphaBlendValue := Value;
		SetLayeredAttribs;
	end;
end;
}
procedure TDLabel.SetTransparentColorValue(const Value: TColor);
begin
	if FTransparentColorValue <> Value then
	begin
		FTransparentColorValue := Value;
//		SetLayeredAttribs;
	end;
end;

procedure TDLabel.SetTransparentColor(const Value: Boolean);
begin
	if FTransparentColor <> Value then
	begin
		FTransparentColor := Value;
//		SetLayeredAttribs;
	end;
end;

{procedure TDLabel.InitAlphaBlending(var Params: TCreateParams);
begin
	if not (csDesigning in ComponentState) and (assigned(SetLayeredWindowAttributes)) then
		if FAlphaBlend or FTransparentColor then
			Params.ExStyle := Params.ExStyle or WS_EX_LAYERED;
end;

procedure InitProcs;
const
	sUser32 = 'User32.dll';
var
	ModH: HMODULE;
begin
	ModH := GetModuleHandle(sUser32);
	if ModH <> 0 then
		 @SetLayeredWindowAttributes := GetProcAddress(ModH, 'SetLayeredWindowAttributes');
end;}

procedure Register;
begin
	RegisterComponents('DComp', [TDLabel]);
end;

initialization
//	InitProcs;

end.
