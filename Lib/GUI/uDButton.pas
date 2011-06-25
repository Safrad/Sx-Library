// * File:     Lib\GUI\uDButton.pas
// * Created:  1999-09-01
// * Modified: 2009-12-10
// * Version:  1.1.45.113
// * Author:   David Safranek (Safrad)
// * E-Mail:   safrad at email.cz
// * Web:      http://safrad.own.cz

unit uDButton;

interface

{$R *.RES}
uses
	uTypes, uMath, uFiles, uDBitmap,
	Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
	ExtCtrls, CommCtrl, uWave, uDTimer;

type
	THighlight = (hlNone, hlRect, hlBar, hlRectMov, hlBarHorz, hlBarVert, hlUnderlight);
	TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);

	TDButton = class(TButton)
	private
		Bitmap: TDBitmap;
//    FDefault: Boolean;
//    FCancel: Boolean;
//		FModalResult: TModalResult;

		FSmall: BG;

		FHighlight: THighlight;
		FHighNow: Boolean;
		FDown: Boolean;
		FDownNow: Boolean;
		FEnabled: Boolean;
		FLastDown: Boolean;
		FAutoChange: Boolean;

		FColor: TColor;
		FLayout: TButtonLayout;
		FSpacing: Integer;
		FMargin: Integer;
		IsFocused: Boolean;
		FMouseDown: Boolean;
		FEllipseSize: SG;

		procedure InitRect;
		procedure PlayButtonSound(const IsDown: BG);
		procedure SetColor(Value: TColor);
//    procedure SetDefault(Value: Boolean);
		procedure SetDown(Value: Boolean);
		procedure SetHighlight(Value: THighLight);
		procedure SetEnabled2(Value: Boolean);
		function IsCustomCaption: Boolean;
		procedure SetLayout(Value: TButtonLayout);
		procedure SetSpacing(Value: Integer);
		procedure SetMargin(Value: Integer);

		procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
			protected
		procedure CreateHandle; override;
		procedure CreateParams(var Params: TCreateParams); override;
		procedure SetButtonStyle(ADefault: Boolean); override;
	public
		FGlyph: TDBitmap;
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Click; override;
	published
		property Action;
		property Anchors;
		property AutoChange: Boolean read FAutoChange write FAutoChange default False;
		property BiDiMode;
//		property Cancel: Boolean read FCancel write FCancel default False;
		property Caption stored IsCustomCaption;
		property Color: TColor read FColor write SetColor default clBtnFace;
		property Constraints;
//		property Default: Boolean read FDefault write SetDefault default False;
		property Down: Boolean read FDown write SetDown default False;
		property Enabled read FEnabled write SetEnabled2 default True;
		property Font;
		property Highlight: THighLight read FHighlight write SetHighlight default hlUnderlight;
		property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
		property Margin: Integer read FMargin write SetMargin default - 1;
//		property ModalResult: TModalResult read FModalResult write FModalResult default 0;
		property ParentFont;
		property ParentShowHint;
		property ParentBiDiMode;
		property ShowHint;
		property Spacing: Integer read FSpacing write SetSpacing default 4;
		property TabOrder;
		property TabStop default True;
		property Visible;
{		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyUp;
		property OnMouseMove;
		property OnClick;}
	end;

procedure Register;

implementation

uses
	Consts, SysUtils, ActnList, ImgList, MMSystem, Math,
	uGraph, uScreen, uStrings, uColor, uMenus, uDrawStyle,
	uSounds, uSysInfo, uDWinControl;

{ TDButton }
var
	CDefault, CCancel, CDefaultCancel: TColor;

procedure TDButton.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	Message.Result := 1;
end;

procedure TDButton.InitRect;
var
	hR: THandle;
//	Po: array[0..9] of tagPOINT;
begin
	FSmall := (Width <= IconSize) and (Height <= IconSize);
	if RegionCompatibily = False then Exit;
	if not FSmall then
	begin
(*		Po[0].x := 0;
		Po[0].y := 0;
		Po[1].x := Width;
		Po[1].y := 0;
		Po[2].x := Width;
		Po[2].y := Height;

		Po[3].x := Width div 2 + 8;
		Po[3].y := Height;

		Po[4].x := Width div 2 + 4;
		Po[4].y := Height - 4;

		Po[5].x := Width div 2;
		Po[5].y := Height - 6;

		Po[6].x := Width div 2 - 4;
		Po[6].y := Height - 4;

{		Po[4].x := Width div 2;
		Po[4].y := Height - 8;}


		Po[7].x := Width div 2 - 8;
		Po[7].y := Height;

		Po[8].x := 0;
		Po[8].y := Height;
		Po[9].x := 0;
		Po[9].y := 0;   *)

		FEllipseSize := Min(Width, Height) div 2;
		hR := CreateRoundRectRgn(0, 0, Width + 1, Height + 1, FEllipseSize, FEllipseSize);

//			hR := CreateEllipticRgn(0, 0, Width, Height);
//			hR := CreateRectRgn(0, 0, Width, Height);
//		hR := CreatePolygonRgn(Po[0], Length(Po), {ALTERNATE}	WINDING);
		SetWindowRgn(Handle, hR, True);
		DeleteObject(hR);
	end
	else
	begin
		hR := CreateRectRgn(0, 0, Width, Height);
		SetWindowRgn(Handle, hR, True);
		DeleteObject(hR);
	end;
end;

procedure TDButton.WMSize(var Message: TWMSize);
begin
	inherited;
	InitRect;
end;

constructor TDButton.Create(AOwner: TComponent);
begin
	inherited;
	FLayout := blGlyphLeft;
	FSpacing := 4;
	FMargin := -1;
	FColor := clBtnFace;
	FHighlight := hlUnderlight;
	FDown := False;
	FDownNow := False;
	FEnabled := True;
	FLastDown := False;
	ControlStyle := [csSetCaption, csDoubleClicks];
	ControlStyle := ControlStyle + [csReflector];
	Width := 75;
	Height := 25;
	TabStop := True;

	// Caption can not be changed there
end;

destructor TDButton.Destroy;
begin
	FGlyph.Free;
	Bitmap.Free;
	inherited;
end;

procedure TDButton.CreateHandle;
begin
	inherited CreateHandle;
end;

procedure TDButton.CreateParams(var Params: TCreateParams);
var
	i: SG;
	s: string;
begin
	Font.Color := clBtnText;
	s := Caption;
	i := Pos('/', s);
//	if (Length(s) > 0) and (s[1] = '/') then
	if i = 1 then
	begin
		Delete(s, i, 1);
		Insert(AddSpace(ButtonNameToFileName(Name)), s, i);
	end;

	if (Length(s) > 0) and (s[1] <> '&') then
		s := '&' + s;
	Caption := s;

	inherited CreateParams(Params);
	Params.Style := Params.Style or BS_OWNERDRAW;
end;

procedure TDButton.SetButtonStyle(ADefault: Boolean);
begin
	if ADefault <> IsFocused then
	begin
		IsFocused := ADefault;
		Invalidate;
	end;
end;

procedure TDButton.Click;
begin
	if FEnabled then
	begin
		if FAutoChange then
		begin
			FDown := not FDown;
			Invalidate;
		end;
		inherited Click;
	end;
end;

procedure TDButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
	Message.MeasureItemStruct^.itemWidth := Width;
	Message.MeasureItemStruct^.itemHeight := Height;
end;

procedure TDButton.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	{ Don't draw a border if DragMode <> dmAutomatic since this button is meant to
		be used as a dock client. }
	if (not (csDesigning in ComponentState)) then
	if (DragMode <> dmAutomatic) then
	begin
		if FMouseDown then FDownNow := True;
		FHighNow := True;
		Invalidate;
	end;
end;

procedure TDButton.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if (not (csDesigning in ComponentState)) then
	if not Dragging then
	begin
		FDownNow := False;
		FHighNow := False;
		Invalidate;
	end;
end;

procedure TDButton.CNDrawItem(var Message: TWMDrawItem);
const
	Border = 2;
var
	FileName: TFileName;

	IsDown, IsDefault: BG;
	Rec, Recta: TRect;
	GlyphPos, GlyphSize: TPoint;
	TextA: TAlignment;
	TextL: TTextLayout;

	Co: array[0..3] of TColor;
	E: TColor;
	s: string;
	Orient: SG;
	v: SG;
begin
	IsDefault := Message.DrawItemStruct.itemState and ODS_FOCUS <> 0;
	IsDown := Message.DrawItemStruct.itemState and ODS_SELECTED <> 0;
	if FDown then IsDown := not IsDown;
	if FAutoChange and (FDown <> IsDown) and (FLastDown <> IsDown) then Exit;

	Rec := ClientRect;
	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Rec.Right - Rec.Left;
	Recta.Bottom := Rec.Bottom - Rec.Top;

	if not Assigned(Bitmap) then
	begin
		Bitmap := TDBitmap.Create;
		Bitmap.Canvas.Font := Font;
	end;
	Bitmap.SetSize(Recta.Right, Recta.Bottom, clBtnFace);

	PlayButtonSound(IsDown);

	// Glyph
	if not Assigned(FGlyph) then
	begin
		FGlyph := TDBitmap.Create;
		FileName := GraphDir + 'Images' + PathDelim + ButtonNameToFileName(Name) + IconExt;
		if FileExists(FileName) then
		begin
			FGlyph.LoadFromFile(FileName);
			if FGlyph.Height > 0 then
			if FGlyph.Height + 3 * Border > Height then
			begin
				v := Height - 3 * Border;
				FGlyph.Resize(RoundDiv(FGlyph.Width * v, FGlyph.Height), v);
			end;
		end;
	end;

	// Draw
	InflateRect(Recta, -2, -2);
	Co[0] := ColorDiv(FColor, 5 * 16384);
	Co[1] := ColorDiv(FColor, 3 * 16384);
	Co[2] := Co[0];
	Co[3] := Co[1];
//	{$ifopt d-}
	if FSmall then
	begin
		if IsDown then
			Bitmap.Bar(Recta, LighterColor(FColor), ef16)
		else
			Bitmap.Bar(Recta, FColor, ef16);
	end
	else
		Bitmap.GenerateRGBEx(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			gfFade2x, Co, ef16, 0, nil);
//	{$else}
//	Bitmap.Bar(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1, FColor, ef16);
//	{$endif}

	if IsDown then OffsetRect(Recta, 1, 1);
	if FAutoChange then
	begin
		if FDown then
			Bitmap.Canvas.Font.Style := [fsUnderline]
		else
			Bitmap.Canvas.Font.Style := [fsStrikeOut];
	end
	else
		Bitmap.Canvas.Font.Style := [];

	s := Caption;
	
	if s = 'Up' then Orient := 0
	else if s = 'Left' then Orient := 1
	else if s = 'Down' then Orient := 2
	else if s = 'Right' then Orient := 3
	else Orient := -1;

	if Orient <> -1 then
	begin
		Bitmap.DrawArrow(Recta.Left - 1, Recta.Top - 1, Recta.Right - 0, Recta.Bottom - 0, FDown, FHighNow and FEnabled, Orient, ef16);
	end
	else
	begin
		// Icon
		// Layout
		// Spacing
		// Margin
		GlyphSize := Point(FGlyph.Width, FGlyph.Height);

	(*	if Length(s) > 0 then
		begin
			TextBounds := Rect(0, 0, Recta.Right - Recta.Left, 0);
			TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
				TextBounds.Top);
		end
		else
		begin
			TextBounds := Rect(0, 0, 0, 0);
			TextSize := Point(0, 0);
		end;*)

		{ If the layout has the glyph on the right or the left, then both the
			text and the glyph are centered vertically.  If the glyph is on the top
			or the bottom, then both the text and the glyph are centered horizontally.}
		if Layout in [blGlyphLeft, blGlyphRight] then
		begin
			GlyphPos.Y := (Bitmap.Height - GlyphSize.Y + 1) div 2;
	//		TextPos.Y := (Recta.Bottom - TextSize.Y) div 2;
		end
		else
		begin
			GlyphPos.X := (Bitmap.Width - GlyphSize.X + 1) div 2;
	//		TextPos.X := (Recta.Right - TextSize.X) div 2;
		end;

		{ if there is no text or no bitmap, then Spacing is irrelevant }
	{	if (TextSize.X = 0) or (GlyphSize.X = 0) then
			Spacing := 0;}

		{ adjust Margin and Spacing }
	{	if Margin = -1 then
		begin
			if Spacing = -1 then
			begin
				TotalSize := Point(GlyphSize.X + TextSize.X, GlyphSize.Y + TextSize.Y);
				if Layout in [blGlyphLeft, blGlyphRight] then
					Margin := (ClientSize.X - TotalSize.X) div 3
				else
					Margin := (ClientSize.Y - TotalSize.Y) div 3;
				Spacing := Margin;
			end
			else
			begin
				TotalSize := Point(GlyphSize.X + Spacing + TextSize.X, GlyphSize.Y +
					Spacing + TextSize.Y);
				if Layout in [blGlyphLeft, blGlyphRight] then
					Margin := (ClientSize.X - TotalSize.X + 1) div 2
				else
					Margin := (ClientSize.Y - TotalSize.Y + 1) div 2;
			end;
		end
		else
		begin
			if Spacing = -1 then
			begin
				TotalSize := Point(ClientSize.X - (Margin + GlyphSize.X), ClientSize.Y -
					(Margin + GlyphSize.Y));
				if Layout in [blGlyphLeft, blGlyphRight] then
					Spacing := (TotalSize.X - TextSize.X) div 2
				else
					Spacing := (TotalSize.Y - TextSize.Y) div 2;
			end;
		end;}

	{	TextRect.Left := 2;
		TextRect.Top := 2;
		TextRect.Right := Bitmap.Width - 2;
		TextRect.Bottom := Bitmap.Height - 2;}

		TextA := taCenter;
		TextL := tlCenter;

		if FGlyph.Empty = False then
		begin
			Margin := 6;
			Spacing := 2;

			if s = '' then
				GlyphPos.X := (Bitmap.Width - GlyphSize.X + 1) div 2
			else
			case Layout of
			blGlyphLeft:
			begin
	//			TextA := taLeftJustify;
				GlyphPos.X := Margin;
				Inc(Recta.Left, GlyphPos.X + GlyphSize.X + Spacing);
			end;
			blGlyphRight:
			begin
	//			TextA := taRightJustify;
				GlyphPos.X := Bitmap.Width - Margin - GlyphSize.X;
				Dec(Recta.Right, Spacing + GlyphSize.X);
			end;
			blGlyphTop:
			begin
	//			TextL := tlTop;
				GlyphPos.Y := Margin;
				Inc(Recta.Top, GlyphPos.Y + GlyphSize.Y + Spacing);
			end;
			blGlyphBottom:
			begin
	//			TextL := tlBottom;
				GlyphPos.Y := Bitmap.Height - Margin - GlyphSize.Y;
				Dec(Recta.Bottom, Spacing + GlyphSize.Y);
			end;
			end;

			if FEnabled then
				Bitmap.Bmp(GlyphPos.x, GlyphPos.y, FGlyph, ef16)
			else
				Bitmap.Bmp(GlyphPos.x, GlyphPos.y, FGlyph, ef04);
		end;


		if not FEnabled then
		begin
			Bitmap.Canvas.Font.Color := clDepth[1];
			Bitmap.Canvas.Brush.Color := clDepth[3];
		end
		else
		begin
			Bitmap.Canvas.Brush.Color := Color;
			Bitmap.Canvas.Font.Color := Font.Color;
		end;
		Bitmap.Canvas.Brush.Style := bsClear;
		if s <> '' then // TODO New Field Hint
		begin
			if DrawCuttedText(Bitmap.Canvas, Recta, TextA, TextL, s, True, IdealShadow(Bitmap.Canvas)) then
			begin
				Hint := DelCharsF(s, '&');
				ShowHint := True;
			end
			else
			begin
				Hint := '';
				ShowHint := False;
			end;
		end;
	end;
	Bitmap.Canvas.Brush.Style := bsClear;

	if IsDown then OffsetRect(Recta, -1, -1);

	if Default and Cancel then
	begin
		E := CDefaultCancel;
	end
	else if Default then
	begin
		E := CDefault;
	end
	else if Cancel then
	begin
		E := CCancel;
	end
	else
		E := clNone;

	if E <> clNone then
	begin
		Bitmap.Canvas.Pen.Color := E;
		Bitmap.Canvas.RoundRect(2, 2, Width - 2, Height - 2, FEllipseSize, FEllipseSize);
{		Bitmap.Rec(2, 2,
			Bitmap.Width - 3, Bitmap.Height - 3, E, ef16);}
	end;

	if (Orient = -1) and FHighNow and FEnabled then
	begin
		Co[0] := ColorDiv(FColor, 21504);
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		Bitmap.GenerateRGB(gfFade2x, Co, efAdd, nil);
	end;

	if IsFocused and IsDefault then
	begin
		Bitmap.Bar(Border, Border, Bitmap.Width - 1 - Border, Bitmap.Height - 1 - Border, clHighlight, ef08);
	end;

	if IsDown then
		Bitmap.Canvas.Pen.Color := clDepth[1]
	else if not FSmall then
		Bitmap.Canvas.Pen.Color := clDepth[3]
	else
		Bitmap.Canvas.Pen.Color := FColor;

	if (FSmall = False) then
		Bitmap.Canvas.RoundRect(0, 0, Width, Height, FEllipseSize + 2, FEllipseSize + 2)
	else
		Bitmap.Canvas.Rectangle(0, 0, Width, Height);

	if IsDown then
		Bitmap.Canvas.Pen.Color := clDepth[3]
	else if not FSmall then
		Bitmap.Canvas.Pen.Color := clDepth[1]
	else
		Bitmap.Canvas.Pen.Color := FColor;
	if (FSmall = False) then
		Bitmap.Canvas.RoundRect(1, 1, Width - 1, Height - 1, FEllipseSize, FEllipseSize)
	else
		Bitmap.Canvas.Rectangle(1, 1, Width - 1, Height - 1);

	Bitmap.DrawToDC(Message.DrawItemStruct.hDC, 0, 0);
//	Invalidate;
	Message.Result := 1;
end;

procedure TDButton.CMFontChanged(var Message: TMessage);
begin
	inherited;
	Invalidate;
end;

procedure TDButton.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
	FEnabled := TWinControl(Self).Enabled;
	Invalidate;
end;

procedure TDButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
	Perform(WM_LBUTTONDOWN, Message.Keys, S4(Message.Pos));
end;

function TDButton.IsCustomCaption: Boolean;
begin
	Result := True;
end;

procedure TDButton.SetLayout(Value: TButtonLayout);
begin
	if FLayout <> Value then
	begin
		FLayout := Value;
		Invalidate;
	end;
end;

procedure TDButton.SetSpacing(Value: Integer);
begin
	if FSpacing <> Value then
	begin
		FSpacing := Value;
		Invalidate;
	end;
end;

procedure TDButton.SetMargin(Value: Integer);
begin
	if (Value <> FMargin) and (Value >= -1) then
	begin
		FMargin := Value;
		Invalidate;
	end;
end;

procedure TDButton.SetColor(Value: TColor);
begin
	if (Value <> FColor) then
	begin
		FColor := Value;
		Invalidate;
	end;
end;

procedure TDButton.SetDown(Value: Boolean);
begin
	if (Value <> FDown) then
	begin
		FDown := Value;
		FLastDown := FDown;
		Invalidate;
	end;
end;

procedure TDButton.SetEnabled2(Value: Boolean);
begin
	if (Value <> FEnabled) then
	begin
		FEnabled := Value;
		Invalidate;
	end;
end;

procedure TDButton.SetHighlight(Value: THighLight);
begin
	if (Value <> FHighlight) then
	begin
		FHighLight := Value;
		Invalidate;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDButton]);
end;

{procedure TDButton.SetDefault(Value: Boolean);
var
	Form: TCustomForm;
begin
	FDefault := Value;
	if HandleAllocated then
	begin
		Form := GetParentForm(Self);
		if Form <> nil then
			Form.Perform(CM_FOCUSCHANGED, 0, Longint(Form.ActiveControl));
	end;
end;}

procedure TDButton.PlayButtonSound(const IsDown: BG);
var
	MonitorRect, Rect: TRect;
	P: TPoint;
begin
	if (FLastDown <> IsDown) then
	begin
		MonitorRect := Screen.MonitorFromWindow(Handle).WorkareaRect;
		P.X := Left + Width div 2;
		P.Y := Top + Height div 2;
		Rect := Parent.BoundsRect;

		PlaySound(SG(IsDown), P.X + Rect.Left - MonitorRect.Left, MonitorRect.Right - MonitorRect.Left - 1);
		FLastDown := IsDown;
	end;
end;

initialization
	if (ColorToRGB(clBtnFace) = ColorToRGB(clActiveBorder)) or
		(ColorToRGB(clBtnFace) = ColorToRGB(clInactiveBorder)) or
		(ColorToRGB(clActiveBorder) = ColorToRGB(clInactiveBorder)) then
	begin
		CDefault := clLime;
		CCancel := clRed;
		CDefaultCancel := clYellow;
	end
	else
	begin
		CDefault := clActiveBorder;
		CCancel := clInactiveBorder;
		CDefaultCancel := MixColors(CDefault, CCancel)
	end;
	AddSounds(['BDown', 'BUp'], True);
end.
