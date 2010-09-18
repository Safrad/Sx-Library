{*******************************************************}
{                                                       }
{       Borland Delphi Runtime Library                  }
{       Windows Messages and Types                      }
{                                                       }
{       Copyright (C) 1991,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

// Build: 09/1999-11/1999 Author: Safranek David

unit uDButton;

interface

{$R *.RES}
uses
	uDBitmap,
	Windows, Messages, Classes, Controls, Forms, Graphics, StdCtrls,
	ExtCtrls, CommCtrl, uWave, uDTimer;

type
	THighlight = (hlNone, hlRect, hlBar, hlRectMov, hlBarHorz, hlBarVert, hlUnderlight);
	TButtonLayout = (blGlyphLeft, blGlyphRight, blGlyphTop, blGlyphBottom);

	TDButton = class(TButton)
	private
		FCanvas: TCanvas;
		FBmpOut: TDBitmap;
		FGlyph: Pointer;

		FHighlight: THighlight;
		FHighNow: Boolean;
		FDown: Boolean;
		FDownNow: Boolean;
		FLastDown: Boolean;
		FAutoChange: Boolean;

		FColor: TColor;
		FTimer: TDTimer;
		FLayout: TButtonLayout;
		FSpacing: Integer;
		FMargin: Integer;
		IsFocused: Boolean;
		FModifiedGlyph: Boolean;
		FMouseDown: Boolean;

//		FHighClock: LongWord;

		procedure DrawItem(const DrawItemStruct: TDrawItemStruct);
		procedure SetColor(Value: TColor);
		procedure SetDown(Value: Boolean);
		procedure SetHighlight(Value: THighLight);
		procedure SetGlyph(Value: TBitmap);
		function GetGlyph: TBitmap;
		procedure GlyphChanged(Sender: TObject);
		function IsCustomCaption: Boolean;
		procedure SetLayout(Value: TButtonLayout);
		procedure SetSpacing(Value: Integer);
		procedure SetMargin(Value: Integer);
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
		procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;
		procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
		procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
		procedure WMLButtonDblClk(var Message: TWMLButtonDblClk);
			message WM_LBUTTONDBLCLK;
		procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
		procedure Timer1Timer(Sender: TObject);
		procedure InitRect;
	protected
		procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
		procedure CreateHandle; override;
		procedure CreateParams(var Params: TCreateParams); override;
		function GetPalette: HPALETTE; override;
		procedure SetButtonStyle(ADefault: Boolean); override;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure Click; override;
	published
		property Action;
		property Anchors;
		property AutoChange: Boolean read FAutoChange write FAutoChange default False;
		property BiDiMode;
		property Cancel;
		property Caption stored IsCustomCaption;
		property Color: TColor read FColor write SetColor default clBtnFace;
		property Constraints;
		property Default;
		property Down: Boolean read FDown write SetDown default False;
		property Enabled;
		property Glyph: TBitmap read GetGlyph write SetGlyph;
		property Highlight: THighLight read FHighlight write SetHighlight default hlUnderlight;
		property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
		property Margin: Integer read FMargin write SetMargin default - 1;
		property ModalResult;
		property ParentShowHint;
		property ParentBiDiMode;
		property ShowHint;
		property Spacing: Integer read FSpacing write SetSpacing default 4;
		property TabOrder;
		property TabStop;
		property Visible;
		property OnEnter;
		property OnExit;
	end;

procedure PlayBSound(const X, MaxX: Integer; const SoundUp: Boolean);

var
	BSounds: Boolean = True;
	BSoundUp, BSoundDown, BSoundBuffer: PWave;
(*
function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
	BevelWidth: Integer; IsRounded, IsDown,
	IsFocused: Boolean): TRect;*)

{
procedure LoadBSounds;
procedure UnloadBSounds;
}

procedure Register;

implementation

uses
	Consts, SysUtils, ActnList, ImgList, MMSystem, Math,
	uGraph, uFiles, uAdd, uScreen, uSysInfo;

{ TDButton data }
var
	BadColors: Boolean;

(*
{ DrawButtonFace - returns the remaining usable area inside the Client rect.}
function DrawButtonFace(Canvas: TCanvas; const Client: TRect;
	BevelWidth: Integer; IsRounded, IsDown,
	IsFocused: Boolean): TRect;
var
	R: TRect;
	DC: THandle;
begin
	R := Client;
	with Canvas do
	begin
			Brush.Color := clBtnFace;
			Brush.Style := bsSolid;
			DC := Canvas.Handle;    { Reduce calls to GetHandle }

			if IsDown then
			begin    { DrawEdge is faster than Polyline }
				DrawEdge(DC, R, BDR_SUNKENINNER, BF_TOPLEFT);              { black     }
				DrawEdge(DC, R, BDR_SUNKENOUTER, BF_BOTTOMRIGHT);          { btnhilite }
				Dec(R.Bottom);
				Dec(R.Right);
				Inc(R.Top);
				Inc(R.Left);
				DrawEdge(DC, R, BDR_SUNKENOUTER, BF_TOPLEFT or BF_MIDDLE); { btnshadow }
			end
			else
			begin
				DrawEdge(DC, R, BDR_RAISEDOUTER, BF_BOTTOMRIGHT);          { black }
				Dec(R.Bottom);
				Dec(R.Right);
				DrawEdge(DC, R, BDR_RAISEDINNER, BF_TOPLEFT);              { btnhilite }
				Inc(R.Top);
				Inc(R.Left);
				DrawEdge(DC, R, BDR_RAISEDINNER, BF_BOTTOMRIGHT or BF_MIDDLE); { btnshadow }
			end;
	end;

	Result := Rect(Client.Left + 1, Client.Top + 1,
		Client.Right - 2, Client.Bottom - 2);
	if IsDown then OffsetRect(Result, 1, 1);
end;    *)

type
	TButtonGlyph = class
	private
		FOriginal: TDBitmap;
		FTransparentColor: TColor;
		FOnChange: TNotifyEvent;
		procedure GlyphChanged(Sender: TObject);
		procedure SetGlyph(Value: TDBitmap);
		procedure DrawButtonGlyph(BmpD: TDBitmap; const GlyphPos: TPoint;
			Transparent: Boolean);
		procedure DrawButtonText(Canvas: TCanvas; const Caption: string;
			TextBounds: TRect; Enabled: Boolean; BiDiFlags: LongInt);
		procedure CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
			const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
			Margin, Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
			BiDiFlags: LongInt);
	public
		constructor Create;
		destructor Destroy; override;
		{ return the text rectangle }
		function Draw(BmpD: TDBitmap; const Client: TRect; const Offset: TPoint;
			const Caption: string; Layout: TButtonLayout; Margin, Spacing: Integer;
			Enabled: Boolean; BiDiFlags: LongInt): TRect;
		property Glyph: TDBitmap read FOriginal write SetGlyph;
		property OnChange: TNotifyEvent read FOnChange write FOnChange;
	end;

{ TButtonGlyph }

constructor TButtonGlyph.Create;
begin
	inherited Create;
	FOriginal := TDBitmap.Create;
	FOriginal.OnChange := GlyphChanged;
	FTransparentColor := clNone;
end;

destructor TButtonGlyph.Destroy;
begin
	FOriginal.Free;
	inherited Destroy;
end;

procedure TButtonGlyph.GlyphChanged(Sender: TObject);
begin
	if Sender = FOriginal then
	begin
		FTransparentColor := GetTransparentColor(FOriginal);
		if Assigned(FOnChange) then FOnChange(Self);
	end;
end;

procedure TButtonGlyph.SetGlyph(Value: TDBitmap);
begin
	FOriginal.Assign(Value);
	if (Value <> nil) and (Value.Width > 0) and (Value.Height > 0) then
	begin
		FTransparentColor := GetTransparentColor(Value);
	end;
end;

procedure TButtonGlyph.DrawButtonGlyph(BmpD: TDBitmap; const GlyphPos: TPoint;
	Transparent: Boolean);
begin
	if FOriginal = nil then Exit;
	if (FOriginal.Width = 0) or (FOriginal.Height = 0) then Exit;

	if Transparent then
		BmpD.BmpE24(GlyphPos.x, GlyphPos.y, FOriginal, FTransparentColor, ef16)
	else
		BmpD.BmpE24(GlyphPos.x, GlyphPos.y, FOriginal, FTransparentColor, ef04);
end;

procedure TButtonGlyph.DrawButtonText(Canvas: TCanvas; const Caption: string;
	TextBounds: TRect; Enabled: Boolean; BiDiFlags: LongInt);
var C: TColor;
begin
	with Canvas do
	begin
		Brush.Style := bsClear;
		if not Enabled then
		begin
			OffsetRect(TextBounds, 1, 1);
			Font.Color := clBtnHighlight;
			DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
				DT_CENTER or DT_VCENTER or BiDiFlags);
			OffsetRect(TextBounds, -1, -1);
			Font.Color := clBtnShadow;
			DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
				DT_CENTER or DT_VCENTER or BiDiFlags);
		end
		else
		begin
			C := Canvas.Font.Color;
			OffsetRect(TextBounds, 1, 1);
			Canvas.Font.Color := MixColors(Canvas.Font.Color, clBtnFace);
			Canvas.Font.Color := MixColors(Canvas.Font.Color, clBtnFace);
			DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
				DT_CENTER or DT_VCENTER or BiDiFlags);

			OffsetRect(TextBounds, -1, -1);
			Canvas.Font.Color := C;
			DrawText(Handle, PChar(Caption), Length(Caption), TextBounds,
				DT_CENTER or DT_VCENTER or BiDiFlags);
		end;
	end;
end;

procedure TButtonGlyph.CalcButtonLayout(Canvas: TCanvas; const Client: TRect;
	const Offset: TPoint; const Caption: string; Layout: TButtonLayout; Margin,
	Spacing: Integer; var GlyphPos: TPoint; var TextBounds: TRect;
	BiDiFlags: LongInt);
var
	TextPos: TPoint;
	ClientSize, GlyphSize, TextSize: TPoint;
	TotalSize: TPoint;
begin
	if (BiDiFlags and DT_RIGHT) = DT_RIGHT then
		if Layout = blGlyphLeft then Layout := blGlyphRight
		else 
			if Layout = blGlyphRight then Layout := blGlyphLeft;
	{ calculate the item sizes }
	ClientSize := Point(Client.Right - Client.Left, Client.Bottom -
		Client.Top);

	if FOriginal <> nil then
		GlyphSize := Point(FOriginal.Width, FOriginal.Height) else
		GlyphSize := Point(0, 0);

	if Length(Caption) > 0 then
	begin
		TextBounds := Rect(0, 0, Client.Right - Client.Left, 0);
		DrawText(Canvas.Handle, PChar(Caption), Length(Caption), TextBounds,
			DT_CALCRECT or BiDiFlags);
		TextSize := Point(TextBounds.Right - TextBounds.Left, TextBounds.Bottom -
			TextBounds.Top);
	end
	else
	begin
		TextBounds := Rect(0, 0, 0, 0);
		TextSize := Point(0, 0);
	end;

	{ If the layout has the glyph on the right or the left, then both the
		text and the glyph are centered vertically.  If the glyph is on the top
		or the bottom, then both the text and the glyph are centered horizontally.}
	if Layout in [blGlyphLeft, blGlyphRight] then
	begin
		GlyphPos.Y := (ClientSize.Y - GlyphSize.Y) div 2;
		TextPos.Y := (ClientSize.Y - TextSize.Y) div 2;
	end
	else
	begin
		GlyphPos.X := (ClientSize.X - GlyphSize.X) div 2;
		TextPos.X := (ClientSize.X - TextSize.X) div 2;
	end;

	{ if there is no text or no bitmap, then Spacing is irrelevant }
	if (TextSize.X = 0) or (GlyphSize.X = 0) then
		Spacing := 0;
		
	{ adjust Margin and Spacing }
	if Margin = -1 then
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
	end;
		
	case Layout of
		blGlyphLeft:
			begin
				GlyphPos.X := Margin;
				TextPos.X := GlyphPos.X + GlyphSize.X + Spacing;
			end;
		blGlyphRight:
			begin
				GlyphPos.X := ClientSize.X - Margin - GlyphSize.X;
				TextPos.X := GlyphPos.X - Spacing - TextSize.X;
			end;
		blGlyphTop:
			begin
				GlyphPos.Y := Margin;
				TextPos.Y := GlyphPos.Y + GlyphSize.Y + Spacing;
			end;
		blGlyphBottom:
			begin
				GlyphPos.Y := ClientSize.Y - Margin - GlyphSize.Y;
				TextPos.Y := GlyphPos.Y - Spacing - TextSize.Y;
			end;
	end;
		
	{ fixup the result variables }
	with GlyphPos do
	begin
		Inc(X, Client.Left + Offset.X);
		Inc(Y, Client.Top + Offset.Y);
	end;
	OffsetRect(TextBounds, TextPos.X + Client.Left + Offset.X,
		TextPos.Y + Client.Top + Offset.X);
end;

function TButtonGlyph.Draw(BmpD: TDBitmap; const Client: TRect;
	const Offset: TPoint; const Caption: string; Layout: TButtonLayout;
	Margin, Spacing: Integer; Enabled: Boolean;
	BiDiFlags: LongInt): TRect;
var
	GlyphPos: TPoint;
begin
	CalcButtonLayout(BmpD.Canvas, Client, Offset, Caption, Layout, Margin, Spacing,
		GlyphPos, Result, BiDiFlags);
	DrawButtonGlyph(BmpD, GlyphPos, Enabled);
	DrawButtonText(BmpD.Canvas, Caption, Result, Enabled, BiDiFlags);
end;

{ TDButton }

procedure TDButton.InitRect;
var
	hR: THandle;
	Po: array[0..9] of tagPOINT;
	m: SG;
begin
	if RegCap = False then Exit;
	if True then
	begin
		Po[0].x := 0;
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
		Po[9].y := 0;

		m := Min(Width, Height) div 2;
		hR := CreateRoundRectRgn(0, 0, Width + 1, Height + 1, m, m);

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
	if Visible = False then Exit;
	if (Message.Width = 0) or (Message.Height = 0) then Exit;
	InitRect;
end;

constructor TDButton.Create(AOwner: TComponent);
begin
	FGlyph := TButtonGlyph.Create;
	TButtonGlyph(FGlyph).OnChange := GlyphChanged;
	inherited Create(AOwner);
	Font.Color := clBtnText;
	FCanvas := TCanvas.Create;
	FBmpOut := TDBitmap.Create;

	FTimer := TDTimer.Create(Self);
	FTimer.Enabled := False;
	FTimer.EventStep := esFrequency;
	FTimer.Interval := 25;
	FTimer.OnTimer := Timer1Timer;

	FLayout := blGlyphLeft;
	FSpacing := 4;
	FMargin := -1;
	FColor := clBtnFace;
	FHighlight := hlUnderlight;
	FDown := False;
	FDownNow := False;
	FLastDown := False;
	ControlStyle := ControlStyle + [csReflector];
end;

destructor TDButton.Destroy;
begin
	FTimer.Enabled := False;
	FTimer.Free; FTimer := nil;
	inherited Destroy;
	TButtonGlyph(FGlyph).Free;
	FCanvas.Free;
	FBmpOut.Free;
end;

procedure TDButton.CreateHandle;
begin
	inherited CreateHandle;
end;

procedure TDButton.CreateParams(var Params: TCreateParams);
begin
	inherited CreateParams(Params);
	with Params do Style := Style or BS_OWNERDRAW;
end;
		
procedure TDButton.SetButtonStyle(ADefault: Boolean);
begin
	if ADefault <> IsFocused then
	begin
		IsFocused := ADefault;
		Refresh;
	end;
end;

procedure TDButton.Click;
begin
	if FAutoChange then
	begin
		FDown := not FDown;
		Invalidate;
	end;
	inherited Click;
end;

procedure TDButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
	with Message.MeasureItemStruct^ do
	begin
		itemWidth := Width;
		itemHeight := Height;
	end;
end;

procedure TDButton.CMMouseEnter(var Message: TMessage);
begin
	inherited;
	{ Don't draw a border if DragMode <> dmAutomatic since this button is meant to
		be used as a dock client. }
	if (DragMode <> dmAutomatic) then
	begin
		if FMouseDown then FDownNow := True;
		FHighNow := True;
		FTimer.Reset;
		FTimer.Enabled := True;
		Invalidate;
	end;
end;

procedure TDButton.CMMouseLeave(var Message: TMessage);
begin
	inherited;
	if not Dragging then
	begin
		FDownNow := False;
		FHighNow := False;
		FTimer.Enabled := False;
		Invalidate;
	end;
end;

procedure TDButton.CNDrawItem(var Message: TWMDrawItem);
begin
	DrawItem(Message.DrawItemStruct^);
end;

procedure TDButton.DrawItem(const DrawItemStruct: TDrawItemStruct);
var
	FileName: TFileName;

	IsDown, IsDefault: Boolean;
	CDefault, CCancel, CDefaultCancel: TColor;
	Recta: TRect;

	x, y, SizeX, SizeY: Integer;
	Co: array[0..3] of TColor;
	E: TColor;
	s: string;
//	m: SG;
const
	Border = 2;
begin
	IsDefault := DrawItemStruct.itemState and ODS_FOCUS <> 0;
	IsDown := DrawItemStruct.itemState and ODS_SELECTED <> 0;
	if FDown then IsDown := not IsDown;
	if FAutoChange and (FDown <> IsDown) and (FLastDown <> IsDown) then Exit;

	// Sound
	if BSounds and (FLastDown <> IsDown) then
	begin
		if (BSoundBuffer <> nil) then
		begin
			PlayBSound(Screen.ActiveForm.Left + Left + Width div 2, Screen.Width - 1,
				IsDown);
		end;
		FLastDown := IsDown;
	end;

	// Glyph
	if Glyph.Height = 0 then
	begin
		Glyph.Height := 16;
		FileName := GraphDir + 'Images\' + ButtonNameToFileName(Name, False) + '.bmp';
		if FileExists(FileName) then
		begin
			Glyph.LoadFromFile(FileName);
		end;
	end;

	FCanvas.Handle := DrawItemStruct.hDC;
	Recta := ClientRect;
	FBmpOut.SetSize(Recta.Right - Recta.Left, Recta.Bottom - Recta.Top);

	{ DrawFrameControl doesn't allow for drawing a button as the
			default button, so it must be done here. }

	{ DrawFrameControl does not draw a pressed button correctly }
	if IsDown then
	begin
		FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			cl3DDkShadow, clBtnHighlight, 1, ef16);
		InflateRect(Recta, -1, -1);
		FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			clBtnShadow, cl3DLight, 1, ef16);
		InflateRect(Recta, -1, -1);
	end
	else
	begin
		FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
			clBtnHighlight, cl3DDkShadow, 1, ef16);
		InflateRect(Recta, -1, -1);

		if ColorToRGB(clBtnFace) <> ColorToRGB(cl3DLight) then
			E := cl3DLight
		else
			E := MixColors(FColor, clBtnHighlight);

{		if RegCap then
		begin
			m := Min(Width, Height) div 2;
			FBmpOut.Canvas.Pen.Color := E;
			FBmpOut.Canvas.RoundRect(Recta.Left, Recta.Top, Recta.Right + 1, Recta.Bottom + 1, m, m);
		end
		else
		begin}
			FBmpOut.Border24(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
				E, clBtnShadow, 1, ef16);
//		end;

		InflateRect(Recta, -1, -1);
	end;
	Co[0] := ColorDiv(FColor, 5 * 16384);
	Co[1] := ColorDiv(FColor, 3 * 16384);
	Co[2] := Co[0];
	Co[3] := Co[1];
	{$ifopt d-}
	FBmpOut.GenerateRGB(Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1,
		clNone, gfFade2x, Co, ScreenCorectColor, ef16, nil);
	{$else}
	FBmpOut.Bar24(clNone, Recta.Left, Recta.Top, Recta.Right - 1, Recta.Bottom - 1, FColor, ef16);
	{$endif}

	if IsDown then OffsetRect(Recta, 1, 1);
	FBmpOut.Canvas.Font := Self.Font;
	if NTSystem then
		if FBmpOut.Canvas.Font.Name = 'MS Sans Serif' then
			FBmpOut.Canvas.Font.Name := 'Microsoft Sans Serif';

	if (Length(Caption) > 0) and (Caption[1] = '/') then
	begin
		s := Caption;
		Delete(s, 1, 1);
		s := ButtonNameToFileName(Name, True) + s;
	end
	else
		s := Caption;

	TButtonGlyph(FGlyph).Draw(FBmpOut, Recta, Point(0, 0), s, FLayout,
		FMargin, FSpacing, Enabled, DrawTextBiDiModeFlags(0));
	if IsDown then OffsetRect(Recta, -1, -1);

	if BadColors then
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
	if Default and Cancel then
	begin
		FBmpOut.Rec24(2, 2,
			FBmpOut.Width - 3, FBmpOut.Height - 3,
			CDefaultCancel, ef16);
	end
	else if Default then
	begin
		FBmpOut.Rec24(2, 2,
			FBmpOut.Width - 3, FBmpOut.Height - 3, CDefault, ef16);
	end
	else if Cancel then
	begin
		FBmpOut.Rec24(2, 2,
			FBmpOut.Width - 3, FBmpOut.Height - 3, CCancel, ef16);
	end;
	if IsFocused and IsDefault then
	begin
		Co[0] := ColorDiv(FColor, 21504);
		Co[1] := clBlack;
		Co[2] := Co[0];
		Co[3] := Co[1];
		FBmpOut.GenerateERGB(clNone, gfFade2x, Co, $00000000, efAdd, nil);
	end;

	if FHighNow and Enabled then
	begin
{		case FHighlight of
		hlRect:
		begin
			FBmpOut.Rec24(2, 2,
				FBmpOut.Width - 3, FBmpOut.Height - 3, clHighlight, ef12);
			FBmpOut.Rec24(3, 3,
				FBmpOut.Width - 4, FBmpOut.Height - 4, clHighlight, ef12);
		end;
		hlBar:
		begin
			FBmpOut.Bar24(clNone, 2, 2,
				FBmpOut.Width - 3, FBmpOut.Height - 3, clHighlight, ef08);
		end;
		hlRectMov:
		begin
			y := Min(Width, Height) and $fffffffe - 2;
			x := FHighClock mod LongWord(y);
			if x > (y div 2) then x := y - x;
			if x < (y div 2) then
			begin
				Inc(x);
				FBmpOut.Bar24(clNone, x, x,
					FBmpOut.Width - x - 1, FBmpOut.Height - x - 1, clHighlight, ef08);
			end;
		end;
		hlBarHorz:
			FBmpOut.GenRGB(clNone, gfSpecHorz, FHighClock, ef08);
		hlBarVert:
			FBmpOut.GenRGB(clNone, gfSpecVert, FHighClock, ef08);
		hlUnderLight:
		begin
			Co[1] := ColorDiv(FColor, 32768  * LinearMax(FHighClock, 6) div 6);
			Co[0] := clBlack;
			Co[2] := Co[0];
			Co[3] := Co[1];
			FBmpOut.GenerateERGB(clNone, gfFade2x, Co, $00000000, efAdd, nil);
		end;
		end;}
		if FTimer.Clock < 3 * PerformanceFrequency then
		begin
			FBmpOut.Bar24(clNone, Border, Border,
				FBmpOut.Width - 1 - Border, FBmpOut.Height - 1 - Border, clHighlight, ef08);
		end
		else
		begin
			SizeX := Width - 2 * Border;
			SizeY := Height - 2 * Border;
			x := (SizeX * (FTimer.Clock - 3 * PerformanceFrequency) div (2 * PerformanceFrequency)) mod SizeX;
			y := (SizeY * (FTimer.Clock - 3 * PerformanceFrequency) div (2 * PerformanceFrequency)) mod SizeY;
			if x >= (SizeX div 2) then
				x := SizeX - x;
			if y >= (Height div 2) then
				y := SizeY - y;
//			if (x < SizeX div 2) and (y < SizeY div 2) then
			begin
				FBmpOut.Bar24(clNone, Border + x - 1, Border + y - 1,
					FBmpOut.Width - x - 0 - Border, FBmpOut.Height - y - 0 - Border, clHighlight, ef08);
			end;

{			y := MinXY and $fffffffe - 2;
			x := FHighClock mod LongWord(y);
			if x > (y div 2) then
				x := y - x;
			if x < (y div 2) then
			begin
				FBmpOut.Bar24(clNone, x, x,
					FBmpOut.Width - x - 1, FBmpOut.Height - x - 1, clHighlight, ef08);
			end;}
		end;
	end;
	FCanvas.Draw(0, 0, FBmpOut);
	FCanvas.Handle := 0;
end;

procedure TDButton.CMFontChanged(var Message: TMessage);
begin
	inherited;
	Invalidate;
end;

procedure TDButton.CMEnabledChanged(var Message: TMessage);
begin
	inherited;
	Invalidate;
end;

procedure TDButton.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
	Perform(WM_LBUTTONDOWN, Message.Keys, LongInt(Message.Pos));
end;

function TDButton.GetPalette: HPALETTE;
begin
	Result := Glyph.Palette;
end;
		
procedure TDButton.SetGlyph(Value: TBitmap);
begin
	TButtonGlyph(FGlyph).Glyph := Value as TDBitmap;
	if TButtonGlyph(FGlyph).Glyph.Width =
		2 * TButtonGlyph(FGlyph).Glyph.Height then
	begin
		TButtonGlyph(FGlyph).Glyph.SetSize(
			TButtonGlyph(FGlyph).Glyph.Width,
			TButtonGlyph(FGlyph).Glyph.Width);
	end;
	FModifiedGlyph := True;
	Invalidate;
end;

function TDButton.GetGlyph: TBitmap;
begin
	Result := TButtonGlyph(FGlyph).Glyph;
end;
		
procedure TDButton.GlyphChanged(Sender: TObject);
begin
	Invalidate;
end;

function TDButton.IsCustomCaption: Boolean;
begin
	Result := True;
{	Result :=	(UpperCase(Caption) <> UpperCase(Name))
		and (UpperCase(Caption) <> UpperCase(ButtonNameToFileName(Name)));}
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

procedure TDButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);

	procedure CopyImage(ImageList: TCustomImageList; Index: Integer);
	begin
		with Glyph do
		begin
			Width := ImageList.Width;
			Height := ImageList.Height;
			Canvas.Brush.Color := clFuchsia; //! for lack of a better color
			Canvas.FillRect(Rect(0, 0, Width, Height));
			ImageList.Draw(Canvas, 0, 0, Index);
		end;
	end;

begin
	inherited ActionChange(Sender, CheckDefaults);
	if Sender is TCustomAction then
		with TCustomAction(Sender) do
		begin
			{ Copy image from action's imagelist }
			if (Glyph.Empty) and (ActionList <> nil) and (ActionList.Images <> nil) and
				(ImageIndex >= 0) and (ImageIndex < ActionList.Images.Count) then
				CopyImage(ActionList.Images, ImageIndex);
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

procedure TDButton.SetHighlight(Value: THighLight);
begin
	if (Value <> FHighlight) then
	begin
		FHighLight := Value;
		Invalidate;
	end;
end;

procedure TDButton.Timer1Timer(Sender: TObject);
begin
	if (FHighNow) {and (FHighlight <> hlNone) and (FHighlight <> hlBar) and (FHighlight <> hlRect)} then
	begin
//		if FHighClock < High(FHighClock) then Inc(FHighClock) else FHighClock := 0;
		Invalidate;
	end;
end;

procedure LoadBSounds;
var
	SoundUpDataBytes, SoundDownDataBytes: Integer;
	FileName: TFileName;
begin
	FileName := SoundsDir + 'BUp.wav';
	if FileExists(FileName) then
		WaveReadFromFile(BSoundUp, FileName);
	FileName := SoundsDir + 'BDown.wav';
	if FileExists(FileName) then
		WaveReadFromFile(BSoundDown, FileName);
	if BSoundUp <> nil then
		SoundUpDataBytes := BSoundUp.DataBytes div BSoundUp.Channels
	else
		SoundUpDataBytes := 0;
	if BSoundDown <> nil then
		SoundDownDataBytes := BSoundDown.DataBytes div BSoundDown.Channels
	else
		SoundDownDataBytes := 0;
	if (SoundUpDataBytes > 0) or (SoundDownDataBytes > 0) then
		GetMem(BSoundBuffer, WaveHead + 2 * Max(SoundUpDataBytes, SoundDownDataBytes));
end;

procedure UnloadBSounds;
begin
	FreeMem(BSoundUp); BSoundUp := nil;
	FreeMem(BSoundDown); BSoundDown := nil;
	FreeMem(BSoundBuffer); BSoundBuffer := nil;
end;

procedure PlayBSound(const X, MaxX: Integer; const SoundUp: Boolean);
var SoundLeft, SoundRight: Integer;
begin
		SoundLR(SoundLeft, SoundRight, X, MaxX);
	if SoundUp then
	begin
		ConvertChannels(BSoundDown, BSoundBuffer, 2, SoundLeft, SoundRight);
		PlayWave(BSoundBuffer);
	end
	else
	begin
		ConvertChannels(BSoundUp, BSoundBuffer, 2, SoundLeft, SoundRight);
		PlayWave(BSoundBuffer);
	end;
end;

procedure TDButton.WMEraseBkGnd;
begin
	Message.Result := -1;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDButton]);
end;

initialization
	LoadBSounds;
	BadColors :=
		(ColorToRGB(clBtnFace) = ColorToRGB(clActiveBorder)) or
		(ColorToRGB(clBtnFace) = ColorToRGB(clInactiveBorder)) or
		(ColorToRGB(clActiveBorder) = ColorToRGB(clInactiveBorder));
finalization
	UnloadBSounds;
end.
