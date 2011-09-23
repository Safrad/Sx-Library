unit uDLabel;

interface

{$R *.RES}
uses
	uTypes, uMath, uDWinControl,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls, uGraph, uDBitmap, uDispl, uDrawStyle;

type
	TDLabel = class(TDWinControl)
	private
		FBmpText: TDBitmap;

		// Properties
		FAlignment: TAlignment;
		FBevelInner: TPanelBevel;
		FBevelOuter: TPanelBevel;
		FBevelWidth: TBevelWidth;
		FBorderWidth: TBorderWidth;
		FBorderStyle: TBorderStyle;
		FFocusControl: TWinControl;
		FCaption: string;
		FFontEffect: TEffect;
		FFontAngle: TAngle;
		FFontShadow: SG;
		FDispl: TDispl;
		FLayout: TTextLayout;
		FCanvas: TCanvas;

		FTransparent: Boolean;
		FWordWrap: Boolean;

		// Events
		FOnPaint: TNotifyEvent;
		procedure SetCaption(Value: string);

		procedure SetFontShadow(Value: SG);
		procedure SetFontAngle(Value: TAngle);
		procedure SetFontEffect(Value: TEffect);
		procedure DisplChanged(ADispl: TObject);
		procedure SetDispl(Value: TDispl);

		procedure SetBevelInner(Value: TPanelBevel);
		procedure SetBevelOuter(Value: TPanelBevel);
		procedure SetBevelWidth(Value: TBevelWidth);
		procedure SetBorderWidth(Value: TBorderWidth);
		procedure SetBorderStyle(Value: TBorderStyle);

		procedure SetWordWrap(const Value: Boolean);
		procedure SetLayout(const Value: TTextLayout);
		procedure SetAlignment(const Value: TAlignment);

		procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
	protected
		{ Protected declarations }
		property Canvas: TCanvas read FCanvas;
		procedure FillBitmap; override;
	public
		{ Public declarations }
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
	published
		{ Published declarations }
		property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
		property Caption: string read FCaption write SetCaption;
		property Color default clBtnFace;

		property FocusControl: TWinControl read FFocusControl write FFocusControl;
		property Font;
		property FontEffect: TEffect read FFontEffect write SetFontEffect default ef16;
		property FontAngle: TAngle read FFontAngle write SetFontAngle default 0;
		property FontShadow: SG read FFontShadow write SetFontShadow default 0;
		property Displ: TDispl read FDispl write SetDispl;

		property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
		property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
		property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
		property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
		property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsNone;

		property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
		property ParentColor;
		property Hint;
		property ShowHint;
		property ParentShowHint;
		property ParentFont;
		property PopupMenu;

		property Transparent: Boolean read FTransparent write FTransparent default False;

		property WordWrap: Boolean read FWordWrap write SetWordWrap default False;

		property OnClick;
		property OnDblClick;
		property OnMouseMove;
		property OnMouseDown;
		property OnMouseUp;

		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

procedure Register;

implementation

uses uStrings, uScreen, uMsg, uColor;

constructor TDLabel.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);

//	DoubleBuffered := True; // Blinks with black if DoubleBuffer enabled
	
	FCanvas := TControlCanvas.Create;
	TControlCanvas(FCanvas).Control := Self;

	FDispl := TDispl.Create;
	FDispl.OnChange := DisplChanged;

	FBevelOuter := bvRaised;
	FBevelWidth := 1;
	FFontEffect := ef16;
	FAlignment := taLeftJustify;
	FLayout := tlCenter;

	Width := 64;
	Height := 64;
end;

destructor TDLabel.Destroy;
begin
	FreeAndNil(FCanvas);
	FreeAndNil(FDispl);
	FreeAndNil(FBmpText);
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

procedure TDLabel.SetWordWrap(const Value: Boolean);
begin
	if FWordWrap <> Value then
	begin
		FWordWrap := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetCaption(Value: string);
begin
	if Value <> FCaption then
	begin
		FCaption := Value;
		Invalidate;
	end;
end;

procedure TDLabel.SetFontShadow(Value: SG);
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

procedure TDLabel.SetLayout(const Value: TTextLayout);
begin
	if FLayout <> Value then
	begin
		FLayout := Value;
		Invalidate;
	end;
end;

procedure TDLabel.FillBitmap;
var
	Recta: TRect;
	TopColor, BottomColor: TColor;
	i: Integer;
	{$ifopt d-}Co: array[0..3] of TColor;{$endif}
begin
	inherited;
	Recta.Left := 0;
	Recta.Top := 0;
	Recta.Right := Width - 1;
	Recta.Bottom := Height - 1;

// Border
	if (FBorderStyle <> bsNone) then
	begin
		Bitmap.Border(clBtnShadow, clBtnHighlight, 1, ef16);
		Bitmap.Border(1, 1, Bitmap.Width - 2, Bitmap.Height - 2,
			cl3DDkShadow, cl3DLight, 1, ef16);
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
		Bitmap.Border(Recta.Left, Recta.Top, Recta.Right, Recta.Bottom,
			TopColor, BottomColor, FBevelWidth, ef16);
		InflateRect(Recta, -SG(FBevelWidth), -SG(FBevelWidth));
	end;
	if (Color <> clNone) then
	begin
		for i := 0 to FBorderWidth - 1 do
			Bitmap.Rec(Recta.Left + i, Recta.Top + i,
				Recta.Right - i, Recta.Bottom - i,
				Color, ef16);
		InflateRect(Recta, -FBorderWidth, -FBorderWidth);
	end;
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
		Bitmap.Border(Recta.Left, Recta.Top, Recta.Right, Recta.Bottom,
			TopColor, BottomColor, FBevelWidth, ef16);
		InflateRect(Recta, -FBevelWidth, -FBevelWidth);
	end;
// Background
	if Color <> clNone then
	begin
		{$ifopt d-}
		Co[0] := LighterColor(ColorToRGB(Color));
		Co[1] := DarkerColor(ColorToRGB(Color));
		Co[2] := Co[0];
		Co[3] := Co[1];
		Bitmap.GenerateRGBEx(Recta.Left, Recta.Top, Recta.Right, Recta.Bottom,
			gfFade2x, Co, ef16, 0, nil);
//			FBmpOut.FormBitmap(Color);
		{$else}
		Bitmap.Bar(Recta, Color, ef16);
		{$endif}
	end;

//Caption
	if (Caption <> '') and (FFontEffect <> ef00) then
	begin
		if (not Assigned(FBmpText)) then
		begin
			FBmpText := TDBitmap.Create;
		end;

		FBmpText.SetSize(Bitmap.Width, Bitmap.Height, Color);
		FBmpText.TransparentColor := Color;

		FBmpText.Canvas.Brush.Style := bsClear;
		FBmpText.Canvas.Font := Font;
		Bitmap.Canvas.Font := Font;

		FBmpText.Canvas.Font := Font;
		Bitmap.Canvas.Font := Font;
		InflateRect(Recta, -1, -1);
		if Displ.Enabled then
		begin
			if FFontEffect <> ef16 then
				DisplDraw(FBmpText, RemoveSingleAmp(Caption), FDispl, Recta, FAlignment, FLayout,
					ef16)
			else
				DisplDraw(Bitmap, RemoveSingleAmp(Caption), FDispl, Recta, FAlignment, FLayout,
					ef16);
		end
		else
		begin
			if (FFontAngle = 0) and (FFontEffect = ef16) then
			begin
				Bitmap.Canvas.Brush.Color := Color;
				Bitmap.Canvas.Brush.Style := bsClear;
				DrawCuttedText(Bitmap.Canvas, Recta, FAlignment, FLayout, Caption, FWordWrap, FFontShadow);
			end
			else
			begin
				DrawCuttedText(FBmpText.Canvas, Recta, FAlignment, FLayout, Caption, FWordWrap, FFontShadow);
			end;
		end;

		if FFontAngle = 0 then
		begin
			if FFontEffect <> ef16 then
				Bitmap.Bmp(0, 0, FBmpText, FFontEffect);
		end
		else
		begin
			RotateDef(Bitmap, FBmpText, 0, FFontAngle, FFontEffect);
		end;
	end;
end;

procedure TDLabel.SetAlignment(const Value: TAlignment);
begin
	if FAlignment <> Value then
	begin
		FAlignment := Value;
		Invalidate;
	end;
end;

procedure TDLabel.CMDialogChar(var Message: TCMDialogChar);
begin
	if (FFocusControl <> nil) and Enabled {and ShowAccelChar} and
		IsAccel(Message.CharCode, Caption) then
		with FFocusControl do
			if CanFocus then
			begin
				SetFocus;
				Message.Result := 1;
			end;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TDLabel]);
end;

end.
