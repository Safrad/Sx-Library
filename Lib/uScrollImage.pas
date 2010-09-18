// Build: 07/2000-09/2000 Author: Safranek David

unit uScrollImage;

interface

{$R *.RES}
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls,
	uGraph24;

type
	TMouseAction = (mwNone, mwScroll,
		mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2,
		mwScrollV, mwScrollVD, mwScrollVU, mwScrollVD2, mwScrollVU2);
	TScrollImage = class(TImage)
	private
		{ Private declarations }
		FDrawFPS: Boolean;
		FWaitVBlank: Boolean;
		FOnFill: TNotifyEvent;
		FOnPaint: TNotifyEvent;
		MouseX, MouseY: Integer;
		BOfsX, BOfsY: Integer;
		HType, VType: Byte;
		NowMaxWidth, NowMaxHeight: Integer;
		SliderHX1,
		SliderHX2,
		SliderVY1,
		SliderVY2: Integer;
		LTickCount: LongWord;
	protected
		{ Protected declarations }
	public
		{ Public declarations }
		MouseAction: TMouseAction;
		Bitmap24: TBitmap24;
		ScrollBarHWidth, ScrollBarHHeight,
		ScrollBarVWidth, ScrollBarVHeight: Integer;
		OfsX, OfsY: Integer;
		MaxOfsX, MaxOfsY: Integer;
		BitmapWidth, BitmapHeight: Integer;
		FramePerSec: Extended;
		FHandScroll: Boolean;
		procedure InitScrolls;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure DrawArrow(X1, Y1, X2, Y2: Integer; Down: Boolean;
			Orient: Integer);
		procedure OffsetRange(var NOfsX, NOfsY: Integer);
		procedure ScrollTo(NOfsX, NOfsY: Integer);
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure Fill;
		procedure Paint; override;
		function MouseWhere(const X, Y: Integer): TMouseAction;
	published
		{ Published declarations }
		property HandScroll: Boolean read FHandScroll write FHandScroll;
		property DrawFPS: Boolean read FDrawFPS write FDrawFPS;
		property WaitVBlank: Boolean read FWaitVBlank write FWaitVBlank;
		property OnFill: TNotifyEvent read FOnFill write FOnFill;
		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
	end;

procedure ZoomMake(
	BmpSource: TBitmap;
	VisX, VisY: Integer;
	AsWindow: Boolean; Zoom: Extended; XYConst: Boolean; QualityResize: Boolean;
	OX, OY: Integer;
	var SourceWidth, SourceHeight: Integer;
	var SX1, SY1, SXW, SYH: Integer;
	var DX1, DY1, DXW, DYH: Integer;
	var BmpSource2: TBitmap);

procedure Register;

implementation

uses
	Math,
	uAdd, uGraph;
const
	OfsS = 20; // ms; FPS = 1000 / OfsS; 25-30FPS for VR; 50 = TV
	ScrollEf = ef12;
	ScrollEf2 = ef12;

constructor TScrollImage.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	InitBitmap(Picture.Bitmap);
end;

destructor TScrollImage.Destroy;
begin
	inherited Destroy;
	Bitmap24.Free; Bitmap24 := nil;
end;

function TScrollImage.MouseWhere(const X, Y: Integer): TMouseAction;
begin
	Result := mwScroll;
	if VType <> 0 then
	if X + ScrollBarVWidth > Picture.Width  then
	begin // V
		if Y < ScrollBarHHeight then
			Result := mwScrollVD
		else if Y < SliderVY1 then
			Result := mwScrollVD2
		else if Y <= SliderVY2 then
			Result := mwScrollV
		else if Y < ScrollBarVHeight - ScrollBarHHeight then
			Result := mwScrollVU2
		else if Y < ScrollBarVHeight then
			Result := mwScrollVU;
	end;
	if HType <> 0 then
	if Y + ScrollBarHHeight > Picture.Height  then
	begin // H
		if X < ScrollBarVWidth then
			Result := mwScrollHD
		else if X < SliderHX1 then
			Result := mwScrollHD2
		else if X <= SliderHX2 then
			Result := mwScrollH
		else if X < ScrollBarHWidth - ScrollBarVWidth then
			Result := mwScrollHU2
		else if X < ScrollBarHWidth then
			Result := mwScrollHU;
	end;
end;

procedure TScrollImage.OffsetRange(var NOfsX, NOfsY: Integer);
begin
	if NOfsX > BitmapWidth - NowMaxWidth then
		NOfsX := BitmapWidth - NowMaxWidth;
	if NOfsX < 0 then
		NOfsX := 0;

	if NOfsY > BitmapHeight - NowMaxHeight then
		NOfsY := BitmapHeight - NowMaxHeight;
	if NOfsY < 0 then
		NOfsY := 0;
end;

procedure TScrollImage.ScrollTo(NOfsX, NOfsY: Integer);
begin
	OffsetRange(NOfsX, NOfsY);
	if (OfsX <> NOfsX) or (OfsY <> NOfsY) then
	begin
		OfsX := NOfsX;
		OfsY := NOfsY;
		Fill;
	end;
end;

procedure TScrollImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
const
	Speed1 = 65536 div 8; // Pixels / ms (x65536)
	StepInt = 500;
var
	Speed: Int64;
	TimeO, LastTickCount, FrameTickCount: LongWord;
	Cycle: Cardinal;
	NOfsX, NOfsY: Integer;
	MouseA: TMouseAction;
begin
	if Button = mbLeft then
	begin
		MouseA := MouseWhere(X, Y);
		case MouseA of
		mwScrollH:
		begin
			MouseAction := mwScrollH;
			MouseX := X;
			BOfsX := OfsX;
			inherited MouseDown(Button, Shift, X, Y);
		end;
		mwScrollV:
		begin
			MouseAction := mwScrollV;
			MouseY := Y;
			BOfsY := OfsY;
			inherited MouseDown(Button, Shift, X, Y);
		end;
		mwScrollHD, mwScrollHU,
		mwScrollVD, mwScrollVU,
		mwScrollHD2, mwScrollHU2,
		mwScrollVD2, mwScrollVU2:
		begin
			MouseAction := MouseA;
			LastTickCount := GetTickCount;
			case MouseA of
{     mwScrollHD, mwScrollHU,
			mwScrollVD, mwScrollVU: Speed := Speed1;}
			mwScrollHD2, mwScrollHU2: Speed := 64 * Int64(MaxOfsX);
			mwScrollVD2, mwScrollVU2: Speed := 64 * Int64(MaxOfsY);
			else Speed := Speed1;
			end;
			if Speed = 0 then Speed := 1;

			TimeO := 10; // ms
			NOfsX := RoundDiv64(65536 * Int64(OfsX), Speed);
			NOfsY := RoundDiv64(65536 * Int64(OfsY), Speed);
{     case MouseW of
			mwScrollHD, mwScrollHU,
			mwScrollHD2, mwScrollHU2:
			begin
			else
			begin
			end;}
//      MouseW := mdScrollB;
			inherited MouseDown(Button, Shift, X, Y);
			Cycle := 0;
			FrameTickCount := 0;
			while MouseAction <> mwNone do
			begin
				case MouseA of
				mwScrollHD, mwScrollHD2: Dec(NOfsX, TimeO);
				mwScrollHU, mwScrollHU2: Inc(NOfsX, TimeO);
				mwScrollVD, mwScrollVD2: Dec(NOfsY, TimeO);
				mwScrollVU, mwScrollVU2: Inc(NOfsY, TimeO);
				end;
				ScrollTo(RoundDiv64(Int64(Speed) * Int64(NOfsX), 65536),
					RoundDiv64(Int64(Speed) * Int64(NOfsY), 65536));
//				Application.HandleMessage; no
				Application.ProcessMessages;
				TimeO := GetTickCount - LastTickCount;
				if TimeO < OfsS then
				begin
					Sleep(OfsS - TimeO);
					TimeO := OfsS;
				end;
				Inc(LastTickCount, TimeO);
				Inc(Cycle);
				if LastTickCount >= FrameTickCount then
				begin
					FramePerSec := 1000 * Cycle / (LastTickCount - FrameTickCount + StepInt);
					FrameTickCount := LastTickCount + StepInt;
					Cycle := 0;
				end;
			end;
			FramePerSec := 0;
		end;
		mwScroll:
		begin
			if ((MaxOfsX > 0) or (MaxOfsY > 0)) and (HandScroll or (ssShift in Shift)) then
			begin
				Screen.Cursor := 2;
				MouseAction := mwScroll;
				MouseX := OfsX + X;
				MouseY := OfsY + Y;
			end
			else
				MouseAction := mwNone;
			inherited MouseDown(Button, Shift, X, Y);
		end;
		end;
	end;
		inherited MouseDown(Button, Shift, X, Y);
end;

procedure TScrollImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	NOfsX, NOfsY: Integer;
	TickCount: LongWord;
begin
	if ((BitmapWidth - NowMaxWidth > 0) or (BitmapHeight - NowMaxHeight > 0)) and (MouseWhere(X, Y) = mwScroll) and
	(HandScroll or (ssShift in Shift)) then
	begin
		Cursor := 1
	end
	else
		Cursor := crDefault;

	NOfsX := OfsX;
	NOfsY := OfsY;
	case MouseAction of
	mwScroll:
	begin
		if Cursor = 1 then
		begin
			NOfsX := MouseX - X;
			NOfsY := MouseY - Y;
		end;
	end;
	mwScrollH:
	begin
		NOfsX := BOfsX + RoundDiv64(BitmapWidth * Int64(X - MouseX), NowMaxWidth - 2 * ScrollBarHHeight);
		NOfsY := OfsY;
	end;
	mwScrollV:
	begin
		NOfsX := OfsX;
		NOfsY := BOfsY + RoundDiv64(BitmapHeight * (Y - MouseY), NowMaxHeight - 2 * ScrollBarVWidth);
	end;
	end;
	TickCount := GetTickCount;
	if TickCount < LTickCount + OfsS then
	begin
		Sleep(LTickCount + OfsS - TickCount);
		TickCount := LTickCount + OfsS;
	end;
	LTickCount := TickCount;
	ScrollTo(NOfsX, NOfsY);
//  if (MouseW = mwNone){ or (MouseW = mwScroll)} then
	inherited MouseMove(Shift, X, Y);
end;

procedure TScrollImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	if Button = mbLeft then
	begin
		case MouseAction of
		mwScrollHD, mwScrollHU,
		mwScrollVD, mwScrollVU,
		mwScrollHD2, mwScrollHU2,
		mwScrollVD2, mwScrollVU2:
		begin
			MouseAction := mwNone;
			Fill;
		end;
		mwScroll:
		begin
			MouseAction := mwNone;
			Screen.Cursor := crDefault;
		end
		else
		begin
			MouseAction := mwNone;
		end;
		end;
	end;
	inherited MouseUp(Button, Shift, X, Y);
end;

procedure TScrollImage.InitScrolls;
begin
	ScrollBarVWidth := GetSystemMetrics(SM_CXVSCROLL);

	if BitmapWidth > Picture.Width then
		HType := 1
	else if BitmapWidth > Picture.Width - ScrollBarVWidth then
		HType := 2
	else
		HType := 0;

	ScrollBarHHeight := GetSystemMetrics(SM_CYHSCROLL);
	if BitmapHeight > Picture.Height then
	begin
		VType := 1;
		if HType = 2 then HType := 1;
	end
	else if BitmapHeight > Picture.Height - ScrollBarHHeight then
	begin
		case HType of
		0:
			VType := 0;
		2:
		begin
			VType := 0;
			HType := 0;
		end;
		else
			VType := 1;
		end;
	end
	else
	begin
		VType := 0;
		if HType = 2 then HType := 0;
	end;

	NowMaxWidth := Picture.Width - ScrollBarVWidth * VType;
	NowMaxHeight := Picture.Height - ScrollBarHHeight * HType;
	MaxOfsX := BitmapWidth - NowMaxWidth;
	MaxOfsY := BitmapHeight - NowMaxHeight;
end;

procedure TScrollImage.DrawArrow(X1, Y1, X2, Y2: Integer; Down: Boolean;
	Orient: Integer);
var
	C1, C2: Integer;
	i: Integer;
	XM, HX1, HY1, HX2, HY2, H, Len: Integer;
begin
	if Down then
	begin
		C1 := 1;
		C2 := 2;
	end
	else
	begin
		C1 := 3;
		C2 := 0;
	end;
	Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(C1), DepthColor(C2), 1, ScrollEf);
	Bar24(Bitmap24, clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);

	if Down then
	begin
		Inc(X1);
		Inc(X2);
		Inc(Y1);
		Inc(Y2);
	end;
	XM := X1 + X2;
	Len := X2 - X1 + 1;
	for i := 0 to Len div 3 - 1 do
	begin
		HX1 := (XM - 2 * i) div 2;
		HY1 := Y1 + i + (Len + 2) div 3;
		HX2 :=  (XM + 2 * i + 1) div 2;
		HY2 := Y1 + i + (Len + 2) div 3;
		case Orient of
		1:
		begin
			H := HX1;
			HX1 := (HY1 - Y1) + X1;
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := (HY2 - Y1) + X1;
			HY2 := (H - X1) + Y1;
		end;
		2:
		begin
			HY1 := Y2 - (HY1 - Y1);
			HY2 := Y2 - (HY2 - Y1);
		end;
		3:
		begin
			H := HX1;
			HX1 := X2 - (HY1 - Y1);
			HY1 := (H - X1) + Y1;
			H := HX2;
			HX2 := X2 - (HY2 - Y1);
			HY2 := (H - X1) + Y1;
		end;
		end;

		Lin24(Bitmap24, HX1, HY1, HX2, HY2, clBtnText, ef16);
	end;
end;

procedure TScrollImage.Fill;
var
	ScrollLen, ScrollLenS: Integer;
	X1, Y1, X2, Y2: Integer;
	C: TColor;
	SliderC1, SliderC2: TColor;
begin
	Bitmap24 := Conv24(Picture.Bitmap);
	try
		InitScrolls;
		OffsetRange(OfsX, OfsY);
		if Assigned(FOnFill) then FOnFill(Self);
		InitScrolls;
		OffsetRange(OfsX, OfsY);
//    OffsetRange(NOfsX, NOfsY);
//    ScrollTo(OfsX, OfsY);
		SliderC1 := DarkerColor(clScrollBar);
		SliderC2 := LighterColor(clScrollBar);
		// H
		if (BitmapWidth > 0) and (HType = 1) then
		begin
			ScrollBarHWidth := NowMaxWidth;
			Y1 := Integer(Bitmap24.Height) - ScrollBarHHeight;
			Y2 := Bitmap24.Height - 1;

			X1 := 0;
			X2 := ScrollBarVWidth - 1;
	{   Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHD, 1);

			X1 := NowMaxWidth - ScrollBarVWidth;
			X2 := NowMaxWidth - 1;
	{   Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHU, 3);

			// TScrollBoxSlider
			ScrollLen := NowMaxWidth - 2 * ScrollBarVWidth;
			ScrollLenS := NowMaxWidth * ScrollLen div BitmapWidth;
			if ScrollLenS < ScrollBarHHeight div 2 then ScrollLenS := ScrollBarHHeight div 2;

			Y1 := Integer(Bitmap24.Height) - ScrollBarHHeight;
			Y2 := Bitmap24.Height - 1;
			X1 := ScrollBarVWidth + RoundDiv64(Int64(ScrollLen - ScrollLenS) * OfsX, BitmapWidth - NowMaxWidth);
			X2 := X1 + ScrollLenS - 1;
			SliderHX1 := X1;
			SliderHX2 := X2;

			Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);

			// =
			X1 := ScrollBarVWidth;
			X2 := SliderHX1 - 1;
			Y1 := Integer(Bitmap24.Height) - ScrollBarHHeight;
			Y2 := Bitmap24.Height - 1;

			if X2 >= X1 then
			begin
				if (MouseAction <> mwScrollHD2) then
					C := clScrollBar
				else
					C := clHighlight;
				Lin24(Bitmap24, X1, Y1, X2, Y1, SliderC1, ScrollEf2);
				Lin24(Bitmap24, X1, Y2, X2, Y2, SliderC2, ScrollEf2);
				Bar24(Bitmap24, clNone,
					X1, Y1 + 1,
					X2, Y2 - 1, C, ScrollEf2);
			end;

			X1 := SliderHX2 + 1;
			X2 := NowMaxWidth - ScrollBarVWidth - 1;
			Y1 := Integer(Bitmap24.Height) - ScrollBarHHeight;
			Y2 := Bitmap24.Height - 1;

			if X2 >= X1 then
			begin
				if (MouseAction <> mwScrollHU2) then
					C := clScrollBar
				else
					C := clHighlight;
				Lin24(Bitmap24, X1, Y1, X2, Y1, SliderC1, ScrollEf2);
				Lin24(Bitmap24, X1, Y2, X2, Y2, SliderC2, ScrollEf2);
				Bar24(Bitmap24, clNone,
					X1, Y1 + 1,
					X2, Y2 - 1, C, ScrollEf2);
			end;
		end
		else
			ScrollBarHWidth := 0;

		// V
		if (BitmapHeight > 0) and (VType = 1) then
		begin
			ScrollBarVHeight := NowMaxHeight;
			X1 := Integer(Bitmap24.Width) - ScrollBarVWidth;
			X2 := Bitmap24.Width - 1;

			Y1 := 0;
			Y2 := ScrollBarHHeight - 1;
	{   Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVD, 0);

			Y1 := NowMaxHeight - ScrollBarHHeight;
			Y2 := NowMaxHeight - 1;
	{   Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVU, 2);

			// TScrollBoxSlider
			ScrollLen := NowMaxHeight - 2 * ScrollBarHHeight;
			ScrollLenS := NowMaxHeight * ScrollLen div BitmapHeight;
			if ScrollLenS < ScrollBarVWidth div 2 then ScrollLenS := ScrollBarVWidth div 2;

			X1 := Integer(Bitmap24.Width) - ScrollBarVWidth;
			X2 := Bitmap24.Width - 1;
			Y1 := ScrollBarHHeight + (ScrollLen - ScrollLenS) * OfsY div (BitmapHeight - NowMaxHeight);
			Y2 := Y1 + ScrollLenS - 1;
			SliderVY1 := Y1;
			SliderVY2 := Y2;

			Border24(Bitmap24, X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bar24(Bitmap24, clNone, X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);

			// ||
			Y1 := ScrollBarHHeight;
			Y2 := SliderVY1 - 1;
			X1 := Integer(Bitmap24.Width) - ScrollBarVWidth;
			X2 := Bitmap24.Width - 1;

			if Y2 >= Y1 then
			begin
				if (MouseAction <> mwScrollVD2) then
					C := clScrollBar
				else
					C := clHighlight;
				Lin24(Bitmap24, X1, Y1, X1, Y2, SliderC1, ScrollEf2);
				Lin24(Bitmap24, X2, Y1, X2, Y2, SliderC2, ScrollEf2);
				Bar24(Bitmap24, clNone,
					X1 + 1, Y1,
					X2 - 1, Y2, C, ScrollEf2);
			end;

			Y1 := SliderVY2 + 1;
			Y2 := NowMaxHeight - ScrollBarHHeight - 1;
			X1 := Integer(Bitmap24.Width) - ScrollBarVWidth;
			X2 := Bitmap24.Width - 1;

			if Y2 >= Y1 then
			begin
				if (MouseAction <> mwScrollVU2) then
					C := clScrollBar
				else
					C := clHighlight;
				Lin24(Bitmap24, X1, Y1, X1, Y2, SliderC1, ScrollEf2);
				Lin24(Bitmap24, X2, Y1, X2, Y2, SliderC2, ScrollEf2);
				Bar24(Bitmap24, clNone,
					X1 + 1, Y1,
					X2 - 1, Y2, C, ScrollEf2);
			end;
		end
		else
			ScrollBarVHeight := 0;

//    Pix24(Bitmap24.PData, Bitmap24.ByteX, MouseX, MouseY, clWhite, ef16);
		if FDrawFPS then
			if FramePerSec >= 0.1 then
			begin
				ShadowText(Picture.Bitmap.Canvas, 0, 0,
					Using('~### ##0.0', Round(10 * FramePerSec)),
					clWindowText, clNone);
			end;
	finally
		Bitmap24.Free; Bitmap24 := nil;
		Paint;
	end;
end;

procedure TScrollImage.Paint;
begin
	if WaitVBlank then WaitRetrace;
{ SetStretchBltMode(inherited Canvas.Handle, STRETCH_DELETESCANS);
	BitBlt(
		inherited Canvas.Handle,
		Left,
		Top,
		Width,
		Height,
		Picture.Bitmap.Canvas.Handle,
		0,
		0,
		SRCCOPY
	 );}
	inherited Paint;
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure ZoomMake(
	BmpSource: TBitmap;
	VisX, VisY: Integer;
	AsWindow: Boolean; Zoom: Extended; XYConst: Boolean; QualityResize: Boolean;
	OX, OY: Integer;
	var SourceWidth, SourceHeight: Integer;
	var SX1, SY1, SXW, SYH: Integer;
	var DX1, DY1, DXW, DYH: Integer;
	var BmpSource2: TBitmap);

var
	SX, SY: Integer;
	BmpDe, BS: TBitmap24;
begin
	if AsWindow then
	begin
		if XYConst = False then
		begin
			SX := VisX;
			SY := VisY;
		end
		else
		begin
			if VisY * BmpSource.Width >=
				VisX * BmpSource.Height then
			begin
				SX := VisX;
				SY := RoundDiv(VisX * BmpSource.Height,
					BmpSource.Width);
			end
			else
			begin
				SX := RoundDiv(VisY * BmpSource.Width,
					BmpSource.Height);
				SY := VisY;
			end;
		end;

		SX1 := 0;
		SY1 := 0;
		SXW := BmpSource.Width;
		SYH := BmpSource.Height;

		DX1 := 0;
		DY1 := 0;
		DXW := SX;
		DYH := SY;
{   ZoomedWidth := BmpSource.Width;
		ZoomedHeight := BmpSource.Height;}

		SourceWidth := SX;
		SourceHeight := SY;
	end
	else
	begin
		SX := Round(Zoom * BmpSource.Width);
		SY := Round(Zoom * BmpSource.Height);

		SourceWidth := SX;
		SourceHeight := SY;

{   if SX > VisX then SX := VisX;
		if SY > VisY then SY := VisY;}
		if Zoom <= 1 then
		begin
			DX1 := 0;
			DY1 := 0;
		end
		else
		begin
			DX1 := -Round(Zoom * Frac(OX / Zoom));
			DY1 := -Round(Zoom * Frac(OY / Zoom));
		end;

		if Zoom <= 1 then
		begin
			SX1 := OX;
			SY1 := OY;
		end
		else
		begin
			SX1 := Trunc(OX / Zoom);
			SY1 := Trunc(OY / Zoom);
		end;
		SXW := Ceil(VisX / Zoom - DX1);
		SYH := Ceil(VisY / Zoom - DY1);
{   if (DX1 < 0) then
		begin
			Inc(SXW);
		end;
		if (DY1 < 0) then
		begin
			Inc(SYH);
		end;}


		DXW := Round(Zoom * SXW);
		DYH := Round(Zoom * SYH);
	end;

	if (SourceWidth < BmpSource.Width) or (SourceHeight < BmpSource.Height) then
	begin
		if not Assigned(BmpSource2) then
		begin
			BmpSource2 := TBitmap.Create;
			BmpSource2.PixelFormat := pf24bit;
		end;
		if (BmpSource2.Width <> SourceWidth)
		or (BmpSource2.Height <> SourceHeight) then
		begin
			BmpSource2.Width := SourceWidth;
			BmpSource2.Height := SourceHeight;
			if (QualityResize = False) {or (Zoom > 1)} then
			begin

				SetStretchBltMode(BmpSource2.Canvas.Handle, COLORONCOLOR);
				StretchBlt(BmpSource2.Canvas.Handle,
					0, 0,
					BmpSource2.Width, BmpSource2.Height,
					BmpSource.Canvas.Handle,
					0, 0,
					BmpSource.Width, BmpSource.Height,
					SRCCOPY);
	{     BmpSource2.Canvas.StretchDraw(Rect(0, 0,
					BmpSource2.Width, BmpSource2.Height), BmpSource);}
			end
			else
			begin
				Screen.Cursor := crHourGlass;
				BmpDe := Conv24(BmpSource2);
				BS := Conv24(BmpSource);
				Resize24(BmpDe, BS,
					SourceWidth, SourceHeight, nil);
				BS.Free;
				BmpDe.Free;
				Screen.Cursor := crDefault;
			end;
		end;
		SXW := DXW;
		SYH := DYH;
	end
	else
	begin
		if Assigned(BmpSource2) then
		begin
			BmpSource2.Free; BmpSource2 := nil;
		end;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TScrollImage]);
end;


initialization
	Screen.Cursors[1] := LoadCursor(HInstance, PChar('HANDPOINT'));
	Screen.Cursors[2] := LoadCursor(HInstance, PChar('HANDPOINTDOWN'));
end.
