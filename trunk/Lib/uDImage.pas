//* File:     Lib\uDImage.pas
//* Created:  2000-07-01
//* Modified: 2005-02-17
//* Version:  X.X.33.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDImage;

interface

{$R *.RES}
uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls,
	uDBitmap;

type
	TMouseAction = (mwNone, mwScroll,
		mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2,
		mwScrollV, mwScrollVD, mwScrollVU, mwScrollVD2, mwScrollVU2);
	TDImage = class(TWinControl)
	private
		{ Private declarations }                  
		FHotTrack: Boolean;

		FDrawFPS: Boolean;
		FOnFill: TNotifyEvent;
		FOnPaint: TNotifyEvent;
		FOnMouseEnter: TNotifyEvent;
		FOnMouseLeave: TNotifyEvent;
		MouseX, MouseY: Integer;
		BOfsX, BOfsY: Integer;
		HType, VType: Byte;
		NowMaxWidth, NowMaxHeight: Integer;
		SliderHX1,
		SliderHX2,
		SliderVY1,
		SliderVY2: Integer;
		FCanvas: TCanvas;
		LCursor: TCursor;

		procedure WMChar(var Message: TWMChar); message WM_CHAR;

		procedure CMMouseEnter(var Message: TWMMouse); message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TWMMouse); message CM_MOUSELEAVE;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;

		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;

		procedure SetHotTrack(Value: Boolean);
	protected
		{ Protected declarations }
		procedure CreateParams(var Params: TCreateParams); override;

		procedure Paint; virtual;
		procedure PaintWindow(DC: HDC); override;
		property Canvas: TCanvas read FCanvas;
	public
		{ Public declarations }
		Cur: TCursor;
		MouseL, MouseM, MouseR: Boolean;
		MouseAction: TMouseAction;
		MouseWhere: TMouseAction;
		MouseOn: Boolean;

		Bitmap: TDBitmap;
		ScrollBarHWidth, ScrollBarHHeight,
		ScrollBarVWidth, ScrollBarVHeight: Integer;
		OfsX, OfsY: Integer;
		MaxOfsX, MaxOfsY: Integer;
		BitmapWidth, BitmapHeight: Integer;
		FramePerSec: Extended;
		PaintCount: Integer;
		FHandScroll: Boolean;
		procedure InitScrolls;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure OffsetRange(var NOfsX, NOfsY: Integer);
		procedure ScrollTo(NOfsX, NOfsY: Integer);
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
{		function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
			MousePos: TPoint): Boolean; override;}
		function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
		function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
		procedure Fill; {virtual; }dynamic;
//		procedure Paint; //override;
		function MouseWh(const X, Y: Integer): TMouseAction;
	published
		{ Published declarations }
		property DrawFPS: Boolean read FDrawFPS write FDrawFPS;
		property HandScroll: Boolean read FHandScroll write FHandScroll;
		property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
		property OnFill: TNotifyEvent read FOnFill write FOnFill;
		property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;

		property Align;
		property Anchors;
		property BiDiMode;
		property Constraints;
		property Ctl3D;
		property DragCursor;
		property DragKind;
		property DragMode;
		property Enabled;
		property ParentBiDiMode;
		property ParentCtl3D;
		property ParentShowHint;
		property PopupMenu;
		property ShowHint;
		property TabOrder;
		property TabStop default True;
		property Visible;
		property OnContextPopup;
		property OnDragDrop;
		property OnDragOver;
		property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnMouseDown;
		property OnMouseUp;
		property OnMouseMove;
		property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
		property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
		property OnMouseWheel;
		property OnMouseWheelDown;
		property OnMouseWheelUp;

		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnStartDock;
		property OnStartDrag;

		property OnDblClick;
	end;

procedure ZoomMake(
	BmpSource: TDBitmap;
	VisX, VisY: Integer;
	AsWindow: Boolean; Zoom: Extended; XYConst: Boolean; QualityResize: Boolean;
	var OX, OY: Integer;
	out SourceWidth, SourceHeight: Integer;
	out SX1, SY1, SXW, SYH: Integer;
	out DX1, DY1, DXW, DYH: Integer;
	var BmpSource2: TDBitmap);

procedure Register;

implementation

uses
	Math,
	uAdd, uGraph, uSysInfo;
const
	OfsS = 20; // ms; FPS = 1000 / OfsS; 25-30FPS for VR; 50 = TV
	{$ifopt d+}
	ScrollEf = ef16;
	ScrollEf2 = ef16;
	{$else}
	ScrollEf = ef13;
	ScrollEf2 = ef13;
	{$endif}

procedure TDImage.WMPaint(var Message: TWMPaint);
begin
	Paint;
//	DefaultHandler(Message);
	inherited;
end;

procedure TDImage.PaintWindow(DC: HDC);
begin
	FCanvas.Lock;
	try
		FCanvas.Handle := DC;
		try
			TControlCanvas(FCanvas).UpdateTextFlags;
			Paint;
		finally
			FCanvas.Handle := 0;
		end;
	finally
		FCanvas.Unlock;
	end;
end;

procedure TDImage.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	DefaultHandler(Message);
end;

procedure TDImage.WMChar(var Message: TWMChar);
begin
{	with Message do
		if  (((CharCode = VK_RETURN) and FActive) or
			((CharCode = VK_ESCAPE) and FCancel)) and
			(KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
		begin
			Click;
			Result := 1;
		end else
			inherited;}
end;

procedure TDImage.CMMouseEnter(var Message: TWMMouse);
begin
	inherited;
	MouseOn := True;
//	if FHotTrack then
//		Fill;
	if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TDImage.CMMouseLeave(var Message: TWMMouse);
begin
	inherited;
	MouseOn := False;
	if MouseWhere <> mwNone then
	begin
		if MouseWhere <> mwScroll then
		begin
			MouseWhere := mwNone;
			Fill;
		end
		else
			MouseWhere := mwNone;
	end;
//	if FHotTrack then
	if Assigned(FOnMouseLeave) then FOnMouseLeave(Self);
end;

procedure TDImage.WMSize(var Message: TWMSize);
begin
	inherited;
	Fill;
end;

constructor TDImage.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FCanvas := TControlCanvas.Create;
	TControlCanvas(FCanvas).Control := Self;
	FHotTrack := True;

	Bitmap := TDBitmap.Create;
	{$ifopt d-}
	if NTSystem then
		if Font.Name = 'MS Sans Serif' then
		begin
			Font.Name := 'Microsoft Sans Serif';
			Bitmap.Canvas.Font.Name := Font.Name;
		end;
	{$endif}

//	ControlStyle := [csDoubleClicks, csOpaque, csAcceptsControls, csMenuEvents, csDisplayDragImage, csReflector];
	ControlStyle := ControlStyle + [csOpaque, csMenuEvents, csDisplayDragImage, csReflector] - [csSetCaption];
	{	Width := 250;
	Height := 150;}
{	Bitmap.Width := Width;
	Bitmap.Height := Height;}
//	ParentColor := False;
//	TabStop := True;
end;

destructor TDImage.Destroy;
begin
	FCanvas.Free;
	FreeAndNil(Bitmap);
	inherited Destroy;
end;

procedure TDImage.CreateParams(var Params: TCreateParams);
{const
	BorderStyles: array[TBorderStyle] of U4 = (0, );}
begin
	inherited CreateParams(Params);
	LCursor := Cursor;
	with Params do
	begin
//		Style := Style or WS_BORDER;
		WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
//		ExStyle := ExStyle or WS_EX_CLIENTEDGE;
//		WindowClass.style := WindowClass.style or CS_KEYCVTWINDOW;
		Style := Style or BS_OWNERDRAW;
	end;
end;

function TDImage.MouseWh(const X, Y: Integer): TMouseAction;
begin
	Result := mwScroll;
	if VType <> 0 then
	if X + ScrollBarVWidth > Bitmap.Width  then
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
	if Y + ScrollBarHHeight > Bitmap.Height  then
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

procedure TDImage.OffsetRange(var NOfsX, NOfsY: Integer);
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

procedure TDImage.ScrollTo(NOfsX, NOfsY: Integer);
begin
	OffsetRange(NOfsX, NOfsY);
	if (OfsX <> NOfsX) or (OfsY <> NOfsY) then
	begin
		OfsX := NOfsX;
		OfsY := NOfsY;
		Fill;
	end;
end;

procedure TDImage.MouseDown(Button: TMouseButton; Shift: TShiftState;
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
	SetFocus;
	case Button of
	mbLeft:
	begin
		MouseL := True;
		MouseA := MouseWh(X, Y);
		case MouseA of
		mwScrollH:
		begin
			MouseAction := mwScrollH;
			MouseX := X;
			BOfsX := OfsX;
			Fill;
//			inherited MouseDown(Button, Shift, X, Y);
		end;
		mwScrollV:
		begin
			MouseAction := mwScrollV;
			MouseY := Y;
			BOfsY := OfsY;
			Fill;
//			inherited MouseDown(Button, Shift, X, Y);
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
			NOfsX := RoundDivS8(65536 * S8(OfsX), Speed);
			NOfsY := RoundDivS8(65536 * S8(OfsY), Speed);
{     case MouseW of
			mwScrollHD, mwScrollHU,
			mwScrollHD2, mwScrollHU2:
			begin
			else
			begin
			end;}
//      MouseW := mdScrollB;
//			inherited MouseDown(Button, Shift, X, Y);
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
				ScrollTo(RoundDivS8(S8(Speed) * S8(NOfsX), 65536),
					RoundDivS8(S8(Speed) * S8(NOfsY), 65536));
//        Application.HandleMessage; no
				Application.ProcessMessages;
				if GetTickCount >= LastTickCount then
				begin
					TimeO := GetTickCount - LastTickCount;
					if TimeO < OfsS then
					begin
						Sleep(OfsS - TimeO);
						TimeO := OfsS;
					end;
					Inc(LastTickCount, TimeO);
				end;
				Inc(Cycle);
				if LastTickCount >= FrameTickCount then
				begin
					FramePerSec := 1000 * Cycle / (LastTickCount - FrameTickCount + StepInt);
					FrameTickCount := LastTickCount + StepInt;
					Cycle := 0;
				end;
			end;
			MouseAction := MouseA;
			MouseAction := mwNone;
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
			begin
				MouseAction := mwNone;
//				inherited MouseDown(Button, Shift, X, Y);
			end;
		end;
		end;
	end;
	mbRight: MouseR := True;
	mbMiddle: MouseM := True;
	end;

	inherited MouseDown(Button, Shift, X, Y);
end;

procedure TDImage.MouseMove(Shift: TShiftState; X, Y: Integer);
var
	NOfsX, NOfsY: Integer;
	Sc: Boolean;
	MouseW: TMouseAction;
begin
	MouseW := MouseWh(X, Y);
	case MouseW of
	mwScrollH, mwScrollV,
	mwScrollHD, mwScrollHU,
	mwScrollVD, mwScrollVU,
	mwScrollHD2, mwScrollHU2,
	mwScrollVD2, mwScrollVU2:
	begin
		Sc := False;
		if Cursor <> crArrow then
			Cursor := crArrow;
	end
	else
	begin
		Sc := (HandScroll or (ssShift in Shift)) and ((BitmapWidth - NowMaxWidth > 0) or (BitmapHeight - NowMaxHeight > 0));
		if Sc then
			Cur := 1 + SG(MouseL)
		else
			Cur := LCursor;
		if {(Cursor = crArrow) and} (Cursor <> Cur) then
			Cursor := Cur;
	end;
	end;

	if MouseWhere <> MouseW then
	begin
		if ((MouseWhere = mwNone) and (MouseW = mwScroll)) or
		((MouseWhere = mwScroll) and (MouseW = mwNone)) then
			MouseWhere := MouseW
		else
		begin
			MouseWhere := MouseW;
			Fill;
		end;
	end;

	NOfsX := OfsX;
	NOfsY := OfsY;
	case MouseAction of
	mwScroll:
	begin
		if Sc then
		begin
			NOfsX := MouseX - X;
			NOfsY := MouseY - Y;
		end;
	end;
	mwScrollH:
	begin
		NOfsX := BOfsX + RoundDivS8(S8(BitmapWidth) * S8(X - MouseX), NowMaxWidth - 2 * ScrollBarHHeight);
		NOfsY := OfsY;
	end;
	mwScrollV:
	begin
		NOfsX := OfsX;
		NOfsY := BOfsY + RoundDivS8(S8(BitmapHeight) * S8(Y - MouseY), NowMaxHeight - 2 * ScrollBarVWidth);
	end;
	end;
{	TickCount := GetTickCount;
	if TickCount < LTickCount + OfsS then
	begin
		Sleep(LTickCount + OfsS - TickCount);
		TickCount := LTickCount + OfsS;
	end;
	LTickCount := TickCount;}
	ScrollTo(NOfsX, NOfsY);
//  if (MouseW = mwNone){ or (MouseW = mwScroll)} then
	inherited MouseMove(Shift, X, Y);
end;

procedure TDImage.MouseUp(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	Screen.Cursor := crDefault;
	case Button of
	mbLeft:
	begin
		MouseL := False;
		case MouseAction of
		mwScrollHD, mwScrollHU,
		mwScrollVD, mwScrollVU,
		mwScrollHD2, mwScrollHU2,
		mwScrollVD2, mwScrollVU2,
		mwScrollH, mwScrollV:
		begin
			if MouseAction <> mwNone then
			begin
				MouseAction := mwNone;
				Fill;
			end;
		end;
		mwScroll:
		begin
			MouseAction := mwNone;
			if (HandScroll or (ssShift in Shift)) and ((BitmapWidth - NowMaxWidth > 0) or (BitmapHeight - NowMaxHeight > 0)) then
				Cursor := 1
			else
				Cursor := LCursor;
		end;
		end;
	end;
	mbRight: MouseR := False;
	mbMiddle: MouseM := False;
	end;
	inherited MouseUp(Button, Shift, X, Y);
end;

function TDImage.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
	inherited DoMouseWheelDown(Shift, MousePos);
	ScrollTo(OfsX, OfsY + RoundDiv(MaxOfsY, 32));
	Result := False;
end;

function TDImage.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
	inherited DoMouseWheelUp(Shift, MousePos);
	ScrollTo(OfsX, OfsY - RoundDiv(MaxOfsY, 32));
	Result := False;
end;

procedure TDImage.InitScrolls;
begin
	ScrollBarVWidth := GetSystemMetrics(SM_CXVSCROLL);

	if BitmapWidth > Bitmap.Width then
		HType := 1
	else if BitmapWidth > Bitmap.Width - ScrollBarVWidth then
		HType := 2
	else
		HType := 0;

	ScrollBarHHeight := GetSystemMetrics(SM_CYHSCROLL);
	if BitmapHeight > Bitmap.Height then
	begin
		VType := 1;
		if HType = 2 then HType := 1;
	end
	else if BitmapHeight > Bitmap.Height - ScrollBarHHeight then
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

	NowMaxWidth := Bitmap.Width - ScrollBarVWidth * VType;
	NowMaxHeight := Bitmap.Height - ScrollBarHHeight * HType;
	MaxOfsX := BitmapWidth - NowMaxWidth;
	MaxOfsY := BitmapHeight - NowMaxHeight;
	OffsetRange(OfsX, OfsY);
end;

procedure TDImage.Fill;
var
	ScrollLen, ScrollLenS: Integer;
	X1, Y1, X2, Y2: Integer;
	C: TColor;
	Co: array[0..3] of TColor;
	I1, I2: SG;
	SliderC1, SliderC2: TColor;
	s: string;
	i, x, y: SG;
	FontSize: SG;
begin
	if (Visible = False) or (csDesigning in ComponentState) then
	begin
		Bitmap.SetSize(0, 0);
		Exit;
	end;
	if (Bitmap.Width <> Width) or (Bitmap.Height <> Height) then
//		Bitmap.Resize(Bitmap, clNone, Width, Height, nil);}
		Bitmap.SetSize(Width, Height);

	try
		if Assigned(FOnFill) then FOnFill(Self);
		InitScrolls;

		SliderC1 := DarkerColor(clScrollBar);
		SliderC2 := LighterColor(clScrollBar);

		// H
		if (BitmapWidth > 0) and (HType = 1) and (NowMaxWidth > 2 * ScrollBarHHeight) then
		begin
			ScrollBarHWidth := NowMaxWidth;
			Y1 := Integer(Bitmap.Height) - ScrollBarHHeight;
			Y2 := Bitmap.Height - 1;

			X1 := 0;
			X2 := ScrollBarVWidth - 1;
//			Bitmap.Line(X1, Y1, NowMaxWidth - 1, Y1, RColor(238, 237, 229).L, ScrollEf);

	{   Bitmap.Border(X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bitmap.Bar(clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHD, FHotTrack and (MouseWhere = mwScrollHD), 1, ScrollEf);

			X1 := NowMaxWidth - ScrollBarVWidth;
			X2 := NowMaxWidth - 1;
	{   Bitmap.Border(X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
			Bitmap.Bar(clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
			Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHU, FHotTrack and (MouseWhere = mwScrollHU), 3, ScrollEf);

			// TScrollBoxSlider
			ScrollLen := NowMaxWidth - 2 * ScrollBarVWidth;
			ScrollLenS := NowMaxWidth * ScrollLen div BitmapWidth;
			if ScrollLenS < ScrollBarHHeight div 2 then ScrollLenS := ScrollBarHHeight div 2;

			Y1 := Integer(Bitmap.Height) - ScrollBarHHeight + 1;
			Y2 := Bitmap.Height - 1 - 1;
			X1 := ScrollBarVWidth + RoundDivS8(S8(ScrollLen - ScrollLenS) * S8(OfsX), BitmapWidth - NowMaxWidth);
			X2 := X1 + ScrollLenS - 1;
			SliderHX1 := X1;
			SliderHX2 := X2;

			if MouseAction = mwScrollH then
			begin
				I1 := 0;
				I2 := 3;
			end
			else
			begin
				I1 := 3;
				I2 := 0;
			end;
			Bitmap.Border(X1, Y1, X2, Y2, DepthColor(I1), DepthColor(I2), 1, ScrollEf);
//			Bitmap.Bar(clNone, X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, C, ScrollEf);
			if FHotTrack and (MouseWhere = mwScrollH) then
			begin
				Co[0] := RColor(253, 255, 255).L;
				Co[1] := RColor(185, 218, 251).L;
				Co[2] := Co[0];
				Co[3] := Co[1];
			end
			else
			begin
				Co[0] := RColor(214, 230, 255).L;
				Co[1] := RColor(174, 195, 241).L;
				Co[2] := Co[0];
				Co[3] := Co[1];
			end;
			Bitmap.GenerateRGB(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, gfFade2x, Co, 0, ScrollEf, 0, nil);
			x := (X1 + X2) div 2 - RoundDiv(ScrollBarHHeight, 6);
			for i := 0 to RoundDiv(ScrollBarHHeight, 6) - 1 do
			begin
				Bitmap.Line(x, Y1 + 4, x, Y2 - 5, RColor(238, 244, 254).L, ef16);
				Inc(x);
				Bitmap.Line(x, Y1 + 5, x, Y2 - 4, RColor(140, 176, 208).L, ef16);
				Inc(x);
			end;


			// =
			X1 := ScrollBarVWidth;
			X2 := SliderHX1 - 1;
			Y1 := Integer(Bitmap.Height) - ScrollBarHHeight;
			Y2 := Bitmap.Height - 1;

			if X2 >= X1 then
			begin
				if (MouseAction <> mwScrollHD2) then
					C := clScrollBar
				else
					C := clHighlight;
				Bitmap.Line(X1, Y1, X2, Y1, SliderC1, ScrollEf2);
				Bitmap.Line(X1, Y2, X2, Y2, SliderC2, ScrollEf2);
				Bitmap.Bar(
					X1, Y1 + 1,
					X2, Y2 - 1, C, ScrollEf2);
			end;

			X1 := SliderHX2 + 1;
			X2 := NowMaxWidth - ScrollBarVWidth - 1;
			Y1 := Integer(Bitmap.Height) - ScrollBarHHeight;
			Y2 := Bitmap.Height - 1;

			if X2 >= X1 then
			begin
				if (MouseAction <> mwScrollHU2) then
					C := clScrollBar
				else
					C := clHighlight;
				Bitmap.Line(X1, Y1, X2, Y1, SliderC1, ScrollEf2);
				Bitmap.Line(X1, Y2, X2, Y2, SliderC2, ScrollEf2);
				Bitmap.Bar(
					X1, Y1 + 1,
					X2, Y2 - 1, C, ScrollEf2);
			end;
		end
		else
			ScrollBarHWidth := 0;

		// V
		if (BitmapHeight > 0) and (VType = 1) and (NowMaxHeight > 2 * ScrollBarVWidth) then
		begin
			ScrollBarVHeight := NowMaxHeight;
			X1 := Integer(Bitmap.Width) - ScrollBarVWidth;
			X2 := Bitmap.Width - 1;
			if (X1 >= 0) and (X2 > X1) then
			begin
				Y1 := 0;
				Y2 := ScrollBarHHeight - 1;
	//			Bitmap.Line(X1, Y1, X1, NowMaxHeight - 1, RColor(238, 237, 229).L, ScrollEf);
		{   Bitmap.Border(X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
				Bitmap.Bar(clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
				Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVD, FHotTrack and (MouseWhere = mwScrollVD), 0, ScrollEf);

				Y1 := NowMaxHeight - ScrollBarHHeight;
				Y2 := NowMaxHeight - 1;
		{   Bitmap.Border(X1, Y1, X2, Y2, DepthColor(3), DepthColor(0), 1, ScrollEf);
				Bitmap.Bar(clNone, X1 + 1 , Y1 + 1, X2 - 1, Y2 - 1, clBtnFace, ScrollEf);}
				Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVU, FHotTrack and (MouseWhere = mwScrollVU), 2, ScrollEf);
			end;

			// TScrollBoxSlider
			ScrollLen := NowMaxHeight - 2 * ScrollBarHHeight;
			ScrollLenS := NowMaxHeight * ScrollLen div BitmapHeight;
			if ScrollLenS < ScrollBarVWidth div 2 then ScrollLenS := ScrollBarVWidth div 2;

			X1 := Integer(Bitmap.Width) - ScrollBarVWidth + 1;
			X2 := Bitmap.Width - 1 - 1;
			Y1 := ScrollBarHHeight + (ScrollLen - ScrollLenS) * Int64(OfsY) div (BitmapHeight - NowMaxHeight);
			Y2 := Y1 + ScrollLenS - 1;
			SliderVY1 := Y1;
			SliderVY2 := Y2;

			if MouseAction = mwScrollV then
			begin
				I1 := 0;
				I2 := 3;
			end
			else
			begin
				I1 := 3;
				I2 := 0;
			end;
			Bitmap.Border(X1, Y1, X2, Y2, DepthColor(I1), DepthColor(I2), 1, ScrollEf);
//			if FHotTrack and (MouseWhere = mwScrollV) then C := clHighlight else C := clBtnFace;
//			Bitmap.Bar(clNone, X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, C, ScrollEf);
			if FHotTrack and (MouseWhere = mwScrollV) then
			begin
				Co[0] := RColor(253, 255, 255).L;
				Co[1] := RColor(185, 218, 251).L;
				Co[2] := Co[0];
				Co[3] := Co[1];
			end
			else
			begin
				Co[0] := RColor(214, 230, 255).L;
				Co[1] := RColor(174, 195, 241).L;
				Co[2] := Co[0];
				Co[3] := Co[1];
			end;
			Bitmap.GenerateRGB(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, gfFade2x, Co, 0, ScrollEf, 0, nil);
			y := (Y1 + Y2) div 2 - RoundDiv(ScrollBarVWidth, 6);
			for i := 0 to RoundDiv(ScrollBarVWidth, 6) - 1 do
			begin
				Bitmap.Line(X1 + 4, y, X2 - 5, y, RColor(238, 244, 254).L, ef16);
				Inc(y);
				Bitmap.Line(X1 + 5, y, X2 - 4, y, RColor(140, 176, 208).L, ef16);
				Inc(y);
			end;

			// ||
			Y1 := ScrollBarHHeight;
			Y2 := SliderVY1 - 1;
			X1 := Integer(Bitmap.Width) - ScrollBarVWidth;
			X2 := Bitmap.Width - 1;

			if Y2 >= Y1 then
			begin
				if (MouseAction <> mwScrollVD2) then
					C := clScrollBar
				else
					C := clHighlight;
				Bitmap.Line(X1, Y1, X1, Y2, SliderC1, ScrollEf2);
				Bitmap.Line(X2, Y1, X2, Y2, SliderC2, ScrollEf2);
				Bitmap.Bar(
					X1 + 1, Y1,
					X2 - 1, Y2, C, ScrollEf2);
			end;

			Y1 := SliderVY2 + 1;
			Y2 := NowMaxHeight - ScrollBarHHeight - 1;
			X1 := Integer(Bitmap.Width) - ScrollBarVWidth;
			X2 := Bitmap.Width - 1;

			if Y2 >= Y1 then
			begin
				if (MouseAction <> mwScrollVU2) then
					C := clScrollBar
				else
					C := clHighlight;
				Bitmap.Line(X1, Y1, X1, Y2, SliderC1, ScrollEf2);
				Bitmap.Line(X2, Y1, X2, Y2, SliderC2, ScrollEf2);
				Bitmap.Bar(
					X1 + 1, Y1,
					X2 - 1, Y2, C, ScrollEf2);
			end;
		end
		else
			ScrollBarVHeight := 0;

//    Pix(Bitmap.PData, Bitmap.ByteX, MouseX, MouseY, clWhite, ef16);
		{$ifopt d-}
		if FDrawFPS then
		{$endif}
		begin
		{$ifopt d+}
		s := NToS(PaintCount);
		{$endif}
			if FramePerSec >= 0.1 then
			begin
				s := {$ifopt d+}s + ', ' +{$endif}NToS(Round(100 * FramePerSec), 2);
			end;
			FontSize := Bitmap.Canvas.Font.Size;
			Bitmap.Canvas.Font.Size := 8;
			ShadowText(Bitmap.Canvas, Width - Bitmap.Canvas.TextWidth(s) - 1 - 1, 1,
				s,
				clWindowText, clNone);
			Bitmap.Canvas.Font.Size := FontSize;
		end;
	finally
		Inc(PaintCount);
		Paint;
	end;
end;

procedure TDImage.Paint;
begin
	if Bitmap.Empty then
	begin
		FCanvas.Brush.Style := bsSolid;
		FCanvas.Brush.Color := clAppWorkSpace;
		PatBlt(
			FCanvas.Handle,
			0,
			0,
			Width,
			Height,
			PATCOPY
		 );
	end
	else
	begin
//		SetStretchBltMode(FCanvas.Handle, COLORONCOLOR);
		BitBlt(
			FCanvas.Handle,
			0,
			0,
			Width,
			Height,
			Bitmap.Canvas.Handle,
			0,
			0,
			SRCCOPY
		 );
	end;
	if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure ZoomMake(
	BmpSource: TDBitmap;
	VisX, VisY: Integer;
	AsWindow: Boolean; Zoom: Extended; XYConst: Boolean; QualityResize: Boolean;
	var OX, OY: Integer;
	out SourceWidth, SourceHeight: Integer;
	out SX1, SY1, SXW, SYH: Integer;
	out DX1, DY1, DXW, DYH: Integer;
	var BmpSource2: TDBitmap);

var
	SX, SY: Integer;
	LastCursor: TCursor;
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

		// D???
		if OX > SX then OX := 0;//SX;
		if OY > SY then OY := 0;//SY;

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
			BmpSource2 := TDBitmap.Create;
		end;
		if (BmpSource2.Width <> SourceWidth)
		or (BmpSource2.Height <> SourceHeight) then
		begin
			BmpSource2.SetSize(SourceWidth, SourceHeight);
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
				LastCursor := Screen.Cursor;
				Screen.Cursor := crHourGlass;
				BmpSource2.Resize(BmpSource,
					SourceWidth, SourceHeight, nil);
				Screen.Cursor := LastCursor;
			end;
		end;
		SXW := DXW;
		SYH := DYH;
	end
	else
	begin
		if Assigned(BmpSource2) then
		begin
			FreeAndNil(BmpSource2);
		end;
	end;
end;

procedure TDImage.SetHotTrack(Value: Boolean);
begin
	if FHotTrack <> Value then
	begin
		FHotTrack := Value;
		Fill;
	end;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDImage]);
end;

initialization
	Screen.Cursors[1] := LoadCursor(HInstance, PChar('HANDPOINT'));
	Screen.Cursors[2] := LoadCursor(HInstance, PChar('HANDPOINTDOWN'));
end.
