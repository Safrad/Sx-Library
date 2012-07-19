unit uDImage;

interface

{$R *.RES}

uses
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Menus,
	ExtCtrls, StdCtrls,
	uDBitmap, uTypes, uMath, uDTimer, uSimulation, uDIniFile, uDWinControl;

type
	TZoomMenu = (zmIn, zmOut, zm12, zm1, zm2, zmFitImage, zmFitWidth, zmFitHeight, zmCustom,
		zmCenter, zmGrate, zmGrateColor, zmCopy);

	TDisplayMode = (dmCustom, dmFitImage, dmFitWidth, dmFitHeight);

type
	TMouseAction = (mwNone, mwScroll, mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2,
		mwScrollV, mwScrollVD, mwScrollVU, mwScrollVD2, mwScrollVU2);

	TZoom = FG;

	TDImage = class(TDWinControl)
	private
		FAreaCursor: TCursor;
		FDisplayMode: TDisplayMode;
		FShiftState: TShiftState;

		FHandScroll: BG;

		FIntervalFillCount: UG;
		FIntervalStartTime: U4;
		FFPS: UG;

		MouseX, MouseY: Integer;
		FHotTrack: Boolean;

		FDrawFPS: Boolean;

		FOnFill: TNotifyEvent;
		FOnPaint: TNotifyEvent;
		FOnMouseEnter: TNotifyEvent;
		FOnMouseLeave: TNotifyEvent;
		FOnZoomChange: TNotifyEvent;

		BOfsX, BOfsY: Integer;
		HType, VType: U1;
		FNowMaxWidth, FNowMaxHeight: SG;
		SliderHX1, SliderHX2, SliderVY1, SliderVY2: Integer;

		FUserArea: TRect;

		// Zoom
		// V, A: TZoom;
		// LastTickCount: U4;
		TargetZoom: TZoom;
		FEnableZoom: BG;
		ZoomMenu: TMenuItem;
		M: array [TZoomMenu] of TMenuItem;
		BmpS, BmpSourceS: TDBitmap;
		FOfsX, FOfsY: SG; // TODO : TPoint;
		CenterOffset: TPoint;
		FUserBitmap: TDBitmap;

		procedure SetHotTrack(Value: Boolean);
		// procedure SetShortStep(Value: SG);
		procedure ChangeZoom(NewZoom: TZoom);
		procedure InitZoomMenu;
		procedure ZoomClick(Sender: TObject);

		procedure InitScrolls;
		procedure FillUserBitmap;
		procedure UpdateDisplayMode;
		procedure ChangeCursor;
		procedure InitCursor(const X, Y: SG; const Shift: TShiftState);

		// procedure Timer1Timer(Sender: TObject);

		function GetMouseWhere(const X, Y: Integer): TMouseAction;
		procedure WMSize(var Message: TWMSize);
		message WM_SIZE;
		procedure CMMouseEnter(var Message: TWMMouse);
		message CM_MOUSEENTER;
		procedure CMMouseLeave(var Message: TWMMouse);
		message CM_MOUSELEAVE;
		procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
		message CM_WANTSPECIALKEY;
		procedure SetUserArea(const Value: TRect);
		procedure SetDisplayMode(const Value: TDisplayMode);
		procedure SetUserBitmap(const Value: TDBitmap);
		procedure SetAreaCursor(const Value: TCursor);
	protected
		UserWidth: SG;
		UserHeight: SG;
		UserWidth2: SG;
		UserHeight2: SG;

		procedure CreateParams(var Params: TCreateParams); override;
		// procedure PaintWindow(DC: HDC); override;
		// procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
		procedure KeyDown(var Key: Word; Shift: TShiftState); override;
		procedure KeyUp(var Key: Word; Shift: TShiftState); override;
		// override
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
		procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
		procedure PageDownUp(const n: SG); virtual;
		procedure PageLeftRight(const n: SG); virtual;
		procedure LineDownUp(const n: SG); virtual;
		procedure LineLeftRight(const n: SG); virtual;
		procedure FillBitmap; override;
		procedure SetZoom(Value: TZoom); virtual;

//		property Cursor: TCursor read FCursor write FCursor;
	public
		{ Public declarations }
		SmallMovement: BG;
		LMouseX, LMouseY: SG;

		ActualZoom: TZoom;
		MouseL, MouseM, MouseR: Boolean;
		MouseAction: TMouseAction;
		MouseWhere: TMouseAction;
		MouseOn: Boolean;

		// Input
		UserMouse: TPoint;
		UserMouseFlo: TFloPoint;
		SX, SY, SW, SH: SG; // TODO : TRect;

		// Output
		Center, Grate: BG;
		Grating: B1;
		GrateColor: TColor;
		DX, DY, DW, DH: SG; // TODO : TRect;

		MaxOfsX, MaxOfsY: SG; // TODO : TPoint;

		// Scrolls
		LockZoom: BG;
		ScrollBarHWidth, ScrollBarHHeight, ScrollBarVWidth, ScrollBarVHeight: Integer;

		constructor Create(AOwner: TComponent); override;
		procedure CreateZoom(Zoom1: TMenuItem);
		destructor Destroy; override;

		function GetPosX(X: SG): SG;
		function GetPosY(Y: SG): SG;
		function GetPoint(const X, Y: FA): TPoint;
		function GetFloPoint(const X, Y: FA): TFloPoint; overload;
		function GetFloPoint(const P: TFloPoint): TFloPoint; overload;

		function OfsX: SG;
		function OfsY: SG;
		function GetOffset: TPoint; // Image offset on screen
		function GetImageOffset: TPoint;
		function GetDisplayedArea: TRect;

		procedure OffsetRange(var NOfsX, NOfsY: SG);
		procedure ScrollTo(NOfsX, NOfsY: SG); overload;
		procedure ScrollTo(Offset: TPoint); overload;
		procedure OffsetOnRect(const Rect: TRect);

		procedure CursorDownUp(const n: SG); virtual;
		procedure ScrollHome; virtual;
		procedure ScrollEnd; virtual;

		procedure FitImage;
		procedure UserBitmapChanged;
		function SToL(const P: TPoint): TFloPoint;

		procedure Serialize(const IniFile: TDIniFile; const Save: BG);

		procedure ToClipboard;

		function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
			override;

		property UserBitmap: TDBitmap read FUserBitmap write SetUserBitmap;

		property SmallerBitmap: TDBitmap read BmpS;
		// Can be 0 if there is no borders
		property UserArea: TRect read FUserArea write SetUserArea;

		property NowMaxWidth: SG read FNowMaxWidth;
		property NowMaxHeight: SG read FNowMaxHeight;

		property AreaCursor: TCursor read FAreaCursor write SetAreaCursor;
	published
		property Zoom: TZoom read TargetZoom write SetZoom;
		property EnableZoom: BG read FEnableZoom write FEnableZoom default False;
		property DisplayMode: TDisplayMode read FDisplayMode write SetDisplayMode default dmFitImage;
		property DrawFPS: Boolean read FDrawFPS write FDrawFPS default False;
		property HandScroll: Boolean read FHandScroll write FHandScroll default False;
		property HotTrack: Boolean read FHotTrack write SetHotTrack default True;

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

		property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
		property OnDblClick;
	end;

const
	HorizontalOffset = 24;

	{ procedure ZoomMake(
		BmpSource: TDBitmap;
		VisX, VisY: Integer;
		AsWindow: Boolean; Zoom: TZoom; XYConst: Boolean; QualityResize: Boolean;
		var OX, OY: Integer;
		out SourceWidth, SourceHeight: Integer;
		out SX1, SY1, SXW, SYH: Integer;
		out DX1, DY1, DXW, DYH: Integer;
		var BmpSource2: TDBitmap); }

procedure Register;

implementation

uses
	Math, ClipBrd,
	uGraph, uStrings, uGetInt, uGColor, uOutputFormat, uMsg, uColor, uMenus, uDrawStyle, uDictionary,
	uLog,
	Types;

const
	crHandPoint = 1;
	crHandPointDown = 2;
	// ZoomDiv = 2520;
	MenuNames: array [TZoomMenu] of string = ('Zoom In', 'Zoom Out', '1:2', '1:1', '2:1',
		'Fit Image', 'Fit Width', 'Fit Height', 'Zoom To' + cDialogSuffix, 'Center', 'Grate',
		'Grate Color' + cDialogSuffix, 'Copy');
	MenuShort: array [TZoomMenu] of Char = ('I', 'U', #0, 'Q', #0, #0, #0, #0, #0, #0, #0, #0, #0
		{ 'C' - Ctrl+C dnw in edits } );

procedure TDImage.CreateZoom(Zoom1: TMenuItem);
var
	i: TZoomMenu;
	j: SG;
	MS: TMenuItem;
begin
	PopupMenu := TPopupMenu.Create(Self);
	ZoomMenu := Zoom1;

	j := 0;
	for i := Low(TZoomMenu) to High(TZoomMenu) do
	begin
		if i in [zm12, zmCenter] then
		begin
			MS := TMenuItem.Create(nil);
			MS.Caption := cLineCaption;
			{ if ZoomMenu <> nil then
				ZoomMenu.Insert(j, MS); }
			PopupMenu.Items.Insert(j, MS);
			Inc(j);
		end;
		M[i] := TMenuItem.Create(nil);
		M[i].Tag := SG(i);
		M[i].Caption := Translate(MenuNames[i]);
		M[i].Name := ComponentName(MenuNames[i]); // 'Zoom' + IntToStr(SG(i));
		if M[i].Name = 'ZoomIn' then
			M[i].Name := 'InZoom'
		else if M[i].Name = 'ZoomOut' then
			M[i].Name := 'OutZoom';
		if MenuShort[i] <> #0 then
			M[i].ShortCut := ShortCut(Ord(MenuShort[i]), [ssCtrl]);

		M[i].OnClick := ZoomClick;
		{ if ZoomMenu <> nil then
			ZoomMenu.Insert(j, M[i]); }
		PopupMenu.Items.Insert(j, M[i]);
		Inc(j);
	end;
	FEnableZoom := True;
	if ZoomMenu <> nil then
	begin
		MenuCreate(PopupMenu.Items, ZoomMenu);
		MenuUpdate(PopupMenu.Items, ZoomMenu);
	end;
	InitZoomMenu;
	MenuSet(PopupMenu);
end;

const
	OfsS = 20; // ms; FPS = 1000 / OfsS
	ScrollEf = ef14;
	ScrollEf2 = ef12;

var
	Slf: TDImage;

procedure OnApplyNum(Value: S8);
begin
	Slf.ChangeZoom(Value / 1000);
	Slf.Invalidate;
end;

procedure OnApplyColor(Color: TColor);
begin
	Slf.GrateColor := Color;
	Slf.Invalidate;
end;

(*
	procedure TDImage.Timer1Timer(Sender: TObject);
	const
	A0 = 1;
	var
	DT: TZoom;
	Dif, Dif2: TZoom;
	k: TZoom;
	begin
	//	DT := FTimer.ElapsedTime / PerformanceFrequency;
	DT := FTimer.Interval / 1000;
	Dif := TargetZoom - ActualZoom;
	Dif2 := Dif / TargetZoom;
	if (Abs(Dif2) < 0.01){Distance is small} and (V < 100){Speed is slow} then
	begin
	// Parking
	ActualZoom := TargetZoom;
	V := 0;
	A := 0;
	FTimer.Enabled := False;
	end
	else
	begin
	k := 1 * V - 2300{Value < 2000 is unstable!} * (1.1 * Dif2);
	{		if Abs(k) < 100 then
	A := 0
	else} if k > 0 then
	A := -A0
	else
	A := A0;
	if Sgn(A) = Sgn(V) then
	if Abs(A) > A0 div 2 then A := Sgn(A) * A0 / 4; // Slows acceleration
	end;

	//	if V < 100 then A := A0;

	V := V + A * DT * 1000; if Abs(V) > 500 then V := Sgn(V) * 500;
	ActualZoom := ActualZoom * (1 + 0.003 * V * DT);
	{	if S > MaxS then S := MaxS;
	ActualZoom := ((MaxS - S) * StartZoom + S * NewZoom) / MaxS;}

	Invalidate;
	end; *)

procedure TDImage.InitZoomMenu;
begin
	if Assigned(M[zmCenter]) = False then
		Exit;
	M[zmCenter].Checked := Center;
	M[zmGrate].Checked := Grate;

	M[zmIn].Enabled := TargetZoom < 256;
	M[zmOut].Enabled := TargetZoom > 1 / 256;

	M[zm12].Enabled := TargetZoom <> 1 / 2;
	M[zm12].Checked := not M[zm12].Enabled;

	M[zm1].Enabled := TargetZoom <> 1;
	M[zm1].Checked := not M[zm1].Enabled;

	M[zm2].Enabled := TargetZoom <> 2;
	M[zm2].Checked := not M[zm2].Enabled;

	M[zmFitImage].Checked := FDisplayMode = dmFitImage;
	M[zmFitWidth].Checked := FDisplayMode = dmFitWidth;
	M[zmFitHeight].Checked := FDisplayMode = dmFitHeight;

	if ZoomMenu <> nil then
		MenuUpdate(PopupMenu.Items, ZoomMenu);
end;

procedure TDImage.ChangeZoom(NewZoom: TZoom);
begin
	if NewZoom = 0 then
		NewZoom := 1;
	if TargetZoom = 0 then
		TargetZoom := 1;
	if (NewZoom <> TargetZoom) then
	begin
		// NewZoom := RoundTo(NewZoom, -3);
		FOfsX := RoundSG((FOfsX + FNowMaxWidth / 2) * NewZoom / TargetZoom - FNowMaxWidth / 2);
		FOfsY := RoundSG((FOfsY + FNowMaxHeight / 2) * NewZoom / TargetZoom - FNowMaxHeight / 2);
		TargetZoom := NewZoom;
		ActualZoom := NewZoom;
		OffsetRange(FOfsX, FOfsY);
		InitZoomMenu;
		if Assigned(FOnZoomChange) then
			FOnZoomChange(Self);
		{ if FTimer = nil then
			begin
			FTimer := TTimer.Create(Self);
			FTimer.Enabled := False;
			//			FTimer.EventStep := esFrequency;
			//			FTimer.Interval := 25;
			FTimer.Interval := 40;
			FTimer.OnTimer := Timer1Timer;
			end;
			FTimer.Enabled := True;
			LastTickCount := GetTickCount; }
		// Invalidate; TODO
	end;
end;

procedure TDImage.UpdateDisplayMode;
begin
	case FDisplayMode of
	dmCustom:
		;

	dmFitImage:
		if (UserWidth2 > 0) and (UserHeight2 > 0) then
			ChangeZoom(Min(Bitmap.Width / UserWidth2, Bitmap.Height / UserHeight2));
	dmFitWidth:
		if UserWidth2 <> 0 then
			ChangeZoom(FNowMaxWidth { Bitmap.Width } / UserWidth2);
	dmFitHeight:
		if UserHeight2 <> 0 then
			ChangeZoom(FNowMaxHeight { Bitmap.Height } / UserHeight2);
	end;
end;

procedure TDImage.ZoomClick(Sender: TObject);
var
	ZoomI: SG;
begin
	Slf := Self;
	case TZoomMenu(TMenuItem(Sender).Tag) of
	zmIn:
		begin
			FDisplayMode := dmCustom;
			ChangeZoom(TargetZoom * 2);
		end;
	zmOut:
		begin
			FDisplayMode := dmCustom;
			ChangeZoom(TargetZoom / 2);
		end;
	zmFitImage:
		begin
			FDisplayMode := dmFitImage;
			if (UserWidth2 > 0) and (UserHeight2 > 0) then
				ChangeZoom(Min(Bitmap.Width / UserWidth2, Bitmap.Height / UserHeight2));
		end;
	zmFitWidth:
		begin
			FDisplayMode := dmFitWidth;
			if UserBitmap <> nil then
				ChangeZoom(FNowMaxWidth { Bitmap.Width } / UserWidth2);
		end;
	zmFitHeight:
		begin
			FDisplayMode := dmFitHeight;
			if UserBitmap <> nil then
				ChangeZoom(FNowMaxHeight { Bitmap.Height } / UserHeight2);
		end;
	zm12:
		begin
			FDisplayMode := dmCustom;
			ChangeZoom(1 / 2);
		end;
	zm1:
		begin
			FDisplayMode := dmCustom;
			ChangeZoom(1);
		end;
	zm2:
		begin
			FDisplayMode := dmCustom;
			ChangeZoom(2);
		end;
	zmCustom:
		begin
			ZoomI := Round(TargetZoom * 1000);
			if GetNumber('Zoom To (' + CharTimes + '1000)', ZoomI, 1, 1000, 256 * 1000, OnApplyNum) then
			begin
				FDisplayMode := dmCustom;
				TargetZoom := ZoomI / 1000;
			end;
		end;
	zmCenter:
		begin
			Center := not Center;
//			Invalidate;
			M[zmCenter].Checked := Center;
		end;
	zmGrate:
		begin
			Grate := not Grate;
//			Invalidate;
			M[zmGrate].Checked := Grate;
		end;
	zmGrateColor:
		GetColor('Grate Color', GrateColor, clWhite, OnApplyColor);
	zmCopy:
		ToClipboard;
	end;
	Invalidate;
end;

procedure TDImage.CMMouseEnter(var Message: TWMMouse);
begin
	inherited;
	MouseOn := True;
	if Assigned(FOnMouseEnter) then
		FOnMouseEnter(Self);
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
//			Invalidate;
		end
		else
			MouseWhere := mwNone;
	end;
	if Assigned(FOnMouseLeave) then
		FOnMouseLeave(Self);
end;

procedure TDImage.WMSize(var Message: TWMSize);
begin
	inherited;
	Invalidate;
end;

constructor TDImage.Create(AOwner: TComponent);
begin
	inherited;
	ScrollBarVWidth := GetSystemMetrics(SM_CXVSCROLL);
	ScrollBarHHeight := GetSystemMetrics(SM_CYHSCROLL);

	// FDisplayMode := dmFitImage;
	Color := clAppWorkSpace;
	ActualZoom := 1;
	TargetZoom := 1;
	Grate := False;
	Center := True;

	FHotTrack := True;

	TabStop := True;
	// ControlStyle := [csDoubleClicks, csOpaque, csAcceptsControls, csMenuEvents, csDisplayDragImage, csReflector];
	ControlStyle := ControlStyle
	{ + [csOpaque, csMenuEvents, csDisplayDragImage, csReflector] } - [csSetCaption];
end;

destructor TDImage.Destroy;
begin
	// FreeAndNil(FTimer);

	BmpS := nil;
	UserBitmap := nil;
	FreeAndNil(BmpSourceS);
	inherited;
end;

procedure TDImage.CreateParams(var Params: TCreateParams);
{ const
	BorderStyles: array[TBorderStyle] of U4 = (0, ); }
begin
	inherited;
	with Params do
	begin
		// Style := Style or WS_BORDER;
		WindowClass.style := WindowClass.style and not(CS_HREDRAW or CS_VREDRAW);
		// ExStyle := ExStyle or WS_EX_CLIENTEDGE;
		// WindowClass.style := WindowClass.style or CS_KEYCVTWINDOW;
		style := style or BS_OWNERDRAW;
	end;
end;

function TDImage.GetMouseWhere(const X, Y: Integer): TMouseAction;
begin
	if (X < 0) or (X >= Bitmap.Width) or (Y < 0) or (Y >= Bitmap.Height) then
		Result := mwScroll
	else
	begin
		Result := mwScroll;
		if VType <> 0 then
			if X + ScrollBarVWidth > Bitmap.Width then
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
			if Y + ScrollBarHHeight > Bitmap.Height then
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
end;

function TDImage.GetOffset: TPoint;
begin
	Result.X := FOfsX + CenterOffset.X;
	Result.Y := FOfsY + CenterOffset.Y;
end;

procedure TDImage.OffsetRange(var NOfsX, NOfsY: SG);
begin
	if NOfsX > UserWidth - FNowMaxWidth then
		NOfsX := UserWidth - FNowMaxWidth;
	if NOfsX < 0 then
		NOfsX := 0;

	if NOfsY > UserHeight - FNowMaxHeight then
		NOfsY := UserHeight - FNowMaxHeight;
	if NOfsY < 0 then
		NOfsY := 0;
end;

function TDImage.OfsX: SG;
begin
	Result := FOfsX + CenterOffset.X;
end;

function TDImage.OfsY: SG;
begin
	Result := FOfsY + CenterOffset.Y;
end;

procedure TDImage.ScrollTo(NOfsX, NOfsY: SG);
begin
	OffsetRange(NOfsX, NOfsY);
	if (FOfsX <> NOfsX) or (FOfsY <> NOfsY) then
	begin
		FOfsX := NOfsX;
		FOfsY := NOfsY;
		Invalidate;
	end;
end;

procedure TDImage.ScrollTo(Offset: TPoint);
begin
	ScrollTo(Offset.X, Offset.Y);
end;

procedure TDImage.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
	NOfsX, NOfsY: SG;
	MouseA: TMouseAction;
	StepPix: SG;
	First: BG;
	SimTime: U4;
begin
	FShiftState := Shift;
	SetFocus;

	MouseX := X;
	MouseY := Y;
	SmallMovement := False;
	LMouseX := X;
	LMouseY := Y;

	case Button of
	mbLeft:
		begin
			MouseL := True;
			MouseA := GetMouseWhere(X, Y);
			if (ssShift in Shift) and (MouseA in [mwScrollH, mwScrollV, mwScrollHD2, mwScrollHU2,
				mwScrollVD2, mwScrollVU2]) then
			begin
				if MouseA in [mwScrollH, mwScrollHD2, mwScrollHU2] then
					ScrollTo(RoundDiv((X - ScrollBarVWidth) * MaxOfsX, FNowMaxWidth - 2 * ScrollBarVWidth),
						FOfsY)
				else
					ScrollTo(FOfsX, RoundDiv((Y - ScrollBarHHeight) * MaxOfsY,
							FNowMaxHeight - 2 * ScrollBarHHeight));
				InitScrolls;
			end
			else
				case MouseA of
				mwScrollH:
					begin
						MouseAction := mwScrollH;
						MouseX := X;
						BOfsX := FOfsX;
						Invalidate;
					end;
				mwScrollV:
					begin
						MouseAction := mwScrollV;
						MouseY := Y;
						BOfsY := FOfsY;
						Invalidate;
					end;
				mwScrollHD, mwScrollHU, mwScrollVD, mwScrollVU, mwScrollHD2, mwScrollHU2, mwScrollVD2,
					mwScrollVU2:
					begin
						MouseAction := MouseA;
						// LastTickCount := GetTickCount;
						case MouseA of
						mwScrollHD2, mwScrollHU2:
							StepPix := FNowMaxWidth;
						mwScrollVD2, mwScrollVU2:
							StepPix := FNowMaxHeight;
						else
							StepPix := HorizontalOffset;
						end;

						SimTime := GetTickCount;
						First := True;

						while MouseWhere = MouseAction { <> mwNone } do
						begin
							if SimTime < GetTickCount then
							begin
								repeat
									NOfsX := FOfsX;
									NOfsY := FOfsY;
									// Move
									if First then
									begin
										First := False;
										Inc(SimTime, 400);
									end
									else
										Inc(SimTime, 40);
									case MouseA of
									mwScrollHD, mwScrollHD2:
										ScrollTo(NOfsX - StepPix, NOfsY);
									mwScrollHU, mwScrollHU2:
										ScrollTo(NOfsX + StepPix, NOfsY);
									mwScrollVD:
										LineDownUp(-1);
									mwScrollVD2:
										PageDownUp(-1);
									mwScrollVU:
										LineDownUp(1);
									mwScrollVU2:
										PageDownUp(1);
									end;
									InitScrolls;

									MouseWhere := GetMouseWhere(MouseX, MouseY);
									if MouseWhere <> MouseAction then
										Break;

								until SimTime >= GetTickCount;
								// Draw
								if MouseWhere <> MouseAction then
									Break;
							end;
							Application.ProcessMessages;

							if SimTime > GetTickCount then
								Sleep(Range(1, SimTime - GetTickCount, LoopSleepTime));
						end;
						MouseAction := mwNone;
						Invalidate;
						Exit;
					end;
				mwScroll:
					begin
						if ((MaxOfsX > 0) or (MaxOfsY > 0)) and (HandScroll or (ssShift in Shift)) then
						begin
//							Cursor := crHandPointDown;
							MouseAction := mwScroll;
							MouseX := FOfsX + X;
							MouseY := FOfsY + Y;
							ChangeCursor;
						end
						else
						begin
							MouseAction := mwNone;
						end;
					end;
				end;
		end;
	mbRight:
		MouseR := True;
	mbMiddle:
		MouseM := True;
	end;

	ChangeCursor;

	inherited;
end;

procedure TDImage.MouseMove(Shift: TShiftState; X, Y: Integer);
const
	MR = 8; // 0..
var
	NOfsX, NOfsY: Integer;
	Sc: Boolean;
	MouseW: TMouseAction;
begin
	if (X = LMouseX) and (Y = LMouseY) then
		Exit;
	if (SmallMovement = False) and (Abs(X - LMouseX) < MR) and (Abs(Y - LMouseY) < MR) then
		Exit;
	SmallMovement := True;

	Sc := True;
	if MouseAction = mwNone then
	begin
		MouseW := GetMouseWhere(X, Y);
		case MouseW of
		mwScrollH, mwScrollV, mwScrollHD, mwScrollHU, mwScrollVD, mwScrollVU, mwScrollHD2, mwScrollHU2,
			mwScrollVD2, mwScrollVU2:
			begin
				Sc := False;
			end
		else
		begin
			Sc := (HandScroll or (ssShift in Shift)) and
				((UserWidth - FNowMaxWidth > 0) or (UserHeight - FNowMaxHeight > 0));
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
//				Invalidate;
			end;
		end;
		if MouseL = False then
			ChangeCursor;
	end;

	NOfsX := FOfsX;
	NOfsY := FOfsY;

	if FEnableZoom then
		case MouseAction of
		mwNone, mwScroll:
			begin
				UserMouseFlo := SToL(Point(X, Y));
				UserMouse := FloPointToPoint(UserMouseFlo);
{				if DW <> 0 then
					UserMouse.X := Trunc((X + FOfsX - CenterOffset.X) / ActualZoom);
				if DH <> 0 then
					UserMouse.Y := Trunc((Y + FOfsY - CenterOffset.Y) / ActualZoom);}
			end;
		end;

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
			NOfsX := BOfsX + RoundDivS8(S8(UserWidth) * S8(X - MouseX),
				FNowMaxWidth - 2 * ScrollBarHHeight);
			NOfsY := FOfsY;
		end;
	mwScrollV:
		begin
			NOfsX := FOfsX;
			NOfsY := BOfsY + RoundDivS8(S8(UserHeight) * S8(Y - MouseY),
				FNowMaxHeight - 2 * ScrollBarVWidth);
		end;
	end;
	ScrollTo(NOfsX, NOfsY);
	inherited;
	LMouseX := X;
	LMouseY := Y;
end;

procedure TDImage.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
	case Button of
	mbLeft:
		begin
			MouseL := False;
			case MouseAction of
			mwScrollHD, mwScrollHU, mwScrollVD, mwScrollVU, mwScrollHD2, mwScrollHU2, mwScrollVD2,
				mwScrollVU2, mwScrollH, mwScrollV:
				begin
					if MouseAction <> mwNone then
					begin
						MouseAction := mwNone;
						Invalidate;
					end;
				end;
			mwScroll:
				begin
					MouseAction := mwNone;
					{ if (HandScroll or (ssShift in Shift)) and
						((UserWidth - FNowMaxWidth > 0) or (UserHeight - FNowMaxHeight > 0)) then
						Screen.Cursor := crHandPoint
						else
						Screen.Cursor := LCursor; }
				end;
			end;
		end;
	mbRight:
		MouseR := False;
	mbMiddle:
		MouseM := False;
	end;
	InitCursor(X, Y, Shift);
	inherited;
end;

function WheelScrollLines: UG;
begin
	SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0);
end;

function TDImage.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
const
	ZoomStep = 1.189207115002721066; // 1.414213562373095049;
var
	i, L: UG;
begin
	inherited DoMouseWheel(Shift, WheelDelta, MousePos);
	if ssCtrl in Shift then
	begin
		if FEnableZoom and (not LockZoom) then
		begin
			FDisplayMode := dmCustom;
			if WheelDelta > 0 then
				ChangeZoom(TargetZoom * ZoomStep)
			else
				ChangeZoom(TargetZoom / ZoomStep);
			Invalidate;
		end;
	end
	else if not(ssAlt in Shift) then
	begin
		L := WheelScrollLines;
		if (ssShift in Shift) or (L = WHEEL_PAGESCROLL) then
		begin
			if MouseWhere in [mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2] then
				PageLeftRight(-WheelDelta div WHEEL_DELTA)
			else
				PageDownUp(-WheelDelta div WHEEL_DELTA);
		end
		else
		begin
			for i := 1 to L * UG(Abs(WheelDelta)) div WHEEL_DELTA do
			begin
				if MouseWhere in [mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2] then
					LineLeftRight(-Sign(WheelDelta))
				else
					LineDownUp(-Sign(WheelDelta));
			end;
		end;
	end;
	Result := True;
end;

procedure TDImage.InitScrolls;
begin
	UserWidth2 := FUserArea.Right - FUserArea.Left + 1;
	UserHeight2 := FUserArea.Bottom - FUserArea.Top + 1;
	UserWidth := RoundN(Zoom * UserWidth2);
	UserHeight := RoundN(Zoom * UserHeight2);

	if UserWidth > Bitmap.Width then
		HType := 1
	else if UserWidth > Bitmap.Width - ScrollBarVWidth then
		HType := 2
	else
		HType := 0;

	if UserHeight > Bitmap.Height then
	begin
		VType := 1;
		if HType = 2 then
			HType := 1;
	end
	else if UserHeight > Bitmap.Height - ScrollBarHHeight then
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
		if HType = 2 then
			HType := 0;
	end;

	FNowMaxWidth := Bitmap.Width - ScrollBarVWidth * VType;
	FNowMaxHeight := Bitmap.Height - ScrollBarHHeight * HType;
	MaxOfsX := UserWidth - FNowMaxWidth;
	MaxOfsY := UserHeight - FNowMaxHeight;
	// OffsetRange(FOfsX, FOfsY);
end;

procedure ZoomMake(BmpSource: TDBitmap; VisX, VisY: Integer; AsWindow, Center: BG; Zoom: TZoom;
	XYConst: Boolean; QualityResize: Boolean; const OX, OY: Integer; out SourceWidth,
	SourceHeight: SG; out SX1, SY1, SXW, SYH: SG; out DX1, DY1, DXW, DYH: SG;
	var BmpSource2: TDBitmap);
var
	SX, SY: Integer;
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
			if VisY * BmpSource.Width >= VisX * BmpSource.Height then
			begin
				SX := VisX;
				SY := RoundDiv(VisX * BmpSource.Height, BmpSource.Width);
			end
			else
			begin
				SX := RoundDiv(VisY * BmpSource.Width, BmpSource.Height);
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
		{ ZoomedWidth := BmpSource.Width;
			ZoomedHeight := BmpSource.Height; }

		SourceWidth := SX;
		SourceHeight := SY;
	end
	else
	begin
		SX := RoundN(Zoom * BmpSource.Width);
		SY := RoundN(Zoom * BmpSource.Height);

		DX1 := 0;
		DY1 := 0;

		// Smaller := False;
		(* if VisX > SourceWidth then
			begin
			// Smaller := True;
			if Center then
			DX1 := (VisX - SourceWidth) div 2;
			end;
			if VisY > SourceHeight then
			begin
			// Smaller := True;
			if Center then
			DY1 := (VisY - SourceHeight) div 2;
			end; *)

		SourceWidth := SX;
		SourceHeight := SY;

		if Zoom <= 1 then
		begin

		end
		else
		begin
			DX1 := DX1 - RoundN(Zoom * Frac(OX / Zoom));
			DY1 := DY1 - RoundN(Zoom * Frac(OY / Zoom));
		end;

		SXW := Ceil(VisX / Zoom - DX1);
		SYH := Ceil(VisY / Zoom - DY1);
		DXW := RoundN(Zoom * SXW);
		DYH := RoundN(Zoom * SYH);

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

		{ if (DX1 < 0) then
			begin
			Inc(SXW);
			end;
			if (DY1 < 0) then
			begin
			Inc(SYH);
			end; }

	end;

	if (SourceWidth < BmpSource.Width) or (SourceHeight < BmpSource.Height) then
	begin
		// TODO : Zoom Part, update problem!
		if not Assigned(BmpSource2) then
		begin
			BmpSource2 := TDBitmap.Create;
		end;
		if (BmpSource2.Width <> SourceWidth) or (BmpSource2.Height <> SourceHeight) then
		begin
			BmpSource2.SetSize(SourceWidth, SourceHeight, clAppWorkSpace);
			if (QualityResize = False) { or (Zoom > 1) } then
			begin
				SetStretchBltMode(BmpSource2.Canvas.Handle, COLORONCOLOR);
				StretchBlt(BmpSource2.Canvas.Handle, 0, 0, BmpSource2.Width, BmpSource2.Height,
					BmpSource.Canvas.Handle, 0, 0, BmpSource.Width, BmpSource.Height, SRCCOPY);
				{ BmpSource2.Canvas.StretchDraw(Rect(0, 0,
					BmpSource2.Width, BmpSource2.Height), BmpSource); }
			end
			else
			begin
				{ LastCursor := Screen.Cursor;
					Screen.Cursor := crHourGlass; }
				BmpSource2.Resize(SourceWidth, SourceHeight, BmpSource);
				// Screen.Cursor := LastCursor;
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

function TDImage.SToL(const P: TPoint): TFloPoint;
begin
	Result.X := (P.X + FOfsX) / ActualZoom + UserArea.Left;
	Result.Y := (P.Y + FOfsY) / ActualZoom + UserArea.Top;
end;

function TDImage.GetPoint(const X, Y: FA): TPoint;
begin
	Result.X := RoundN(ActualZoom * (X - UserArea.Left) - FOfsX) + CenterOffset.X;
	Result.Y := RoundN(ActualZoom * (Y - UserArea.Top) - FOfsY) + CenterOffset.Y;
end;

function TDImage.GetFloPoint(const X, Y: FA): TFloPoint;
begin
	Result.X := ActualZoom * (X - UserArea.Left) - FOfsX + CenterOffset.X;
	Result.Y := ActualZoom * (Y - UserArea.Top)- FOfsY + CenterOffset.Y;
end;

function TDImage.GetFloPoint(const P: TFloPoint): TFloPoint;
begin
	Result.X := ActualZoom * (P.X - UserArea.Left) - FOfsX + CenterOffset.X;
	Result.Y := ActualZoom * (P.Y - UserArea.Top)- FOfsY + CenterOffset.Y;
end;

function TDImage.GetPosX(X: SG): SG;
begin
	Result := RoundSG(ActualZoom * (X - UserArea.Left) - FOfsX) + CenterOffset.X;
end;

function TDImage.GetPosY(Y: SG): SG;
begin
	Result := RoundSG(ActualZoom * (Y - UserArea.Top) - FOfsY) + CenterOffset.Y
end;

procedure TDImage.FillUserBitmap;
var
	ActualZoomX, ActualZoomY: FA;
	e: FA;
	i: SG;
begin
	if (UserBitmap <> nil) and (BmpS <> nil) then
	begin
		{ if Center then
			Bitmap.AntiBar(DX, DY, DW - 1, DH - 1, clAppWorkSpace, ef16); }
		// else }
		Bitmap.Bar(DX, DY, DW - 1, DH - 1, clAppWorkSpace, ef16); // TODO : Temp
		SetStretchBltMode(Bitmap.Canvas.Handle, COLORONCOLOR);
		StretchBlt(Bitmap.Canvas.Handle, DX + CenterOffset.X, DY + CenterOffset.Y, DW, DH,
			BmpS.Canvas.Handle, SX, SY, SW, SH, SRCCOPY);

		ActualZoomX := DW / SW;
		ActualZoomY := DH / SH;
		if Grate then
		begin // TODO bug offset on frac zoom
			if ActualZoom >= 3 then
			begin
				e := (RoundN(ActualZoomX) - FOfsX) mod RoundN(ActualZoomX);
				i := Round(e);
				while i < UserWidth do
				begin
					Bitmap.Line(i, 0, i, UserHeight - 1, GrateColor, ef12);
					e := e + ActualZoomX;
					i := RoundSG(e);
				end;
				e := (RoundN(ActualZoomY) - FOfsY) mod RoundN(ActualZoomY);
				i := RoundSG(e);
				while i < UserHeight do
				begin
					Bitmap.Line(0, i, UserWidth - 1, i, GrateColor, ef12);
					e := e + ActualZoomY;
					i := RoundN(e);
				end;
			end;
		end;
	end;
	{ else
		Bitmap.Bar(clAppWorkSpace, ef16); }
end;

procedure TDImage.FillBitmap;
var
	ScrollLen, ScrollLenS: Integer;
	X1, Y1, X2, Y2: Integer;
	C: TColor;
	Co: array [0 .. 3] of TColor;
	I1, I2: SG;
	SliderC1, SliderC2: TColor;
	s: string;
	i, X, Y: SG;
	R: TRect;
{$IFDEF info}
	StartTickCount: U4;
{$ENDIF}
begin
	OffsetRange(FOfsX, FOfsY);
	inherited;
{$IFDEF info}
	GetGTime;
	StartTickCount := GTime;
{$ENDIF}
	if (Bitmap.Width <> Width) or (Bitmap.Height <> Height) then
		Bitmap.SetSize(Width, Height, Color);

	if FEnableZoom then
	begin
		SX := 0;
		SY := 0;
		SW := 0;
		SH := 0;
		DX := 0;
		DY := 0;
		DX := 0;
		DH := 0;
		if UserBitmap <> nil then
		begin
			ZoomMake(UserBitmap, Width, Height, False, Center, ActualZoom, False, False
				{ True } , FOfsX + Floor(Zoom * UserArea.Left), FOfsY + Floor(Zoom * UserArea.Top),
				UserWidth, UserHeight, SX, SY, SW, SH, DX, DY, DW, DH, BmpSourceS);
			UserWidth := RoundN(Zoom * UserWidth2);
			UserHeight := RoundN(Zoom * UserHeight2);

			if Assigned(BmpSourceS) then
				BmpS := BmpSourceS
			else
				BmpS := UserBitmap;
		end;
		CenterOffset.X := 0;
		CenterOffset.Y := 0;
{		CenterOffset.X := Floor(-Zoom * UserArea.Left);
		CenterOffset.Y := Floor(-Zoom * UserArea.Top);
		if Center then
		begin
			if UserWidth < DW then
				CenterOffset.X := CenterOffset.X + (DW - ScrollBarHWidth - UserWidth) div 2;
			if UserHeight < DH then
				CenterOffset.Y := CenterOffset.Y + (DH - ScrollBarHHeight - UserHeight) div 2;
		end; }
	end;

	UpdateDisplayMode;
	FillUserBitmap;
	if Assigned(FOnFill) then
	begin
		try
			FOnFill(Self);
		except
			on e: Exception do
				Fatal(e, Self);
		end;
	end;
	InitScrolls;

	SliderC1 := DarkerColor(clScrollBar);
	SliderC2 := LighterColor(clScrollBar);

	// H
	if (UserWidth > 0) and (HType = 1) and (FNowMaxWidth > 2 * ScrollBarHHeight) then
	begin
		ScrollBarHWidth := FNowMaxWidth;
		Y1 := Integer(Bitmap.Height) - ScrollBarHHeight;
		Y2 := Bitmap.Height - 1;

		X1 := 0;
		X2 := ScrollBarVWidth - 1;
		Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHD, FHotTrack and
				(MouseWhere = mwScrollHD), 1, ScrollEf);

		X1 := FNowMaxWidth - ScrollBarVWidth;
		X2 := FNowMaxWidth - 1;
		Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollHU, FHotTrack and
				(MouseWhere = mwScrollHU), 3, ScrollEf);

		// TScrollBoxSlider
		ScrollLen := FNowMaxWidth - 2 * ScrollBarVWidth;
		ScrollLenS := FNowMaxWidth * ScrollLen div UserWidth;
		if ScrollLenS < ScrollBarHHeight div 2 then
			ScrollLenS := ScrollBarHHeight div 2;

		Y1 := Integer(Bitmap.Height) - ScrollBarHHeight + 1;
		Y2 := Bitmap.Height - 1 - 1;
		X1 := ScrollBarVWidth + RoundDivS8(S8(ScrollLen - ScrollLenS) * S8(FOfsX),
			UserWidth - FNowMaxWidth);
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
		Bitmap.Border(X1, Y1, X2, Y2, clDepth[I1], clDepth[I2], 1, ScrollEf);
		if FHotTrack and (MouseWhere = mwScrollH) and (MouseL = False) then
		begin
			Co[0] := $FFFFFD;
			Co[1] := $FBDAB9;
			Co[2] := Co[0];
			Co[3] := Co[1];
		end
		else
		begin
			Co[0] := $FFE6D6;
			Co[1] := $F1C3AE;
			Co[2] := Co[0];
			Co[3] := Co[1];
		end;
		Bitmap.GenerateRGBEx(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, gfFade2x, Co, ScrollEf, 0, nil);
		{ if MouseAction in [mwScrollH, mwScrollHD, mwScrollHU, mwScrollHD2, mwScrollHU2] then
			begin
			PushFont(Bitmap.Canvas.Font);
			Bitmap.Canvas.Font.PixelsPerInch :=2;
			Bitmap.Canvas.Font.Color := clWindowText;
			Bitmap.Canvas.Font.Name := 'Arial';
			Bitmap.Canvas.Font.Height := (ScrollBarHHeight - 2);
			s := NToS(FOfsX);
			x := (X1 + X2 - Bitmap.Canvas.TextWidth(s) + 3) div 2;
			y := (Y1 + Y2 - Bitmap.Canvas.TextHeight(s) + 3) div 2;
			Bitmap.Canvas.TextOut(x, y, s);
			PopFont(Bitmap.Canvas.Font);
			end
			else
			begin
			x := (X1 + X2 - RoundDiv(ScrollBarHHeight, 3)) div 2;
			for i := 0 to RoundDiv(ScrollBarHHeight, 6) - 1 do
			begin
			Bitmap.Line(x, Y1 + 4, x, Y2 - 5, $fef4ee, ef16);
			Inc(x);
			Bitmap.Line(x, Y1 + 5, x, Y2 - 4, $d0b08c, ef16);
			Inc(x);
			end;
			end; }

		for i := -1 to 1 do
		begin
			X := (X1 + X2) div 2 + i * RoundDiv(ScrollBarHHeight, 6) - RoundDiv(ScrollBarHHeight, 24);
			Bitmap.BarBorder(X, Y1 + 3, X + RoundDiv(ScrollBarHHeight, 12), Y2 - 2, $FEF4EE);
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
			Bitmap.Bar(X1, Y1 + 1, X2, Y2 - 1, C, ScrollEf2);
		end;

		X1 := SliderHX2 + 1;
		X2 := FNowMaxWidth - ScrollBarVWidth - 1;
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
			Bitmap.Bar(X1, Y1 + 1, X2, Y2 - 1, C, ScrollEf2);
		end;
	end
	else
		ScrollBarHWidth := 0;

	// V
	if (UserHeight > 0) and (VType = 1) and (FNowMaxHeight > 2 * ScrollBarVWidth) then
	begin
		ScrollBarVHeight := FNowMaxHeight;
		X1 := Integer(Bitmap.Width) - ScrollBarVWidth;
		X2 := Bitmap.Width - 1;
		if (X1 >= 0) and (X2 > X1) then
		begin
			Y1 := 0;
			Y2 := ScrollBarHHeight - 1;
			Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVD, FHotTrack and
					(MouseWhere = mwScrollVD), 0, ScrollEf);

			Y1 := FNowMaxHeight - ScrollBarHHeight;
			Y2 := FNowMaxHeight - 1;
			Bitmap.DrawArrow(X1, Y1, X2, Y2, MouseAction = mwScrollVU, FHotTrack and
					(MouseWhere = mwScrollVU), 2, ScrollEf);
		end;

		// TScrollBoxSlider
		ScrollLen := FNowMaxHeight - 2 * ScrollBarHHeight;
		ScrollLenS := FNowMaxHeight * ScrollLen div UserHeight;
		if ScrollLenS < ScrollBarVWidth div 2 then
			ScrollLenS := ScrollBarVWidth div 2;

		X1 := Integer(Bitmap.Width) - ScrollBarVWidth + 1;
		X2 := Bitmap.Width - 1 - 1;
		Y1 := ScrollBarHHeight + (ScrollLen - ScrollLenS) * Int64(FOfsY) div
			(UserHeight - FNowMaxHeight);
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
		Bitmap.Border(X1, Y1, X2, Y2, clDepth[I1], clDepth[I2], 1, ScrollEf);
		if FHotTrack and (MouseWhere = mwScrollV) and (MouseL = False) then
		begin
			Co[0] := $FFFFFD;
			Co[1] := $FBDAB9;
			Co[2] := Co[0];
			Co[3] := Co[1];
		end
		else
		begin
			Co[0] := $FFE6D6;
			Co[1] := $F1C3AE;
			Co[2] := Co[0];
			Co[3] := Co[1];
		end;
		Bitmap.GenerateRGBEx(X1 + 1, Y1 + 1, X2 - 1, Y2 - 1, gfFade2x, Co, ScrollEf, 0, nil);
		{ if MouseAction in [mwScrollV, mwScrollVD, mwScrollVU, mwScrollVD2, mwScrollVU2] then
			begin
			PushFont(Bitmap.Canvas.Font);
			Bitmap.Canvas.Font.PixelsPerInch :=2;
			Bitmap.Canvas.Font.Color := clWindowText;
			Bitmap.Canvas.Font.Name := 'Arial';
			Bitmap.Canvas.Font.Height := (ScrollBarHHeight - 2);
			s := NToS(FOfsY);
			x := (X1 + X2 - Bitmap.Canvas.TextWidth(s) + 3) div 2;
			y := (Y1 + Y2 - Bitmap.Canvas.TextHeight(s) + 3) div 2;
			Bitmap.Canvas.TextOut(x, y, s);
			PopFont(Bitmap.Canvas.Font);
			end
			else
			begin
			y := (Y1 + Y2) div 2 - RoundDiv(ScrollBarVWidth, 6);
			for i := 0 to RoundDiv(ScrollBarVWidth, 6) - 1 do
			begin
			Bitmap.Line(X1 + 4, y, X2 - 5, y, $fef4ee, ef16);
			Inc(y);
			Bitmap.Line(X1 + 5, y, X2 - 4, y, $d0b08c, ef16);
			Inc(y);
			end;
			end; }
		for i := -1 to 1 do
		begin
			Y := (Y1 + Y2) div 2 + i * RoundDiv(ScrollBarVWidth, 6) - RoundDiv(ScrollBarVWidth, 24);
			Bitmap.BarBorder(X1 + 3, Y, X2 - 2, Y + RoundDiv(ScrollBarVWidth, 12), $FEF4EE);
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
			Bitmap.Bar(X1 + 1, Y1, X2 - 1, Y2, C, ScrollEf2);
		end;

		Y1 := SliderVY2 + 1;
		Y2 := FNowMaxHeight - ScrollBarHHeight - 1;
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
			Bitmap.Bar(X1 + 1, Y1, X2 - 1, Y2, C, ScrollEf2);
		end;
	end
	else
		ScrollBarVHeight := 0;
{$IFNDEF info}
	if FDrawFPS then
{$ENDIF}
	begin
		PushFont(Bitmap.Canvas.Font);
		try
			Bitmap.Canvas.Font.Color := clWindowText;
			Bitmap.Canvas.Font.Height := -Range(8, Min(Width, Height) div 4, 11);
			Bitmap.Canvas.Brush.style := bsClear;

			R := Rect(2, 2, Width - 1 - 2, Height - 1 - 2);
{$IFDEF info}
			if (FOfsX <> 0) or (FOfsY <> 0) then
				s := NToS(FOfsX) + CharTimes + NToS(FOfsY)
			else
				s := '';
			if ActualZoom <> 1 then
				s := s + 'Zoom=' + FToS(ActualZoom) + ':1'; // + LineSep + 'V=' + FToS(V) + LineSep + 'A=' + FToS(A);
			// DrawCuttedText(Bitmap.Canvas, R, taRightJustify, tlBottom, s, True, IdealShadow(Bitmap.Canvas));

			StartTickCount := IntervalFrom(StartTickCount);
			if StartTickCount > 15 then
			begin
				if s <> '' then
					s := s + ', ';
				s := s + MsToStr(StartTickCount, diSD, 3, False);
			end;
{$ENDIF}
			DrawCuttedText(Bitmap.Canvas, R, taRightJustify, tlBottom, s, True, IdealShadow(Bitmap.Canvas)
				);

			Inc(FIntervalFillCount);
			GetGTime;
			if TimeDifference(GTime, FIntervalStartTime) >= Second then
			begin
				if FIntervalStartTime > 0 then
					FFPS := RoundDivS8(1000 * Second * FIntervalFillCount, TimeDifference
							(GTime, FIntervalStartTime));
				FIntervalFillCount := 0;
				FIntervalStartTime := GTime;
			end;
			if FFPS > 0 then
				DrawCuttedText(Bitmap.Canvas, R, taRightJustify, tlTop, NToS(FFPS, 3), True, IdealShadow
						(Bitmap.Canvas));
		finally
			PopFont(Bitmap.Canvas.Font);
		end;
	end;
end;

procedure TDImage.SetZoom(Value: TZoom);
begin
	if TargetZoom <> Value then
	begin
		TargetZoom := Value;
		ActualZoom := Value;
		InitScrolls;
		Invalidate;
		InitZoomMenu;
	end;
end;

procedure TDImage.SetHotTrack(Value: Boolean);
begin
	if FHotTrack <> Value then
	begin
		FHotTrack := Value;
		Invalidate;
	end;
end;

procedure TDImage.SetUserArea(const Value: TRect);
begin
	if not SameRect(Value, FUserArea) then
	begin
		{ if Value < FUserHeight then
			begin
			FUserHeight := Value;
			InitScrolls;
			if FOfsY > Max(0, MaxOfsY) then
			FOfsY := Max(0, MaxOfsY);
			end
			else
			begin
			if FOfsY = MaxOfsY then
			Inc(FOfsY, Value - FUserHeight);
			FUserHeight := Value;
			InitScrolls;
			end; }
		FUserArea := Value;
		InitScrolls;
	end;
end;

procedure TDImage.OffsetOnRect(const Rect: TRect);
var
	X, Y: SG;
	D0, D1: SG;
begin
	// Inputs Rect, FOfsX, FOfsY, FNowMaxWidth, FNowMaxHeight

	// Set values as no change
	X := FOfsX;
	Y := FOfsY;

	// Horizontal alignment
	if (Rect.Left < FOfsX) and (Rect.Right > FOfsX + FNowMaxWidth) then
	begin
		// Rectangle too large
	end
	else if (Rect.Left >= FOfsX) and (Rect.Right < FOfsX + FNowMaxWidth) then
	begin
		// Rectangle is inside of view
	end
	else
	begin
		D0 := Rect.Left - FOfsX;
		D1 := Rect.Right - (FOfsX + FNowMaxWidth - 1);
		Inc(X, AbsMin(D0, D1));
	end;

	// Vertical alignment
	if (Rect.Top < FOfsY) and (Rect.Bottom > FOfsY + FNowMaxHeight) then
	begin
		// Rectangle too large
	end
	else if (Rect.Top >= FOfsY) and (Rect.Bottom < FOfsY + FNowMaxHeight) then
	begin
		// Rectangle is inside of view
	end
	else
	begin
		D0 := Rect.Top - FOfsY;
		D1 := Rect.Bottom - (FOfsY + FNowMaxHeight - 1);
		Inc(Y, AbsMin(D0, D1));
	end;

	ScrollTo(X, Y);
end;

procedure TDImage.PageDownUp(const n: SG);
begin
	ScrollTo(FOfsX, FOfsY + n * FNowMaxHeight);
end;

procedure TDImage.PageLeftRight(const n: SG);
begin
	ScrollTo(FOfsX + n * FNowMaxWidth, FOfsY);
end;

procedure TDImage.LineDownUp(const n: SG);
begin
	ScrollTo(FOfsX, FOfsY + n * HorizontalOffset);
end;

procedure TDImage.LineLeftRight(const n: SG);
begin
	ScrollTo(FOfsX + n * HorizontalOffset, FOfsY);
end;

procedure TDImage.CursorDownUp(const n: SG);
begin
	LineDownUp(n);
end;

procedure TDImage.ScrollHome;
begin
	ScrollTo(FOfsX, 0);
end;

procedure TDImage.ScrollEnd;
begin
	ScrollTo(FOfsX, MaxOfsY);
end;

procedure TDImage.KeyDown(var Key: Word; Shift: TShiftState);
begin
	FShiftState := Shift;
	ChangeCursor;
	if Shift = [ssCtrl] then
	begin
		case Key of
		Ord('C'):
			begin
				ToClipboard;
				// Exit;
			end;
		end;
	end
	else if Shift = [] then
	begin
		case Key of
		VK_LEFT:
			LineLeftRight(-1);
		VK_RIGHT:
			LineLeftRight(1);
		VK_UP:
			LineDownUp(-1);
		VK_DOWN:
			LineDownUp(1);
		VK_PRIOR:
			PageDownUp(-1);
		VK_NEXT:
			PageDownUp(1);
		VK_HOME:
			ScrollHome;
		VK_END:
			ScrollEnd;
		end;
	end;
	inherited;
end;

procedure TDImage.KeyUp(var Key: Word; Shift: TShiftState);
begin
//	if MouseAction <> mwScroll then
	begin
		FShiftState := Shift;
		ChangeCursor;
	end;
	inherited;
end;

procedure TDImage.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
	case Message.CharCode of
	VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT:
		Message.Result := 1;
	end;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TDImage]);
end;

procedure TDImage.Serialize(const IniFile: TDIniFile; const Save: BG);
var
	Section: string;
	XZoom: Extended;
begin
	Section := GetSectionName(Self);

	if Save = False then
		XZoom := 1
	else
		XZoom := Zoom;
	IniFile.RWNum(Section, 'Zoom', XZoom, Save);
	if Save = False then
		Zoom := XZoom;
	IniFile.RWNum(Section, 'OffsetX', FOfsX, Save);
	IniFile.RWNum(Section, 'OffsetY', FOfsY, Save);
	IniFile.RWEnum(Section, TypeInfo(TDisplayMode), U1(FDisplayMode), Save);

	IniFile.RWBool(Section, 'Center', Center, Save);
	IniFile.RWBool(Section, 'Grate', Grate, Save);
	IniFile.RWNum(Section, 'GrateColor', S4(GrateColor), Save);
end;

procedure TDImage.ToClipboard;
begin
	Clipboard.Assign(TBitmap(Bitmap));
end;

function TDImage.GetImageOffset: TPoint;
begin
	Result.X := FOfsX;
	Result.Y := FOfsY;
end;

procedure TDImage.FitImage;
begin
	if (UserWidth2 > 0) and (UserHeight2 > 0) then
	begin
		ChangeZoom(Min(Bitmap.Width / UserWidth2, Bitmap.Height / UserHeight2));
		Invalidate;
	end;
end;

function TDImage.GetDisplayedArea: TRect;
begin
	Result.Left := SX;
	Result.Top := SY;
	Result.Right := Result.Left + SW - 1;
	Result.Bottom := Result.Top + SH - 1;
end;

procedure TDImage.InitCursor(const X, Y: SG; const Shift: TShiftState);
begin
	MouseWhere := GetMouseWhere(X, Y);
	FShiftState := Shift;
	ChangeCursor;
end;

procedure TDImage.ChangeCursor;
var
	ScrollHand: BG;
begin
	case MouseWhere of
	mwScrollH, mwScrollV, mwScrollHD, mwScrollHU, mwScrollVD, mwScrollVU, mwScrollHD2, mwScrollHU2,
		mwScrollVD2, mwScrollVU2:
		begin
			Cursor := crArrow;
		end
	else
	begin
		ScrollHand := (HandScroll or (ssShift in FShiftState)) and
			((UserWidth - FNowMaxWidth > 0) or (UserHeight - FNowMaxHeight > 0));
		if ScrollHand then
			Cursor := 1 + SG(MouseL) // TODO DNW
		else
			Cursor := FAreaCursor;
	end;
	end;
//  if LogDebug then
//    LogAdd('Set Cursor to ' + NToS(Cursor));
end;

procedure TDImage.SetAreaCursor(const Value: TCursor);
begin
	if FAreaCursor <> Value then
	begin
		FAreaCursor := Value;
		ChangeCursor;
	end;
end;

procedure TDImage.SetDisplayMode(const Value: TDisplayMode);
begin
	if FDisplayMode <> Value then
	begin
		FDisplayMode := Value;
		UpdateDisplayMode;
	end;
end;

procedure TDImage.SetUserBitmap(const Value: TDBitmap);
begin
	if FUserBitmap <> Value then
	begin
		FUserBitmap := Value;
		BmpS := nil;
		FreeAndNil(BmpSourceS);
		Invalidate;
	end;
end;

procedure TDImage.UserBitmapChanged;
begin
	FreeAndNil(BmpSourceS);
	Invalidate;
end;

initialization

Screen.Cursors[crHandPoint] := LoadCursor(HInstance, PChar('HANDPOINT'));
Screen.Cursors[crHandPointDown] := LoadCursor(HInstance, PChar('HANDPOINTDOWN'));

end.
