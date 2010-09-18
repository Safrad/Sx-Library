// Build: 08/2001-08/2001 Author: Safranek David

unit uDView;

interface

{$R *.RES}
uses
	uAdd, uDImage,
	Windows, Classes, Controls, Graphics, Messages, CommCtrl;

type
{ TDView }

	TTrackBarOrientation = (trHorizontal, trVertical);
	TTickMark = (tmBottomRight, tmTopLeft, tmBoth);
	TTickStyle = (tsNone, tsAuto, tsManual);

	TDView = class(TWinControl)
	private
		FOrientation: TTrackBarOrientation;
		FTickMarks: TTickMark;
		FTickStyle: TTickStyle;
		FLineSize: Integer;
		FPageSize: Integer;
		FThumbLength: Integer;
		FSliderVisible: Boolean;
		FMin: Integer;
		FMax: Integer;
		FFrequency: Integer;
		FPosition: Integer;
		FSelStart: Integer;
		FSelEnd: Integer;
		FOnChange: TNotifyEvent;
		function GetThumbLength: Integer;
		procedure SetOrientation(Value: TTrackBarOrientation);
		procedure SetParams(APosition, AMin, AMax: Integer);
		procedure SetPosition(Value: Integer);
		procedure SetMin(Value: Integer);
		procedure SetMax(Value: Integer);
		procedure SetFrequency(Value: Integer);
		procedure SetTickStyle(Value: TTickStyle);
		procedure SetTickMarks(Value: TTickMark);
		procedure SetLineSize(Value: Integer);
		procedure SetPageSize(Value: Integer);
		procedure SetThumbLength(Value: Integer);
		procedure SetSliderVisible(Value: Boolean);
		procedure SetSelStart(Value: Integer);
		procedure SetSelEnd(Value: Integer);
		procedure UpdateSelection;
		procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
		procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;

		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
		procedure CreateParams(var Params: TCreateParams); override;
		procedure CreateWnd; override;
		procedure DestroyWnd; override;
		procedure Changed; dynamic;
	public
    constructor Create(AOwner: TComponent); override;
    procedure SetTick(Value: Integer);
		procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
			X, Y: Integer); override;
	published
    property Align;
    property Anchors;
    property BorderWidth;
    property Ctl3D;
    property DragCursor;
		property DragKind;
    property DragMode;
    property Enabled;
    property Constraints;
    property LineSize: Integer read FLineSize write SetLineSize default 1;
    property Max: Integer read FMax write SetMax default 10;
    property Min: Integer read FMin write SetMin default 0;
    property Orientation: TTrackBarOrientation read FOrientation write SetOrientation;
    property ParentCtl3D;
    property ParentShowHint;
    property PageSize: Integer read FPageSize write SetPageSize default 2;
    property PopupMenu;
    property Frequency: Integer read FFrequency write SetFrequency;
    property Position: Integer read FPosition write SetPosition;
    property SliderVisible: Boolean read FSliderVisible write SetSliderVisible default True;
    property SelEnd: Integer read FSelEnd write SetSelEnd;
    property SelStart: Integer read FSelStart write SetSelStart;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property ThumbLength: Integer read GetThumbLength write SetThumbLength default 20;
    property TickMarks: TTickMark read FTickMarks write SetTickMarks;
    property TickStyle: TTickStyle read FTickStyle write SetTickStyle;
    property Visible;
		property OnContextPopup;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
		property OnEndDrag;
		property OnEnter;
		property OnExit;
		property OnKeyDown;
		property OnKeyPress;
		property OnKeyUp;
		property OnStartDock;
		property OnStartDrag;
	end;

procedure Register;

implementation

uses uGraph, uGraph24;

{ TDView }

procedure TDView.WMPaint(var Message: TWMPaint);
begin
//	DefaultHandler(Message);
	inherited;
end;

procedure TDView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	DefaultHandler(Message);
end;


procedure TDView.MouseDown(Button: TMouseButton; Shift: TShiftState;
	X, Y: Integer);
begin
	SetFocus;
end;

constructor TDView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 45;
  TabStop := True;
  FMin := 0;
  FMax := 10;
  FLineSize := 1;
  FPageSize := 2;
  FFrequency := 1;
  FThumbLength := 20;
  FTickMarks := tmBottomRight;
  FTickStyle := tsAuto;
  FOrientation := trHorizontal;
  ControlStyle := ControlStyle - [csDoubleClicks];
  FSliderVisible := True;
end;

function InitCommonControl(CC: Integer): Boolean;
var
  ICC: TInitCommonControlsEx;
begin
  ICC.dwSize := SizeOf(TInitCommonControlsEx);
  ICC.dwICC := CC;
  Result := InitCommonControlsEx(ICC);
  if not Result then InitCommonControls;
end;


procedure TDView.CreateParams(var Params: TCreateParams);
const
	OrientationStyle: array[TTrackbarOrientation] of DWORD = (TBS_HORZ, TBS_VERT);
	TickStyles: array[TTickStyle] of DWORD = (TBS_NOTICKS, TBS_AUTOTICKS, 0);
	ATickMarks: array[TTickMark] of DWORD = (TBS_BOTTOM, TBS_TOP, TBS_BOTH);
const
	CS_OFF = CS_OWNDC or CS_CLASSDC or CS_PARENTDC or CS_GLOBALCLASS;
	CS_ON = CS_VREDRAW or CS_HREDRAW;
var
	SaveInstance: THandle;
	ControlClassName: PChar;
begin
	InitCommonControl(ICC_BAR_CLASSES);
	inherited CreateParams(Params);

//	CreateSubClass(Params, TRACKBAR_CLASS);
ControlClassName := TRACKBAR_CLASS;
			GetClassInfo(HInstance, ControlClassName, Params.WindowClass);
{Params.WindowClass.Style := 16384;
Params.WindowClass.lpszClassName := 'msctls_trackbar32';}
//Params.WindowClass.
//	if ControlClassName <> nil then
{		with Params do
		begin}
//		SaveInstance := WindowClass.hInstance;
{			if not GetClassInfo(HInstance, ControlClassName, WindowClass) and
				not GetClassInfo(0, ControlClassName, WindowClass) and
				not GetClassInfo(MainInstance, ControlClassName, WindowClass) then
				GetClassInfo(WindowClass.hInstance, ControlClassName, WindowClass);}
//			WindowClass.hInstance := SaveInstance;
//			WindowClass.style := WindowClass.style and not CS_OFF or CS_ON;
//		end;

	with Params do
	begin
		Style := Style or OrientationStyle[FOrientation] or
			TickStyles[FTickStyle] or ATickMarks[FTickMarks] or TBS_FIXEDLENGTH or
			TBS_ENABLESELRANGE;
		WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW) or
			CS_DBLCLKS;
		if not FSliderVisible then
			Style := Style or TBS_NOTHUMB;
	end;
end;

procedure TDView.CreateWnd;
begin
  inherited CreateWnd;
  if HandleAllocated then
  begin
    SendMessage(Handle, TBM_SETTHUMBLENGTH, FThumbLength, 0);
    SendMessage(Handle, TBM_SETLINESIZE, 0, FLineSize);
    SendMessage(Handle, TBM_SETPAGESIZE, 0, FPageSize);
    SendMessage(Handle, TBM_SETRANGEMIN, 0, FMin);
    SendMessage(Handle, TBM_SETRANGEMAX, 0, FMax);
    UpdateSelection;
    SendMessage(Handle, TBM_SETPOS, 1, FPosition);
    SendMessage(Handle, TBM_SETTICFREQ, FFrequency, 1);
  end;
end;

procedure TDView.DestroyWnd;
begin
  inherited DestroyWnd;
end;

procedure TDView.CNHScroll(var Message: TWMHScroll);
begin
  inherited;
  FPosition := SendMessage(Handle, TBM_GETPOS, 0, 0);
  Changed;
  Message.Result := 0;
end;

procedure TDView.CNVScroll(var Message: TWMVScroll);
begin
  inherited;
  FPosition := SendMessage(Handle, TBM_GETPOS, 0, 0);
  Changed;
  Message.Result := 0;
end;

function TDView.GetThumbLength: Integer;
begin
  if HandleAllocated then
    Result := SendMessage(Handle, TBM_GETTHUMBLENGTH, 0, 0)
  else
    Result := FThumbLength;
end;

procedure TDView.SetOrientation(Value: TTrackBarOrientation);
begin
  if Value <> FOrientation then
  begin
    FOrientation := Value;
    if ComponentState * [csLoading, csUpdating] = [] then
      SetBounds(Left, Top, Height, Width);
    RecreateWnd;
  end;
end;

procedure TDView.SetParams(APosition, AMin, AMax: Integer);
begin
	if APosition < AMin then APosition := AMin;
  if APosition > AMax then APosition := AMax;
  if (FMin <> AMin) then
  begin
    FMin := AMin;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETRANGEMIN, 1, AMin);
  end;
  if (FMax <> AMax) then
  begin
    FMax := AMax;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETRANGEMAX, 1, AMax);
  end;
  if FPosition <> APosition then
  begin
    FPosition := APosition;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETPOS, 1, APosition);
    Changed;
  end;
end;

procedure TDView.SetPosition(Value: Integer);
begin
  SetParams(Value, FMin, FMax);
end;

procedure TDView.SetMin(Value: Integer);
begin
  if Value <= FMax then
    SetParams(FPosition, Value, FMax);
end;

procedure TDView.SetMax(Value: Integer);
begin
  if Value >= FMin then
    SetParams(FPosition, FMin, Value);
end;

procedure TDView.SetFrequency(Value: Integer);
begin
  if Value <> FFrequency then
  begin
    FFrequency := Value;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETTICFREQ, FFrequency, 1);
  end;
end;

procedure TDView.SetTick(Value: Integer);
begin
  if HandleAllocated then
    SendMessage(Handle, TBM_SETTIC, 0, Value);
end;

procedure TDView.SetTickStyle(Value: TTickStyle);
begin
  if Value <> FTickStyle then
  begin
    FTickStyle := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetTickMarks(Value: TTickMark);
begin
  if Value <> FTickMarks then
  begin
    FTickMarks := Value;
    RecreateWnd;
  end;
end;

procedure TDView.SetLineSize(Value: Integer);
begin
  if Value <> FLineSize then
  begin
    FLineSize := Value;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETLINESIZE, 0, FLineSize);
  end;
end;

procedure TDView.SetPageSize(Value: Integer);
begin
  if Value <> FPageSize then
  begin
    FPageSize := Value;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETPAGESIZE, 0, FPageSize);
  end;
end;

procedure TDView.SetThumbLength(Value: Integer);
begin
  if Value <> FThumbLength then
  begin
    FThumbLength := Value;
    if HandleAllocated then
      SendMessage(Handle, TBM_SETTHUMBLENGTH, Value, 0);
  end;
end;

procedure TDView.SetSliderVisible(Value: Boolean);
begin
  if FSliderVisible <> Value then
  begin
    FSliderVisible := Value;
    RecreateWnd;
  end;
end;

procedure TDView.UpdateSelection;
begin
  if HandleAllocated then
  begin
    if (FSelStart = 0) and (FSelEnd = 0) then
      SendMessage(Handle, TBM_CLEARSEL, 1, 0)
    else
      SendMessage(Handle, TBM_SETSEL, Integer(True), MakeLong(FSelStart, FSelEnd));
  end;
end;

procedure TDView.SetSelStart(Value: Integer);
begin
  if Value <> FSelStart then
	begin
    FSelStart := Value;
    UpdateSelection;
  end;
end;

procedure TDView.SetSelEnd(Value: Integer);
begin
  if Value <> FSelEnd then
  begin
    FSelEnd := Value;
    UpdateSelection;
  end;
end;

procedure TDView.Changed;
begin
	if Assigned(FOnChange) then FOnChange(Self);
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDView]);
end;

end.
