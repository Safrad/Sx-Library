unit uDForm;

interface

{$R *.RES}

uses
	uTypes, uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
	ExtCtrls, StdCtrls;

type
	TBackground = (baNone, baUser, baStandard, baGradient, baOpenGL, baOpenGLBitmap);

	TPosition = (poUnknown = -1, poLeft, poRight, poTop, poBottom);

	TRWOptionsEvent = procedure(Sender: TObject; Save: Boolean) of object;

	TDForm = class(TForm)
	private
		{ Private declarations }
		FCaption: string;
		FStoreWindow: Boolean;
		FWindowPlacement: TWindowPlacement;
		FWindowLong: S4;

		FBitmapB: TDBitmap;
		FLastColor: TColor;

		FBackground: TBackground;
		FFullScreen: Boolean;
		FChangeMode: Boolean;

		FOnRWOptions: TRWOptionsEvent;

		procedure ResizeMessage;
		procedure CheckPos;
		procedure Common(Value: Boolean);
		procedure SetFullScreen(Value: Boolean);
		procedure InitBackground(const Direct: BG);
		procedure SetBackground(Value: TBackground);
		procedure SetChangeMode(Value: Boolean);
		procedure SetCaption(Value: string);

		procedure WMSize(var Message: TWMSize);
		message WM_SIZE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd);
		message WM_ERASEBKGND;
		procedure WMShow(var Message: TWMShowWindow);
		message WM_SHOWWINDOW;
		procedure WMSysColorChange(var Message: TWMSysColorChange);
		message WM_SYSCOLORCHANGE;
	public
		{ Public declarations }
		RC: HGLRC;
		FontBase: U4;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CreateParams(var Params: TCreateParams); override;
		// function CloseQuery: Boolean; override;

		procedure RestoreWindow;
		procedure StoreWindow;

		// procedure KeyDown(var Key: U2; Shift: TShiftState); override;

		procedure Paint; override;
		procedure ResizeScene;
		procedure Center;
		function CenterPoint: TPoint;
		procedure SetVisible(const Value: Boolean);
		procedure ChangeVisible;
		procedure AlignControl(const AControl: TControl; const AAnchors: TAnchors);
		procedure AlignControlRight(const AControl: TControl);
		procedure AlignControlBottom(const AControl: TControl; const KeepSize: BG);
		procedure AlignControlRightTop(const AControl: TControl);
	published
		{ published declarations }
		property Caption: string read FCaption write SetCaption;
		property BackBitmap: TDBitmap read FBitmapB;
		property Background: TBackground read FBackground write SetBackground default baGradient;
		property FullScreen: Boolean read FFullScreen write SetFullScreen default False;
		property ChangeMode: Boolean read FChangeMode write SetChangeMode default False;

		property OnRWOptions: TRWOptionsEvent read FOnRWOptions write FOnRWOptions;
		// property OnMouseMove;
	end;

procedure FormFree(var Form: TDForm); overload;
procedure FormFree(var Form: TForm); overload;
function FormDraw(const Form: TForm): BG;
procedure ActivateForm(Form: TForm);

procedure glShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: AnsiString;
	const CF, CB: TColor; const Shadow: SG);
procedure glTextOut(Canvas: TCanvas; const X, Y: Integer; const Text: string; const C: TColor);
procedure ShowTaskBar(Visible: Boolean);
function GetTaskBarPos: TPosition;
// function GetDesktopRect(out Rect: TRect): SG; deprecated;

procedure Register;

const
	FormBorder = 8;
	FreeFormAfterClose = True;

var
	DesktopHWnd: HWnd;
	DesktopDC: HDC;
	DisableShowHideTaskBar: BG;

function ActiveForm: TForm;
procedure SetControlEnabled(Component: TComponent; E: BG);
function GetDesktop: BG;
procedure ReleaseDesktop;

implementation

uses
	Types, Math,
  uDictionary,
	uGraph, uFiles, OpenGL12, uScreen, uStrings, uColor, uProjectInfo, uDWinControl, uSysInfo, uCommon, uLog;

const
	OneBuffer = False;

function ActiveForm: TForm;
var
	i: SG;
begin
	Result := Application.MainForm;
	for i := 0 to Screen.FormCount - 1 do
	begin
		if Screen.Forms[i].Active then
		begin
			Result := Screen.Forms[i];
			Break;
		end;
	end;
end;

procedure SetControlEnabled(Component: TComponent; E: BG);
var
	i: SG;
begin
	for i := 0 to Component.ComponentCount - 1 do
	begin
		if Component.Components[i] is TControl then
			TControl(Component.Components[i]).Enabled := E;
	end;
end;

function GetDesktop: BG;
begin
	Result := False;
	if DesktopHWnd = INVALID_HANDLE_VALUE then
	begin
		DesktopDC := 0;
		Exit;
	end;
	if (DesktopDC = 0) then
	begin
		DesktopHWnd := 0; // GetDesktopWindow;
		if DesktopHWnd <> INVALID_HANDLE_VALUE then
		begin
			DesktopDC := GetDC(DesktopHWnd);
			if DesktopDC <> 0 then
				Result := True;
		end;
	end
	else
		Result := True;
end;

procedure ReleaseDesktop;
begin
	if (DesktopHWnd <> INVALID_HANDLE_VALUE) and (DesktopDC <> 0) then
	begin
		ReleaseDC(DesktopHWnd, DesktopDC);
		DesktopHWnd := 0;
		DesktopDC := 0;
	end;
end;

function FormDraw(const Form: TForm): BG;
// var WindowLong: S4;
begin
	Result := False;
	if not Assigned(Form) then
		Exit;

	{ WindowLong := GetWindowLong(Form.Handle, GWL_STYLE);
		if WindowLong and WS_VISIBLE = 0 then Exit; }
	// Assert(Form.Visible = True);
	if Form.Visible = False then
		Exit;
	// if Form.WindowState = wsMinimized then Exit; // DNW
	Result := True;
end;

procedure FormFree(var Form: TDForm);
begin
	if Assigned(Form) then
	begin
		Form.Close; // Free does not call Close and CloseQuery events
		FreeAndNil(Form);
	end;
end;

procedure FormFree(var Form: TForm);
begin
	if Assigned(Form) then
	begin
		Form.Close; // Free does not call Close and CloseQuery events
		FreeAndNil(Form);
	end;
end;

procedure glTextOut(Canvas: TCanvas; const X, Y: Integer; const Text: string; const C: TColor);
begin
	glShadowText(Canvas, X, Y, Text, C, clNone, 0);
	{ glGetIntegerv(GL_VIEWPORT, @Params[0]);

		if (Params[2] = 0) or (Params[3] = 0) then Exit;
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity;

		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity;

		glColor3ubv(PGLUByte(@C));
		glRasterPos2d(2 * X / Params[2] - 1, -2 * (Y + 11) / Params[3] + 1);
		glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1]))); }
end;

procedure glShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: AnsiString;
	const CF, CB: TColor; const Shadow: SG);
var
	Params: array [0 .. 3] of SG;
	C: TRGBA;
	sx, sy, wx, wy: Single;
	// px: array[0..3] of Double;
begin
	if Text = '' then
		Exit;

	glGetIntegerv(GL_VIEWPORT, @Params[0]);

	if (Params[2] = 0) or (Params[3] = 0) then
		Exit;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

	if CB <> clNone then
	begin
		sx := 2 * (X - 1) / Params[2] - 1;
		sy := -2 * (Y + 1 + Canvas.TextHeight(Text)) / Params[3] + 1;
		wx := 2 * (Canvas.TextWidth(Text) + 2) / Params[2];
		wy := 2 * (Canvas.TextHeight(Text) + 2) / Params[3];
		{ sx := 2 * (X + 1) / Params[2] - 1;
			sy := -2 * (Y + 1 + Canvas.TextHeight(Text)) / Params[3] + 1;
			wx := 2 * (Canvas.TextWidth(Text) + 1) / Params[2];
			wy := 2 * (Canvas.TextHeight(Text) + 1) / Params[3]; }
		C.L := CB;
		C.A := $FF;

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glColor4ubv(PGLUByte(@C));
		glBegin(GL_QUADS);
		glVertex3f(sx, sy, 1);
		glVertex3f(sx + wx, sy, 1);
		glVertex3f(sx + wx, sy + wy, 0.5);
		glVertex3f(sx, sy + wy, 0.5);
		glEnd;
		glDisable(GL_BLEND);
	end;

	(* C.L := MixColors(CF, CB);
		glColor3ubv(PGLUByte(@C));
		glRasterPos3d(2 * (X + 1) / Params[2] - 1, -2 * (Y + 1 + 11) / Params[3] + 1, 1); // OpenGL FP Exception
		{	glGetDoublev(GL_CURRENT_RASTER_POSITION, @Px[0]);
		glTexCoord4d(1, 1, 1, 1);
		glRasterPos4d(68, 0, 0, 1);
		glBitmap(0, 0, 0, 0, 0, 0, nil);}
		glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1]))); *)

	C.L := CF;
	glColor3ubv(PGLUByte(@C));
	glRasterPos3d(2 * X / Params[2] - 1, -2 * (Y + 11) / Params[3] + 1, 0); // OpenGL FP Exception
	// glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@AnsiString(Text)[1])));
	glCallLists(Length(Text), {$IFDEF UNICODE} GL_UNSIGNED_BYTE {$ELSE} GL_UNSIGNED_BYTE
{$ENDIF}, Pointer(Integer(@Text[1])));
end;

procedure ShowTaskBar(Visible: Boolean);
var
	hTaskBar: HWnd;
begin
	if DisableShowHideTaskBar then Exit;
	hTaskBar := FindWindow('Shell_TrayWnd', nil);
	if Visible then
	begin
		ShowWindow(hTaskBar, SW_SHOWNA);
		MainLogAdd('ShowTaskBar', mlInformation);
	end
	else
	begin
		ShowWindow(hTaskBar, SW_HIDE);
		MainLogAdd('HideTaskBar', mlInformation);
	end;
end;

function GetTaskBarPos: TPosition;
var
	hTaskBar: HWnd;
	RectT: TRect;
	w, h: SG;
begin
	hTaskBar := FindWindow('Shell_TrayWnd', nil);
	GetWindowRect(hTaskBar, RectT);
	w := Screen.Width;
	h := Screen.Height;

	if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Top <= 0) then
	begin
		Result := poTop;
	end
	else if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Bottom >= h) then
	begin
		Result := poBottom;
	end
	else if (RectT.Left <= 0) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
		Result := poLeft;
	end
	else if (RectT.Right >= w) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
		Result := poRight;
	end
	else
		Result := poUnknown;
end;

{ function GetDesktopRect(out Rect: TRect): SG;
	var
	hTaskBar: HWND;
	RectT: TRect;
	w, h: SG;
	begin
	hTaskBar := FindWindow('Shell_TrayWnd', nil);
	GetWindowRect(hTaskBar, RectT);
	w := Screen.Width;
	h := Screen.Height;
	Rect.Left := 0;
	Rect.Right := w;
	Rect.Top := 0;
	Rect.Bottom := h;

	if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Top <= 0) then
	begin
	Rect.Top := RectT.Bottom; // Top
	Result := 0;
	end
	else if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Bottom >= h) then
	begin
	Rect.Bottom := RectT.Top; // Bottom
	Result := 1;
	end
	else if (RectT.Left <= 0) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
	Rect.Left := RectT.Right; // Left
	Result := 2;
	end
	else if (RectT.Right >= w) and (RectT.Top <= 0) and (RectT.Bottom >= h) then
	begin
	Rect.Right := RectT.Left; // Right
	Result := 3;
	end
	else
	Result := -1;
	end;

	procedure TDForm.AfterCreate;
	begin
	if Parent.WindowState = wsMDIForm then
	begin
	Form.Style := fsMDIChild;
	end;
	end; }

procedure TDForm.Common(Value: Boolean);
const
	FullScreenMode: TScreenMode = (Width: 640; Height: 480; Bits: 32; RefreshRate: 0);
var
	Style: S4;
	Rect: TRect;
	// LActive: BG;
begin
	// LActive := Active;
	if Value then
	begin
		StoreWindow;

		if FChangeMode then
		begin
			ReadScreenModes;
			SetScreenMode(FullScreenMode, False, False, False, False, True);
		end;
		Style := GetWindowLong(Handle, GWL_STYLE);
		Style := Style and not WS_CAPTION;
		Style := Style and not WS_THICKFRAME;
		SetWindowLong(Handle, GWL_STYLE, Style);
		if Active then
			WindowState := wsMaximized;
		Rect := Screen.MonitorFromWindow(Handle).BoundsRect;
		SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
		// -> PopupMenu is visibled
		InitBackground(False);
	end
	else
	begin
		if FChangeMode then
			RestoreStartMode;
		Style := GetWindowLong(Handle, GWL_STYLE);
		Style := Style or (WS_CAPTION);
		Style := Style or (WS_THICKFRAME);
		SetWindowLong(Handle, GWL_STYLE, Style);
		if Active then
			WindowState := wsNormal;
		RestoreWindow;
	end;

	{ if LActive = False then
		SendToBack; }
	ResizeMessage;
end;

function SameRect(const R1, R2: TRect): BG;
begin
	Result := (R1.Left = R2.Left) and (R1.Right = R2.Right) and (R1.Top = R2.Top) and
		(R1.Bottom = R2.Bottom);
end;

procedure TDForm.SetFullScreen(Value: Boolean);
begin
	if FFullScreen <> Value then
	begin
		FFullScreen := Value;
		if not SameRect(Screen.MonitorFromWindow(Handle).WorkareaRect, Screen.MonitorFromWindow(Handle)
				.BoundsRect) then
			ShowTaskBar(not Value);
		Common(Value);
	end;
end;

procedure TDForm.SetChangeMode(Value: Boolean);
begin
	if FChangeMode <> Value then
	begin
		FChangeMode := Value;
		if FFullScreen then
		begin
			Common(Value);
		end;
	end;
end;

procedure TDForm.InitBackground(const Direct: BG);
begin
	if Assigned(FBitmapB) then
	begin
		case FBackground of
		baNone, baOpenGL:
			begin
				FBitmapB.SetSize(0, 0, clNone);
				Exit;
			end;
		end;
		if Direct or (FBitmapB.Width <> ClientWidth) or (FBitmapB.Height <> ClientHeight) or (Color <> FLastColor) then
		begin
			FBitmapB.SetSize(0, 0);
			FBitmapB.SetSize(ClientWidth, ClientHeight, Color);
			FBitmapB.ChangeRB := FBackground = baOpenGLBitmap;
			FLastColor := Color;

			if FBitmapB.Empty = False then
			begin
				case FBackground of
				baStandard:
					begin
						// FBitmapB.Bar(Color, ef16);
					end;
				baGradient:
					begin
						// {$ifopt d-}
						if GetBackgroundWindowTexture then
							FBitmapB.FormBitmap(Color);
						// {$endif}
					end;
				end;
			end;
		end;
	end;
end;

// Delphi <=5
type
	TFPUException = (exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision);
	TFPUExceptionMask = set of TFPUException;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
	CtlWord: U2;
begin
	CtlWord := Get8087CW;
	Set8087CW((CtlWord and $FFC0) or U1(Mask));
	U1(Result) := CtlWord and $3F;
end;

procedure TDForm.SetBackground(Value: TBackground);
begin
	if FBackground <> Value then
	begin
		case FBackground of
		baOpenGL, baOpenGLBitmap:
			begin
				// FreeOpenGL; Math
				glDeleteLists(FontBase, 256);
				DestroyRenderingContext(RC);
				RC := 0;
				SetExceptionMask([exDenormalized, exUnderflow .. exPrecision]);
			end;
		end;

		FBackground := Value;

		InitBackground(True);

		case FBackground of
		baOpenGL, baOpenGLBitmap:
			begin
				SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow,
					exPrecision]);
				if OneBuffer then
					RC := CreateRenderingContext(Canvas.Handle, [], 32, 0)
				else
					RC := CreateRenderingContext(Canvas.Handle, [opDoubleBuffered], 32, 0);
				// CreateOpenGL(Handle, Canvas);
				// SelectObject(Canvas.Handle, GetStockObject(ANSI_VAR_FONT));
				// create the bitmap display lists
				// we're making images of glyphs 0 thru 255
				// the display list numbering starts at 1000, an arbitrary choice

				ActivateRenderingContext(Canvas.Handle, RC); // make context drawable
				FontBase := glGenLists(256);
				SelectObject(Canvas.Handle, Canvas.Font.Handle { GetStockObject (SYSTEM_FONT) } );
				wglUseFontBitmaps(Canvas.Handle, 0, 255, FontBase);
				ResizeMessage;
				DeactivateRenderingContext; // make context undrawable
			end;
		end;

		Invalidate;
	end;
end;

procedure TDForm.CheckPos;
var
	Rect: TRect;
begin
	Rect := Screen.MonitorFromWindow(Handle).WorkareaRect;
	if Left + Width > Rect.Right then
		Left := Rect.Right - Width;
	if Top + Height > Rect.Bottom then
		Top := Rect.Bottom - Height;
	if Left < Rect.Left then
		Left := Rect.Left;
	if Top < Rect.Top then
		Top := Rect.Top;
end;

constructor TDForm.Create(AOwner: TComponent);
var
	FileName: TFileName;
begin
	DefaultMonitor := dmDesktop; // Replace dmActiveForm
	Position := poDesigned; // Replace poDefaultPosOnly
	// DoubleBuffered := True;

	inherited Create(AOwner);

	FLastColor := Color;
	
	CorrectFont(Font);
	CheckPos;

	HorzScrollBar.Tracking := True;
	VertScrollBar.Tracking := True;
	FBackground := baGradient;

	FBitmapB := TDBitmap.Create;
	FBitmapB.Canvas.Font := Font;
	FBitmapB.SetSize(0, 0, clNone);

	if FormStyle <> fsMDIChild then
	begin
		FileName := Name;
		if Length(FileName) > 0 then
		begin
			if FileName[1] = 'f' then
				Delete(FileName, 1, 1);
			if FileName = 'Main' then
				FileName := GetProjectInfo(piInternalName);
		end;

		FileName := GraphDir + FileName + '.ico';
		if FileExists(FileName) then
			Icon.LoadFromFile(FileName);
	end
	else
	begin
		// Icon := nil;
	end;

	if Assigned(FOnRWOptions) then
		FOnRWOptions(Self, False);

	Dictionary.TranslateForm(Self);
end;

destructor TDForm.Destroy;
begin
	if FFullScreen then
	begin
		if FChangeMode then
			RestoreStartMode;
		ShowTaskBar(True);
	end;
	FreeAndNil(FBitmapB);
	case FBackground of
	baOpenGL, baOpenGLBitmap:
		begin
			glDeleteLists(FontBase, 256);
			DestroyRenderingContext(RC);
			RC := 0;
			// FreeOpenGL;
		end;
	end;
	inherited Destroy;
end;

{ function TDForm.CloseQuery: Boolean;
	begin
	//procedure TDForm.CloseQuery(Sender: TObject; var CanClose: Boolean);
	if inherited CloseQuery then
	if Assigned(FOnRWOptions) then FOnRWOptions(Self, True);
	end; }

{ procedure TDForm.KeyDown(var Key: Word; Shift: TShiftState);
	begin
	if (Key = VK_RETURN) and (ssAlt in Shift) then
	FullScreen := not FullScreen
	else
	inherited KeyDown(Key, Shift);
	end; }

procedure TDForm.ResizeScene;
begin
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;
	if (ClientHeight > 0) and (ClientWidth > 0) then
		gluPerspective(60, ClientWidth / ClientHeight, 0.1, 100000.0);
	glViewport(0, 0, ClientWidth, ClientHeight);
end;

procedure TDForm.Paint;
begin
	if FBackground in [baUser, baOpenGLBitmap] then
		inherited; // FOnPaint Method
	case FBackground of
	baNone:
		begin

		end;
	baOpenGL, baOpenGLBitmap:
		begin
			ActivateRenderingContext(Canvas.Handle, RC);
		end
	else
	begin
		InitBackground(False);
		BitBlt(Canvas.Handle, 0, 0, FBitmapB.Width, FBitmapB.Height, FBitmapB.Canvas.Handle, 0, 0,
			SRCCOPY);
	end;
	end;

	if FBackground = baOpenGLBitmap then
	begin
		glClear(GL_DEPTH_BUFFER_BIT); // TNT2 Error read at 0
		{ glClear(GL_CURRENT_BIT);
			glClear(GL_TRANSFORM_BIT);
			glClear(GL_ALL_ATTRIB_BITS); }
		{ glDisable(GL_LIGHT0);
			glDisable(GL_LIGHT1);
			glDisable(GL_LIGHTING);
			glDisable(GL_COLOR_MATERIAL);
			glDisable(GL_NORMALIZE);
			glDisable(GL_POINT_SMOOTH);
			glDisable(GL_POINT_SIZE); }

		// glDisable($ffff);
		// glDrawPixels(16, 16, GL_RGB, GL_UNSIGNED_BYTE, FBitmapB.GLData);

		(*
			* Disable stuff that's likely to slow down
			* glDrawPixels.(Omit as much of this as possible,
			* when you know in advance that the OpenGL state is
			* already set correctly.)
			*)
		glDisable(GL_ALPHA_TEST);
		glDisable(GL_BLEND);
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_DITHER);
		glDisable(GL_FOG);
		glDisable(GL_LIGHTING);
		glDisable(GL_LOGIC_OP);
		glDisable(GL_STENCIL_TEST);
		glDisable(GL_TEXTURE_1D);
		glDisable(GL_TEXTURE_2D);
		glPixelTransferi(GL_MAP_COLOR, GL_FALSE);
		glPixelTransferi(GL_RED_SCALE, 1);
		glPixelTransferi(GL_RED_BIAS, 0);
		glPixelTransferi(GL_GREEN_SCALE, 1);
		glPixelTransferi(GL_GREEN_BIAS, 0);
		glPixelTransferi(GL_BLUE_SCALE, 1);
		glPixelTransferi(GL_BLUE_BIAS, 0);
		glPixelTransferi(GL_ALPHA_SCALE, 1);
		glPixelTransferi(GL_ALPHA_BIAS, 0);

		(*
			* Disable extensions that could slow down
			* glDrawPixels.(Actually, you should check for the
			* presence of the proper extension before making
			* these calls.I omitted that code for simplicity.)
			*)

		glDisable(GL_CONVOLUTION_1D_EXT);
		glDisable(GL_CONVOLUTION_2D_EXT);
		glDisable(GL_SEPARABLE_2D_EXT);

		glDisable(GL_HISTOGRAM_EXT);
		glDisable(GL_MINMAX_EXT);

		glDisable(GL_TEXTURE_3D_EXT);

		(*
			* The following is needed only when using a
			* multisample-capable visual.
			*)

		// glDisable(GL_MULTISAMPLE_SGIS);

		glDrawPixels(FBitmapB.Width, FBitmapB.Height, GL_FORMAT, GL_UNSIGNED_BYTE, FBitmapB.GLData);
	end;

	if not(FBackground in [baUser, baOpenGLBitmap]) then
		inherited Paint; // FOnPaint Method

	case FBackground of
	baOpenGL, baOpenGLBitmap:
		begin
			if OneBuffer then
				glFlush
			else
				SwapBuffers(Canvas.Handle);
			DeactivateRenderingContext; // make context drawable
		end;
	end;
end;

procedure TDForm.WMEraseBkgnd;
begin
	Message.Result := 1;
end;

procedure TDForm.ResizeMessage;
var
	Message: TWMSize;
begin
	Message.Msg := WM_SIZE;
	if FFullScreen then
		Message.SizeType := SIZEFULLSCREEN
	else
		Message.SizeType := SIZENORMAL;
	Message.Width := Width;
	Message.Height := Height;
	Message.Result := 0;
	WMSize(Message);
end;

procedure TDForm.WMSize(var Message: TWMSize);
begin
	if (Visible = False) or (Message.Width = 0) or (Message.Height = 0) then
		Exit;

	case FBackground of
	baOpenGL, baOpenGLBitmap:
		begin
			ActivateRenderingContext(Canvas.Handle, RC);
		end;
	end;
	InitBackground(False);

	inherited; // FOnResize Method

	case FBackground of
	baOpenGL, baOpenGLBitmap:
		begin
			DeactivateRenderingContext; // make context drawable
		end;
	end;
	Invalidate; // Required when new size is smaller
end;

procedure TDForm.WMShow(var Message: TWMShowWindow);
begin
	if (Message.Show) and (Message.Status = 0) then
	begin
		CheckPos;
		ResizeMessage;
		// InitBackground(False);
	end;
	inherited;
end;

procedure TDForm.WMSysColorChange;
begin
	if not(FBackground in [baUser, baOpenGLBitmap]) then
	begin
		InitBackground(True);
	end;
end;

procedure TDForm.RestoreWindow;
begin
	if FStoreWindow then
	begin
		// SetWindowPlacement(Handle, @FWindowPlacement);
		{ SetWindowPos(Handle, 0,
			FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Top,
			FWindowPlacement.rcNormalPosition.Right - FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Bottom - FWindowPlacement.rcNormalPosition.Top,
			SWP_NOZORDER + SWP_NOACTIVATE); }
		SetWindowLong(Handle, GWL_STYLE, FWindowLong);
		SetWindowPlacement(Handle, @FWindowPlacement);
		{ SetBounds(
			FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Top,
			FWindowPlacement.rcNormalPosition.Right - FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Bottom - FWindowPlacement.rcNormalPosition.Top); TaskBar problem }
		FStoreWindow := False;
		// BringToFront;
	end;
end;

procedure TDForm.StoreWindow;
begin
	FWindowLong := GetWindowLong(Handle, GWL_STYLE);
	FWindowLong := FWindowLong or WS_VISIBLE;
	FWindowPlacement.Length := SizeOf(FWindowPlacement);
	FStoreWindow := GetWindowPlacement(Handle, @FWindowPlacement);
	// FStoreWindow := True;
end;

procedure TDForm.SetCaption(Value: string);
begin
	if FCaption <> Value then
	begin
		FCaption := Value;
		inherited Caption := Value; // + ' (' + NToS(ClientWidth) + ' ' + CharTimes + ' ' + NToS(ClientHeight) + ')';
	end;
end;

procedure Register;
begin
	RegisterComponents(ComponentPageName, [TDForm]);
end;

procedure TDForm.Center;
var
	P: TPoint;
begin
	P := CenterPoint;
	SetBounds(P.X, P.Y, Width, Height);
end;

function TDForm.CenterPoint: TPoint;
var
	CenterForm: TForm;
	Rect: TRect;
begin
	CenterForm := ActiveForm;
	if Assigned(CenterForm) and (CenterForm <> Self) then
	begin
		Result.X := ((CenterForm.Width - Width) div 2) + CenterForm.Left;
		Result.Y := ((CenterForm.Height - Height) div 2) + CenterForm.Top;
	end
	else
	begin
		Rect := Screen.MonitorFromWindow(Handle).WorkareaRect;
		Result.X := Rect.Left + (Rect.Right - Rect.Left - Width) div 2;
		Result.Y := Rect.Top + (Rect.Bottom - Rect.Top - Height) div 2;
	end;
end;


procedure TDForm.ChangeVisible;
begin
	SetVisible(not Visible);
end;

procedure TDForm.SetVisible(const Value: Boolean);
begin
	// if FormStyle = fsMDIChild then
	begin
		if Value then
		begin
			Self.FormStyle := fsMDIChild;
			Show;
		end
		else
		begin
			// Close;
			Self.FormStyle := fsNormal;
			Hide;
			// FreeAndNil(Self);
		end;
	end
	{ else
		begin
		if Value then
		Show
		else
		Hide;
		end; }
end;

function Get8087CW: U2;
asm
	PUSH 0
	FNSTCW [ESP].U2
{$ifdef CPUX64}
	POP RAX
{$else}
	POP EAX
{$endif}
end
;

procedure ActivateForm(Form: TForm);
begin
	if Form.Visible = False then
		Form.Show
	else
	begin
//		Form.SendToBack;
			Form.BringToFront;
//{			ShowWindow(Handle, SW_SHOW); // SW_NORMAL, SW_RESTORE break windows stay on top!
			SetForegroundWindow(Form.Handle); // Blink Taskbar
	end;
end;

procedure TDForm.AlignControlRight(const AControl: TControl);
begin
	AControl.Width := ClientWidth - AControl.Left - FormBorder;
end;

procedure TDForm.AlignControlBottom(const AControl: TControl; const KeepSize: BG);
begin
	if KeepSize then
		AControl.Top := ClientHeight - AControl.Height - FormBorder
	else
		AControl.Height := ClientHeight - AControl.Top - FormBorder;
end;

procedure TDForm.AlignControlRightTop(const AControl: TControl);
begin
	AlignControlRight(AControl);
end;

procedure TDForm.AlignControl(const AControl: TControl;
	const AAnchors: TAnchors);
{var
	Anchors: TAnchors;
	FOriginalParentSize: TPoint;
	FAnchorRules: TPoint;}
begin
(* TODO :	if not (csLoading in ComponentState) then
	begin
		Anchors := AAnchors;
		if Anchors = [akLeft, akTop] then
		begin
			FOriginalParentSize.X := 0;
			FOriginalParentSize.Y := 0;
			Exit;
		end;
		if akRight in Anchors then
			if akLeft in Anchors then
				FAnchorRules.X := Width else
				FAnchorRules.X := Left
		else
			FAnchorRules.X := Left + Width div 2;
		if akBottom in Anchors then
			if akTop in Anchors then
				FAnchorRules.Y := Height else
				FAnchorRules.Y := Top
		else
			FAnchorRules.Y := Top + Height div 2;
		if Parent <> nil then
			if csReading in Parent.ComponentState then
			begin
				if not (csDesigning in ComponentState) then
					FOriginalParentSize := Parent.DesignSize
			end
			else if Parent.HandleAllocated then
				FOriginalParentSize := Parent.ClientRect.BottomRight
			else
			begin
				FOriginalParentSize.X := Parent.Width;
				FOriginalParentSize.Y := Parent.Height;
			end;
	end; *)
end;

procedure TDForm.CreateParams(var Params: TCreateParams);
begin
	inherited;
	if not (csDesigning in ComponentState) then
		Color := GetBackgroundWindowColor;
	
//	Params.Style := Params.Style and not WS_CLIPCHILDREN;
end;

end.
