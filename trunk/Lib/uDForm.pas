//* File:     Lib\uDForm.pas
//* Created:  2001-12-01
//* Modified: 2005-12-05
//* Version:  X.X.35.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDForm;

interface

{$R *.RES}
uses
	uTypes, uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
	TBackground = (baNone, baUser, baStandard, baGradientOnly, baGradient,
		baBitmap, baOpenGL, baOpenGLBitmap);

	TRWOptionsEvent = procedure(Sender: TObject; Save: Boolean) of object;

	TDForm = class(TForm)
	private
		{ Private declarations }
		FCaption: string;
		FStoreWindow: Boolean;
		FWindowPlacement: TWindowPlacement;
		FWindowLong: S4;

		FBitmapB: TDBitmap;

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

		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMShow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
		procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;
	protected
		{ Protected declarations }
	public
		{ Public declarations }
		RC: HGLRC;
		FontBase: U4;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
//		function CloseQuery: Boolean; override;

		procedure RestoreWindow;
		procedure StoreWindow;

//		procedure KeyDown(var Key: U2; Shift: TShiftState); override;

		procedure Paint; override;
		procedure ResizeScene;
	published
		{ published declarations }
		property Caption: string read FCaption write SetCaption;
		property BackBitmap: TDBitmap read FBitmapB;
		property Background: TBackground read FBackground write SetBackground default baNone;
		property FullScreen: Boolean read FFullScreen write SetFullScreen default False;
		property ChangeMode: Boolean read FChangeMode write SetChangeMode default False;

		property OnRWOptions: TRWOptionsEvent read FOnRWOptions write FOnRWOptions;
//		property OnMouseMove;
	end;

procedure FormFree(var Form: TForm);
function FormDraw(Form: TForm): BG;

procedure glShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor; Shadow: SG);
procedure glTextOut(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const C: TColor);
procedure ShowTaskBar(Visible: Boolean);
function GetScreen(var Rect: TRect): SG{Up, Down, Left, Right};

procedure Register;

const
	FormBorder = 8;
var
	DesktopHWnd: HWnd;
	DesktopDC: HDC;

procedure SetControlEnabled(Component: TComponent; E: BG);
function GetDesktop: BG;
procedure ReleaseDesktop;

implementation

uses
	Math,
	uGraph, uFiles, OpenGL12, uScreen, uSysInfo, uFormat, uStrings;
const
	OneBuffer = False;
var
	FBitmapF: TDBitmap;

procedure SetControlEnabled(Component: TComponent; E: BG);
var i: SG;
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
		DesktopHWnd := 0; //GetDesktopWindow;
		if DesktopHWnd <> INVALID_HANDLE_VALUE then
		begin
			DesktopDC := GetDC(DesktopHWnd);
			if DesktopDC <> 0 then Result := True;
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

function FormDraw(Form: TForm): BG;
begin
	Result := False;
	if not Assigned(Form) then Exit;
	if Form.Visible = False then Exit;
	if Form.WindowState = wsMinimized then Exit; // D??? DNW
//	Style := GetWindowLong(Handle, GWL_STYLE);
	Result := True;
end;

procedure FormFree(var Form: TForm);
begin
	if Assigned(Form) then
	begin
		Form.Close; // Free does not call Close and CloseQuery events
		FreeAndNil(Form);
	end;
end;

procedure glTextOut(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const C: TColor);
var
	Params: array[0..3] of SG;
begin
	glGetIntegerv(GL_VIEWPORT, @Params[0]);

	if (Params[2] = 0) or (Params[3] = 0) then Exit;
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

	glColor3ubv(PGLUByte(@C));
	glRasterPos2d(2 * X / Params[2] - 1, -2 * (Y + 11) / Params[3] + 1);
	glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1])));
end;

procedure glShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor; Shadow: SG);
var
	Params: array[0..3] of SG;
	C: TRGBA;
	sx, sy, wx, wy: Single;
//	px: array[0..3] of Double;
begin
	glGetIntegerv(GL_VIEWPORT, @Params[0]);

	if (Params[2] = 0) or (Params[3] = 0) then Exit;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

	if CB <> clNone then
	begin
		sx := 2 * (X + 1) / Params[2] - 1;
		sy := -2 * (Y + 1 + Canvas.TextHeight(Text)) / Params[3] + 1;
		wx := 2 * (Canvas.TextWidth(Text) + 1) / Params[2];
		wy := 2 * (Canvas.TextHeight(Text) + 1) / Params[3];
		C.L := CB;
		C.A := $ff;

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glColor4ubv(PGLUByte(@C));
		glBegin(GL_QUADS);
			glVertex3f(sx, sy, 1);
			glVertex3f(sx + wx, sy, 1);
			glVertex3f(sx + wx, sy + wy, 0.5);
			glVertex3f(sx, sy + wy , 0.5);
		glEnd;
		glDisable(GL_BLEND);
	end;

(*	C.L := MixColors(CF, CB);
	glColor3ubv(PGLUByte(@C));
	glRasterPos3d(2 * (X + 1) / Params[2] - 1, -2 * (Y + 1 + 11) / Params[3] + 1, 1); // Open GL FP Exception
{	glGetDoublev(GL_CURRENT_RASTER_POSITION, @Px[0]);
	glTexCoord4d(1, 1, 1, 1);
	glRasterPos4d(68, 0, 0, 1);
	glBitmap(0, 0, 0, 0, 0, 0, nil);}
	glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1]))); *)


	C.L := CF;
	glColor3ubv(PGLUByte(@C));
	glRasterPos3d(2 * X / Params[2] - 1, -2 * (Y + 11) / Params[3] + 1, 0); // Open GL FP Exception
	glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1])));

end;

procedure ShowTaskBar(Visible: Boolean);
var hTaskBar: HWND;
begin
	hTaskBar := FindWindow('Shell_TrayWnd', nil);
	if Visible then
		ShowWindow(hTaskBar, SW_SHOWNA)
	else
		ShowWindow(hTaskBar, SW_HIDE);
end;

function GetScreen(var Rect: TRect): SG;
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
		Rect.Top := RectT.Bottom; // Up
		Result := 0;
	end
	else if (RectT.Left <= 0) and (RectT.Right >= w) and (RectT.Bottom >= h) then
	begin
		Rect.Bottom := RectT.Top; // Down
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
{
procedure TDForm.AfterCreate;
begin
	if Parent.WindowState = wsMDIForm then
	begin
		Form.Style := fsMDIChild;
	end;
end;}

procedure TDForm.Common(Value: Boolean);
var
	Style: S4;
begin
	if Value then
	begin
		StoreWindow;

		if FChangeMode then
		begin
			ReadScreenModes;
			SetScreenMode(640, 480, 32, 0, False, False, False, False, True);
		end;
		Style := GetWindowLong(Handle, GWL_STYLE);
		Style := Style and not WS_CAPTION;
		Style := Style and not WS_THICKFRAME;
		SetWindowLong(Handle, GWL_STYLE, Style);
		WindowState := wsMaximized;
		SetBounds(0, 0, Screen.Width, Screen.Height); // -> PopupMenu is visibled
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
		WindowState := wsNormal;
		RestoreWindow;
	end;
	ResizeMessage;
end;

procedure TDForm.SetFullScreen(Value: Boolean);
begin
	if FFullScreen <> Value then
	begin
		FFullScreen := Value;
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
			FBitmapB.SetSize(0, 0);
			Exit;
		end;
		end;
		if Direct or (FBitmapB.Width <> ClientWidth) or (FBitmapB.Height <> ClientHeight) then
		begin
			FBitmapB.SetSize(ClientWidth, ClientHeight);
			FBitmapB.ChangeRB := FBackground = baOpenGLBitmap;

			if FBitmapB.Empty = False then
			begin
				case FBackground of
				baStandard:
				begin
					FBitmapB.Bar(Color, ef16);
				end;
				baGradient:
				begin
					FBitmapB.FormBitmap(Color);
					if FBitmapF <> nil then
						FBitmapB.Texture(FBitmapF, ef04);
				end;
				baGradientOnly:
				begin
					FBitmapB.FormBitmap(Color);
				end;
				baBitmap:
				begin
					if FBitmapF <> nil then
						FBitmapB.Texture(FBitmapF, ef16);
				end;
				end;
			end;
		end;
	end;
end;

// Delphi <=5
type
	TFPUException = (exInvalidOp, exDenormalized, exZeroDivide,
									 exOverflow, exUnderflow, exPrecision);
	TFPUExceptionMask = set of TFPUException;

function Get8087CW: U2;
asm
	PUSH 0
	FNSTCW [ESP].U2
	POP EAX
end;

function SetExceptionMask(const Mask: TFPUExceptionMask): TFPUExceptionMask;
var
	CtlWord: U2;
begin
	CtlWord := Get8087CW;
	Set8087CW( (CtlWord and $FFC0) or U1(Mask) );
	U1(Result) := CtlWord and $3F;
end;

procedure TDForm.SetBackground(Value: TBackground);
var
	FileName: TFileName;
begin
	if FBackground <> Value then
	begin
		case FBackground of
		baOpenGL, baOpenGLBitmap:
		begin
//			FreeOpenGL; Math
			glDeleteLists(FontBase, 256);
			DestroyRenderingContext(RC); RC := 0;
			SetExceptionMask([exDenormalized, exUnderflow..exPrecision]);
		end;
		end;

		FBackground := Value;

		case FBackground of
		baBitmap, baGradient:
		begin
			if FBitmapF = nil then
			begin
				FBitmapF := TDBitmap.Create;
				FBitmapF.SetSize(0, 0);
				FileName := GraphDir + 'Form.png';
				if FileExists(FileName) then
					FBitmapF.LoadFromFile(FileName)
				else
				begin
					FileName := GraphDir + 'Form.jpg';
					if FileExists(FileName) then
						FBitmapF.LoadFromFile(FileName);
				end;
			end;
		end;
		end;

		InitBackground(True);

		case FBackground of
		baOpenGL, baOpenGLBitmap:
		begin
			SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
			if OneBuffer then
				RC:=CreateRenderingContext(Canvas.Handle, [], 32, 0)
			else
				RC:=CreateRenderingContext(Canvas.Handle, [opDoubleBuffered], 32, 0);
//			CreateOpenGL(Handle, Canvas);
			SelectObject(Canvas.Handle, GetStockObject(ANSI_VAR_FONT));
				// create the bitmap display lists
				// we're making images of glyphs 0 thru 255
				// the display list numbering starts at 1000, an arbitrary choice

			ActivateRenderingContext(Canvas.Handle,RC); // make context drawable
			FontBase := glGenLists(256);
			wglUseFontBitmaps(Canvas.Handle, 0, 255, FontBase);
			DeactivateRenderingContext; // make context drawable
			ResizeMessage;
		end;
		end;

		Invalidate;
	end;
end;

procedure TDForm.CheckPos;
var Rect: TRect;
begin
	GetScreen(Rect);
	if Left + Width > Rect.Right - Rect.Left then Left := Rect.Right - Rect.Left - Width;
	if Top + Height > Rect.Bottom - Rect.Top then Top := Rect.Bottom - Rect.Top - Height;
	if Left < Rect.Left then Left := Rect.Left;
	if Top < Rect.Top then Top := Rect.Top;
end;

constructor TDForm.Create(AOwner: TComponent);
var
	FileName: TFileName;
begin
	inherited Create(AOwner);

	CheckPos;

	HorzScrollBar.Tracking := True;
	VertScrollBar.Tracking := True;
	FBackground := baNone;

	FBitmapB := TDBitmap.Create;
	FBitmapB.SetSize(0, 0);

	FileName := Name;
	if FileName[1] = 'f' then Delete(FileName, 1, 1);
	if FileName = 'Main' then FileName := Application.Title;

	FileName := GraphDir + FileName + '.ico';
	if FileExists(FileName) then
		Icon.LoadFromFile(FileName);

	if Assigned(FOnRWOptions) then FOnRWOptions(Self, False);
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
		DestroyRenderingContext(RC); RC := 0;
//		FreeOpenGL;
	end;
	end;
	inherited Destroy;
end;

{function TDForm.CloseQuery: Boolean;
begin
//procedure TDForm.CloseQuery(Sender: TObject; var CanClose: Boolean);
	if inherited CloseQuery then
		if Assigned(FOnRWOptions) then FOnRWOptions(Self, True);
end;}

{procedure TDForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = VK_RETURN) and (ssAlt in Shift) then
		FullScreen := not FullScreen
	else
		inherited KeyDown(Key, Shift);
end;}

procedure TDForm.ResizeScene;
begin
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity;
		if (ClientHeight > 0) and (Screen.Height > 0) then
			gluPerspective(60 * ClientHeight / Screen.Height,
				ClientWidth / ClientHeight,
				0.1,
				100000.0);
		glViewport(0, 0, ClientWidth, ClientHeight);
end;

procedure TDForm.Paint;
begin
	if FBackground = baUser then
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
		BitBlt(Canvas.Handle, 0, 0, FBitmapB.Width, FBitmapB.Height,
			FBitmapB.Canvas.Handle,
			0, 0,
			SRCCOPY);
	end;
	end;

	if FBackground = baOpenGLBitmap then
	begin
		glClear(GL_DEPTH_BUFFER_BIT); // TNT2 Error read at 0
{		glClear(GL_CURRENT_BIT);
		glClear(GL_TRANSFORM_BIT);
		glClear(GL_ALL_ATTRIB_BITS);}
{	glDisable(GL_LIGHT0);
	glDisable(GL_LIGHT1);
	glDisable(GL_LIGHTING);
	glDisable(GL_COLOR_MATERIAL);
	glDisable(GL_NORMALIZE);
	glDisable(GL_POINT_SMOOTH);
	glDisable(GL_POINT_SIZE);}

//		glDisable($ffff);
//		glDrawPixels(16, 16, GL_RGB, GL_UNSIGNED_BYTE, FBitmapB.GLData);


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

//				glDisable(GL_MULTISAMPLE_SGIS);

		glDrawPixels(FBitmapB.Width, FBitmapB.Height, GL_FORMAT, GL_UNSIGNED_BYTE, FBitmapB.GLData);
	end;

	if FBackground <> baUser then
		inherited; // FOnPaint Method

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

procedure TDForm.WMEraseBkGnd;
begin
	Message.Result := 1;
end;

procedure TDForm.ResizeMessage;
var
	Message: TWMSize;
begin
	Message.Msg := WM_SIZE;
	Message.SizeType := 0;
	Message.Width := Width;
	Message.Height := Height;
	Message.Result := 0;
	WMSize(Message);
end;

procedure TDForm.WMSize(var Message: TWMSize);
begin
	if (Visible = False) or (Message.Width = 0) or (Message.Height = 0) then Exit;

	case FBackground of
	baOpenGL, baOpenGLBitmap:
	begin
		ActivateRenderingContext(Canvas.Handle,RC);
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
		InitBackground(False);
	end;
	inherited;
end;

procedure TDForm.WMSysColorChange;
begin
	if not (FBackground in [baUser, baOpenGLBitmap]) then
	begin
		InitBackground(True);
	end;
end;

procedure TDForm.RestoreWindow;
begin
	if FStoreWindow then
	begin
//		SetWindowPlacement(Handle, @FWindowPlacement);
{		SetWindowPos(Handle, 0,
			FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Top,
			FWindowPlacement.rcNormalPosition.Right - FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Bottom - FWindowPlacement.rcNormalPosition.Top,
			SWP_NOZORDER + SWP_NOACTIVATE);}
		SetWindowLong(Handle, GWL_STYLE, FWindowLong);
		SetWindowPlacement(Handle, @FWindowPlacement);
{		SetBounds(
			FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Top,
			FWindowPlacement.rcNormalPosition.Right - FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Bottom - FWindowPlacement.rcNormalPosition.Top); TaskBar problem }
		FStoreWindow := False;
		BringToFront;
	end;
end;

procedure TDForm.StoreWindow;
begin
	FWindowLong := GetWindowLong(Handle, GWL_STYLE);
	FWindowLong := FWindowLong or WS_VISIBLE;
	FWindowPlacement.Length := SizeOf(FWindowPlacement);
	FStoreWindow := GetWindowPlacement(Handle, @FWindowPlacement);
//	FStoreWindow := True;
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
	RegisterComponents('DComp', [TDForm]);
end;

Initialization

Finalization
	FreeAndNil(FBitmapF);
end.
