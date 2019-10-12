unit uDForm;

interface

uses
	uTypes,
  uDBitmap,
  UITypes,
  Vcl.Graphics,
	Winapi.Windows, Winapi.Messages, Classes, Vcl.Controls, Vcl.Forms;

type
	TBackground = (baNone, baUser, baStandard, baGradient, baOpenGL, baOpenGLBitmap);

	TRWOptionsEvent = procedure(Sender: TObject; Save: Boolean) of object;

	TDForm = class(TForm)
	private
		FCaption: string;
		FStoreWindow: Boolean;
		FWindowPlacement: TWindowPlacement;
		FWindowLong: S4;

		FBitmapB: TDBitmap;
		FLastColor: TColor;

		FBackground: TBackground;
		FFullScreen: Boolean;

		FOnRWOptions: TRWOptionsEvent;
    FFontBase: U4;
    FRC: HGLRC;

		procedure ResizeMessage;
		procedure CheckPos;
		procedure InternalSetFullScreen(Value: Boolean);
		procedure SetFullScreen(Value: Boolean);
		procedure InitBackground(const Direct: BG);
		procedure SetBackground(Value: TBackground);
		procedure SetCaption(Value: string);

		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMShow(var Message: TWMShowWindow); message WM_SHOWWINDOW;
		procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		procedure CreateParams(var Params: TCreateParams); override;

		procedure RestoreWindow;
		procedure StoreWindow;

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

		property RC: HGLRC read FRC;
		property FontBase: U4 read FFontBase;
	published
		property Caption: string read FCaption write SetCaption;
		property BackBitmap: TDBitmap read FBitmapB;
		property Background: TBackground read FBackground write SetBackground default baGradient;
		property FullScreen: Boolean read FFullScreen write SetFullScreen default False;

		property OnRWOptions: TRWOptionsEvent read FOnRWOptions write FOnRWOptions;
	end;

procedure FormFree(var Form: TDForm); overload; // Close and Free Form
procedure FormFree(var Form: TForm); overload; // Close and Free Form
function FormDraw(const Form: TForm): BG; // Is form visible
procedure ActivateForm(Form: TForm);

procedure glShadowText(Canvas: TCanvas; const X, Y: Integer; const Text: AnsiString;
	const CF, CB: TColor; const Shadow: SG);
procedure glTextOut(Canvas: TCanvas; const X, Y: Integer; const Text: string; const C: TColor);

// Logical size to real pixels (depends on current DPI)
const
  DefaultDPI = 96;

function LgToPx(const Value: SG): SG; overload;
function LgToPx(const Value: SG; const OriginalDPI: SG): SG; overload;

var
	FormBorder: SG = 8;

function ActiveForm: TForm;
procedure SetControlEnabled(Component: TComponent; E: BG);

implementation

uses
  SysUtils,
  uMath,
  uDictionary,
  uVCLDictionary,
	uFiles,
  OpenGL12,
  uColor,
  uProjectInfo,
  uDWinControl,
  uOperatingSystem,
  uGlobalOptions;

const
	OneBufferForOpenGL = False;

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

procedure ActivateForm(Form: TForm);
begin
	if Form.Visible = False then
		Form.Show
	else
	begin
		Form.BringToFront;
		SetForegroundWindow(Form.Handle); // Blink Taskbar
	end;
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

function FormDraw(const Form: TForm): BG;
begin
	Result := False;
	if (not Assigned(Form)) or (Form.Visible = False) then
		Exit;

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
	glShadowText(Canvas, X, Y, AnsiString(Text), C, clNone, 0);
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
	Params: array [0 .. 3] of S4;
	C: TRGBA;
	sx, sy, wx, wy: Single;
	// px: array[0..3] of Double;
begin
	if Text = '' then
		Exit;

	glGetIntegerv(GL_VIEWPORT, PGLInt(@Params[0]));

	if (Params[2] = 0) or (Params[3] = 0) then
		Exit;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

	if CB <> clNone then
	begin
		sx := 2 * (X - 1) / Params[2] - 1;
		sy := -2 * (Y + 1 + Canvas.TextHeight(string(Text))) / Params[3] + 1;
		wx := 2 * (Canvas.TextWidth(string(Text)) + 2) / Params[2];
		wy := 2 * (Canvas.TextHeight(string(Text)) + 2) / Params[3];
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

{ TDForm }

procedure TDForm.InternalSetFullScreen(Value: Boolean);
var
	Style: S4;
	Rect: TRect;
begin
	if Value then
	begin
		Style := GetWindowLong(Handle, GWL_STYLE);
		Style := Style and (not WS_CAPTION) and (not WS_THICKFRAME);
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
		Style := GetWindowLong(Handle, GWL_STYLE);
		Style := Style or WS_CAPTION or WS_THICKFRAME;
		SetWindowLong(Handle, GWL_STYLE, Style);
		if Active then
			WindowState := wsNormal;
	end;

	ResizeMessage;
end;

procedure TDForm.SetFullScreen(Value: Boolean);
begin
	if FFullScreen <> Value then
	begin
		FFullScreen := Value;
		InternalSetFullScreen(Value);
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
						if GetBackgroundWindowTexture then
							FBitmapB.FormBitmap(Color);
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
var
  RCOptions: TRCOptions;
  Palette: HPALETTE;
begin
	if FBackground <> Value then
	begin
		case FBackground of
		baOpenGL, baOpenGLBitmap:
			begin
				// FreeOpenGL; Math
				glDeleteLists(FFontBase, 256);
				DestroyRenderingContext(FRC);
				FRC := 0;
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
				if OneBufferForOpenGL then
          RCOptions := []
				else
          RCOptions := [opDoubleBuffered];
				FRC := CreateRenderingContext(Canvas.Handle, RCOptions, 32, 0, 0, 0, 0, 0, Palette);
				// CreateOpenGL(Handle, Canvas);
				// SelectObject(Canvas.Handle, GetStockObject(ANSI_VAR_FONT));
				// create the bitmap display lists
				// we're making images of glyphs 0 thru 255
				// the display list numbering starts at 1000, an arbitrary choice

				ActivateRenderingContext(Canvas.Handle, RC); // make context drawable
				FFontBase := glGenLists(256);
				SelectObject(Canvas.Handle, Canvas.Font.Handle { GetStockObject (SYSTEM_FONT) } );
				wglUseFontBitmaps(Canvas.Handle, 0, 255, FFontBase);
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

	TVCLDictionary(Dictionary).TranslateForm(Self);
end;

destructor TDForm.Destroy;
begin
	FreeAndNil(FBitmapB);
	case FBackground of
	baOpenGL, baOpenGLBitmap:
		begin
			glDeleteLists(FFontBase, 256);
			DestroyRenderingContext(RC);
			FRC := 0;
			// FreeOpenGL;
		end;
	end;
	inherited Destroy;
end;

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
			if OneBufferForOpenGL then
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
end;

procedure TDForm.SetCaption(Value: string);
begin
	if FCaption <> Value then
	begin
		FCaption := Value;
		inherited Caption := Value;
	end;
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
  if Value then
  begin
    Self.FormStyle := fsMDIChild;
    Show;
  end
  else
  begin
    Self.FormStyle := fsNormal;
    Hide;
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
  begin
		Color := GetBackgroundWindowColor;
    if OperatingSystem.IsAero then
    begin
      if BorderStyle = bsSizeToolWin then
        BorderStyle := bsSizeable
      else if BorderStyle = bsToolWindow then
        BorderStyle := bsSingle;
    end;
  end;
//	Params.Style := Params.Style and not WS_CLIPCHILDREN;
end;

function LgToPx(const Value: SG): SG; overload;
begin
  if Screen.PixelsPerInch = DefaultDPI then
    Result := Value
  else
    Result := RoundDiv(Value * Screen.PixelsPerInch, DefaultDPI);
end;

function LgToPx(const Value: SG; const OriginalDPI: SG): SG; overload;
begin
  if Screen.PixelsPerInch = OriginalDPI then
    Result := Value
  else
    Result := RoundDiv(Value * Screen.PixelsPerInch, OriginalDPI);
end;

initialization
{$IFNDEF NoInitialization}
  FormBorder := LgToPx(FormBorder);
{$ENDIF NoInitialization}
end.
