//* File:     Lib\uDForm.pas
//* Created:  2001-12-01
//* Modified: 2003-10-12
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDForm;

interface

{$R *.RES}
uses
	uAdd, uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
	TBackground = (baNone, baUser, baStandard, baGradientOnly, baGradient,
		baBitmap, baOpenGL, baOpenGLBitmap);

	TRWOptionsEvent = procedure(Sender: TObject; Save: Boolean) of object;

	TDForm = class(TForm)
	private
		{ Private declarations }
		FStoreWindow: Boolean;
		FWindowPlacement: TWindowPlacement;
		FWindowLong: LongInt;
//		ALeft, ATop, AWidth, AHeight: LongInt;

		FBitmapB: TDBitmap;

//		Image: TImage;
		FBackground: TBackground;
		FFullScreen: Boolean;
		FChangeMode: Boolean;

		FOnRWOptions: TRWOptionsEvent;

		procedure InitRect;
		procedure CheckPos;
		procedure Common(Value: Boolean);
		procedure SetFullScreen(Value: Boolean);
		procedure InitBackground;
		procedure SetBackground(Value: TBackground);
		procedure SetChangeMode(Value: Boolean);
//		procedure WMPaint(var Message: TWMPaint); //message WM_PAINT;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMShow(var Message: TWMShowWindow); message WM_SHOWWINDOW;

{		procedure WMHScroll(var Message: TWMScroll); message WM_HSCROLL;
		procedure WMVScroll(var Message: TWMScroll); message WM_VSCROLL;}

		procedure WMSysColorChange(var Message: TWMSysColorChange); message WM_SYSCOLORCHANGE;

	protected
		{ Protected declarations }
	public
		{ Public declarations }
		RC: HGLRC;
		FontBase: LongWord;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
//		function CloseQuery: Boolean; override;

		procedure RestoreWindow;
		procedure StoreWindow;

		procedure KeyDown(var Key: Word; Shift: TShiftState); override;

		procedure Fill;
		procedure ResizeScene;
		procedure Paint; override;
		procedure ResizeMessage;
	published
		{ published declarations }
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
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
procedure glTextOut(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const C: TColor);
procedure ShowTaskBar(Visible: Boolean);

procedure Register;

const
	FormBorder = 8;
var
	DesktopHWnd: HWnd;
	DesktopDC: HDC;

function GetDesktop: BG;
procedure ReleaseDesktop;

implementation

uses
	uGraph, uFiles, OpenGL12, uScreen, uSysInfo;
const
	OneBuffer = False;
var
	FBitmapF: TDBitmap;

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
		DesktopHWnd := GetDesktopWindow;
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
	if Form.WindowState = wsMinimized then Exit; // D??? Not Work
//	Style := GetWindowLong(Handle, GWL_STYLE);
	Result := True;
end;

procedure FormFree(var Form: TForm);
begin
	if Assigned(Form) then
	begin
		Form.Close;
		Form.Free;
		Form := nil;
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
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
var
	Params: array[0..3] of SG;
	C: TRColor;
	sx, sy, wx, wy: Single;
begin
	glGetIntegerv(GL_VIEWPORT, @Params[0]);

	if (Params[2] = 0) or (Params[3] = 0) then Exit;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity;

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity;

	C.L := CF;
	glColor3ubv(PGLUByte(@C));
	glRasterPos2d(2 * X / Params[2] - 1, -2 * (Y + 11) / Params[3] + 1);
	glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1])));

	C.L := ShadowColor(CF);
	glColor3ubv(PGLUByte(@C));
	glRasterPos2d(2 * (X + 1) / Params[2] - 1, -2 * (Y + 1 + 11) / Params[3] + 1);
	glCallLists(Length(Text), GL_UNSIGNED_BYTE, Pointer(Integer(@Text[1])));

	if CB <> clNone then
	begin
		sx := 2 * (X + 1) / Params[2] - 1;
		sy := -2 * (Y + 1 + 11) / Params[3] + 1;
		wx := 2 * (Canvas.TextWidth(Text) + 1) / Params[2];
		wy := 2 * (Canvas.TextHeight(Text) + 1) / Params[3];
		C.L := CB;
		C.T := 95;

		glEnable(GL_BLEND);
		glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
		glColor4ubv(PGLUByte(@C));
		glBegin(GL_QUADS);
			glVertex3f(sx, sy, 0);
			glVertex3f(sx + wx, sy, 0);
			glVertex3f(sx + wx, sy + wy, 0);
			glVertex3f(sx, sy + wy , 0);
		glEnd;
		glDisable(GL_BLEND);
	end;

{	glTextOut(Canvas, X, Y, Text, CF);
	glTextOut(Canvas, X + 1, Y + 1, Text, ShadowColor(CF));}
end;

procedure ShowTaskBar(Visible: Boolean);
var hTaskBar: HWND;
begin
	hTaskBar := FindWindow('Shell_TrayWnd', nil);
	if Visible then
		ShowWindow(hTaskBar, SW_SHOW)
	else
		ShowWindow(hTaskBar, SW_HIDE);
end;

procedure TDForm.Common(Value: Boolean);
var Style: LongInt;
begin
{		if FBackground = baOpenGL then
		begin
			FreeOpenGL;
		end;}
		if Value then
		begin
			StoreWindow;

			if FChangeMode then
			begin
				ReadScreenModes;
				SetScreenMode(640, 480, 32, 0, False, False, False, False, True);
			end;
			Style := GetWindowLong(Handle, GWL_STYLE);
			Style := Style and (not WS_CAPTION);
			Style := Style and (not WS_THICKFRAME);
			SetWindowLong(Handle, GWL_STYLE, Style);
//			if FBackground = baOpenGL then
				SetBounds(0, 0, Screen.Width, Screen.Height + 1); // -> PopupMenu is visibled
{			else
				SetBounds(0, 0, Screen.Width, Screen.Height);}
			if FBackground <> baOpenGL then
			begin
				InitBackground;
			end;
		end
		else
		begin
			if FChangeMode then
				RestoreStartMode;
			RestoreWindow;
{			Style := GetWindowLong(Handle, GWL_STYLE);
			Style := Style or (WS_CAPTION);
			Style := Style or (WS_THICKFRAME);
			SetWindowLong(Handle, GWL_STYLE, Style);
			Show;}
		end;
{		if FBackground = baOpenGL then
		begin
			CreateOpenGL(Handle, Canvas);
		end;}
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

procedure TDForm.InitBackground;
var C: TRColor;
begin
	if Assigned(FBitmapB) then
	begin
		if (FBitmapB.Width <> ClientWidth) or
			(FBitmapB.Height <> ClientHeight) then
		begin
//			if Background <> baOpenGLBitmap then
				FBitmapB.SetSize(ClientWidth, ClientHeight);
{			else D???
				FBitmapB.SetSize(1 shl CalcShr(ClientWidth), 1 shl CalcShr(ClientHeight));}

			if (ClientWidth = 0) or (ClientHeight = 0) then Exit;
			if FBitmapB.Empty = False then
			begin
				case FBackground of
				baStandard:
				begin
					FBitmapB.BarE24(clNone, clBtnFace, ef16);
				end;
				baGradient:
				begin
					FBitmapB.FormBitmap(Color);
					if FBitmapF <> nil then
						FBitmapB.Texture24(FBitmapF, clNone, ef04);
					if (FBitmapB.Width >= 4) and (FBitmapB.Height >=4) then
					begin
						C.T := 0;
						C.R := 117;
						C.G := 140;
						C.B := 220;

						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, 1, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, 2, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 2, 0, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, 3, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 3, 0, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, 1, C, ef10);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 2, 1, C, ef04);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, 2, C, ef04);

						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, FBitmapB.Height - 1 - 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, FBitmapB.Height - 1 - 1, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, FBitmapB.Height - 1 - 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, FBitmapB.Height - 1 - 2, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 2, FBitmapB.Height - 1 - 0, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 0, FBitmapB.Height - 1 - 3, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 3, FBitmapB.Height - 1 - 0, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, FBitmapB.Height - 1 - 1, C, ef10);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 2, FBitmapB.Height - 1 - 1, C, ef04);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, 1, FBitmapB.Height - 1 - 2, C, ef04);

						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, 1, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, 2, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 2, 0, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, 3, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 3, 0, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, 1, C, ef10);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 2, 1, C, ef04);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, 2, C, ef04);

						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, FBitmapB.Height - 1 - 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, FBitmapB.Height - 1 - 1, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, FBitmapB.Height - 1 - 0, C, ef16);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, FBitmapB.Height - 1 - 2, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 2, FBitmapB.Height - 1 - 0, C, ef12);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 0, FBitmapB.Height - 1 - 3, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 3, FBitmapB.Height - 1 - 0, C, ef06);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, FBitmapB.Height - 1 - 1, C, ef10);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 2, FBitmapB.Height - 1 - 1, C, ef04);
						Pix24(FBitmapB.Data, FBitmapB.ByteX, FBitmapB.Width - 1 - 1, FBitmapB.Height - 1 - 2, C, ef04);
					end;
				end;
				baGradientOnly:
				begin
					FBitmapB.FormBitmap(Color);
				end;
				baBitmap:
				begin
					if FBitmapF <> nil then
						FBitmapB.Texture24(FBitmapF, clNone, ef16);
				end;
				end;
			end;
		end;
	end;
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
//			FreeOpenGL;
			glDeleteLists(FontBase, 256);
			DestroyRenderingContext(RC); RC := 0;
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
				FileName := GraphDir + 'Form.jpg';
				if FileExists(FileName) then
					FBitmapF.LoadFromFile(FileName);
			end;
		end;
		end;

		case FBackground of
		baNone, baOpenGL:
		begin
//			Image.Visible := False;
			FBitmapB.SetSize(0, 0);
		end
		else
		begin
			FBitmapB.SetSize(0, 0);
			InitBackground;
//			Image.Visible := True;
			if FBackground <> baOpenGLBitmap then
				Invalidate;
		end;
		end;

		case FBackground of
		baOpenGL, baOpenGLBitmap:
		begin
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

		if Visible then
			Paint;
	end;
end;

procedure TDForm.InitRect;
var
	hR: THandle;
	Po: array[0..9] of tagPOINT;
begin
	if (SysInfo.OS.dwMajorVersion < 4) or ((SysInfo.OS.dwMajorVersion = 4) and (SysInfo.OS.dwMinorVersion < 10)) then Exit;
	if (SysInfo.OS.dwMajorVersion >= 5) then Exit;
	if (FFullScreen = False) and (WindowState <> wsMaximized) then
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
//		hR := CreateRoundRectRgn(0, 0, Width + 1, Height + 1, 40, 40);
//			hR := CreateEllipticRgn(0, 0, Width, Height);
//			hR := CreateRectRgn(0, 0, Width, Height);
//		hR := CreateRectRgn(0, 0, Width, Height);
		hR := CreatePolygonRgn(Po[0], Length(Po), {ALTERNATE}	WINDING);
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

procedure TDForm.CheckPos;
begin
	if Left + Width > Screen.Width then Left := Screen.Width - Width;
	if Top + Height > Screen.Height then Top := Screen.Height - Height;
	if Left < 0 then Left := 0;
	if Top < 0 then Top := 0;
end;

constructor TDForm.Create(AOwner: TComponent);
var
	FileName: TFileName;
begin
	inherited Create(AOwner);

	if NTSystem then
		if Font.Name = 'MS Sans Serif' then
			Font.Name := 'Microsoft Sans Serif';
	Canvas.Font.Name := Font.Name;

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
	BitmapFree(FBitmapB);
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

procedure TDForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = VK_RETURN) and (ssAlt in Shift) then
		FullScreen := not FullScreen
	else
		inherited KeyDown(Key, Shift);
end;

procedure TDForm.Fill;
begin
	OnPaint(nil);
	Paint;
end;

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
var
	NewX: SG;
	ZoomX, ZoomY: Single;
begin
	case FBackground of
	baNone:
	begin

	end;
	baOpenGL, baOpenGLBitmap:
	begin
		ActivateRenderingContext(Canvas.Handle, RC); // make context drawable
//		BeforeDraw;
	end
	else
	begin
		BitBlt(Canvas.Handle, 0, 0, FBitmapB.Width, FBitmapB.Height,
			FBitmapB.Canvas.Handle,
			0, 0,
			SRCCOPY);
//		Canvas.Draw(0, 0, FBitmapB);
//		InitBackground;
	end;
	end;

	if FBackground = baOpenGLBitmap then
	begin
		glPushAttrib(GL_ALL_ATTRIB_BITS);
		glPushMatrix;


//		ResizeScene;


{		gluLookAt(0, 0, 0,
			0, 0, 10,
			0, 1, 0);}

		glClearColor(0, 0, 0, 1);
//			glDisable(GL_LIGHTING);
//			glDisable(GL_CULL_FACE);
			glClear(GL_DEPTH_BUFFER_BIT or GL_ACCUM_BUFFER_BIT); // CPU GL_ACCUM_BUFFER_BIT!!!!!!!!!!!

		if (not Assigned(FBitmapB)) then
		begin
			glNormal3f(0.0, 0.0, -1.0);
			glBegin(GL_QUADS);
				glColor3ub(Random(256), 0, 0);
				glVertex3f(1.0, 1.0, 1.0);
				glColor3ub(0, Random(256), 0);
				glVertex3f(-1.0, 1.0, 1.0);
				glColor3ub(0, 0, 255);
				glVertex3f(-1.0, -1.0, 1.0);
				glColor3ub(0, 255, Random(256));
				glVertex3f(1.0, -1.0, 1.0);
			glEnd;
		end
		else
		begin
			NewX := 1 shl CalcShr(FBitmapB.Width);
			if FBitmapB.Width <> NewX then
			begin
				FBitmapB.Resize24E(FBitmapB, clNone, NewX, NewX div 2, nil);
				FBitmapB.SwapRB24;
			end;
//			FBitmapB.GLSetSize;

			glEnable(GL_TEXTURE_2D);
			glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, FBitmapB.Width,
				FBitmapB.Height, 0, GL_RGB, GL_UNSIGNED_BYTE,
				FBitmapB.GLData);
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP{GL_REPEAT});
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP{GL_REPEAT});
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
			glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

			glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

			ZoomX := 512 / 822;
			ZoomY := 1;
			glBegin(GL_QUADS);
				glNormal3f(0.0, 0.0, -1.0);

//				glColor3ub(255, 0, 0);
				glTexCoord2f(ZoomX, ZoomY);
				glVertex3f(1.0, 1.0, -1.0);
//				glColor3ub(0, 255, 0);
				glTexCoord2f(0, ZoomY);
				glVertex3f(-1.0, 1.0, -1.0);
//				glColor3ub(0, 0, 255);
				glTexCoord2f(0, 0);
				glVertex3f(-1.0, -1.0, -1.0);
//				glColor3ub(0, 255, 255);
				glTexCoord2f(ZoomX, 0);
				glVertex3f(1.0, -1.0, -1.0);
			glEnd;
			glDisable(GL_TEXTURE_2D);
		end;
(*

		glDisable(GL_LIGHTING);
		glDisable(GL_DEPTH_TEST);
		glDisable(GL_CULL_FACE);
//	glShadeModel(GL_FLAT);}

		glClearColor(0, 0, 0, 1);
//			glDisable(GL_LIGHTING);
//			glDisable(GL_CULL_FACE);
			glClear(GL_DEPTH_BUFFER_BIT {or GL_ACCUM_BUFFER_BIT}); // CPU GL_ACCUM_BUFFER_BIT!!!!!!!!!!!
	//		glClearDepth(1);
	//  glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
		glEnable(GL_AUTO_NORMAL);
		glEnable(GL_NORMALIZE);
//		glDisable(GL_AUTO_NORMAL);
//		glDisable(GL_NORMALIZE);
	//	glDisable(GL_ALPHA_TEST);
	//	glDepthFunc(GL_GREATER);

		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity;

		glDisable(GL_LIGHT1);

			// 2


	glEnable(GL_COLOR_MATERIAL);

{	LightPos[0] := UserX;
	LightPos[1] := UserY;
	LightPos[2] := UserZ;

	glEnable(GL_LIGHT0);
	glLightfv(GL_LIGHT0, GL_AMBIENT, @glfLightAmbient[0]);
	glLightfv(GL_LIGHT0, GL_DIFFUSE, @glfLightDiffuse[0]);
	glLightfv(GL_LIGHT0, GL_SPECULAR, @glfLightSpecular[0]);
	glLightfv(GL_LIGHT0, GL_POSITION, @LightPos[0]);
	glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, 60);
	glfDirect[0] := Sin(AngleXZ);
	glfDirect[1] := AngleY;
	glfDirect[2] := -Cos(AngleXZ);
	glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, @glfDirect[0]);}

	glLoadIdentity;
	glScalef(10, 10, 10);
	glColor3ub(255, 255, 255);

				glEnable(GL_TEXTURE_2D);
				glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, FBitmapB.Width,
					FBitmapB.Height, 0, GL_RGB, GL_UNSIGNED_BYTE,
					FBitmapB.GLData);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
				glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

				glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

		glBegin(GL_QUADS);
			glNormal3f(0.0, 0.0, -1.0);
//			glColor3ub(255, 0, 0);
			glTexCoord2f(0, 0);
			glVertex3f(1.0, 1.0, -1.0);
//			glColor3ub(0, 255, 0);
			glTexCoord2f(4, 0);
			glVertex3f(-1.0, 1.0, -1.0);
//			glColor3ub(0, 0, 255);
			glTexCoord2f(4, 4);
			glVertex3f(-1.0, -1.0, -1.0);
//			glColor3ub(0, 255, 255);
			glTexCoord2f(0, 4);
			glVertex3f(1.0, -1.0, -1.0);
		glEnd;
				glDisable(GL_TEXTURE_2D);


		glDisable(GL_COLOR_MATERIAL);*)


		glPopMatrix;
		glPopAttrib;
	end;

	inherited Paint; // FOnPaint Method

	case FBackground of
	baOpenGL, baOpenGLBitmap:
	begin
		if OneBuffer then
			glFlush
		else
			SwapBuffers(Canvas.Handle);
		DeactivateRenderingContext; // make context drawable
//		AfterDraw;
	end;
	end;
end;
{
procedure TDForm.WMPaint;
begin
//	Paint;
//	DefaultHandler(Message);
	inherited;
end;}

procedure TDForm.WMEraseBkGnd;
begin
	Message.Result := -1;
//	InitBackground;
//	Canvas.FillRect(Rect(0, 0, ClientWidth, ClientHeight));
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
	if Visible = False then Exit;
	if (Message.Width = 0) or (Message.Height = 0) then Exit;
//	if (Message.Width <> Width) or (Message.Height <> Height) then
	InitRect;

	case FBackground of
	baGradient, baGradientOnly:
	begin
//		Invalidate;
	end;
	baBitmap, baStandard:
	begin
{		if (Message.Width > Width) or (Message.Height > Height) then
			Invalidate;}
	end;
	baOpenGL, baOpenGLBitmap:
	begin
		ActivateRenderingContext(Canvas.Handle,RC); // make context drawable
{		if FBackground = baOpenGLBitmap then
			ResizeScene; D???}
//		BeforeResize; }
	end;
	end;

	inherited; // FOnResize Method

	case FBackground of
	baNone:
	begin

	end;
	baOpenGL, baOpenGLBitmap:
	begin
//		AfterResize;
		Paint;
		DeactivateRenderingContext; // make context drawable
	end
	else
	begin
//		if (Message.Width <> Width) or (Message.Height <> Height) then
			InitBackground;
	end;
	end;
end;

procedure TDForm.WMShow(var Message: TWMShowWindow);
begin
	if (Message.Show) and (Message.Status = 0) then
	begin
		CheckPos;
		InitBackground;
		InitRect;
	end;
	inherited;
end;

{procedure TDForm.WMHScroll(var Message: TWMScroll);
begin
	inherited;
	Paint;
end;

procedure TDForm.WMVScroll(var Message: TWMScroll);
begin
	inherited;
	Paint;
end;}

procedure TDForm.WMSysColorChange;
begin
	FBitmapB.SetSize(0, 0);
	InitBackground;
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
		SetBounds(
			FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Top,
			FWindowPlacement.rcNormalPosition.Right - FWindowPlacement.rcNormalPosition.Left,
			FWindowPlacement.rcNormalPosition.Bottom - FWindowPlacement.rcNormalPosition.Top);
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
	FStoreWindow := True;
end;

procedure Register;
begin
	RegisterComponents('DComp', [TDForm]);
end;

Initialization

Finalization
	BitmapFree(FBitmapF);
end.
