//* File:     Lib\uDOpenGL.pas
//* Created:  2000-05-01
//* Modified: 2004-04-28
//* Version:  X.X.31.X
//* Author:   Safranek David (Safrad)
//* E-Mail:   safrad@email.cz
//* Web:      http://safrad.webzdarma.cz

unit uDOpenGL;

interface

uses OpenGL12, Windows, Graphics;

(*
// Open GL 1.1 Enhacement
procedure glBindTexture(target: GLenum; texture: GLuint); stdcall;
	{$EXTERNALSYM glBindTexture}
procedure glGenTextures(n: GLsizei; textures: PGLuint); stdcall;
	{$EXTERNALSYM glGenTextures}*)

function SetDCPixelFormat(Handle: HDC): Boolean;

procedure CreateOpenGL(Handle: HWND; Canvas: TCanvas);
procedure BeforeDraw;
procedure AfterDraw;
procedure BeforeResize;
procedure AfterResize;
procedure FreeOpenGL;

//procedure glTextOutE(Canvas: TCanvas; Text: string);

var
	OpenGLInit: Boolean;
	OneBuffer: Boolean;

	hrc: HGLRC; //OpenGL rendering context
	DC: HDC; // Secondary Buffer

	FontBase: LongWord;
var
	Hand: HWND;
	Canv: TCanvas;

implementation

uses
	Dialogs,
	uGraph, uDBitmap, uAdd, uError;

{
procedure glBindTexture(target: GLenum; texture: GLuint); external opengl32 name 'glBindTexture';
procedure glGenTextures(n: GLsizei; textures: PGLuint); external opengl32 name 'glGenTextures';}

function SetDCPixelFormat(Handle: HDC): Boolean;
//setup the device context format
var
	pfd: TPixelFormatDescriptor; //see win32 help for details
	PixelFormat: Integer;
begin
	FillChar(pfd, SizeOf(pfd), 0);
	with pfd do
	begin
		nSize     := SizeOf(pfd);
		nVersion  := 1;

		dwFlags   := PFD_SUPPORT_OPENGL;
		if GetObjectType(DC) = OBJ_MEMDC then
			dwFlags:=dwFlags or PFD_DRAW_TO_BITMAP
		else
			dwFlags:=dwFlags or PFD_DRAW_TO_WINDOW;

		if OneBuffer = False then
			dwFlags := dwFlags or PFD_DOUBLEBUFFER{ or PFD_SWAP_COPY};
		iPixelType := PFD_TYPE_RGBA; //PFD_TYPE_COLORINDEX;
		cColorBits := 32; //24; // 8
		cDepthBits := 32; // 16
		cStencilBits := 0;
		iLayerType := PFD_MAIN_PLANE;
	end;
	PixelFormat := ChoosePixelFormat(Handle, @pfd);
	if PixelFormat = 0 then
		Result := False
	else
	begin
		Result := SetPixelFormat(Handle, PixelFormat, @pfd);
	end;
end;

procedure CreateOpenGL(Handle: HWND; Canvas: TCanvas);
begin
	if OpenGLInit = False then
	begin
		if InitOpenGL = False then Exit;
		Hand := Handle;
		Canv := Canvas;
		OpenGLInit := True;
		hrc := CreateRenderingContext(Canvas.Handle,[opDoubleBuffered],32,0);
		Exit;
		DC := GetDC(Hand);
//		hrc := wglCreateContext(DC); // D???
{		if hrc <> 0 then}
		begin
//			wglMakeCurrent(DC, hrc);
			OpenGLInit := SetDCPixelFormat(Canv.Handle);
			if OpenGLInit then
			begin
				hrc := wglCreateContext(Canv.Handle);
				wglMakeCurrent(Canv.Handle, hrc);
				// Font
				SelectObject(DC, GetStockObject(ANSI_VAR_FONT));
				// create the bitmap display lists
				// we're making images of glyphs 0 thru 255
				// the display list numbering starts at 1000, an arbitrary choice

				FontBase := glGenLists(256);
				wglUseFontBitmaps(DC, 0, 255, FontBase);

				// display a string:
				// indicate start of glyph display lists
			end;
		end
{		else
			ErrorMessage(ErrorMes(GetLastError));}
	end
	else
		MessageD('OpenGL already created', mtError, [mbOk]);
end;

procedure BeforeDraw;
begin
	if OpenGLInit then
		ActivateRenderingContext(Canv.Handle, hrc);
//		wglMakeCurrent(Canv.Handle, hrc);
end;

procedure AfterDraw;
begin
	if OpenGLInit then
	begin
		if OneBuffer then
			glFlush
		else
			SwapBuffers(DC);
		DeactivateRenderingContext;
//		wglMakeCurrent(0, 0);
	end;
end;

procedure BeforeResize;
begin
		ActivateRenderingContext(Canv.Handle, hrc);
{	if OpenGLInit then
		wglMakeCurrent(Canv.Handle, hrc);}
end;

procedure AfterResize;
begin
		DeactivateRenderingContext;
{	if OpenGLInit then
		wglMakeCurrent(0, 0);}
end;

procedure FreeOpenGL;
begin
	if OpenGLInit then
	begin
		DestroyRenderingContext(hrc);
		OpenGLInit := False;
		Exit;
		glDeleteLists(FontBase, 256);
		// Delete OpenGL rendering context
		wglMakeCurrent(0, 0);
		ReleaseDC(Hand, DC); Hand := 0; DC := 0;
		wglDeleteContext(hrc); hrc := 0;
		Canv := nil;
	end
	else
		MessageD('OpenGL already freed', mtError, [mbOk]);
end;

procedure glTextOutE(Canvas: TCanvas; Text: string);
var
	Bmp: TDBitmap;
	x, y: SG;
begin
	if OpenGLInit then
	begin
		Bmp := TDBitmap.Create;
		x := Canvas.TextWidth(Text);
		y := Canvas.TextHeight(Text);
		Bmp.SetSize(x, y);

		Bmp.Canvas.Font := Canvas.Font;
		Bmp.Canvas.Brush := Canvas.Brush;
		Bmp.Canvas.TextOut(0, 0, Text);

		glDrawPixels(Bmp.Width, Bmp.Height, GL_RGB, GL_UNSIGNED_BYTE,
			Bmp.GLData);

		Bmp.Free;
	end;
end;

end.
