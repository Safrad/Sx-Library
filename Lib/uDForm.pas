// Build: 12/2001-12/2001 Author: Safranek David

unit uDForm;

interface

{$R *.RES}
uses
	uDBitmap,
	Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	ExtCtrls, StdCtrls;

type
	TBackground = (baNone, baUser, baStandard, baGradientOnly, baGradient, baBitmap, baOpenGL);

	TDForm = class(TForm)
	private
		{ private declarations }
		FStoreWindow: Boolean;
		FWindowPlacement: TWindowPlacement;
		FWindowLong: LongInt;
//		ALeft, ATop, AWidth, AHeight: LongInt;

		FBitmapB, FBitmapF: TDBitmap;

//		Image: TImage;
		FBackground: TBackground;
		FFullScreen: Boolean;
		FChangeMode: Boolean;

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
		{ public declarations }
		RC: HGLRC;
		FontBase: LongWord;

		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;

		procedure RestoreWindow;
		procedure StoreWindow;

//		procedure KeyDown(var Key: Word; Shift: TShiftState); override;

		procedure Paint; override;
		procedure ResizeMessage;
	published
		{ published declarations }
		property BackBitmap: TDBitmap read FBitmapB;
		property Background: TBackground read FBackground write SetBackground default baNone;
		property FullScreen: Boolean read FFullScreen write SetFullScreen default False;
		property ChangeMode: Boolean read FChangeMode write SetChangeMode default False;
	end;

procedure DFormFree(var DForm: TDForm);
procedure glShadowText(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const CF, CB: TColor);
procedure glTextOut(Canvas: TCanvas;
	const X, Y: Integer; const Text: string; const C: TColor);
procedure ShowTaskBar(Visible: Boolean);

procedure Register;

implementation

uses
	uGraph, uAdd, uFiles, OpenGL12, uScreen, uSysInfo;

procedure DFormFree(var DForm: TDForm);
begin
	DForm.Free;
	DForm := nil;
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
begin
	if Assigned(FBitmapB) then
	begin
		if (FBitmapB.Width <> ClientWidth) or
			(FBitmapB.Height <> ClientHeight) then
		begin
			FBitmapB.SetSize(ClientWidth, ClientHeight);
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
					FBitmapB.Texture24(FBitmapF, clNone, ef04);
				end;
				baGradientOnly:
				begin
					FBitmapB.FormBitmap(Color);
				end;
				baBitmap:
				begin
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
		baOpenGL:
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
			FileName := GraphDir + 'Form.jpg';
			if FileExists(FileName) then
				FBitmapF.LoadFromFile(FileName);
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
			Invalidate;
		end;
		end;

		case FBackground of
		baOpenGL:
		begin
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
	Po: array[0..7] of tagPOINT;
begin
	if (SysInfo.OS.dwMajorVersion < 4) or ((SysInfo.OS.dwMajorVersion = 4) and (SysInfo.OS.dwMinorVersion < 10)) then Exit;
	if FFullScreen = False then
	begin
		Po[0].x := 0;
		Po[0].y := 0;
		Po[1].x := Width;
		Po[1].y := 0;
		Po[2].x := Width;
		Po[2].y := Height;
		Po[3].x := Width div 2 + 8;
		Po[3].y := Height;
		Po[4].x := Width div 2;
		Po[4].y := Height - 8;
		Po[5].x := Width div 2 - 8;
		Po[5].y := Height;
		Po[6].x := 0;
		Po[6].y := Height;
		Po[7].x := 0;
		Po[7].y := 0;
		//	hR := CreateEllipticRgn(0, 0, Width, Height);
		//	hR := CreateRectRgn(0, 0, Width, Height);
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

	CheckPos;

	HorzScrollBar.Tracking := True;
	VertScrollBar.Tracking := True;
	FBackground := baNone;
	FBitmapF := TDBitmap.Create;
	FBitmapF.SetSize(0, 0);

{	Image := TImage.Create(Self);
	Image.Width := 0;
	Image.Height := 0;}
//	InitImage(Image, clNone);
	FBitmapB := TDBitmap.Create;
//	Image.Bitmap; // Picture.Bitmap;
	FBitmapB.SetSize(0, 0);

//	InsertControl(Image);
{	Image.Align := alClient;
	Image.SendToBack;
	Image.Enabled := False;
	Image.Visible := False;}

	FileName := Name;
	if FileName[1] = 'f' then Delete(FileName, 1, 1);
	if FileName = 'Main' then FileName := Application.Title;

	FileName := GraphDir + FileName + '.ico';
	if FileExists(FileName) then
		Icon.LoadFromFile(FileName);
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
	BitmapFree(FBitmapF);
//	RemoveControl(Image);
//	Image.Free; Image := nil;
	if FBackground = baOpenGL then
	begin
		glDeleteLists(FontBase, 256);
		DestroyRenderingContext(RC); RC := 0;
//		FreeOpenGL;
	end;
	inherited Destroy;
end;
{
procedure TDForm.KeyDown(var Key: Word; Shift: TShiftState);
begin
	if (Key = VK_RETURN) and (ssAlt in Shift) then
		FullScreen := not FullScreen;
	inherited KeyDown(Key, Shift);
end;}

procedure TDForm.Paint;
begin
	case FBackground of
	baNone, baOpenGL:
	begin
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
	if FBackground = baOpenGL then
	begin
		ActivateRenderingContext(Canvas.Handle,RC); // make context drawable
//		BeforeDraw;
	end;

	inherited Paint; // FOnPaint Method

	if FBackground = baOpenGL then
	begin
		SwapBuffers(Canvas.Handle);
		DeactivateRenderingContext; // make context drawable
//		AfterDraw;
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
	baOpenGL:
	begin
		ActivateRenderingContext(Canvas.Handle,RC); // make context drawable
//		BeforeResize; }
	end;
	end;

	inherited; // FOnResize Method

	case FBackground of
	baNone, baOpenGL:
	begin
	end
	else
	begin
//		if (Message.Width <> Width) or (Message.Height <> Height) then
			InitBackground;
	end;
	end;

	if FBackground = baOpenGL then
	begin
//		AfterResize; 
		Paint;
		DeactivateRenderingContext; // make context drawable
	end;
end;

procedure TDForm.WMShow(var Message: TWMShowWindow);
begin
	if (Message.Show) and (Message.Status = 0) then
	begin
		CheckPos;
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

end.
