//* File:     Lib\GUI\uDWinControl.pas
//* Created:  2007-05-27
//* Modified: 2007-05-27
//* Version:  1.1.39.8
//* Author:   David Safranek (Safrad)
//* E-Mail:   safrad at email.cz
//* Web:      http://safrad.own.cz

unit uDWinControl;

interface

uses
	uTypes,
	Classes,
	Controls,
	Messages,
	Graphics,
	uDBitmap;

type
	TDWinControl = class(TWinControl)
	private
		FCanvas: TCanvas;
		FBitmap: TDBitmap;
		FNeedFill: BG;
		{$ifopt d+}
		FFillCount, FPaintCount: UG;
		{$endif}
		procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
		procedure WMSize(var Message: TWMSize); message WM_SIZE;
		procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
	protected
		procedure FillBitmap; virtual;
	public
		constructor Create(AOwner: TComponent); override;
		destructor Destroy; override;
		property Canvas: TCanvas read FCanvas;
		procedure Invalidate; override;
	property
		Bitmap: TDBitmap read FBitmap;
	end;

implementation

uses SysUtils, Windows;

{ TDWinControl }

constructor TDWinControl.Create(AOwner: TComponent);
begin
	inherited;
	FBitmap := TDBitmap.Create;
	FBitmap.Canvas.Font := Font;

	FCanvas := TControlCanvas.Create;
	TControlCanvas(FCanvas).Control := Self;
end;

destructor TDWinControl.Destroy;
begin
	FreeAndNil(FCanvas);
	FreeAndNil(FBitmap);
	inherited;
end;

procedure TDWinControl.FillBitmap;
begin
	FNeedFill := False;
	{$ifopt d+}
	Inc(FFillCount);
	{$endif}
end;

procedure TDWinControl.Invalidate;
begin
	FNeedFill := True;
	inherited Invalidate;
end;

procedure TDWinControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
	Message.Result := 1;
end;

procedure TDWinControl.WMPaint(var Message: TWMPaint);
begin
	inherited;
	{$ifopt d+}
	Inc(FPaintCount);
	{$endif}
	if FNeedFill then FillBitmap;

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
		FBitmap.DrawToDC(FCanvas.Handle, 0, 0);
	end;
{	if Assigned(FOnPaint) then
	begin
		try
			FOnPaint(Self);
		except
			on E: Exception do
				Fatal(E, Self);
		end;
	end;}

	{$ifopt d+}
{	Canvas.Brush.Style := bsClear;
	Canvas.Font.Color := clWhite;
	Canvas.TextOut(0, 0, IntToStr(FFillCount) + '/' + IntToStr(FPaintCount));}
	{$endif}
end;

procedure TDWinControl.WMSize(var Message: TWMSize);
begin
	inherited;
	FBitmap.SetSize(Width, Height);
end;

end.
