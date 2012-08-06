unit uScreenshots;

interface

uses
  uTypes,
  uDBitmap,
  Types,
  Controls,
  Forms;

type
  TScreenshots = class
  private
		FPath: string;
    FUnnamedIndex: SG;
    FClientOnly: BG;
    procedure ExpandBmp(const FormRect: TRect; const BmpD: TDBitmap; const HighlightedControls: array of TControl; const HighlightedTexts: array of string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure WaitForNewForm;
    procedure SetFormSize(const Form: TForm; const Width, Height: SG);
    procedure SetFormClientSize(const Form: TForm; const Width, Height: SG);
		procedure TakeScreenshot(const Form: TCustomForm; const Name: string = ''); overload;
		procedure TakeScreenshot(const Form: TCustomForm; const Name: string; const HighlightedControls: array of TControl); overload;
		procedure TakeScreenshot(const Form: TCustomForm; const Name: string; const HighlightedControls: array of TControl; const HighlightedTexts: array of string); overload;

    property Path: string read FPath write FPath;
  end;

function TakeScreenshots: BG;

implementation

uses
  SysUtils, Windows, Graphics,
  uFiles, uSystemColors, uDrawStyle, uGraph, uDForm;

function TakeScreenshots: BG;
begin
  Result := FindCmdLineSwitch('screenshots');
end;

{ TScreenshots }

constructor TScreenshots.Create;
begin
	inherited;
  FPath := AppDataDir + 'screenshots\';

	SetSystemColors([COLOR_BTNFACE, COLOR_MENU], [clSilver, clSilver]);
end;

destructor TScreenshots.Destroy;
begin
	RestoreSystemColors;

  inherited;
end;

procedure TScreenshots.ExpandBmp(const FormRect: TRect; const BmpD: TDBitmap;
  const HighlightedControls: array of TControl; const HighlightedTexts: array of string);
{const
  Space = 32;
  ox = 4;
  oy = -7;}
var
  i: SG;
  Control: TControl;
  R: TRect;
begin
//	BmpD.SetSize(BmpD.Width, BmpD.Height + Space);
	for i := 0 to Length(HighlightedControls) - 1 do
  begin
    Control := HighlightedControls[i];

    R.Left := Control.ClientOrigin.X - FormRect.Left;
    R.Top := Control.ClientOrigin.Y - FormRect.Top;
    R.Right := R.Left + Control.Width;
    R.Bottom := R.Top + Control.Height;

{    R := Control.BoundsRect;
    R.Left := Control.Left + ox;
    R.Right := Control.Left + Control.Width - 1 + ox;
    R.Top := Control.Top + oy;
    R.Bottom := Control.Top + Control.Height - 1 + oy;}

    BmpD.Bar(R, clRed, ef06);

    InflateRect(R, 2, 2);
    BmpD.Border(R, clRed, clRed, 3, ef16);

    if i < Length(HighlightedTexts) then
    begin
      ShadowText(BmpD.Canvas, R.Left, R.Top, HighlightedTexts[i], clBlack, clWhite);
    end;
  end;
end;

procedure TScreenshots.Reset;
begin
  FUnnamedIndex := 0;
end;

procedure TScreenshots.SetFormClientSize(const Form: TForm; const Width,
  Height: SG);
begin
	Form.ClientWidth := Width;
  Form.ClientHeight := Height;
end;

procedure TScreenshots.SetFormSize(const Form: TForm; const Width, Height: SG);
begin
	Form.Width := Width;
  Form.Height := Height;
end;

procedure TScreenshots.TakeScreenshot(const Form: TCustomForm;
  const Name: string);
begin
   TakeScreenshot(Form, Name, []);
end;

function GetDesktopScreenshot(const R: TRect): TDBitmap;
var
  Width, Height: SG;
begin
	Result := TDBitmap.Create;
	if GetDesktop then
	begin
		try
      Width := R.Right - R.Left;
      Height := R.Bottom - R.Top;
			Result.SetSize(Width, Height);
			BitBlt(Result.Canvas.Handle, 0, 0, Width, Height, DesktopDC, R.Left, R.Top, SRCCOPY);
			// Pix(Bmp.Data, Bmp.ByteX, 1, Bmp.Height -1, @BackgroundColor, ef16); // Replace red point (cause NVidia driver version 15.70). BitBlt problem (driver or windows)
//			Bmp.TransparentColor := BackgroundColor;
		finally
			ReleaseDesktop;
		end;
	end;
end;

procedure TScreenshots.TakeScreenshot(const Form: TCustomForm;
  const Name: string; const HighlightedControls: array of TControl; const HighlightedTexts: array of string);
var
  Bmp: TBitmap;
  BmpD: TDBitmap;
  FileName: TFileName;
  R: TRect;
begin
  CreateDirEx(FPath);

  if FClientOnly then
  begin
//	  Application.ProcessMessages;
//	  Form.Repaint;
	  R := Form.ClientRect;
		Bmp := Form.GetFormImage;
  end
  else
  begin
		R := Form.BoundsRect;
    Bmp := GetDesktopScreenshot(R);
  end;

  BmpD := TDBitmap.Create;
  try
    BmpD.FromBitmap(Bmp);
    Bmp.Free;

    if Length(HighlightedControls) > 0 then
    begin
      ExpandBmp(R, BmpD, HighlightedControls, HighlightedTexts);
    end;

    if Name = '' then
    begin
      FileName := FPath + IntToStr(FUnnamedIndex) + '.png';
      Inc(FUnnamedIndex);
     end
     else
      FileName := FPath + Name + '.png';

    BmpD.SaveToFile(FileName);
	finally
    BmpD.Free;
  end;
end;

procedure TScreenshots.WaitForNewForm;
begin
  Application.ProcessMessages;
// Required for Windows 7
// run "adjust perf"
// uncheck "Animate windows when minimizing and maximizing"
//  Sleep(500);
end;

procedure TScreenshots.TakeScreenshot(const Form: TCustomForm;
  const Name: string; const HighlightedControls: array of TControl);
begin
   TakeScreenshot(Form, Name, HighlightedControls, []);
end;

end.
