unit uText;

interface

uses
  uTypes,
  uTextAlignment,
  uGraphicObject,
  uDBitmap,
  uSxFont,
  uTransformation,
  uClipping,
  UITypes,
  uDrawStyle;

type
  TText = class(TGraphicObject)
  private
    FFont: TSxFont;
    FText: string;
    FTextAlignment: TTextAlignment;
    procedure SetFont(const Value: TSxFont);
    procedure SetText(const Value: string);
    procedure SetTextAlignment(const Value: TTextAlignment);

  public
    constructor Create;
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property Font: TSxFont read FFont write SetFont;
    property Alignment: TTextAlignment read FTextAlignment write SetTextAlignment;

    procedure Render(const ADBitmap: TDBitmap; const ATransformation: TTransformation; const AClipping: TClipping); override;
  end;

implementation

uses
  Types,
  uFontCache;

{ TText }

constructor TText.Create;
begin
  inherited;

  FFont := TSxFont.Create;
end;

destructor TText.Destroy;
begin
  FFont.Free;

  inherited;
end;

procedure TText.Render(const ADBitmap: TDBitmap; const ATransformation: TTransformation; const AClipping: TClipping);
var
  Rect: TRect;
  FontHeight: SG;
begin
  inherited;

  Rect := ATransformation.TransformToInt(Position);
  FontHeight := Round(ATransformation.Zoom * FFont.Size);
  if (FontHeight >= 5) and (Rect.Width >= 5) and (Rect.Height >= 5) and
    AClipping.IsPartialyVisible(Rect) then
  begin
{    ADBitmap.Canvas.Brush.Style := bsClear;
    ADBitmap.Canvas.Font.Name := FFont.Name;
    ADBitmap.Canvas.Font.Color := FFont.Color;
    ADBitmap.Canvas.Font.Style := FFont.Style;
    ADBitmap.Canvas.Font.Height := FontHeight;

    DrawCuttedText(ADBitmap.Canvas, Rect, FTextAlignment, Text, True, Round(FFont.ShadowSize * IdealShadow(ADBitmap.Canvas)));}

    FontCache.DrawText(ADBitmap, Rect, FTextAlignment, FFont, FontHeight, Text, AClipping);
  end;
end;

procedure TText.SetFont(const Value: TSxFont);
begin
  FFont := Value;
end;

procedure TText.SetTextAlignment(const Value: TTextAlignment);
begin
  FTextAlignment := Value;
end;

procedure TText.SetText(const Value: string);
begin
  FText := Value;
end;

end.

