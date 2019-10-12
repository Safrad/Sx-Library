unit uLine;

interface

uses
  uTypes,
  uGraphicObject,
  uDBitmap,
  uTransformation,
  uClipping,
  UITypes,
  uDrawStyle;

type
  TLine = class(TGraphicObject)
  private
    FColor: TColor;
    FEffect: TEffect;
    FWidth: FG;
    procedure SetColor(const Value: TColor);
    procedure SetEffect(const Value: TEffect);
    procedure SetWidth(const Value: FG);
  public
    constructor Create;

    property Color: TColor read FColor write SetColor;
    property Effect: TEffect read FEffect write SetEffect;
    property Width: FG read FWidth write SetWidth;

    procedure Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping); override;
  end;

implementation

uses
  Types;

{ TLine }

constructor TLine.Create;
begin
  inherited;

  FWidth := 1;
end;

procedure TLine.Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping);
var
  Rect: TRect;
begin
  inherited;

  Rect := ATransfromation.TransformToInt(Position);
  if AClipping.IsPartialyVisible(Rect) then
  begin
    AClipping.ClipRect(Rect);
    ADBitmap.LineFast(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Color, Effect, 1); //Round(ATransfromation.Zoom * Width));
//    ADBitmap.Line(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Color, Effect, ATransfromation.Zoom * Width);
  end;
end;

procedure TLine.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TLine.SetEffect(const Value: TEffect);
begin
  FEffect := Value;
end;

procedure TLine.SetWidth(const Value: FG);
begin
  FWidth := Value;
end;

end.

