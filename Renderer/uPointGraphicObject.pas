unit uPointGraphicObject;

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
  TPointGraphicObject = class(TGraphicObject)
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

{ TPointGraphicObject }

constructor TPointGraphicObject.Create;
begin
  inherited;

  FWidth := 1;
end;

procedure TPointGraphicObject.Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping);
var
  Rect: TRect;
  P: PPixel;
begin
  inherited;

  Rect := ATransfromation.TransformToInt(Position);
  if AClipping.IsPartialyVisible(Rect.TopLeft) then
  begin
    P := ADBitmap.GetPixelAddr(Rect.Left, Rect.Top);
    P^.L := Color;
  end;
end;

procedure TPointGraphicObject.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TPointGraphicObject.SetEffect(const Value: TEffect);
begin
  FEffect := Value;
end;

procedure TPointGraphicObject.SetWidth(const Value: FG);
begin
  FWidth := Value;
end;

end.

