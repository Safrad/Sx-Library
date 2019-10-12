unit uBar;

interface

uses
  uGraphicObject,
  uDBitmap,
  uTransformation,
  uClipping,
  UITypes,
  uDrawStyle;

type
  TBar = class(TGraphicObject)
  private
    FColor: TColor;
    FEffect: TEffect;
    procedure SetColor(const Value: TColor);
    procedure SetEffect(const Value: TEffect);
  public
    constructor Create;

    property Color: TColor read FColor write SetColor;
    property Effect: TEffect read FEffect write SetEffect;

    procedure Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping); override;
  end;

implementation

uses
  Types;

{ TBar }

constructor TBar.Create;
begin
  inherited;

  FColor := TColors.Silver;
  FEffect := ef16;
end;

procedure TBar.Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping);
var
  Rect: TRect;
begin
  inherited;

  Rect := ATransfromation.TransformToInt(Position);
  if AClipping.IsPartialyVisible(Rect) then
  begin
    AClipping.ClipRect(Rect);
    ADBitmap.Bar(Rect, Color, Effect);
  end;
end;

procedure TBar.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TBar.SetEffect(const Value: TEffect);
begin
  FEffect := Value;
end;

end.
