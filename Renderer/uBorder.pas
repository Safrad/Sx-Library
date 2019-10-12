unit uBorder;

interface

uses
  uTypes,
  uGraphicObject,
  uDBitmap,
  uTransformation,
  uClipping,
  UITypes,
  Windows,
  uDrawStyle;

type
  TBorder = class(TGraphicObject)
  private
    FColor: TColor;
    FEffect: TEffect;
    FWidth: FG;
    FTopLeftColor: TColor;
    FBottomRightColor: TColor;
    procedure SetEffect(const Value: TEffect);
    procedure SetWidth(const Value: FG);
    procedure SetBottomRightColor(const Value: TColor);
    procedure SetTopLeftColor(const Value: TColor);
  public
    constructor Create;

    property TopLeftColor: TColor read FTopLeftColor write SetTopLeftColor;
    property BottomRightColor: TColor read FBottomRightColor write SetBottomRightColor;
    property Effect: TEffect read FEffect write SetEffect;
    property Width: FG read FWidth write SetWidth;

    procedure Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping); override;
  end;

implementation

{ TBar }

constructor TBorder.Create;
begin
  inherited;

  FColor := TColors.Rosybrown;
  FEffect := ef16;
  FWidth := 1;
end;

procedure TBorder.Render(const ADBitmap: TDBitmap; const ATransfromation: TTransformation; const AClipping: TClipping);
var
  Rect: TRect;
begin
  inherited;

  Rect := ATransfromation.TransformToInt(Position);
  if AClipping.IsPartialyVisible(Rect) then
    ADBitmap.Border(Rect, TopLeftColor, BottomRightColor, Round(ATransfromation.Zoom * Width), Effect); // TODO : AClipping
end;

procedure TBorder.SetBottomRightColor(const Value: TColor);
begin
  FBottomRightColor := Value;
end;

procedure TBorder.SetEffect(const Value: TEffect);
begin
  FEffect := Value;
end;

procedure TBorder.SetTopLeftColor(const Value: TColor);
begin
  FTopLeftColor := Value;
end;

procedure TBorder.SetWidth(const Value: FG);
begin
  FWidth := Value;
end;

end.
