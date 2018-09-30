unit uTextAlignment;

interface

uses
  uTypes;

type
  THorizontalAlignment = (haLeft, haCenter, haRight);

  TVerticalAlignment = (vaTop, vaCenter, vaBottom);

  TTextAlignment = class
  private
    FHorizontalAlignment: THorizontalAlignment;
    FVerticalAlignment: TVerticalAlignment;
    FWordWrap: BG;
    procedure SetHorizontalAlignment(const Value: THorizontalAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    procedure SetWordWrap(const Value: BG);
  public
    property HorizontalAlignment: THorizontalAlignment read FHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment;
    property WordWrap: BG read FWordWrap write SetWordWrap;
  end;

implementation

{ TTextAlignment }

procedure TTextAlignment.SetHorizontalAlignment(const Value: THorizontalAlignment);
begin
  FHorizontalAlignment := Value;
end;

procedure TTextAlignment.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  FVerticalAlignment := Value;
end;

procedure TTextAlignment.SetWordWrap(const Value: BG);
begin
  FWordWrap := Value;
end;

end.
