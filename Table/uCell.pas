unit uCell;

interface

uses
  uTypes,
  uTextLines,
  uTextAlignment;

type
  TCell = class(TTextLines)
  private
    FTextAlignment: TTextAlignment;
    procedure SetTextAlignment(const Value: TTextAlignment);
    procedure SetHorizontalAlignment(const Value: THorizontalAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    function GetHorizontalAlignment: THorizontalAlignment;
    function GetVerticalAlignment: TVerticalAlignment;
  public
    property TextAlignment: TTextAlignment read FTextAlignment write SetTextAlignment;
    property HorizontalAlignment: THorizontalAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: TVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
  end;

implementation

{ TCell }

function TCell.GetHorizontalAlignment: THorizontalAlignment;
begin
  Result := TextAlignment.Horizontal;
end;

function TCell.GetVerticalAlignment: TVerticalAlignment;
begin
  Result := TextAlignment.Vertical;
end;

procedure TCell.SetHorizontalAlignment(const Value: THorizontalAlignment);
begin
  FTextAlignment.Horizontal := Value;
end;

procedure TCell.SetTextAlignment(const Value: TTextAlignment);
begin
  FTextAlignment := Value;
end;

procedure TCell.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  FTextAlignment.Vertical := Value;
end;

end.

