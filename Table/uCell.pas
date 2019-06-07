unit uCell;

interface

uses
  uTypes,
  uConsoleColor,
  uTextLines,
  uTextAlignment;

type
  TCell = class(TTextLines)
  private
    FTextAlignment: TTextAlignment;
    FTextColor: TConsoleColor;
    procedure SetTextAlignment(const Value: TTextAlignment);
    procedure SetHorizontalAlignment(const Value: THorizontalAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
    function GetHorizontalAlignment: THorizontalAlignment;
    function GetVerticalAlignment: TVerticalAlignment;
    procedure SetTextColor(const Value: TConsoleColor);
  public
    constructor Create;

    property TextAlignment: TTextAlignment read FTextAlignment write SetTextAlignment;
    property HorizontalAlignment: THorizontalAlignment read GetHorizontalAlignment write SetHorizontalAlignment;
    property VerticalAlignment: TVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property TextColor: TConsoleColor read FTextColor write SetTextColor;
  end;

implementation

{ TCell }

constructor TCell.Create;
begin
  inherited;

  FTextColor := ccLightGray;
end;

function TCell.GetHorizontalAlignment: THorizontalAlignment;
begin
  Result := TextAlignment.Horizontal;
end;

function TCell.GetVerticalAlignment: TVerticalAlignment;
begin
  Result := TextAlignment.Vertical;
end;

procedure TCell.SetTextColor(const Value: TConsoleColor);
begin
  FTextColor := Value;
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

