unit uStroke;

interface

uses
  uTypes,
  UITypes;

type
  TStroke = class
  private
    FWidth: SG;
    FColor: TColor;
    procedure SetColor(const Value: TColor);
    procedure SetWidth(const Value: SG);
  public
    property Color: TColor read FColor write SetColor;
    property Width: SG read FWidth write SetWidth;
  end;

implementation

{ TStroke }

procedure TStroke.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TStroke.SetWidth(const Value: SG);
begin
  FWidth := Value;
end;

end.
