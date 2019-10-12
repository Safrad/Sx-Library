unit uFill;

interface

uses
  UITypes;

type
  TFill = class
  private
    FColor: TColor;
    procedure SetColor(const Value: TColor);
  public
    property Color: TColor read FColor write SetColor;
  end;

implementation

{ TFill }

procedure TFill.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

end.
