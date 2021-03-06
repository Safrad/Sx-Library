unit uPathGraphicObject;

interface

uses
  uGraphicObject,
  uFill,
  uStroke;

type
  TPathGraphicObject = class(TGraphicObject)
  private
    FName: string;
    FFill: TFill;
    FStroke: TStroke;
    procedure SetName(const Value: string);
    procedure SetFill(const Value: TFill);
    procedure SetStroke(const Value: TStroke);
  public
    property Name: string read FName write SetName;
    property Fill: TFill read FFill write SetFill;
    property Stroke: TStroke read FStroke write SetStroke;
  end;

implementation

{ TPathGraphicObject }

procedure TPathGraphicObject.SetFill(const Value: TFill);
begin
  FFill := Value;
end;

procedure TPathGraphicObject.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TPathGraphicObject.SetStroke(const Value: TStroke);
begin
  FStroke := Value;
end;

end.
