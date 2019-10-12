unit uGraphicObject;

interface

uses
  uTypes,
  uDBitmap,
  uTransformation,
  uClipping,
  uGeometry2D;

type
  TGraphicObject = class
  private
    FDepth: FG;
    FPosition: TRect2D;
    procedure SetDepth(const Value: FG);
  protected
    procedure SetPosition(const Value: TRect2D); virtual;
  public
    procedure Render(const ADBitmap: TDBitmap; const ATransformation: TTransformation; const AClipping: TClipping); virtual; abstract;

    property Position: TRect2D read FPosition write SetPosition;
    property Depth: FG read FDepth write SetDepth;
  end;

implementation

{ TGraphicObject }

procedure TGraphicObject.SetDepth(const Value: FG);
begin
  FDepth := Value;
end;

procedure TGraphicObject.SetPosition(const Value: TRect2D);
begin
  FPosition := Value;
end;

end.
