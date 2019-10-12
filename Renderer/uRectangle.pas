unit uRectangle;

interface

uses
  uGraphicObject,
  uGeometry2D,
  uBar,
  uBorder,
  uDBitmap,
  uTransformation,
  uClipping;

type
  TRectangle = class(TGraphicObject)
  private
    FBar: TBar;
    FBorder: TBorder;
  protected
    procedure SetPosition(const Value: TRect2D); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Render(const ADBitmap: TDBitmap; const ATransformation: TTransformation; const AClipping: TClipping); override;

    property Bar: TBar read FBar;
    property Border: TBorder read FBorder;
  end;

implementation

{ TRectangle }

constructor TRectangle.Create;
begin
  inherited;

  FBar := TBar.Create;
  FBorder := TBorder.Create;
end;

destructor TRectangle.Destroy;
begin
  FBorder.Free;
  FBar.Free;

  inherited;
end;

procedure TRectangle.Render(const ADBitmap: TDBitmap; const ATransformation: TTransformation;
  const AClipping: TClipping);
begin
  FBar.Render(ADBitmap, ATransformation, AClipping);
  FBorder.Render(ADBitmap, ATransformation, AClipping);
end;

procedure TRectangle.SetPosition(const Value: TRect2D);
begin
  inherited;

  FBar.Position := Value;
  FBorder.Position := Value;
end;

end.
