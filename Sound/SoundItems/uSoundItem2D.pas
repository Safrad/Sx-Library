unit uSoundItem2D;

interface

uses
  uGeometry2D,
  uSoundItem;

type
  TSoundItem2D = class(TSoundItem)
  private
    FPosition: TPoint2D;
    procedure SetPosition(const Value: TPoint2D);
  public
    property Position: TPoint2D read FPosition write SetPosition;
  end;

implementation

{ TSoundItem2D }

procedure TSoundItem2D.SetPosition(const Value: TPoint2D);
begin
  FPosition := Value;
end;

end.
