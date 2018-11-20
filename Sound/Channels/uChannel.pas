unit uChannel;

interface

uses
  uGeometry2D,
  uSpeakerId;

type
  TChannel = record
  private
    FPosition: TPoint2D;
    FSpeakerIs: TSpeakerId;
    procedure SetPosition(const Value: TPoint2D);
    procedure SetSpeakerId(const Value: TSpeakerId);
  public
    property Position: TPoint2D read FPosition write SetPosition;
    property SpeakerId: TSpeakerId read FSpeakerIs write SetSpeakerId;
  end;

implementation

{ TChannel }

procedure TChannel.SetPosition(const Value: TPoint2D);
begin
  FPosition := Value;
end;

procedure TChannel.SetSpeakerId(const Value: TSpeakerId);
begin
  FSpeakerIs := Value;
end;

end.
