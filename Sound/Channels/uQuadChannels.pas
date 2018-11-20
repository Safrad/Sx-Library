unit uQuadChannels;

interface

uses
  uChannels;

type
  TQuadChannels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uSpeakerId,
  uGeometry2D,
  uChannel;

{ TQuadChannels }

constructor TQuadChannels.Create;
begin
  inherited;

  SetLength(FChannelList, 4);

  FChannelList[0].SpeakerId := siFrontLeft;
  FChannelList[0].Position := CreatePoint2D(-1, -1);

  FChannelList[1].SpeakerId := siFrontRight;
  FChannelList[1].Position := CreatePoint2D(+1, -1);

  FChannelList[2].SpeakerId := siBackLeft;
  FChannelList[2].Position := CreatePoint2D(-1, +1);

  FChannelList[3].SpeakerId := siBackRight;
  FChannelList[3].Position := CreatePoint2D(+1, +1);
end;

end.
