unit u5Point0Channels;

interface

uses
  uChannels;

type
  T5Point0Channels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uSpeakerId,
  uGeometry2D,
  uChannel;

{ T5Point0Channels }

constructor T5Point0Channels.Create;
begin
  inherited;

  SetLength(FChannelList, 5);

  FChannelList[0].SpeakerId := siFrontLeft;
  FChannelList[0].Position := CreatePoint2D(-1, -1);

  FChannelList[1].SpeakerId := siFrontRight;
  FChannelList[1].Position := CreatePoint2D(+1, -1);

  FChannelList[2].SpeakerId := siFrontCenter;
  FChannelList[2].Position := CreatePoint2D(+1, -1);

  FChannelList[3].SpeakerId := siBackLeft;
  FChannelList[3].Position := CreatePoint2D(-1, +1);

  FChannelList[4].SpeakerId := siBackRight;
  FChannelList[4].Position := CreatePoint2D(+1, +1);
end;

end.
