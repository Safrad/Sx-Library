unit u5Point1Channels;

interface

uses
  uChannels;

type
  T5Point1Channels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uSpeakerId,
  uGeometry2D,
  uChannel;

{ T5Point1Channels }

constructor T5Point1Channels.Create;
begin
  inherited;

  SetLength(FChannelList, 6);

  FChannelList[0].SpeakerId := siFrontLeft;
  FChannelList[0].Position := CreatePoint2D(-1, -1);

  FChannelList[1].SpeakerId := siFrontRight;
  FChannelList[1].Position := CreatePoint2D(+1, -1);

  FChannelList[2].SpeakerId := siFrontCenter;
  FChannelList[2].Position := CreatePoint2D(+1, -1);

  FChannelList[3].SpeakerId := siLowFrequency;
  FChannelList[3].Position := CreatePoint2D(0, -1);

  FChannelList[4].SpeakerId := siBackLeft;
  FChannelList[4].Position := CreatePoint2D(-1, +1);

  FChannelList[5].SpeakerId := siBackRight;
  FChannelList[5].Position := CreatePoint2D(+1, +1);
end;

end.
