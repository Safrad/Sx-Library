unit uSurroundChannels;

interface

uses
  uChannels;

type
  TSurroundChannels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uSpeakerId,
  uGeometry2D,
  uChannel;

{ TSurroundChannels }

constructor TSurroundChannels.Create;
begin
  inherited;

  SetLength(FChannelList, 4);

  FChannelList[0].SpeakerId := siFrontLeft;
  FChannelList[0].Position := CreatePoint2D(-1, -1);

  FChannelList[1].SpeakerId := siFrontRight;
  FChannelList[1].Position := CreatePoint2D(+1, -1);

  FChannelList[2].SpeakerId := siFrontCenter;
  FChannelList[2].Position := CreatePoint2D(0, -1);

  FChannelList[3].SpeakerId := siBackCenter;
  FChannelList[3].Position := CreatePoint2D(0, +1);
end;

end.
