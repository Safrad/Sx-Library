unit uStereoChannels;

interface

uses
  uChannels;

type
  TStereoChannels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uGeometry2D,
  uChannel,
  uSpeakerId;

{ TStereoChannels }

constructor TStereoChannels.Create;
begin
  inherited;

  SetLength(FChannelList, 2);

  FChannelList[0].SpeakerId := siFrontLeft;
  FChannelList[0].Position := CreatePoint2D(-1, 0);

  FChannelList[1].SpeakerId := siFrontRight;
  FChannelList[1].Position := CreatePoint2D(+1, 0);
end;

end.
