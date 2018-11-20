unit uMonoChannels;

interface

uses
  uChannels;

type
  TMonoChannels = class(TChannels)
  public
    constructor Create;
  end;

implementation

uses
  uGeometry2D,
  uChannel,
  uSpeakerId;

{ TMonoChannels }

constructor TMonoChannels.Create;
begin
  inherited;

  SetLength(FChannelList, 1);

  FChannelList[0].SpeakerId := siFrontCenter;
  FChannelList[0].Position := CreatePoint2D(0, -1);
end;

end.
