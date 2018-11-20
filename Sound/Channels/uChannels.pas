unit uChannels;

interface

uses
  uTypes,
  uChannel,
  uSpeakerConfiguration;

type
  TChannels = class
  private
    function GetCount: SG;
    function GetMask: U4;
  protected
    FChannelList: array of TChannel;
  public
    destructor Destroy; override;

    property Count: SG read GetCount;
    property Mask: U4 read GetMask;
    function Get(const AIndex: SG): TChannel;
  end;

implementation

{ TChannels }

destructor TChannels.Destroy;
begin
  SetLength(FChannelList, 0);

  inherited;
end;

function TChannels.Get(const AIndex: SG): TChannel;
begin
  Result := FChannelList[AIndex];
end;

function TChannels.GetMask: U4;
var
  ChannelIndex: Integer;
begin
  Result := 0;
  for ChannelIndex := 0 to Count - 1 do
  begin
    Result := Result or U4(Get(ChannelIndex).SpeakerId);
  end;
end;

function TChannels.GetCount: SG;
begin
  Result := Length(FChannelList);
end;

end.
