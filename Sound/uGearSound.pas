unit uGearSound;

interface

uses
  uTypes,
  uSoundItem;

type
  TGearSound = class(TSoundItem)
  private
    FSpeed: FG;
    procedure SetSpeed(const Value: FG);
  public
    property Speed: FG read FSpeed write SetSpeed;
  end;

implementation

{ TGearSound }

procedure TGearSound.SetSpeed(const Value: FG);
begin
  FSpeed := Value;
end;

end.
