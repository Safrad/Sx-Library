unit uEngineSound;

interface

uses
  uTypes,
  uISoundItem,
  uSoundItem,
  uSoundMixer;

type
  TEngineSound = class(TInterfacedObject, ISoundItem)
  private
    FSpeed: FG;
    procedure SetSpeed(const Value: FG);

  protected
    function GetSample(const ASampleIndex: SG): TSample;
  public
    constructor Create;
    destructor Destroy; override;

    property Speed: FG read FSpeed write SetSpeed;
  end;

implementation

uses
  uTone,
  uGearSound;

const
  PetrolIdleSpeed = 800;
  DieselIdleSpeed = 540;
  SingleCylinderMotorcycleIdleSpeed = 1350;

{ TEngineSound }

constructor TEngineSound.Create;
begin
  inherited;

{  SetLength(FSounds, 5);
  FSounds[0] := TTone.Create;
  TTone(FSounds[0]).Frequency := 77;
  TTone(FSounds[0]).Amplitude := 0.5;

  FSounds[1] := TGearSound.Create;
  FSounds[1].Amplitude := 0.8;

  FSounds[2] := TTone.Create;
  FSounds[2].Amplitude := 0.7;

  FSounds[3] := TTone.Create;
  FSounds[3].Amplitude := 0.5;

  FSounds[4] := TTone.Create;
  FSounds[4].Amplitude := 0.2;}

  Speed := PetrolIdleSpeed;
end;

destructor TEngineSound.Destroy;
begin

  inherited;
end;

function TEngineSound.GetSample(const ASampleIndex: SG): TSample;
var
  i: SG;
begin
  Result := 0;
{  for i := 0 to Length(FSounds) - 1 do
  begin
    Inc(Result, FSounds
  end;}
end;

procedure TEngineSound.SetSpeed(const Value: FG);
begin
  FSpeed := Value;
{  FSounds[1].Frequency := Value;
  FSounds[2].Frequency := 2 * Value;
  FSounds[3].Frequency := Value / 3;
  FSounds[4].Frequency := 7 * Value;}
end;

end.
