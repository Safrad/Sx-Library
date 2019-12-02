unit uDrawManager;

interface

uses
  uTypes,
  uSxRandomGenerator,

  uScore;

type
  TDrawManager = class
  private
    FRandomGenerator: TSxRandomGenerator;
    FAcceptDraw: BG;
    FOfferDraw: BG;
    FContemptValue: TScore;
    FOfferProbability: U8;
    function GetOfferProbability(const AScore: TScore): U8;
    procedure SetContemptValue(const Value: TScore);
  public
    constructor Create;
    destructor Destroy; override;

    // Input
    property ContemptValue: TScore read FContemptValue write SetContemptValue;

    // Process
    procedure Clear;
    procedure Update(const AScore: TScore);

    // Output
    property AcceptDraw: BG read FAcceptDraw;
    property OfferDraw: BG read FOfferDraw;
    property OfferProbability: U8 read FOfferProbability;
  end;

implementation

{ TDrawManager }

procedure TDrawManager.Clear;
begin
  FAcceptDraw := False;
  FOfferDraw := False;
end;

constructor TDrawManager.Create;
begin
  inherited;

  FRandomGenerator := TSxRandomGenerator.Create;
end;

destructor TDrawManager.Destroy;
begin
  try
    FRandomGenerator.Free;
  finally
    inherited;
  end;
end;

// Probability: 1 / Result
function TDrawManager.GetOfferProbability(const AScore: TScore): U8;
begin
  if AScore < 50 then
    Result := 5000
  else if AScore < 75 then
    Result := 300
  else if AScore < 90 then
    Result := 50
  else if AScore < 110 then
    Result := 7
  else if AScore < 125 then
    Result := 13
  else if AScore < 175 then
    Result := 20
  else if AScore < 250 then
    Result := 50
  else if AScore < 500 then
    Result := 250
  else
    Result := 1000;
end;

procedure TDrawManager.SetContemptValue(const Value: TScore);
begin
  FContemptValue := Value;
end;

procedure TDrawManager.Update(const AScore: TScore);
begin
  if AScore = scoNone then
    Exit;

  if AScore <= ContemptValue then
  begin
    FAcceptDraw := True;
    FOfferProbability := GetOfferProbability(-AScore);
    Assert(FOfferProbability > 0);

    FOfferDraw := FRandomGenerator.RangeU4(FOfferProbability - 1) = 0;
  end
  else
  begin
    FAcceptDraw := False;
    FOfferProbability := 0;
    FOfferDraw := False;
  end;
end;

end.
