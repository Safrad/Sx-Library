/// Fisher–Yates Shuffle Generator
/// https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle
/// Works as incremental

unit uShuffleGenerator;

interface

uses
  uTypes,
  uSxRandomGenerator;

type
  TShuffleGenerator = class
  private
    FSxRandomGenerator: TSxRandomGenerator;
    FNumbers: array of U4;
    FGeneratedNumbers: U4;
    FCount: U4;
    procedure SetCount(const AValue: U4);
  public
    constructor Create;
    destructor Destroy; override;

    function GetNextNumber: U4;
    procedure Reset;

    property GeneratedNumbers: U4 read FGeneratedNumbers;
    property Count: U4 read FCount write SetCount;
  end;

implementation

uses
  SysUtils,

  uMath;

{ TShuffleGenerator }

constructor TShuffleGenerator.Create;
begin
  inherited;

  FSxRandomGenerator := TSxRandomGenerator.Create;
  FCount := 0;
  FGeneratedNumbers := 0;
end;

destructor TShuffleGenerator.Destroy;
begin
  try
    FSxRandomGenerator.Free;
  finally
    inherited;
  end;
end;

function TShuffleGenerator.GetNextNumber: U4;
var
  RandomIndex: U4;
begin
  if FGeneratedNumbers >= FCount then
    raise Exception.Create('No more numbers available.');

  RandomIndex := FSxRandomGenerator.RangeExactDistributionU4(FGeneratedNumbers, FCount - 1);
  Exchange(FNumbers[FGeneratedNumbers], FNumbers[RandomIndex]);

  Result := FNumbers[FGeneratedNumbers];
  Inc(FGeneratedNumbers);
end;

procedure TShuffleGenerator.Reset;
begin
  FGeneratedNumbers := 0;
  SetLength(FNumbers, 0);
  SetLength(FNumbers, FCount);
  if FCount > 0 then
    FillOrderU4(FNumbers[0], FCount);
end;

procedure TShuffleGenerator.SetCount(const AValue: U4);
begin
  if FCount <> AValue then
  begin
    FCount := AValue;
    Reset;
  end;
end;

end.
