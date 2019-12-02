// Ancesor for TChessMoveGenerator in uChessMoveGenerator

unit uMoveGenerator;

interface

uses
  uTypes,

  uScore,
  uPosition,
  uMoveList,
  uMoveStack;

type
  TMoveGenerator = class
  private
    FScore: TScore;
    FPosition: TPosition;
    FMoveStack: TMoveStack;
    procedure SetScore(const Value: TScore);
  protected
    procedure SetPosition(const Value: TPosition); virtual;
  public
    constructor Create;
    destructor Destroy; override;

    function Generate: PMoveList; virtual;
    procedure UpdateScore; virtual;
    procedure RemovePrevious;

    // Input
    property Position: TPosition read FPosition write SetPosition;

    // Output
    property Score: TScore read FScore write SetScore;
    procedure ScoreToOutput; virtual;
  end;

implementation

{ TMoveGenerator }

constructor TMoveGenerator.Create;
begin
  inherited;

  FMoveStack := TMoveStack.Create;
end;

destructor TMoveGenerator.Destroy;
begin
  FMoveStack.Free;

  inherited;
end;

function TMoveGenerator.Generate: PMoveList;
begin
  Assert(FPosition <> nil);

  Result := FMoveStack.GetNewMoveList;

	FScore := scoNone;
end;

procedure TMoveGenerator.RemovePrevious;
begin
  FMoveStack.RemovePrevious;
end;

procedure TMoveGenerator.ScoreToOutput;
begin
  // No code
end;

procedure TMoveGenerator.SetPosition(const Value: TPosition);
begin
  FPosition := Value;
end;

procedure TMoveGenerator.SetScore(const Value: TScore);
begin
  FScore := Value;
end;

procedure TMoveGenerator.UpdateScore;
begin
  // No code
end;

end.
