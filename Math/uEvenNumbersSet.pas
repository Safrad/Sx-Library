unit uEvenNumbersSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TEvenNumericalSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TEvenNumericalSet }

function TEvenNumericalSet.Contains(const ANumber: S8): BG;
begin
  Result := (ANumber and 1) = 0;
end;

function TEvenNumericalSet.Description: string;
begin
  Result := 'Odd number';
end;

end.

