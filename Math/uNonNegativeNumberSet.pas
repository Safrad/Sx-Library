unit uNonNegativeNumberSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TNonNegativeNumberSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TNonNegativeNumberSet }

function TNonNegativeNumberSet.Contains(const ANumber: S8): BG;
begin
  Result := ANumber >= 0;
end;

function TNonNegativeNumberSet.Description: string;
begin
  Result := 'Non-negative number';
end;

end.

