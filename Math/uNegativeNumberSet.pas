unit uNegativeNumberSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TNegativeNumberSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TNegativeNumberSet }

function TNegativeNumberSet.Contains(const ANumber: S8): BG;
begin
  Result := ANumber < 0;
end;

function TNegativeNumberSet.Description: string;
begin
  Result := 'Negative number';
end;

end.

