unit uNonPositiveNumberSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TNonPositiveNumberSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TNonPositiveNumberSet }

function TNonPositiveNumberSet.Contains(const ANumber: S8): BG;
begin
  Result := ANumber <= 0;
end;

function TNonPositiveNumberSet.Description: string;
begin
  Result := 'Non-negative number';
end;

end.

