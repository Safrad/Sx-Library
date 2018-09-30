unit uOddNumbersSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TOddNumericalSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TOddNumericalSet }

function TOddNumericalSet.Contains(const ANumber: S8): BG;
begin
  Result := (ANumber and 1) <> 0;
end;

function TOddNumericalSet.Description: string;
begin
  Result := 'Odd number';
end;

end.
