unit uPositiveNumberSet;

interface

uses
  uTypes,
  uNumericalSet;

type
  TPositiveNumberlSet = class(TNumericalSet)
  public
    function Contains(const ANumber: S8): BG; override;
    function Description: string; override;
  end;

implementation

{ TPositiveNumberlSet }

function TPositiveNumberlSet.Contains(const ANumber: S8): BG;
begin
  Result := ANumber > 0;
end;

function TPositiveNumberlSet.Description: string;
begin
  Result := 'Positive number';
end;

end.
