unit uInfiniteLevel;

interface

uses
  uTypes,
  uCustomLevel;

type
  TInfiniteLevel = class(TCustomLevel)
  public
    function GetAsString: string; override;
  end;

implementation

{ TInfiniteLevel }

function TInfiniteLevel.GetAsString: string;
begin
  Result := 'Infinite';
end;

end.
