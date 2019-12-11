unit uPermutationList;

interface

uses
  Generics.Collections,

  uTypes,
  uIntegerList;

type
  TPermutationList = class(TObjectList<TIntegerList>)
  public
    procedure Add(const APermutation: TArrayOfSG);
    function Exists(const APermutation: TArrayOfSG): BG;
  end;

implementation

uses
  Classes;

function ArrayOfSGToIntegerList(const APermutation: TArrayOfSG): TIntegerList;
var
  ol: TIntegerList;
  i: SG;
begin
  ol := TIntegerList.Create;
  for i := 0 to Length(APermutation) - 1 do
  begin
    ol.Add(APermutation[i])
  end;
  Result := ol;
end;

{ TPermutationList }

procedure TPermutationList.Add(const APermutation: TArrayOfSG);
begin
  inherited Add(ArrayOfSGToIntegerList(APermutation));
end;

function TPermutationList.Exists(const APermutation: TArrayOfSG): BG;
var
  I: SG;
  Permutation: TIntegerList;
begin
  Result := False;
  Permutation := ArrayOfSGToIntegerList(APermutation);
  try
    for i := 0 to Count - 1 do
    begin
      if TIntegerList(Self[i]).Equals(Permutation) then
      begin
        Result := True;
        Exit;
      end;
    end;
  finally
    Permutation.Free;
  end;
end;

end.
