unit uGameVariants;

interface

uses
  uGameVariant,
  Generics.Collections;

type
  TGameVariants = class(TObjectList<TGameVariant>)
  public
    constructor Create;

    function FindByName(const AGameVaraintName: string): TGameVariant;
    function ToXBoardString: string;
  end;

var
  GameVariants: TGameVariants;

implementation

uses
  SysUtils,

  uTypes,
  uStrings;

{ TGameVariants }

constructor TGameVariants.Create;
begin
  inherited;

  OwnsObjects := True;
end;

function TGameVariants.FindByName(const AGameVaraintName: string): TGameVariant;
var
  i: SG;
  GameVaraintNameUpperCase: string;
begin
  GameVaraintNameUpperCase := UpperCase(AGameVaraintName);
  for i := 0 to Count - 1 do
  begin

    if UpperCase(Self[i].Name) = GameVaraintNameUpperCase then
    begin
      Result := Self[i];
      Exit;
    end;
  end;
  Result := nil;
end;

function TGameVariants.ToXBoardString: string;
var
  i: SG;
begin
  Result := '';
  for i := 0 to Count - 1 do
  begin
    AppendStr(Result, Self[i].Name);
    if i < Count - 1 then
      AppendStr(Result, ',');
  end;
end;

initialization
  GameVariants := TGameVariants.Create;
finalization
  GameVariants.Free;
end.
