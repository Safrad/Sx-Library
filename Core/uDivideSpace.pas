unit uDivideSpace;

interface

uses
  uTypes, uGraph, uMath, uDivideSpaceOptions, Windows;

type
  TDivideSpace = class
  private
    procedure CheckDivideSpaceOptions(const Value: TDivideSpaceOptions);
  public
    function Divide1D(const ARange: TRange; const AOneSideDivideSpaceOptions: TOneSideDivideSpaceOptions): TRangeArray;
    function Divide2D(const AGroup: TRect; const ADivideSpaceOptions: TDivideSpaceOptions): TRectArray;
  end;

implementation

uses
  Types, Math, SysUtils;

function Separators(const F, T: SG; const AMaxParts: SG): TRangeArray;
var
  V1, V2: SG;
  i: SG;
  MaxParts: SG;
begin
  MaxParts := Range(1, AMaxParts, T - F + 1);

  SetLength(Result, MaxParts);
  V2 := F - 1;
  i := 0;
  while i < MaxParts do
  begin
    V1 := V2 + 1;
    V2 := F + ((i + 1) * (T - F)) div MaxParts;
    Result[i].F := V1;
    Result[i].T := V2;
    Inc(i);
  end;
end;

{ TDivideSpace }

function TDivideSpace.Divide2D(const AGroup: TRect; const ADivideSpaceOptions: TDivideSpaceOptions): TRectArray;
var
  HorizontalSeparators, VerticalSeparators: TRangeArray;
  x, y: SG;
  i: SG;
begin
  CheckDivideSpaceOptions(ADivideSpaceOptions);

  HorizontalSeparators := Divide1D(CreateRange(AGroup.Left, AGroup.Right), ADivideSpaceOptions.Horizontal);
  VerticalSeparators := Divide1D(CreateRange(AGroup.Top, AGroup.Bottom), ADivideSpaceOptions.Vertical);

  SetLength(Result, Length(HorizontalSeparators) * Length(VerticalSeparators));
  i := 0;
  for y := 0 to Length(VerticalSeparators) - 1 do
    for x := 0 to Length(HorizontalSeparators) - 1 do
    begin
      Result[i] := Rect(HorizontalSeparators[x].F, VerticalSeparators[y].F, HorizontalSeparators[x].T,
        VerticalSeparators[y].T);
      Inc(i);
    end;
end;

function TDivideSpace.Divide1D(const ARange: TRange; const AOneSideDivideSpaceOptions: TOneSideDivideSpaceOptions):
  TRangeArray;
begin
  if AOneSideDivideSpaceOptions.Divided = False then
  begin
    SetLength(Result, 1);
    Result[0] := ARange;
  end
  else
  begin
    Result := Separators(ARange.F, ARange.T, RoundDiv((ARange.T - ARange.F + 1), AOneSideDivideSpaceOptions.Size));
  end;
end;

procedure TDivideSpace.CheckDivideSpaceOptions(const Value: TDivideSpaceOptions);
begin
  if (Value.Horizontal.Divided) and (Value.Horizontal.Size <= 0) then
    raise EArgumentException.Create('Invalid Horizontal.Size');
  if (Value.Vertical.Divided) and (Value.Vertical.Size <= 0) then
    raise EArgumentException.Create('Invalid Vertical.Size');
end;

end.

