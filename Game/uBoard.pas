unit uBoard;

interface

uses
  uTypes;

type
  TBoard<T> = class
  private
    FSquares: array of T;

    FWidth: SG;
    FHeight: SG;
    FOutOfBoard: T;
    procedure SetOutOfBoard(const Value: T);
    function SquareToStr(const AX, AY: SG): string;
  public
    // Setup
    procedure SetSize(const AWidth, AHeight: SG);
    property OutOfBoard: T read FOutOfBoard write SetOutOfBoard;

    // Process
    procedure Clear;
    procedure Fill(const ASquare: T);
    procedure FillBorder(const ASquare: T);
    procedure SetSquare(const AX, AY: SG; const ASquare: T);

    // Output
    function GetSquare(const AX, AY: SG): T;
    property Width: SG read FWidth;
    property Height: SG read FHeight;
  end;

implementation

uses
  SysUtils;

{ TBoard<T> }

procedure TBoard<T>.Clear;
begin
  ClearMemory(FSquares, Length(FSquares));
end;

procedure TBoard<T>.Fill(const ASquare: T);
var
  i: SG;
begin
  for i := 0 to Length(FSquares) - 1 do
  begin
    FSquares[i] := ASquare;
  end;
end;

procedure TBoard<T>.FillBorder(const ASquare: T);
var
  i: SG;
begin
  for i := 0 to Width - 1 do
  begin
    SetSquare(i, 0, ASquare);
    SetSquare(i, Height - 1, ASquare);
  end;

  for i := 0 to Height - 1 do
  begin
    SetSquare(0, i, ASquare);
    SetSquare(Width - 1, i, ASquare);
  end;
end;

function TBoard<T>.GetSquare(const AX, AY: SG): T;
begin
  if (AX >= 0) and (AX < Width) and (AY >= 0) and (AY < Height) then
    Result := FSquares[AX + AY * Width]
  else
    Result := OutOfBoard;
end;

procedure TBoard<T>.SetOutOfBoard(const Value: T);
begin
  FOutOfBoard := Value;
end;

procedure TBoard<T>.SetSize(const AWidth, AHeight: SG);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FSquares, 0);
  SetLength(FSquares, AWidth * AHeight);
end;

procedure TBoard<T>.SetSquare(const AX, AY: SG; const ASquare: T);
begin
  if (AX >= 0) and (AX < Width) and (AY >= 0) and (AY < Height) then
    FSquares[AX + AY * Width] := ASquare
  else
    raise EArgumentException.Create('Square ' + SquareToStr(AX, AY) + ' is out of range ' + SquareToStr(FWidth, FHeight) + '.');
end;

function TBoard<T>.SquareToStr(const AX, AY: SG): string;
begin
  Result := '[' + IntToStr(AX) +  '; ' + IntToStr(AY) + ']';
end;

end.
