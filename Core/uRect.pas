unit uRect;

interface

uses
  uTypes,
  Types;

function CreateRect(const Left, Top, Width, Height: LongInt): TRect;
function SameRect(const R1, R2: TRect): BG;
function RectWidth(const Rect: TRect): LongInt;
function RectHeight(const Rect: TRect): LongInt;
function MoveRectInside(const SourceRect, ContainerRect: TRect; const PrefferSize: BG = True): TRect;
function InnerRect(const TargetRect: TRect; const SourceWidth, SourceHeight: SG): TRect;

implementation

uses
  uMath;

function CreateRect(const Left, Top, Width, Height: LongInt): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Left + Width;
  Result.Bottom := Top + Height;
end;

function SameRect(const R1, R2: TRect): BG;
begin
	Result := SameData(@R1, @R2, SizeOf(TRect));
end;

function RectWidth(const Rect: TRect): LongInt;
begin
  Result := Rect.Right - Rect.Left;
end;

function RectHeight(const Rect: TRect): LongInt;
begin
  Result := Rect.Bottom - Rect.Top;
end;

procedure MoveInside(var SourceFrom, SourceTo: LongInt; const ContainerFrom, ContainerTo: LongInt; const PrefferSize: BG = True);
begin
  if SourceFrom < ContainerFrom then
  begin
    if PrefferSize then
    begin
      Inc(SourceTo, ContainerFrom - SourceFrom);
      if SourceTo > ContainerTo then
        SourceTo := ContainerTo; // Size is changed
    end;

    SourceFrom := ContainerFrom;
  end
  else if SourceTo > ContainerTo then
  begin
    if PrefferSize then
    begin
      Dec(SourceFrom, SourceTo - ContainerTo);
      if SourceFrom < ContainerFrom then
        SourceFrom := ContainerFrom; // Size is changed
    end;

    SourceTo := ContainerTo;
  end;
end;

function MoveRectInside(const SourceRect, ContainerRect: TRect; const PrefferSize: BG = True): TRect;
begin
	Result := SourceRect;
  MoveInside(Result.Left, Result.Right, ContainerRect.Left, ContainerRect.Right, PrefferSize);
  MoveInside(Result.Top, Result.Bottom, ContainerRect.Top, ContainerRect.Bottom, PrefferSize);
end;

function InnerRect(const TargetRect: TRect; const SourceWidth, SourceHeight: SG): TRect;
var
  Width, Height: SG;
  Dif: SG;
begin
	Width := RectWidth(TargetRect);
	Height := RectHeight(TargetRect);
	if (Width = 0) or (Height = 0) then
    Exit;

  Dif := Width * SourceHeight - SourceWidth * Height;
  if Dif = 0 then
    // Same size
  else if Dif > 0 then
    Width := RoundDiv(Height * SourceWidth, SourceHeight)
  else // if Dif < 0 then
    Height := RoundDiv(Width * SourceHeight, SourceWidth);

  Result.Left := TargetRect.Left + (RectWidth(TargetRect) - Width) div 2;
  Result.Right := Result.Left + Width;
  Result.Top := TargetRect.Top + (RectHeight(TargetRect) - Height) div 2;
  Result.Bottom := Result.Top + Height;
end;

end.
