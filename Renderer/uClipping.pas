// https://en.wikipedia.org/wiki/Clipping_(computer_graphics)

unit uClipping;

interface

uses
  Types,

  uTypes;

type
  TClipping = class
  private
    FClippingRect: TRect;
    procedure SetClippingRect(const Value: TRect);
  public
    // Setup
// TODO     procedure AddClippingRect(const AClippingRect: TRect);
    property ClippingRect: TRect read FClippingRect write SetClippingRect;

    // Use
    function IsPartialyVisible(const AX, AY: SG): BG; overload;
    function IsPartialyVisible(const APoint: TPoint): BG; overload;
    function IsPartialyVisible(const ARect: TRect): BG; overload;
    function IsPartialyVisible(const AF, AT: TPoint): BG; overload;
    function IsWholeVisible(const ARect: TRect): BG; overload;
    function IsWholeVisible(const AF, AT: TPoint): BG; overload;

    // TODO : Add polygon

    function ClipRect(var ARectToClip: TRect): BG;
    function ClipRects(var ARectToClip, ASourceRect: TRect): BG;
  end;

implementation

uses
  uMath;

{ TClipping }

function TClipping.ClipRect(var ARectToClip: TRect): BG;
begin
	Result := True;

	if ARectToClip.Left > ARectToClip.Right then
    Exchange(ARectToClip.Left, ARectToClip.Right);
	if ARectToClip.Top > ARectToClip.Bottom then
    Exchange(ARectToClip.Top, ARectToClip.Bottom);

	if ARectToClip.Left > FClippingRect.Right then
    Exit;
	if ARectToClip.Left < FClippingRect.Left then
	begin
		ARectToClip.Left := FClippingRect.Left;
	end;

	if ARectToClip.Top > FClippingRect.Bottom then
    Exit;
	if ARectToClip.Top < FClippingRect.Top then
	begin
		ARectToClip.Top := FClippingRect.Top;
	end;

	if ARectToClip.Right < FClippingRect.Left then
    Exit;
	if ARectToClip.Right > FClippingRect.Right then
	begin
		ARectToClip.Right := FClippingRect.Right;
	end;
	if ARectToClip.Left > ARectToClip.Right then
    Exit;

	if ARectToClip.Bottom < FClippingRect.Top then
    Exit;
	if ARectToClip.Bottom > FClippingRect.Bottom then
	begin
		ARectToClip.Bottom := FClippingRect.Bottom;
	end;
	if ARectToClip.Top > ARectToClip.Bottom then
    Exit;

	Result := False;
end;

function TClipping.ClipRects(var ARectToClip, ASourceRect: TRect): BG;
begin
  Assert(ARectToClip.Width = ASourceRect.Width);
  Assert(ARectToClip.Height = ASourceRect.Height);

	Result := True;

	if ARectToClip.Left > ARectToClip.Right then
    Exchange(ARectToClip.Left, ARectToClip.Right);
	if ARectToClip.Top > ARectToClip.Bottom then
    Exchange(ARectToClip.Top, ARectToClip.Bottom);

	if ARectToClip.Left > FClippingRect.Right then
    Exit;
	if ARectToClip.Left < FClippingRect.Left then
	begin
    Inc(ASourceRect.Left, FClippingRect.Left - ARectToClip.Left);
		ARectToClip.Left := FClippingRect.Left;
	end;

	if ARectToClip.Top > FClippingRect.Bottom then
    Exit;
	if ARectToClip.Top < FClippingRect.Top then
	begin
    Inc(ASourceRect.Top, FClippingRect.Top - ARectToClip.Top);
		ARectToClip.Top := FClippingRect.Top;
	end;

	if ARectToClip.Right < FClippingRect.Left then
    Exit;
	if ARectToClip.Right > FClippingRect.Right then
	begin
    Dec(ASourceRect.Right, ARectToClip.Right - FClippingRect.Right);
		ARectToClip.Right := FClippingRect.Right;
	end;
	if ARectToClip.Left > ARectToClip.Right then
    Exit;

	if ARectToClip.Bottom < FClippingRect.Top then
    Exit;
	if ARectToClip.Bottom > FClippingRect.Bottom then
	begin
    Dec(ASourceRect.Bottom, ARectToClip.Bottom - FClippingRect.Bottom);
		ARectToClip.Bottom := FClippingRect.Bottom;
	end;
	if ARectToClip.Top > ARectToClip.Bottom then
    Exit;

	Result := False;
end;

function TClipping.IsPartialyVisible(const AX, AY: SG): BG;
begin
  Result := False;

  if (AX > FClippingRect.Right) or (AX < FClippingRect.Left) then
    Exit;
  if (AY > FClippingRect.Bottom) or (AY < FClippingRect.Top) then
    Exit;

  Result := True;
end;

function TClipping.IsPartialyVisible(const APoint: TPoint): BG;
begin
  Result := False;

  if (APoint.X > FClippingRect.Right) or (APoint.X < FClippingRect.Left) then
    Exit;
  if (APoint.Y > FClippingRect.Bottom) or (APoint.Y < FClippingRect.Top) then
    Exit;

  Result := True;
end;

function TClipping.IsPartialyVisible(const ARect: TRect): BG;
begin
  Result := False;

  if (ARect.Left > FClippingRect.Right) then
    Exit;
  if (ARect.Right < FClippingRect.Left) then
    Exit;
  if (ARect.Top > FClippingRect.Bottom) then
    Exit;
  if (ARect.Bottom < FClippingRect.Top) then
    Exit;

  Result := True;
end;

function TClipping.IsPartialyVisible(const AF, AT: TPoint): BG;
begin
  Result := True; // TODO
end;

function TClipping.IsWholeVisible(const AF, AT: TPoint): BG;
begin
  Result := True; // TODO
end;

function TClipping.IsWholeVisible(const ARect: TRect): BG;
begin
  Result := False;

  if (ARect.Left > FClippingRect.Right) or (ARect.Right > FClippingRect.Right) then
    Exit;
  if (ARect.Left < FClippingRect.Left) or (ARect.Right < FClippingRect.Left) then
    Exit;
  if (ARect.Top > FClippingRect.Bottom) or (ARect.Bottom > FClippingRect.Bottom) then
    Exit;
  if (ARect.Top < FClippingRect.Top) or (ARect.Bottom < FClippingRect.Top) then
    Exit;

  Result := True;
end;

procedure TClipping.SetClippingRect(const Value: TRect);
begin
  FClippingRect := Value;
end;

end.
