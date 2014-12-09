unit uWHRect;

interface

uses
  Windows;

type
  TWHRect = record
{$if CompilerVersion > 20}
    function Right: LongInt;
    function Bottom: LongInt;
    function Rect: TRect;
{$ifend}

    case Integer of
      0: (Left, Top, Width, Height: Longint);
      1: (TopLeft: TPoint; Size: TSize);
  end;

function WHRectRight(const WHRect: TWHRect): LongInt;
function WHRectBottom(const WHRect: TWHRect): LongInt;
function WHRectToRect(const WHRect: TWHRect): TRect;
function RectToWHRect(const Rect: TRect): TWHRect;

implementation

function WHRectRight(const WHRect: TWHRect): LongInt;
begin
  Result := WHRect.Left + WHRect.Width - 1;
end;

function WHRectBottom(const WHRect: TWHRect): LongInt;
begin
  Result := WHRect.Top + WHRect.Height - 1;
end;

function WHRectToRect(const WHRect: TWHRect): TRect;
begin
  Result.Left := WHRect.Left;
  Result.Top := WHRect.Top;
  Result.Right := WHRectRight(WHRect);
  Result.Bottom := WHRectBottom(WHRect);
end;

function RectToWHRect(const Rect: TRect): TWHRect;
begin
  Result.Left := Rect.Left;
  Result.Top := Rect.Top;
  Result.Width := Rect.Right - Rect.Left + 1;
  Result.Height := Rect.Bottom - Rect.Top + 1;
end;

{ TWHRect }

{$if CompilerVersion > 20}
function TWHRect.Right: LongInt;
begin
  Result := Left + Width - 1;
end;

function TWHRect.Bottom: LongInt;
begin
  Result := Top + Height - 1;
end;

function TWHRect.Rect: TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Right := Right;
  Result.Bottom := Bottom;
end;
{$ifend}

end.
