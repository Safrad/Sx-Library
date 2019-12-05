unit uWHRect;

interface

uses
  Types,

  uTypes;

type
  TWHRect = record
{$if CompilerVersion > 20}
    function Right: S4;
    function Bottom: S4;
    function Rect: TRect;
{$endif}

    case Integer of
      0: (Left, Top, Width, Height: S4);
      1: (TopLeft: TPoint; Size: TSize);
  end;

function WHRectRight(const WHRect: TWHRect): S4;
function WHRectBottom(const WHRect: TWHRect): S4;
function WHRectToRect(const WHRect: TWHRect): TRect;
function RectToWHRect(const Rect: TRect): TWHRect;

implementation

function WHRectRight(const WHRect: TWHRect): S4;
begin
  Result := WHRect.Left + WHRect.Width - 1;
end;

function WHRectBottom(const WHRect: TWHRect): S4;
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
function TWHRect.Right: S4;
begin
  Result := Left + Width - 1;
end;

function TWHRect.Bottom: S4;
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
{$endif}

end.
