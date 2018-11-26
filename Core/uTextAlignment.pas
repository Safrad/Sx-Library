unit uTextAlignment;

interface

uses
  uTypes,
  Windows;

type
  THorizontalAlignment = (haLeft, haCenter, haRight);

  TVerticalAlignment = (vaTop, vaCenter, vaBottom);

  TTextAlignment = record
    Horizontal: THorizontalAlignment;
    Vertical: TVerticalAlignment;
    function GetLeft(const ARect: TRect; const ATextWidth: SG): SG;
    function GetTop(const ARect: TRect; const ATextHeight: SG): SG;
  end;

var
  CenterAlignment: TTextAlignment = (Horizontal: haCenter; Vertical: vaCenter);

implementation

uses
  SysUtils;

{ TTextAlignment }

function TTextAlignment.GetLeft(const ARect: TRect; const ATextWidth: SG): SG;
begin
  case Horizontal of
    haLeft:
      Result := ARect.Left;
    haCenter:
      // Centered or one pixel to the left
      Result := ARect.Left + (ARect.Right - ARect.Left + 1 - ATextWidth) div 2;
    haRight:
      Result := ARect.Right - ATextWidth + 1;
    else
      raise EArgumentException.Create('Invalid TTextAlignment.Horizontal value.');
  end;
end;

function TTextAlignment.GetTop(const ARect: TRect; const ATextHeight: SG): SG;
begin
  case Vertical of
    vaTop:
      Result := ARect.Top;
    vaCenter:
      // Centered or one pixel up
      Result := ARect.Top + (ARect.Bottom - ARect.Top + 1 - ATextHeight) div 2;
    vaBottom:
      Result := ARect.Bottom - ATextHeight + 1;
    else
      raise EArgumentException.Create('Invalid TTextAlignment.Vertical value.');
  end;
end;

end.
