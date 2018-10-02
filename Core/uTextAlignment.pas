unit uTextAlignment;

interface

uses
  uTypes;

type
  THorizontalAlignment = (haLeft, haCenter, haRight);

  TVerticalAlignment = (vaTop, vaCenter, vaBottom);

  TTextAlignment = record
    Horizontal: THorizontalAlignment;
    Vertical: TVerticalAlignment;
  end;

var
  CenterAlignment: TTextAlignment = (Horizontal: haCenter; Vertical: vaCenter);

implementation

end.
