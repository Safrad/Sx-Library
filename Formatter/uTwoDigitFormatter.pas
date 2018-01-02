unit uTwoDigitFormatter;

interface

uses
  uTypes,
  uMaskFormatter;

type
  TTwoDigitFormatter = class(TMaskFormatter)
  public
    constructor Create;
  end;

implementation


{ TTwoDigitFormatter }

constructor TTwoDigitFormatter.Create;
begin
  inherited;

  Mask := '#0';
end;

end.
