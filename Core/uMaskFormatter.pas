unit uMaskFormatter;

interface

uses
  uTypes,
  uFormatter;

type
  TMaskFormatter = class(TFormatter)
  private
    FMask: string;
    procedure SetMask(const Value: string);
  public
    property Mask: string read FMask write SetMask;

    function Format(const AValue: S8): string; override;
  end;

implementation

uses
  uOutputFormat;

{ TMaskFormatter }

function TMaskFormatter.Format(const AValue: S8): string;
begin
  Result := NToS(AValue, Mask);
end;

procedure TMaskFormatter.SetMask(const Value: string);
begin
  FMask := Value;
end;

end.
