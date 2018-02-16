unit uByteFormatter;

interface

uses
  uTypes,
  uFormatter;

type
  TByteFormatter = class(TFormatter)
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;
  end;

implementation

uses
  uOutputFormat;

{ TByteFormatter }

function TByteFormatter.Format(const AValue: S8): string;
begin
  Result := BToStr(AValue);
end;

function TByteFormatter.Format(const AValue: FG): string;
begin
  Result := BToStr(Round(AValue));
end;

end.
