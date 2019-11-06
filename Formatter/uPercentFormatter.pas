unit uPercentFormatter;

interface

uses
  uTypes,
  uNumberFormatter;

type
  TPercentFormatter = class(TNumberFormatter)
  public
    function Format(const AValue: S8): string; override;
    function Format(const AValue: FG): string; override;
  end;

implementation

uses
  uChar,
  uStrings;

{ TPercentFormatter }

function TPercentFormatter.Format(const AValue: FG): string;
var
  AbsValue: FG;
begin
  AbsValue := Abs(AValue);
  if AbsValue >= 10 then
    Result := inherited Format(AValue) + CharTimes
  else if (AbsValue > 0) and (AbsValue < 0.01) then
    Result := inherited Format(AValue * 1000) + CharUnbrokableSpace + '‰'
  else
    Result := inherited Format(AValue * 100) + CharUnbrokableSpace + '%';
end;

function TPercentFormatter.Format(const AValue: S8): string;
begin
  Result := Format(AValue / 1);
end;

end.
