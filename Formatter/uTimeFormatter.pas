unit uTimeFormatter;

interface

uses
  uTypes,
  uNumberFormatter;

type
  TTimeFormatter = class(TNumberFormatter)
  public
    function Format(const AValue: S8): string; override;
  end;

implementation

uses
  uTimeSpan,
  uOutputFormat;

{ TTimeFormatter }

function TTimeFormatter.Format(const AValue: S8): string;
var
  TimeSpan: TTimeSpan;
begin
  TimeSpan.Ticks := AValue;
  Result := MsToStr(TimeSpan.Milliseconds, diMSD, Precision);
end;

end.
