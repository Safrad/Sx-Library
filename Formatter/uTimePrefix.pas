unit uTimePrefix;

interface

uses
  uTypes,
  uUnitPrefix,
  uMetricPrefix;

type
  TTimePrefix = class(TUnitPrefix)
  public
    const
      YearAsF = 31536e3; // Not exact (365/366 days)
      WeekAsF = 6048e2;
      DayAsF = 864e2;
      HourAsF = 36e2;
      MinuteAsF = 6e1;
      SecondAsF = 1e0;

      YearAsI = 31536000; // Not exact (365/366 days)
      WeekAsI = 604800;
      DayAsI = 86400;
      HourAsI = 3600;
      MinuteAsI = 60;
      SecondAsI = 1;
  protected
    class function GetCount: SG; override;
    class function GetMultiplicator(const AIndex: SG): FG; override;
    class function GetString(const AIndex: SG): string; override;
  private
    type
      TEnum = (
        mpYear,
        mpWeek,
        mpDay,
        mpHour,
        mpMinute,
        mpSecond,
        mpMilliSecond,
        mpMicroSecond,
        mpNanoSecond,
        mpPicoSecond,
        mpFemtoSecond,
        mpAttoSecond,
        mpZeptoSecond,
        mpYoctoSecond
      );
    const
      Strings: array[TEnum] of string = (
        'years',
        'weeks',
        'days',
        'h',
        'm',
        's',
        'ms',
        'µs',
        'ns',
        'ps',
        'fs',
        'as',
        'zs',
        'ys');
      Multiplicator: array[TEnum] of FG = (
        YearAsF,
        WeekAsF,
        DayAsF,
        HourAsF,
        MinuteAsF,
        SecondAsF,
        TMetricPrefix.MilliAsF,
        TMetricPrefix.MicroAsF,
        TMetricPrefix.NanoAsF,
        TMetricPrefix.PicoAsF,
        TMetricPrefix.FemtoAsF,
        TMetricPrefix.AttoAsF,
        TMetricPrefix.ZeptoAsF,
        TMetricPrefix.YoctoAsF
      );
  end;

implementation

{ TTimePrefix }

class function TTimePrefix.GetCount: SG;
begin
  Result := Length(Multiplicator);
end;

class function TTimePrefix.GetMultiplicator(const AIndex: SG): FG;
begin
  Result := Multiplicator[TEnum(AIndex)];
end;

class function TTimePrefix.GetString(const AIndex: SG): string;
begin
  Result := Strings[TEnum(AIndex)];
end;

end.

