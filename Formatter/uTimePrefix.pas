unit uTimePrefix;

interface

uses
  uTypes,
  uUnitPrefix;

type
  TTimePrefix = class(TUnitPrefix)
  public
    const
      Year = 31536e3; // Not exact (365/366 days)
      Week = 6048e2;
      Day = 864e2;
      Hour = 36e2;
      Minute = 6e1;
      Second = 1e0;
      MilliSecond = 1e-3;
      MicroSecond = 1e-6;
      NanoSecond = 1e-9;
      PicoSecond = 1e-12;
      FemtoSecond = 1e-15;
      AttoSecond = 1e-18;
      ZeptoSecond = 1e-21;
      YoctoSecond = 1e-24;
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
        Year,
        Week,
        Day,
        Hour,
        Minute,
        Second,
        MilliSecond,
        MicroSecond,
        NanoSecond,
        PicoSecond,
        FemtoSecond,
        AttoSecond,
        ZeptoSecond,
        YoctoSecond
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

