unit uMetricPrefix;

interface

uses
  uTypes,
  uUnitPrefix;

type
  TMetricPrefix = class(TUnitPrefix)
  protected
    class function GetCount: SG; override;
    class function GetMultiplicator(const AIndex: SG): FG; override;
    class function GetString(const AIndex: SG): string; override;
  public
    const
      Yotta = 1e24;
      Zetta = 1e21;
      Exa = 1e18;
      Peta = 1e15;
      Tera = 1e12;
      Giga = 1e9;
      Mega = 1e6;
      Kilo = 1e3;
      Hecto = 1e2;
      Deca = 1e1;
      None = 1e0;
      Deci = 1e-1;
      Centi = 1e-2;
      Milli = 1e-3;
      Micro = 1e-6;
      Nano = 1e-9;
      Pico = 1e-12;
      Femto = 1e-15;
      Atto = 1e-18;
      Zepto = 1e-21;
      Yocto = 1e-24;
  type
    TEnum = (
      mpYotta,
      mpZetta,
      pmExa,
      mpPeta,
      mpTera,
      mpGiga,
      mpMega,
      mpKilo,
      mpNone,
      mpMilli,
      mpMicro,
      mpNano,
      mpPico,
      mpFemto,
      mpAtto,
      mpZepto,
      mpYocto
    );
  private
    const
      Strings: array[TEnum] of string = (
        'Y',
        'Z',
        'E',
        'P',
        'T',
        'G',
        'M',
        'K',
        '',
        'm',
        'µ',
        'n',
        'p',
        'f',
        'a',
        'z',
        'y');
      Multiplicator: array[TEnum] of FG = (
        Yotta,
        Zetta,
        Exa,
        Peta,
        Tera,
        Giga,
        Mega,
        Kilo,
        None,
        Milli,
        Micro,
        Nano,
        Pico,
        Femto,
        Atto,
        Zepto,
        Yocto
      );
  end;

implementation

{ TMetricPrefix }

class function TMetricPrefix.GetCount: SG;
begin
  Result := Length(Multiplicator);
end;

class function TMetricPrefix.GetMultiplicator(const AIndex: SG): FG;
begin
  Result := Multiplicator[TEnum(AIndex)];
end;

class function TMetricPrefix.GetString(const AIndex: SG): string;
begin
  Result := Strings[TEnum(AIndex)];
end;

end.
