unit uBinaryPrefix;

interface

uses
  uTypes,
  uUnitPrefix;

type
  TBinaryPrefix = class(TUnitPrefix)
  protected
    class function GetCount: SG; override;
    class function GetMultiplicator(const AIndex: SG): FG; override;
    class function GetString(const AIndex: SG): string; override;
  private
    const
      Yotta = 1.20892581961463E24;
      Zetta = 1.18059162071741E21;
      Exa = 1152921504606846976;
      Peta = 1125899906842624;
      Tera = 1099511627776;
      Giga = 1073741824;
      Mega = 1048576;
      Kilo = 1024;
      None = 1;
      Milli = 0.0009765625;
      Micro = 9.5367431640625E-7;
      Nano = 9.31322574615479E-10;
      Pico = 9.09494701772928E-13;
      Femto = 8.88178419700125E-16;
      Atto = 8.67361737988404E-19;
      Zepto = 8.470329472543E-22;
      Yocto = 8.27180612553028E-25;
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
  const
    Strings: array[TEnum] of string = (
      'Yi',
      'Zi',
      'Ei',
      'Pi',
      'Ti',
      'Gi',
      'Mi',
      'Ki',
      '',
      'mi',
      'µi',
      'ni',
      'pi',
      'fi',
      'ai',
      'zi',
      'yi');
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

{ TBinaryPrefix }

class function TBinaryPrefix.GetCount: SG;
begin
  Result := Length(Multiplicator);
end;

class function TBinaryPrefix.GetMultiplicator(const AIndex: SG): FG;
begin
  Result := Multiplicator[TEnum(AIndex)];
end;

class function TBinaryPrefix.GetString(const AIndex: SG): string;
begin
  Result := Strings[TEnum(AIndex)];
end;

end.
