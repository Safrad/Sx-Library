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
      YottaAsF = 1e24;
      ZettaAsF = 1e21;
      ExaAsF = 1e18;
      PetaAsF = 1e15;
      TeraAsF = 1e12;
      GigaAsF = 1e9;
      MegaAsF = 1e6;
      KiloAsF = 1e3;
      HectoAsF = 1e2;
      DecaAsF = 1e1;
      NoneAsF = 1e0;
      DeciAsF = 1e-1;
      CentiAsF = 1e-2;
      MilliAsF = 1e-3;
      MicroAsF = 1e-6;
      NanoAsF = 1e-9;
      PicoAsF = 1e-12;
      FemtoAsF = 1e-15;
      AttoAsF = 1e-18;
      ZeptoAsF = 1e-21;
      YoctoAsF = 1e-24;

      YottaDivisorAsF = 1e-24;
      ZettaDivisorAsF = 1e-21;
      ExaDivisorAsF = 1e-18;
      PetaDivisorAsF = 1e-15;
      TeraDivisorAsF = 1e-12;
      GigaDivisorAsF = 1e-9;
      MegaDivisorAsF = 1e-6;
      KiloDivisorAsF = 1e-3;
      HectoDivisorAsF = 1e-2;
      DecaDivisorAsF = 1e-1;
      NoneDivisorAsF = 1e0;
      DeciDivisorAsF = 1e1;
      CentiDivisorAsF = 1e2;
      MilliDivisorAsF = 1e3;
      MicroDivisorAsF = 1e6;
      NanoDivisorAsF = 1e9;
      PicoDivisorAsF = 1e12;
      FemtoDivisorAsF = 1e15;
      AttoDivisorAsF = 1e18;
      ZeptoDivisorAsF = 1e21;
      YoctoDivisorAsF = 1e24;

      ExaAsI = 1000000000000000000;
      PetaAsI = 1000000000000000;
      TeraAsI = 1000000000000;
      GigaAsI = 1000000000;
      MegaAsI = 1000000;
      KiloAsI = 1000;
      HectoAsI = 100;
      DecaAsI = 10;
      NoneAsI = 1;

      NoneDivisorAsI = 1;
      DeciDivisorAsI = 10;
      CentiDivisorAsI = 100;
      MilliDivisorAsI = 1000;
      MicroDivisorAsI = 1000000;
      NanoDivisorAsI = 1000000000;
      PicoDivisorAsI = 1000000000000;
      FemtoDivisorAsI = 1000000000000000;
      AttoDivisorAsI = 1000000000000000000;
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
        YottaAsF,
        ZettaAsF,
        ExaAsF,
        PetaAsF,
        TeraAsF,
        GigaAsF,
        MegaAsF,
        KiloAsF,
        NoneAsF,
        MilliAsF,
        MicroAsF,
        NanoAsF,
        PicoAsF,
        FemtoAsF,
        AttoAsF,
        ZeptoAsF,
        YoctoAsF
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
