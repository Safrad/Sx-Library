unit uBuildConfiguration;

interface

uses
  uTypes;

type
  TBuildConfiguration = record
    Optimization: BG;
    StackFrames: BG;
    PentiumSafeFDiv: BG;
    RecordFieldAlignment: UG;

    RangeChecking: BG;
    IOChecking: BG;
    OverflowChecking: BG;

    DebugInformation: BG;
    LocalSymbols: BG;
    ReferenceInfo: BG;
    Assertions: BG;
    RuntimeTypeInformation: BG;
    ShowHints: BG;
  end;

  BuildConfiguration = class
  private
    class var FBuildConfiguration: TBuildConfiguration;
    class procedure CheckValue(const AActual, AExpected: BG; const AName: string); overload;
    class procedure CheckValue(const AActual, AExpected: SG; const AName: string); overload;
  public
    class constructor Create;
    class procedure CheckCorrect;
    class function AsString: string;
    class property Get: TBuildConfiguration read FBuildConfiguration;
  end;

implementation

uses
  SysUtils,

  uStrings,
  uChar;

procedure AppendBoolValue(var AText: string; const ABuildParam: string; const ATurnedOn: BG);
begin
  AText := AText + ABuildParam;
  if ATurnedOn then
    AText := AText + '+'
  else
    AText := AText + CharMinus;
  AText := AText + LineSep;
end;

{ BuildConfiguration }

class function BuildConfiguration.AsString: string;
var
  s: string;

begin
  Result := '';

  AppendLine(Result, 'Code generation:');
  AppendBoolValue(Result, 'o (Optimization)', FBuildConfiguration.Optimization);
  AppendBoolValue(Result, 'w (Stack frames)', FBuildConfiguration.StackFrames);
  AppendBoolValue(Result, 'u (Pentium-safe FDIV)', FBuildConfiguration.PentiumSafeFDiv);
  AppendLine(Result, 'a (Record field alignment): ' + IntToStr(FBuildConfiguration.RecordFieldAlignment));

  AppendLine(Result, 'Runtime errors:');
  AppendBoolValue(Result, 'i (I/O checking)', FBuildConfiguration.IOChecking);
  AppendBoolValue(Result, 'q (Overflow checking)', FBuildConfiguration.OverflowChecking);
  AppendBoolValue(Result, 'r (Range checking)', FBuildConfiguration.RangeChecking);

  AppendLine(Result, 'Debugging:');
  AppendBoolValue(Result, 'd (Debug information)', FBuildConfiguration.DebugInformation);
  AppendBoolValue(Result, 'l (Local symbols)', FBuildConfiguration.LocalSymbols);
  AppendBoolValue(Result, 'y (Reference info)', FBuildConfiguration.ReferenceInfo);
  AppendBoolValue(Result, 'c (Assertions)', FBuildConfiguration.Assertions);
  AppendBoolValue(Result, 'm (Runtime type information)', FBuildConfiguration.RuntimeTypeInformation);
  AppendBoolValue(Result, 'h (Show Hints)', FBuildConfiguration.ShowHints);
end;

class procedure BuildConfiguration.CheckValue(const AActual, AExpected: BG; const AName: string);
begin
  if AActual <> AExpected then
    raise Exception.Create(
      'Build configuration ' + AddQuoteF(AName) +
      ' value ' + AddQuoteF(FalseTrue[SG(AActual)]) +
      ' is invalid, expected ' + AddQuoteF(FalseTrue[SG(AExpected)])
    );
end;

class procedure BuildConfiguration.CheckValue(const AActual, AExpected: SG; const AName: string);
begin
  if AActual <> AExpected then
    raise Exception.Create(
      'Build configuration ' + AddQuoteF(AName) +
      ' value ' + AddQuoteF(IntToStr(AActual)) +
      ' is invalid, expected ' + AddQuoteF(IntToStr(AExpected))
    );
end;

class constructor BuildConfiguration.Create;
type
  TAlignedData = U8;

  TTestRecord = record
    BaseData: U1;
    AlignedData: TAlignedData;
  end;
begin
  FBuildConfiguration.Optimization := {$ifopt o+}True{$else}False{$endif};
  FBuildConfiguration.StackFrames := {$ifopt w+}True{$else}False{$endif};
  FBuildConfiguration.PentiumSafeFDiv := {$ifopt u+}True{$else}False{$endif};
  FBuildConfiguration.RecordFieldAlignment := SizeOf(TTestRecord) - SizeOf(TAlignedData);

  FBuildConfiguration.IOChecking := {$ifopt i+}True{$else}False{$endif};
  FBuildConfiguration.OverflowChecking := {$ifopt q+}True{$else}False{$endif};
  FBuildConfiguration.RangeChecking := {$ifopt r+}True{$else}False{$endif};

  FBuildConfiguration.DebugInformation := {$ifopt d+}True{$else}False{$endif};
  FBuildConfiguration.LocalSymbols := {$ifopt l+}True{$else}False{$endif};
  FBuildConfiguration.ReferenceInfo := {$ifopt y+}True{$else}False{$endif};
  FBuildConfiguration.Assertions := {$ifopt c+}True{$else}False{$endif};
  FBuildConfiguration.RuntimeTypeInformation := {$ifopt m+}True{$else}False{$endif};
  FBuildConfiguration.ShowHints := {$ifopt h+}True{$else}False{$endif};
end;

class procedure BuildConfiguration.CheckCorrect;
begin
  CheckValue(FBuildConfiguration.Optimization, IsRelease, 'Optimization');
  CheckValue(FBuildConfiguration.StackFrames, False, 'Stack frames');
  CheckValue(FBuildConfiguration.PentiumSafeFDiv, False, 'Pentium-safe FDIV');
  CheckValue(FBuildConfiguration.RecordFieldAlignment, 8, 'Record field alignment');

  CheckValue(FBuildConfiguration.IOChecking, True, 'I/O checking');
  CheckValue(FBuildConfiguration.OverflowChecking, IsDebug, 'Overflow checking');
  CheckValue(FBuildConfiguration.RangeChecking, IsDebug, 'Range checking');

  CheckValue(FBuildConfiguration.DebugInformation, IsDebug, 'Debug information');
  CheckValue(FBuildConfiguration.LocalSymbols, IsDebug, 'Local symbols');
  CheckValue(FBuildConfiguration.ReferenceInfo, IsDebug, 'Reference info');
  CheckValue(FBuildConfiguration.Assertions, IsDebug, 'Assertions');
//  CheckValue(FBuildConfiguration.RuntimeTypeInformation, , 'Runtime type information');
  CheckValue(FBuildConfiguration.ShowHints, True, 'Range checking');
end;

end.
