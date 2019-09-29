unit uFirst;

{$WARN SYMBOL_PLATFORM OFF}

interface

{.$define FastMM4} // Optional

uses
{$ifdef FastMM4}
  FastMM4,
{$endif}  
  uTypes;

var
  ApplicationStartTicks: U8;

implementation

uses
  Diagnostics;

initialization
{$IFNDEF NoInitialization}
  NoErrMsg := IsRelease;
  Diagnostics.TStopwatch.Create; // required for hi resolution GetTimeStamp (calls InitStopwatchType)
  ApplicationStartTicks := Diagnostics.TStopwatch.GetTimeStamp;

  {$ifndef FastMM4}
  ReportMemoryLeaksOnShutdown := IsDebug; // Optional, can take long time for many unfreed objects
  {$endif}
{$ENDIF NoInitialization}
end.
