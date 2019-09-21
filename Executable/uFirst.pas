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
  ApplicationStartTicks := Diagnostics.TStopwatch.GetTimeStamp;
  NoErrMsg := IsRelease;

  {$ifndef FastMM4}
  ReportMemoryLeaksOnShutdown := IsDebug; // Optional, can take long time for many unfreed objects
  {$endif}
{$ENDIF NoInitialization}
end.
