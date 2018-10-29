unit uFirst;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uTypes;

var
  ApplicationStartTicks: U8;

implementation

uses
  Windows;

initialization
{$IFNDEF NoInitialization}
  QueryPerformanceCounter(TLargeInteger(ApplicationStartTicks));
	if IsRelease then
		NoErrMsg := True;

	if IsDebug then
		ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
{$ENDIF NoInitialization}
end.
