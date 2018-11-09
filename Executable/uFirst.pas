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
  Windows;

initialization
{$IFNDEF NoInitialization}
  QueryPerformanceCounter(TLargeInteger(ApplicationStartTicks));
	if IsRelease then
		NoErrMsg := True;

	{$ifndef FastMM4}	
	if IsDebug then
		ReportMemoryLeaksOnShutdown := True; // Optional, can take long time for many unfreed objects
	{$endif}
{$ENDIF NoInitialization}
end.
