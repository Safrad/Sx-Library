unit uFirst;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows,
  uTypes;

// Warning this unit can be used only in exe, not in dll/bpl
// in some cases it is possible to use it with correctly set ImageBase of dll
// Executable file is smaller
{$SETPEFLAGS IMAGE_FILE_RELOCS_STRIPPED}

var
  ApplicationStartTicks: U8;

implementation

initialization
{$IFNDEF NoInitialization}
  QueryPerformanceCounter(TLargeInteger(ApplicationStartTicks));
	if IsRelease then
		NoErrMsg := True;

//	if IsDebug then
//		ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
{$ENDIF NoInitialization}
end.
