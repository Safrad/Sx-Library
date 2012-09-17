unit uFirst;

{$WARN SYMBOL_PLATFORM OFF}

{$ifopt d-}
// Warning this unit can be used only in exe, not in dll/bpl
// in some cases it is possible to use it with correctly set ImageBase of dll
// Executable file is smaller
{$SETPEFLAGS IMAGE_FILE_RELOCS_STRIPPED}
{$endif}

interface

uses
  {$ifopt d+}
//  SafeMMInstall,
  {$else}
	FastMM4,
  {$endif}
  uTypes;

implementation

initialization
{$IFNDEF NoInitialization}
	{$ifndef LINUX}
	if IsRelease then
		NoErrMsg := True;
	{$endif}

//	if IsDebug then
//		ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
{$ENDIF NoInitialization}
end.
