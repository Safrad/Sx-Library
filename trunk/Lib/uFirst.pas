unit uFirst;

{$WARN SYMBOL_PLATFORM OFF}

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
	{$ifndef LINUX}
	if IsRelease then
		NoErrMsg := True;
	{$endif}

//	if IsDebug then
//		ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
end.
