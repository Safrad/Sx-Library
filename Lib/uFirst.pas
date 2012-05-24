unit uFirst;

interface

uses
	FastMM4,
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
