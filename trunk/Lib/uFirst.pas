unit uFirst;

interface

{$if CompilerVersion < 18}
uses
	FastMM4;
{$ifend}

implementation

initialization
	{$ifndef LINUX}
	if IsRelease then
		NoErrMsg := True;
	{$endif}

//	if IsDebug then
//		ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
end.
