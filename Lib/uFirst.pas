unit uFirst;

interface

{$if CompilerVersion < 18}
uses
	FastMM4;
{$ifend}

implementation

initialization
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}

	{$ifopt d+}
	// ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
	{$endif}
end.
