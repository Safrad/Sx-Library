unit uFirst;

interface

{$ifndef UNICODE}
uses
	FastMM4;
{$endif}

implementation

initialization
	{$ifndef LINUX}
	{$ifopt d-}
	NoErrMsg := True;
	{$endif}
	{$endif}

	{$ifopt d+}
	{$ifdef UNICODE}
	ReportMemoryLeaksOnShutdown := True; // Can take long time for many unfreed objects
	{$endif}
	{$endif}
end.
