Contents
--------

1. Setup Notes for enabling the use of FASTMM by DUnit
2. Note for Delphi 5 to Delphi 2005 for Leak reporting on shutdown
3. Note for Delphi 6 users

1. Setup Notes for enabling the use of FASTMM by DUnit
------------------------------------------------------

FASTMM support in DUnit can be enabled to provide automatic memory leak detection and requires FastMM4 v427 or higher for correct operation.

If FastMM is not yet installed, download and install FastMM4 to a suitable folder. Versions from 427 and above have been tested and do not require modification.

It is not necessary to replace Borland's memory manager at this or any stage for the modified DUnit code to execute.

Your DUnit (.DPR) test projects require the addition of the following code block (it is already included in the standard DUnit projects).

uses
  {$IFDEF FASTMM}    // From Project | Options | Directories/Conditionals
    ($IFNDEF CLR}
      FastMM4,
    {$ENDIF}
  {$ENDIF}

2. Note for Delphi 5 to Delphi 2005 for Leak reporting on shutdown
----------------------------------------------------------------

Delphi / BDS 2006 and later already include FastMM support for leak reporting on shutdown and so does not require the conditional definitions mentioned below for earlier versions of Delphi.

To enable leak reporting on shutdown by FastMM when using Delphi versions 5 through to 2005 include "ManualLeakReportingControl" in Project - Options - Directories/Conditionals - Conditional defines.

Alternatively enable "ManualLeakReportingControl" in "FastMM4Options.inc". 

Finally, ensure that FastMM4 and the new DUnit code are on the library search paths AHEAD of any Delphi standard paths.

The example project in the folder <example\MemLeakDetect> shows some manipulation of the memory leak detection.

Most of FastMM's additional capability besides just being a good memory manager is turned off by default. This was done to minimize the impact of FastMM on the unit tests until people have had on opportunity to better evaluate it's functionality. There may be scope for more sophisticated use of FastMM's memory leak reporting in the future. It would be really nice if someone better versed in FastMM could chip in and run with this initiative.

2. Note for Delphi 6 users
--------------------------

Due to some known memory leaks in Delphi 6 it is necessary to include the following statements in every application that is compiled with Delphi 6 and linked with FASTMM.

{$IFDEF VER140}
  {$IFDEF ManualLeakReportingControl}
    // It is Delphi 6 and FASTMM so register its known memory leaks
    RegisterExpectedMemoryLeak(36, 1); // TWinHelpViewer x 1
    RegisterExpectedMemoryLeak(20, 3); // TObjectList x 3
    RegisterExpectedMemoryLeak(20, 3); // Unknown x 3
    RegisterExpectedMemoryLeak(52, 1); // THelpManager x 1
  {$ENDIF}
{$ENDIF}

If the leaks are not registered as described above then the application will always display the standard FASTMM leak report indicating a small number of memory leaks at the end of the run.

These lines are included in the four DUnit projects shown below, so no action should be necessary for those projects, but the lines have NOT been include in the rest of the projects in the example folder, since they are not configured to use FASTMM at all.

	src\DUnit.dpr
	tests\DUnitTestLib.dpr
	tests\DUnitTests.dpr
	examples\MemLeakDetect\LeakTestDemo.dpr
