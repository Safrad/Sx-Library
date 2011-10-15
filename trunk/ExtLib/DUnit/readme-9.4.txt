Recent additions to DUnit 9.4.x
-------------------------------

Support for Delphi 2007 and 2009.


Recent additions to DUnit 9.3.x
-------------------------------

FASTMM support, including optional memory leak checking on a per test basis, resulting in improved execution speed of many tests.
New DUnitW32 project group for Delphi 2005+ with new projects DUnitW32, DUnitTestLibW32 and UnitTestsW32.
New DUnit4Net project group for Delphi 8+ with existing project UnitTests4Net.
Automatic MadExcept support for stack tracing in TestFramework.pas.
Optional checking that each test case calls at least one CheckXXX method.
Optional detection of each test case that overrides the global GUI test case settings.
Optional halting of a repeated test on the first failure.
Carried the GUITestRunner.pas changes to the main form creation over to NGUITestRunner.pas and QGUITestRunner.pas.
Added a display refresh timer to GUITestRunner to ensure display update in long unit tests.
A couple of minor bug fixes.

DUnit is fully backwards compatible with older versions of DUnit and FASTMM, and fully supports all versions of Delphi from version 5 through to Delphi 2009 inclusive. 

Delphi 4 and prior are NOT supported in DUnit 9.x, however. The most recent version of DUnit that supports Delphi 4 is 7.2.1, which is still available for download from the SourceForge Web site http://sourceforge.net/projects/dunit

1. FASTMM support, including optional memory leak checking
----------------------------------------------------------

A new capability to detect memory leaks for the Setup/Run/Teardown process for each TestCase has been added.
This is universally controllable from the Main Menu-Options Menu by setting or clearing "Fail TestCase if memory leaked".
A corresponding run-time property can also be set True or False to override the universal GUI selection.
It is possible to allow leaks of 1..4 known sizes to occur per test case without raising an error while  still enabling failure if the leak size differs from the assigned values. If no leak occurs tests always pass.
In addition it is possible to fail an individual TestCase if some previously allocated memory is freed and not re-allocated.
As with leaks, recovered memory failure can be detected and optionally allowed for a pre-set size. This later feature is currently not available from the GUI and is controlled by run-time properties.
A GUI selectable option exists to Ignore leaks in the SetUp and TearDown procedures.
In addition FastMM can now be used to report memory leaks and leak types on shudown of the DUnit code. The functionality is GUI controlled so automated tests do not have to wait for operator intervention.


Discussion
FastMM provides a new memory utilisation snapshot capability.
DUnit has been enhanced to make use of this by taking snapshots of memory usage before and after the TestCase Setup/Run/Teardown process and comparing the difference. Because it is regular practice to put common memory assignment and release into the Setup and Teardown procedures, leak detection is performed after the TestCase Setup/Run/Teardown process and is not confined to just the Test Procedure. 

FastMM differs from early Borland memory managers and allocates predefined block sizes and overhead bytes, so allocation sizes go in chunks, not byte sized increments. Consequently leak sizes always exceed the actual memory required by Delphi code meaning that pre-calculation of leak sizes is not easily done within code just using SizeOf() functions. Setting up TestCases to allow for known leak sizes will generally require the user to run the test suite, note down the actual memory consumed then set the AllowedLeakSize property.  

See readme-fastmm.txt for more details on how to set this up.


2. Optional checking that each test case calls at least one CheckXXX method
---------------------------------------------------------------------------

Tests with no call to a check method may be giving a misleading pass, where the required call to a check method has unwittingly been bypassed or forgotten, and these are now optionally detectable.
The detection is controlled by selecting or clearing "Fail TestCase if no checks executed" on the Options sub menu of the Main Menu.

Discussion
DUnit, out of the box, will automatically fail empty TestCases if compiled with optimization on.
It is possible, however, that differences between the code generation of Delphi 5 through to Delphi 2005 prevent universal empty tests detection with optimization off.
The value of this empty test detection is marginal at best, simply because just a single compiled line of code defeats all detection.
So with the modified DUnit code any call to check within a test procedure sets a flag regardless of the optimization setting.
This flag is sensed post test and may raise an error if the GUI setting "Fail TestCase if no checks executed." is True. 
A corresponding run-time property can be set true or false to override the universal GUI selection.

Now generally, it would not make sense to force empty test failures at the GUI level then override it in a test procedure and never be the wiser. Consequently a new capability to view the run-time settings (post execution) has been added. This will be discussed later.
A Results View field has been introduced to display the number of testcases where the GUI selected run-time properties have been overridden at the testcase level. This count is to alert the user only and does not represent a form of failure or error.


3. Post execution Run-Time Property Inspector
---------------------------------------------

To view post execution Run-Time Properties, select an individual TestCase and use the keyboard shortcut Ctrl+Shift+T.
Alternatively right click on the TestCase and from the top item in the submenu select TestCase Properties.
The special submenu has three active menu items "Previous", "Run Current Test" and "Next", with obvious meaning. TestSuites are not selectable via this submenu.
It may not be obvious but the properties shown are applied either by the GUI setting or by the TestCase to itself during execution. These are not tested until return to DUnit code from each TestCase.
So, their meaning is really only useful after the test suite has executed, hence the added ability to run the test from within the property inspection popup menu.
This submenu is also designed to show each available property name and serve as a reminder of the exact spelling for coding purposes.
Clicking on the named property copies the property name to the clipboard, to reduce the potential for coding errors while setting up test code.

TestSuite Integrity
In a large test suite it is not possible to reliably scan through all the properties and interpret those which differ from the GUI settings.
A new class of warning is provided to automate this process.
To determine if any TestCases override the GUI settings select Main Menu,-Options- "Warn if Fail Test is over-ridden".
When the test suite executes, tests which pass, i.e. have no ordinary failures but have suppressed a failure warning, show as yellow instead of green.
These TestCases do not get counted as an error. This GUI selection is persisted. 
Another option is provided to show all TestCases with active run-time properties applied from the GUI or run-time properties.
This setting is not persisted however because it is only ever likely to require occasional one-off activation.

 
4. Optional halting of a repeated test on the first failure
-----------------------------------------------------------

A new TestCase decorator class has been added with a property to allow the user to halt a repeat test on the first failure.
The test time is now remembered after clicking in the TreeView and each TestCase's time is displayed as before.
Run counts and failure counts are also remembered after clicking in the TreeView. This property is not yet included in the run-time property inspector.
