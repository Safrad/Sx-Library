NLDJoystick
===========


HOW TO INSTALL / HOW TO ADD TO THE COMPONENT PALETTE?
-----------------------------------------------------
Instructions for Delphi 7:
1 - Download all the files from http://svn.nldelphi.com/nldelphi/opensource/ngln/NLDJoystick/
2 - Start Delphi
3 - Close All
4 - Open downloaded package named NLDJoystick70.dpk 
5 - When this file is opened in the editor, then first hit F12 to bring the package window in front
6 - Click the "Install" button

Alternative:

5 - Build package (Menu > Project > Build NLDJoystick70)
6 - Copy (CTRL-C) the output directory where the compiled package is placed (Menu > Project > Options > Directories/Conditionals > Directories > Output directory)
7 - Choose "Install Packages..." from Menu > Component
8 - Click Add
9 - Paste (CTRL_V) the copied directory in the file name edit box
10 - Click Open
11 - Select "NLDJoystick70.bpl"
12 - Click Open


HOW TO USE IN A PROJECT?
------------------------
1 - Drop one component for each attached joystick from the component palette onto the form designer
2 - Set the "Active" property to True (this will not succeed when there is no joystick connected)
3 - Implement the desired events
4 - Set additional properties
5 - Make sure the component source code can be found for the project (Menu > Project > Options > Directories/Conditionals > Directories > Search path)


CAPABILITIES
------------
- Supports up to 2 joysticks
- Supports up to 6 axis
- Supports up to 32 buttons
- Supports "Point of view" (POV)


MANUAL
------
Properties:
- AbsCenter, AbsMax, AbsMin: TJoyAbsPos;
  Reads the absolute position of the joystick on the 6 axis
- Active: Boolean default False;
  Reads or sets the whether the component is active
- Advanced: Boolean default False;
  When False: the code is message based, and the component supports only up to 2 axis and 4 buttons
  When True: the code is polling based, and the component supports all capabilities
- AxisCount: Byte;
  The actual number of available axis on the joystick
- Axises: TJoyAxises;
  The actual set of available axis on the joystick
- ButtonCount: Byte;
  The actual number of buttons on the joystick
- HasPOV: Boolean;
  Reads whether the joystick has POV available
- ID: TJoyID;
  The WinAPI identifier (0 or 1)
- PollingInterval: Cardinal default 40;
  Reads or sets the polling interval for an advanced joystick in milliseconds
- RepeatButtonDelay: Cardinal default 350;
  Reads or sets the repeat button delay duration in milliseconds
- RepeatMoveDelay: Cardinal default 350;
  Reads or sets the repeat move delay duration in milliseconds
- SuspendScreensaver: Boolean default False;
  Reads or sets whether joystick activity should suspend the screensaver
- ThresholdFactor: Double;
  Reads or sets the threshold factor

Events:
- OnButtonDown: TJoyButtonEvent;
- OnButtonUp: TJoyButtonEvent;
- OnMove: TJoyMoveEvent;
- OnPOVChanged: TJoyPOVChangedEvent;