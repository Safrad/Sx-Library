rem run before Delphi IDE starts

rem set user environment variable sx-library to current path
setx sx-library "%cd%"

@if not errorlevel 0 pause

rem set user environment variable ifer to current path
PathsToReg.exe

@if not errorlevel 0 pause