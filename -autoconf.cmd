rem run before Delphi IDE starts

rem remove user environment variable sx-library
setx sx-library ""

@IF NOT %ERRORLEVEL% EQU 0 PAUSE

rem set user environment variable ifer to current path
PathsToReg.exe

@IF NOT %ERRORLEVEL% EQU 0 PAUSE