rem run before Delphi IDE starts

rem set user environment variable sx-library to current path
setx sx-library "%cd%"

@IF NOT %ERRORLEVEL% EQU 0 PAUSE

rem set user environment variable ifer to current path
PathsToReg.exe

@IF NOT %ERRORLEVEL% EQU 0 PAUSE