rem run before Delphi IDE starts

rem set user environment variable ifer to current path
SxConfDelphi.exe -r -l lib.txt -b bpl.txt

@IF NOT %ERRORLEVEL% EQU 0 PAUSE

rem remove user environment variable sx-library
setx sx-library ""

@IF NOT %ERRORLEVEL% EQU 0 PAUSE
