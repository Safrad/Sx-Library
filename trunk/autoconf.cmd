rem run before Delphi IDE starts

rem set user environment variable sx-library to current path
setx sx-library "%cd%"

@IF NOT %ERRORLEVEL% EQU 0 PAUSE

SxConfDelphi.exe -l lib.txt -b bpl.txt

@IF NOT %ERRORLEVEL% EQU 0 PAUSE