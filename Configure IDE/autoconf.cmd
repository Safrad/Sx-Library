rem run before Delphi IDE starts

rem set user environment variable sx-library to repository root
pushd ..
setx sx-library "%cd%"
@IF NOT %ERRORLEVEL% EQU 0 PAUSE
popd 

SxConfDelphi.exe -l lib.txt -b bpl.txt

@IF NOT %ERRORLEVEL% EQU 0 PAUSE