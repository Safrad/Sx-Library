echo off
Project1Test.exe %1
if ERRORLEVEL 1 echo ********* Test(s) failed! ********
