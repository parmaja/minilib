@echo off

fpc GPSLog.lpr -oGPSLog.exe -Twin32 -XPi386-win32- -dLCLwin32 -FUunits\i386-win32 -B -FuC:\lazarus\lcl\interfaces\win32 -FuC:\lazarus\lcl\widgetset -FuC:\lazarus\lcl -FiC:\lazarus\lcl\include
if errorlevel 1 goto erroroccurred

e:\utils\upx GPSLog.exe
if errorlevel 1 goto erroroccurred

goto noerrors
:erroroccurred
echo % %
echo ############## Error while compile ##############
pause
goto :EOF
:noerrors
echo % %
echo ---------------- Compile successful ----------------
pause
