@echo off
fpc GPSMon.lpr @extrafpc.cfg -oGPSMon-ARM.exe -Parm -Twince -XParm-wince- -dLCL -dLCLwince -FUunits\arm-wince -B
if errorlevel 1 goto erroroccurred

e:\utils\upx -9 GPSMon-ARM.exe
if errorlevel 1 goto erroroccurred

goto noerrors
:erroroccurred
echo ############## Error while compile ##############
pause
goto :EOF
:noerrors
echo %
echo ----------------------------------------------------
echo ---------------- Compile successful ----------------
echo ----------------------------------------------------
pause
