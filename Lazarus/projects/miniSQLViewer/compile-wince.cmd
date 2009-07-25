@echo off
del bin\wince\sqliteviewer.exe

fpc gui\sqliteviewer.lpr @extra.cfg -obin\wince\sqliteviewer.exe -d%1  -B -Parm -Twince -XParm-wince- -dLCL -dLCLwince -FUunits\wince
if errorlevel 1 goto erroroccurred

e:\utils\upx bin\wince\sqliteviewer.exe
if errorlevel 1 goto erroroccurred

call copy-pocketpc.cmd
if errorlevel 1 goto erroroccurred

goto noerrors

:erroroccurred
echo ???????????????????
echo    Error compile
echo ???????????????????
pause
goto :EOF
:noerrors
echo #######################
echo    Compile completed
echo #######################
pause
