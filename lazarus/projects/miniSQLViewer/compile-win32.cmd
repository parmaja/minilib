@echo off
del bin\sqliteviewer.exe

fpc gui\sqliteviewer.lpr @extrafpc.cfg -B -d%1
if errorlevel 1 goto erroroccurred

upx bin\win32\sqliteviewer.exe
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
