@echo off
del bin\wince\sqliteviewer.exe

fpc gui\sqliteviewer.lpr @extrafpc.cfg -B -d%1 -TWinCE -Parm
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
