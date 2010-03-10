@echo off
set path=V:\programs\FPC\2.4\bin\i386-win32;%path%
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
