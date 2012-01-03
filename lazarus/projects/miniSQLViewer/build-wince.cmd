@echo off
del bin\wince\sqliteviewer-arm.exe
 
lazbuild --build-mode=wince gui\sqliteviewer.lpr -r -B
if errorlevel 1 goto erroroccurred

strip bin\sqliteviewer-arm.exe
if errorlevel 1 goto erroroccurred

upx bin\sqliteviewer-arm.exe
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
