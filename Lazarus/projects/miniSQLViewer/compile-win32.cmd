@echo off
del bin\sqliteviewer.exe

fpc gui\sqliteviewer.lpr @windows.cfg -obin\sqliteviewer.exe -B -dLCLwin32 -FUunits\
if errorlevel 1 goto erroroccurred

e:\utils\upx bin\sqliteviewer.exe
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
