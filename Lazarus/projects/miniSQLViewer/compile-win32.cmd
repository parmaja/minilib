@echo off
del bin\sqliteviewer.exe

fpc gui\sqliteviewer.lpr @extra.cfg -obin\sqliteviewer.exe -d%1  -B -dLCL -FUunits\
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
