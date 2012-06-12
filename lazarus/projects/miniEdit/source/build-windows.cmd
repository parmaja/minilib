@echo off
del ..\bin\mne.exe
 
lazbuild --build-mode=windows gui\mne.lpr -r -B
if errorlevel 1 goto erroroccurred

strip ..\bin\mne.exe
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
