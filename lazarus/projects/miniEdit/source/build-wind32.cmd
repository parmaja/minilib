@echo off
if exist ..\bin\mne.exe (del ..\bin\mne.exe)
 
lazbuild --build-mode=release_win32 gui\mne.lpr -r -B
if errorlevel 1 goto erroroccurred

rem strip ..\bin\mne.exe
rem if errorlevel 1 goto erroroccurred

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
