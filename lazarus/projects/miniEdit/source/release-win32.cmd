@echo off
if exist ..\bin\mne.exe (del ..\bin\mne.exe)
 
lazbuild --build-mode=release_win32 gui\mne.lpr -r -B
if errorlevel 1 goto erroroccurred

7z a "../release/miniedit-%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%.7z" ../bin/mne.exe ../README.TXT
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
