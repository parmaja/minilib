@echo off
rem Build MakeCertDemo (Delphi console demo) from a command prompt.
rem Adjust RSVARS/RTL below to match your Delphi installation if needed.

set RSVARS=C:\Program Files (x86)\Embarcadero\Studio\37.0\bin64\rsvars64.bat
set RTL=C:\Program Files (x86)\Embarcadero\Studio\37.0\lib\win64\release
set DCC=dcc64

if not exist "%RSVARS%" goto missing

call "%RSVARS%" >nul 2>&1
cd /d "%~dp0"
%DCC% -CC -Q -B -NS"System;Winapi" -I"..\..\..\source;..\..\..\..\lib" -U"..\..\..\source;..\..\..\..\lib;%RTL%" MakeCertDemo.dpr
if errorlevel 1 exit /b 1
echo.
echo Build OK. Copy the OpenSSL 3.x DLLs (libssl-3-x64.dll + libcrypto-3-x64.dll)
echo next to MakeCertDemo.exe or put them on PATH, then run MakeCertDemo.exe.
exit /b 0

:missing
echo rsvars64.bat not found: "%RSVARS%"
echo Edit build.bat and point RSVARS at your Delphi installation.
exit /b 1