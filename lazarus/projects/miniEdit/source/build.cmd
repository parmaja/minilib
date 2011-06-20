@echo off
set path=V:\programs\FPC\2.4\bin\i386-win32;%path%
fpc gui\mne.lpr @extrafpc.cfg -o../bin/mne.exe -dLCL -B
pause
