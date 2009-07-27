fpc gui\Calculator.lpr -obin\Calculator.exe @extra.cfg -B -Twin32 -XPi386-win32- -dLCLwin32
upx.exe bin\Calculator.exe
pause
