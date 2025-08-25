rem put it in fpc root folder not bin
set p=%~dp0
set "p=%p:~0,-1%"

cd %p%\bin\i386-win32
fpcmkcfg -d "basepath=%p%" -o fpc.cfg
cd %p%\bin\x86_64-win64
fpcmkcfg -d "basepath=%p%" -o fpc.cfg
cd %p%
pause