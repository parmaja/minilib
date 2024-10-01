echo off

set d=%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%
cd %d%

7z x fpc.zip -o*
rename .\fpc\fpc src

7z x fpc-3.3.1.i386-win32.zip -o.\fpc\
7z x fpc-3.3.1.x86_64-win64.built.on.i386-win32.zip -o.\fpc\ -y

7z x fpcbuild.zip -o.\fpc\ -y

copy .\fpc\fpcbuild\install\binw32\*.* .\fpc\bin\i386-win32

copy ..\makecfg.cmd .\fpc\bin\i386-win32\