echo off

set d=%DATE:~-4%-%DATE:~4,2%-%DATE:~7,2%
md %d%
cd %d%

echo Downloadinng FPC sources
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/source/fpc.zip

echo Downloading FPC Utils
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/source/fpcbuild.zip

echo Downloading FPC compiler for Linux
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/x86_64-linux/fpc-3.3.1.x86_64-linux.tar.gz

echo Downloading FPC Win32 compiler for windows
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/i386-win32/fpc-3.3.1.i386-win32.zip

echo Downloading FPC Win32 cross to Win64 for windows
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/x86_64-win64/fpc-3.3.1.x86_64-win64.built.on.i386-win32.zip

echo Downloading win64 compileer for windows
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/snapshot/trunk/x86_64-win64/fpc-3.3.1.x86_64-win64.built.on.x86_64-linux.tar.gz

curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/contrib/utils/win32/makew32.zip
curl -C - --progress-bar --remote-name http://downloads.freepascal.org/fpc/contrib/cross/mingw/win64/binutils-2.21-win32-x86_64-win64.zip

:erroroccurred
rem cd ..
pause