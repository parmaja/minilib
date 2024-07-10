echo off
rem run it in bin directory
set bin=%~dp0
pushd
cd ..\..
set p=%CD%
%bin%fpcmkcfg -d "basepath=%p%" -o %bin%fpc.cfg
popd
pause