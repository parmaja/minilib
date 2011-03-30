#!/usr/bin/env bash
if fpc gui/SqliteViewer.lpr @linux.cfg -obin/sqliteviewer -d%1  -B -dLCL -FUunits/; then
  echo '##### Compile #####'
else
  echo '????? Error  ?????'  
  exit
fi
echo 'Press Enter to exit.'
read
