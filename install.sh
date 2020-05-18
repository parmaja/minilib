#Adding packages to Lazarus IDE 
set -e 
lazarusdir="../../programs/lazarus"
opt="--lazarusdir=$lazarusdir"
$lazarusdir/lazbuild $opt ./lib/MiniCommons.lpk
$lazarusdir/lazbuild $opt ./xml/source/MiniXML.lpk
$lazarusdir/lazbuild $opt ./socket/source/MiniSockets.lpk
#$lazarusdir/lazbuild $opt ./connection/src/MiniConnections.lpk
$lazarusdir/lazbuild $opt ./comm/source/MiniComm.lpk
$lazarusdir/lazbuild $opt ./lazarus/lib/MiniLib.lpk
$lazarusdir/lazbuild $opt --add-package ./lazarus/components/native/NativeLib.lpk --add-package ./pos/source/poslib.lpk
$lazarusdir/lazbuild --add-package ./pos/source/poslib.lpk
$lazarusdir/lazbuild --build-ide=
