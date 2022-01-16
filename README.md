## Mini Lib for Pascal ##

Pascal library, FreePascal 3.2 or later or DelphiXE

### Folders ###

lib: most common utils functions (MiniLib.lpk)

connection: it is a database wrappers for SQLite, FirebirdSQL, MySQL, PostgreSQL (MiniConnections.lpk).

socket: Socket tcp/ip client and server objects, includes webserver and simple REST server, and OpenSSL 1.1 support (MiniSockets.lpk).

xml: XML file/stream reader, line by line not use huge memory to parse it (MiniXML.lpk).

comm: ComPort like COM1 COM2 and serial ports (MiniComm.lpk).

### Compiler ###

Free Pascal 3.2

Delphi XE 10.4 

### Install ###

#### Lazarus ####

Just open this packages (in order) and compile it in Lazarus IDE (Menu->Package-Open Package file (.lpk))

Main package used by other packages

    MiniLib.lpk

Usefull Packages

    MiniConnections.lpk
    MiniSockets.lpk
    MiniXML.lpk

Optional packages (rarly tested)

    MiniComm.lpk

Abandonned pachages

    MiniTranslator.lpk

### Branches ###

Use "release" branch for last stable code.

Use "master" branch for last update of code.

### Developers ###

Zaher Dirkey zaherdirkey at gmail dot com

Belal hamed belalhamed at gmail dot com
