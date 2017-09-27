{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniSockets;

{$warn 5023 off : no warning about unused units}
interface

uses
  mnClients, mnConnectionCommands, mnConnections, mnServers, mnSockets, 
  mnSocketStreams, mnIRCClients, mnHttpServer, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniSockets', @Register);
end.
