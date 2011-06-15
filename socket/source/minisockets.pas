{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit minisockets; 

interface

uses
  mnClients, mnCommandClients, mnCommandServers, mnConnections, mnHttpServer, 
  mnServers, mnSockets, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('minisockets', @Register); 
end.
