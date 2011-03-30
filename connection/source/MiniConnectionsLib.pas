{ This file was automatically created by Lazarus. Do not edit!
This source is only used to compile and install the package.
 }

unit MiniConnectionsLib; 

interface

uses
  mncCSV, mncConnections, LazarusPackageIntf; 

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('MiniConnectionsLib', @Register); 
end.
