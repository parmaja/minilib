{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniConnections; 

interface

uses
  mncCSV, mncConnections, mncSchemas, mncSQL, mncSQLUtils, mncSQLite, 
  mncSQLiteSchemas, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('MiniConnections', @Register); 
end.
