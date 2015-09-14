{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniConnections;

interface

uses
  mncConnections, mncCSV, mncCSVExchanges, mncCommons, mncSQL, mncSQLUtils, 
  mncDB, mncSQLite, mncMetas, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniConnections', @Register);
end.
