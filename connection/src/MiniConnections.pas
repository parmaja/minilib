{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniConnections;

{$warn 5023 off : no warning about unused units}
interface

uses
  mncConnections, mncCSV, mncCSVExchanges, mncCommons, mncDB, mncMetas, 
  mncSQL, mncMySQL, mncMySQLdyn, mncPGHeader, mncPostgre, mncSQLDA, mncFBBlob, 
  mncFBClient, mncFBErrors, mncFBHeader, mncFBMetas, mncFBStrings, mncFBTypes, 
  mncFBUtils, mncFirebird, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniConnections', @Register);
end.
