{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniDB;

interface

uses
  mncDB, mncMetas, mncSQL, mncSQLUtils, mncSQLite, mncSQLiteHeader, mncMySQL, 
  mncMySQLdyn, mncPGHeader, mncPostgre, mncSQLDA, mncFBBlob, mncFBClient, 
  mncFBErrors, mncFBHeader, mncFBMetas, mncFBStrings, mncFBTypes, mncFBUtils, 
  mncFirebird, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniDB', @Register);
end.
