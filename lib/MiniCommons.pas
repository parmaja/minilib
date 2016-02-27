{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniCommons;

interface

uses
  mnFields, mnParams, mnStreams, mnUtils, mnBase64, minibidi, MsgBox, 
  HejriDates, HejriUtils, mnDrivers, mnLogs, UniDates, mnClasses, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniCommons', @Register);
end.
