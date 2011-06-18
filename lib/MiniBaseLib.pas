{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniBaseLib; 

interface

uses
  minibidi, mnFields, mnParams, mnStreams, mnUtils, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('MiniBaseLib', @Register); 
end.
