{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit poslib; 

interface

uses
  posUtils, posButtons, posControls, posEdits, posGrids, posKeyboards, 
  posLists, posStuffs, posRegCtrls, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('posRegCtrls', @posRegCtrls.Register); 
end; 

initialization
  RegisterPackage('poslib', @Register); 
end.
