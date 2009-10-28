{ This file was automatically created by Lazarus. do not edit !
  This source is only used to compile and install the package.
 }

unit poslib; 

interface

uses
    posButtons, posControls, posEdits, posGrids, posKeyboards, posLists, 
  posRegCtrls, posUtils, posStuffs, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('posRegCtrls', @posRegCtrls.Register); 
end; 

initialization
  RegisterPackage('poslib', @Register); 
end.
