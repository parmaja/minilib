{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NativeLib; 

interface

uses
    ntvCtrls, ntvRegCtrls, ntvPageControls, ntvProgressBars, ntvDotMatrix,
  ntvTabSets, ntvTabs, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ntvRegCtrls', @ntvRegCtrls.Register); 
end; 

initialization
  RegisterPackage('NativeLib', @Register); 
end.
