{ This file was automatically created by Lazarus. do not edit !
  This source is only used to compile and install the package.
 }

unit NativeLib; 

interface

uses
    ntvCtrls, ntvRegCtrls, ntvUtils, ntvPageControls, ntvProgressBars, 
  ntvDotMatrix, LazarusPackageIntf;

implementation

procedure Register; 
begin
  RegisterUnit('ntvRegCtrls', @ntvRegCtrls.Register); 
end; 

initialization
  RegisterPackage('NativeLib', @Register); 
end.
