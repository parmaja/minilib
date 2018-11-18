{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit NativeLib;

{$warn 5023 off : no warning about unused units}
interface

uses
  ntvRegCtrls, ntvPageControls, ntvProgressBars, ntvDotMatrix, ntvTabSets, 
  ntvTabs, ntvSplitters, ntvImgBtns, ntvThemes, ntvBoard, ntvPanels, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ntvRegCtrls', @ntvRegCtrls.Register);
end;

initialization
  RegisterPackage('NativeLib', @Register);
end.
