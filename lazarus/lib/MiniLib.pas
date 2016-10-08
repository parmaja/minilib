{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniLib;

{$warn 5023 off : no warning about unused units}
interface

uses
  PHPUtils, GUIMsgBox, mnFonts, ColorUtils, mnSynHighlighterSARD, 
  mnSynHighlighterStdSQL, mnSynHighlighterXHTML, mnSynUtils, PHPProcessor, 
  HTMLProcessor, mnSynHighlighterApache, mnSynHighlighterConfig, 
  mnSynHighlighterCpp, mnSynHighlighterD, mnSynHighlighterFirebird, 
  mnSynHighlighterLua, mnSynHighlighterMultiProc, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniLib', @Register);
end.
