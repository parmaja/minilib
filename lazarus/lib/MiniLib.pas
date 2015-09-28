{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniLib;

interface

uses
  HTMLProcessor, PHPProcessor, PHPUtils, GUIMsgBox, mnFonts, ColorUtils, 
  SynHighlighterApache, SynHighlighterXHTML, SynHighlighterFirebird, 
  SynHighlighterSARD, SynHighlighterD, SynHighlighterConfig, 
  SynHighlighterStdSQL, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniLib', @Register);
end.
