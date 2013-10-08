{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniLib;

interface

uses
  HTMLProcessor, PHPProcessor, PHPUtils, SynHighlighterApache, 
  SynHighlighterXHTML, SynHighlighterSQLite, GUIMsgBox, 
  SynHighlighterFirebird, SynHighlighterSARD, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('MiniLib', @Register);
end.
