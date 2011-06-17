{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit MiniXML; 

interface

uses
  mnXML, mnXMLBase64, mnXMLFPClasses, mnXMLNodes, mnXMLReader, mnXMLRtti, 
  mnXMLRttiProfile, mnXMLRttiReader, mnXMLRttiStdClasses, mnXMLRttiWriter, 
  mnXMLScanner, mnXMLStreams, mnXMLUtils, mnXMLWriter, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('MiniXML', @Register); 
end.
