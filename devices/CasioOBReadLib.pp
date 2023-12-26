unit CasioOBReadLib;

interface

uses
  Classes, SysUtils, TypInfo, LCLType;
  
const
  OBReadLibDLL = 'OBReadLib.dll';

  { Scanning Key }
  OBR_TRIGGERKEY_L = $00000001;       { Trigger key L }
  OBR_TRIGGERKEY_R = $00000002;       { Trigger key R }
  OBR_MULTIKEY = $00000004;       { Multi key }
  OBR_CURSORKEY_L = $00000008;       { Cursor key L }
  OBR_CURSORKEY_R = $00000010;       { Cursor key R }
  OBR_CURSORKEY_U = $00000020;       { Cursor key U }
  OBR_CURSORKEY_D = $00000040;       { Cursor key D }
  OBR_GUNTRIGGER = $00000080;       { Gun grip trigger key }
  OBR_GUNGRIP = $00000080;       { Gun grip trigger key }
  OBR_CENTERTRIGGER = $00000100;       { Center trigger }

  { Output Buffer Specification }
  
  OBR_BUFOBR = $00;       { OBR buffer }
  OBR_STOFF = $02;       { Key (Window message) }
  { (Without start code) }
  OBR_ASTOFF = $03;       { Clipboard }
  OBR_KEYBOARD = $04;       { Key (Keyboard event) }

  { Reading Code Specification }
  OBR_CD39 = $00000001;       { Code39 }
  OBR_NW_7 = $00000002;       { NW-7 }
  OBR_WPCA = $00000004;       { WPC Addon }
  OBR_WPC = $00000008;       { WPC }
  OBR_UPEA = $00000010;       { UPC-E Addon }
  OBR_UPE = $00000020;       { UPC-E }
  OBR_IDF = $00000040;       { IDF }
  OBR_ITF = $00000080;       { ITF }
  OBR_CD93 = $00000100;       { Code93 }
  OBR_CD128 = $00000200;       { Code128 }
  OBR_MSI = $00000400;       { MSI }
  OBR_IATA = $00000800;       { IATA }
  OBR_RSS14 = $00001000;       { RSS-14 }
  OBR_RSSLTD = $00002000;       { RSS Limited }
  OBR_RSSEXP = $00004000;       { RSS Expanded }
  OBR_RSS14S = $00008000;       { RSS-14 Stacked }
  OBR_RSSEXPS = $00010000;       { RSS Expanded Stacked }
  OBR_ALL = $0001FFFF;       { All Code }
  OBR_DCAL = $00000000;       { All code selected () }
  OBR_NONDT = $FFFFFFFF;       { No data (OBRGets) }
  OBR_DWPC = OBR_WPCA or OBR_WPC;       { WPC Addon & WPC }
  OBR_DUPC = OBR_UPEA or OBR_UPE;       { UPC-E Addon & UPC-E }
  OBR_DCWU = OBR_DWPC or OBR_DUPC;       { WPC & UPC-E }
  OBR_IDTF = OBR_IDF or OBR_ITF;       { Industrial 2of5 & Interleaved 2of5 }
  OBR_CHK_ON = $80000000;       { Calculate of check digit (OBROpen) }
  OBR_OUT_ON = $40000000;       { Output of check character (OBROpen) }
  
  OBR_ENDCR = 0;	// CR
  OBR_ENDLF = 1;		// LF
  OBR_ENDCL = 2;		// CR + LF
  OBR_ENDTAB = 3;		// TAB (HT)
  OBR_ENDNULL = 4;	// NULL


  function OBROpen(hWnd:HWND; dwMode:DWORD): integer; cdecl; external OBReadLibDLL;
  function OBRClose: integer; cdecl; external OBReadLibDLL;
  function OBRLoadConfigFile: integer; cdecl; external OBReadLibDLL;
  function OBRSetDefaultSymbology:integer; cdecl; external OBReadLibDLL;
  function OBRSetScanningKey(dwKey:DWORD): integer; cdecl; external OBReadLibDLL;
  function OBRSetScanningCode(dwMode:DWORD): integer; cdecl;external OBReadLibDLL;
  function OBRSetBuffType(byType:BYTE): integer;cdecl; external OBReadLibDLL;
  function OBRSetSuffixChar(bySuffix:BYTE): integer;cdecl; external OBReadLibDLL;


implementation
end.
