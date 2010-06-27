unit CasioSystemLib;

interface

uses
  Classes, SysUtils, TypInfo, LCLType;

const
  SystemLibDLL = 'SystemLib.dll';

  LED_OFF = $00000000;
  LED_RED = $00000001;
  LED_GREEN = $00000002;

function SysSetLED(mode:DWORD; num:DWORD; ontime:DWORD; offtime:DWORD):DWORD; cdecl; external SystemLibDLL;
function SysGetLED:DWORD; cdecl; external SystemLibDLL;
function SysGetLEDState(dwType:DWORD):DWORD; cdecl; external SystemLibDLL;

implementation
end.
