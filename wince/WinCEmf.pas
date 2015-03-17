unit WinCEmf;
{
  WinCE missed functions
}

{$mode objfpc}{$H+}

interface

uses
  Windows, Messages, SysUtils, Registry;

type
  // http://www.cnblogs.com/Lisen/archive/2010/01/30/1660070.html#A121
  TGESTUREINFO = record
    cbSize: Cardinal;
    dwFlags: dword;
    dwID: dword;
    hwndTarget: hwnd;
    ptsLocation: Pointer;
    dwInstanceID: dword;
    dwSequenceID: dword;
    ullArguments: Long;
    cbExtraArgs: Cardinal;
  end;

  PGESTUREINFO = ^TGESTUREINFO;

  TWAGINFO =  record
      cbSize: SIZE_T;
      dwFlags: DWord;
      nOwnerAnimateMessage: UINT;
      nAnimateStatusMessage: UINT;
      hExtentBrush: HBRUSH;
      nItemHeight: UINT;
      nItemWidth: UINT;
      bHorizontalExtent: BYTE;
      bVerticalExtent: BYTE;
  end;

  PWAGINFO = ^TWAGINFO;

const
  WM_GESTURE = $0119;
  WM_TOUCH = $0240;

  WAGIF_OWNERANIMATE = 1;//this number from my mind
  WAGIF_VSCROLLABLE = 2;
  WAGIF_HSCROLLABLE = 4;
  WAGIF_LOCKAXES = 8;
  WAGIF_IGNOREPAN = 16;
  WAGIF_IGNORESCROLL = 32;

  POWER_STATE_ON = $00010000;        // On state
  POWER_STATE_OFF = $00020000;        // No power, full off
  POWER_STATE_CRITICAL =     $00040000;        // Critical off
  POWER_STATE_BOOT =        $00080000;        // Boot state
  POWER_STATE_IDLE =        $00100000;        // Idle state
  POWER_STATE_SUSPEND =       $00200000;        // Suspend state
  POWER_STATE_RESET =       $00800000;        // Reset state
  POWER_STATE_PASSWORD=     $10000000;        // This state is password protected.

  POWER_NAME               = $00000001; // default
  POWER_FORCE              = $00001000;

  function SetSystemPowerState(psState: PWideChar; StateFlags: DWORD; Options : DWORD):DWORD; stdcall; external 'coredll.dll' name 'SetSystemPowerState';
  function PlaySoundW(x1: PWideChar; x2: HMODULE; x3: DWORD): longbool; stdcall; external 'coredll.dll' name 'PlaySoundW';
  function GetDeviceUniqueID(AppData: LPCWSTR; cbApplictionData: Integer; dwDeviceIDVersion: Integer; var deviceIDOuput; var pcbDeviceIDOutput: DWORD): Integer; external 'coredll.dll' name 'GetDeviceUniqueID';

//  function OffsetViewportOrgEx(DC:HDC; DX:longint; DY:longint; point:LPPOINT):WINBOOL; cdecl; external 'coredll.dll'; //not found in 5.0 and less
  function GetWindowExtEx(DC: HDC; var Size: TSize): BOOL; cdecl; external 'coredll.dll';
  function GetViewportExtEx(DC: HDC; var Size: TSize): BOOL; cdecl; external 'coredll.dll';

  //function GetGestureInfo(hGestureInfo: cardinal; pGestureInfo: PGESTUREINFO): BOOL; cdecl; external 'coredll.dll';
  //function TKSetWindowAutoGesture(Handle: HWND; lpAutoGestureInfo: PWAGINFO): BOOL; cdecl; external 'coredll.dll';

implementation

end.

