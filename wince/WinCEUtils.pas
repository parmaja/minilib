unit WinCEUtils;

{$mode delphi}{$H+}

interface

uses
  Windows,
  SysUtils,
  aygshell,
  md5,
  Registry,
  WinCEmf;

function GetWinCEVersion: string;
function GetDeviceName: string;
function GetDeviceID(var Value: Ansistring): Boolean;
function GetDeviceIDByIO(var Value: AnsiString): Boolean;
function GetProtectSerialNumber(var SerialNumber :string): Boolean;

//todo List Ports
//http://code.google.com/p/pockettune/source/browse/trunk/PocketTune/listports.c

implementation

//C:\lazarus\fpc\2.1.5\src\rtl\wince\wininc\defines.inc
//http://msdn2.microsoft.com/en-us/library/ms893522.aspx
//http://peterfoot.net/RetrieveIMEIThroughTAPI.aspx
//http://blogs.msdn.com/jehance/archive/2004/07/12/181067.aspx

//  SystemParametersInfo(SPI_GETOEMINFO, sizeof(Info) - 1, @Info, 0);

function MyMDPrint(const Digest: TMDDigest): AnsiString;
var
  s: AnsiString;
  I: Byte;
begin
  Result := '';
  for I := 0 to 15 do
  begin
    if (i > 0) and ((i mod 2) = 0) then
      Result := Result + '-'; //add space make the string wrap in label and more eye friendly
    s := HexStr(Digest[i], 2);
    Result := Result + UpperCase(s);
  end;
end;

function GetDeviceName: string;
var
  aReg:TRegistry;
begin
  aReg := TRegistry.Create(KEY_READ);
  try
    aReg.RootKey := HKEY_LOCAL_MACHINE;
    aReg.OpenKey('Ident', False);
    if aReg.ValueExists('Name') then
      Result := aReg.ReadString('Name')
    else
      Result := 'GUEST';
  finally
    aReg.Free;
  end;
end;

function GetDeviceID(var Value: AnsiString): Boolean;
var
  AppData: array[0..19] of WideChar;
  DeviceID: array[0..19] of Byte;
  Count: DWORD;
  Res:Integer;
begin
  //not sure about Unicode
  AppData := Utf8Decode('MY_SIG');//any string you like
  Count := SizeOf(DeviceID);
  FillChar(DeviceID, Count, #0);
  Res := GetDeviceUniqueID(AppData, SizeOF(AppData), 1, DeviceID, Count);
  Value := '';
  if Res = 0 then
  begin
    Result := True;
    Value := MyMDPrint(MD5Buffer(DeviceID, Count));
  end
  else
    Result := False;//error accord
end;

//-------------------------------

function KernelIoControl(IoControlCode: DWORD; InBuf: Pointer; InBufSize:DWORD; OutBuf: Pointer; OutBufSize: DWORD; BytesReturned: LPDWORD): WinBool; external KernelDLL name 'KernelIoControl';

const
  METHOD_BUFFERED = 0;
  METHOD_IN_DIRECT = 1;
  METHOD_OUT_DIRECT = 2;
  METHOD_NEITHER = 3;

  FILE_DEVICE_HAL	=	$00000101;
  FILE_DEVICE_CONSOLE = $00000102;
  FILE_DEVICE_PSL	=	$00000103;
  FILE_DEVICE_SERVICE	=	$00000104;

  FILE_ANY_ACCESS = 0;
  FILE_READ_ACCESS = $0001;
  FILE_WRITE_ACCESS = $0002;
  IOCTL_CDROM_BASE = $00000002;
  IOCTL_SCSI_BASE = $00000004;
 
type
  TDEVICE_ID = packed record
    Size: DWORD;
    PresetIDOffset: DWORD;
    PresetIDBytes: DWORD;
    PlatformIDOffset: DWORD;
    PlatformIDBytes: DWORD;
  end;

  TIDBuffer = array[0..1024] of Byte;

  TDEVICE_ID_BUFFER = packed record
    DeviceID: TDEVICE_ID;
    BUFFER: TIDBuffer;
  end;

  PDEVICE_ID_BUFFER = ^TDEVICE_ID_BUFFER;

function PrintBuffer(const Buffer: TIDBuffer; Offset, Count: Integer): AnsiString;
var
  S: AnsiString;
  I: Integer;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    if (I > 0) and ((I mod 4) = 0) then
      Result := Result + '-'; //add space make the string wrap in label and more eye friendly
    S := IntToHex(Buffer[I + Offset], 2);
    Result := Result + S;
  end;
end;

//http://msdn.microsoft.com/en-us/library/aa446562.aspx

function GetDeviceIDByIO(var Value: AnsiString): Boolean;
var
  OutputBuffer: PDEVICE_ID_BUFFER;
  OutputBufferSize, BytesReturned: DWORD;
  Res: WINBOOL;
  Error: Integer;
  C: Integer;
const
  IOCTL_HAL_GET_DEVICEID: longint =
  ((FILE_DEVICE_HAL) shl 16) or ((FILE_ANY_ACCESS) shl 14)
     or ((21) shl 2) or (METHOD_BUFFERED);
begin
  Value := '';
  Result := False;

  OutputBuffer := AllocMem(SizeOf(TDEVICE_ID));
  BytesReturned := 0;
  OutputBuffer^.DeviceID.Size := 0;
  OutputBufferSize := SizeOf(TDEVICE_ID);
  Res := KernelIoControl(IOCTL_HAL_GET_DEVICEID,
          nil,
          0,
          OutputBuffer,
          OutputBufferSize,
          @BytesReturned);

  Error:=GetLastOSError;

  if Error = ERROR_INSUFFICIENT_BUFFER then
  begin
    OutputBufferSize := OutputBuffer^.DeviceID.Size;
    FreeMem(OutputBuffer);
    OutputBuffer := AllocMem(OutputBufferSize);
    FillChar(OutputBuffer^, OutputBufferSize, #0);
    OutputBuffer^.DeviceID.Size := OutputBufferSize;
    BytesReturned := 0;
    Res := KernelIoControl(IOCTL_HAL_GET_DEVICEID,
            nil,
            0,
            OutputBuffer,
            OutputBufferSize,
            @BytesReturned);

    if (Res) then
    begin
      C := SizeOf(TDEVICE_ID);
      Result := True;
      if OutputBuffer^.DeviceID.PresetIDBytes > 0 then
        Value := Value + PrintBuffer(OutputBuffer^.BUFFER, OutputBuffer^.DeviceID.PresetIDOffset - C, OutputBuffer^.DeviceID.PresetIDBytes);
      if OutputBuffer^.DeviceID.PlatformIDBytes > 0 then
      begin
        if Value <> '' then
           Value := Value + '-';
        Value := Value + PrintBuffer(OutputBuffer^.BUFFER, OutputBuffer^.DeviceID.PlatformIDOffset - C, OutputBuffer^.DeviceID.PlatformIDBytes);
      end;
    end
    else if not Res then
      Error := GetLastOSError;
  end;

end;

//for Opticon H15
{$ifdef Opticon}
type
  TSysGetSerialNumber = function (SerialNumber: PWideChar): BOOL; stdcall;//cdecl;

var
  sysctldlllib: THandle = 0;
  SysGetSerialNumber: TSysGetSerialNumber = nil;

const
  sysctldll = 'sysctl.dll';
{$endif Opticon}

function GetProtectSerialNumber(var SerialNumber :string): Boolean;
  {$IFDEF WINCE}
var
  s: AnsiString;
  w: WideString;
  p: PWideChar;
const
  c = 128;
  {$ENDIF}
begin
{$IFDEF WINCE}
  P := nil;
  SerialNumber := '';
  {$ifdef Opticon}
  if sysctldlllib = 0 then
    sysctldlllib := LoadLibrary(sysctldll);
  if sysctldlllib <> 0 then
  begin
    SysGetSerialNumber := TSysGetSerialNumber(GetProcAddress(sysctldlllib, 'SysGetSerialNumber'));
  end;
  if Assigned(SysGetSerialNumber) then
  begin
    GetMem(P, c);
    try
      FillChar(P^, c, #0);
      Result := SysGetSerialNumber(P);
      //becuase it always return False
      Result := True;
      w := Trim(p);
      if Result then
        SerialNumber := w;
    finally
      FreeMem(P);
    end;
  end
  else
  begin
  {$endif Opticon}
    Result := GetDeviceIDByIO(s);
    if Result then
      SerialNumber := s
    else
      SerialNumber := '';
  {$ifdef Opticon}
  end;
  {$endif Opticon}
{$ELSE}
  SerialNumber := '0123456789';
  Result := True;
{$ENDIF}
end;

function GetWinCEVersion: string;
var
  versionInfo: OSVERSIONINFO;
begin
  Result := '';
  System.FillChar(versionInfo, sizeof(OSVERSIONINFO), #0);
  versionInfo.dwOSVersionInfoSize := sizeof(OSVERSIONINFO);
  if GetVersionEx(@versionInfo) then
  begin
    Result := IntToStr(versionInfo.dwMajorVersion) + '.'+ IntToStr(versionInfo.dwMinorVersion);
  end;
end;

end.

