unit mnLinuxCommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * Some function ported from ComPort at sourceforge thanks for them

 http://forum.kernelnewbies.org/read.php?12,213
 search for break_ctl();
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils,
  mnStreams, mnCommClasses,
  termio, libc;

const
  INVALID_HANDLE: Integer = -1;

type
  { TmnOSCommStream }

  TmnOSCommStream = class(TmnCustomCommStream)
  private
    FHandle: Integer; //THandle
  protected
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: Boolean; override;
    function InternalWrite(const Buffer; Count: Integer): Integer; override;
    function InternalRead(var Buffer; Count: Integer): Integer; override;

    function Check(R: Integer): Boolean;
    procedure Created; override;
    function DoWaitRead: Boolean; override;
    function DoWaitWrite: Boolean; override;
  public
    function GetInQue: Integer;//TODO
    procedure Flush; override;
    procedure Purge; override;
  end;

implementation

{ TmnOSCommStream }

procedure TmnOSCommStream.DoConnect;
var
  P: string;
  aMode: Integer;
  tio: termios;
  aBaudRate: Integer;
  mcs: Integer;
begin
  P := Port; //you must add full path e.g. '/dev/ttyUSB0'
  aMode := O_NOCTTY or O_SYNC;//O_NOCTTY or O_NDELAY;//O_SYNC;
  case ConnectMode of
    ccmReadWrite: aMode := aMode or O_RDWR;
    ccmRead: aMode := aMode or O_RDONLY;
    ccmWrite: aMode := aMode or O_WRONLY;
  end;
  FHandle := THandle(libc.open(pchar(P), aMode));
  Check(FHandle);

  tcflush(FHandle, TCIOFLUSH);

  cfmakeraw(tio);
  //ported from moserial/SerialConnection.vala

  case BaudRate of
    300:
      aBaudRate:=B300;
    600:
      aBaudRate:=B600;
    1200:
      aBaudRate:=B1200;
    2400:
      aBaudRate:=B2400;
    4800:
      aBaudRate:=B4800;
    9600:
      aBaudRate:=B9600;
    19200:
      aBaudRate:=B19200;
    38400:
      aBaudRate:=B38400;
    57600:
      aBaudRate:=B57600;
    115200:
      aBaudRate:=B115200;
    230400:
      aBaudRate:=B230400;
    460800:
      aBaudRate:=B460800;
    576000:
      aBaudRate:=B576000;
    921600:
      aBaudRate:=B921600;
    1000000:
      aBaudRate:=B1000000;
    2000000:
      aBaudRate:=B2000000;
  end;
  cfsetospeed(tio, aBaudRate);
  cfsetispeed(tio, aBaudRate);

{  // We generate mark and space parity
  if (settings.dataBits == 7 && (settings.parity==Settings.Parity.MARK || settings.parity==Settings.Parity.SPACE))
          dataBits=8;}

  case (DataBits) of
  dbFive:
     tio.c_cflag := (tio.c_cflag and not CSIZE) or CS5;
  dbSix:
    tio.c_cflag := (tio.c_cflag and not CSIZE) or CS6;
  dbSeven:
    tio.c_cflag := (tio.c_cflag and not CSIZE) or CS7;
  dbEight:
  else
    tio.c_cflag := (tio.c_cflag and not CSIZE) or CS8;
  end;
  tio.c_cflag := tio.c_cflag or CLOCAL or CREAD;

  //Parity
  tio.c_cflag := tio.c_cflag and not (PARENB or PARODD);

  if (Parity = prEven) then
    tio.c_cflag := tio.c_cflag or PARENB
  else if (Parity = prOdd) then
    tio.c_cflag := tio.c_cflag or (PARENB or PARODD);

  tio.c_cflag := tio.c_cflag and CRTSCTS;

  //Stop Bits
  if (StopBits = sbTwoStopBits) then
    tio.c_cflag := tio.c_cflag or CSTOPB
  else
    tio.c_cflag := tio.c_cflag and not CSTOPB;

  //Input Settings
  tio.c_iflag := IGNBRK;

  //Handshake
  if (Handshake = hsSoftware) or (Handshake = hsBoth) then
    tio.c_iflag := tio.c_iflag or IXON or IXOFF
  else
    tio.c_iflag := tio.c_iflag and not (IXON or IXOFF or IXANY);

  if (Handshake = hsHARDWARE) or (Handshake = hsBoth) then
    tio.c_cflag := tio.c_cflag or CRTSCTS
  else
    tio.c_cflag := tio.c_cflag and not CRTSCTS;

  tio.c_lflag := 0;
  tio.c_oflag := 0;

  tio.c_cc[VTIME] := Char(1); //Timeout
  tio.c_cc[VMIN] := Char(1);

  tio.c_lflag := tio.c_lflag and not (ECHONL or NOFLSH);

  ioctl(FHandle, TIOCMGET, mcs);
  mcs := mcs or TIOCM_RTS;
  ioctl(FHandle, TIOCMSET, mcs);

  Check(tcsetattr(FHandle, TCSANOW, @tio));
end;

procedure TmnOSCommStream.DoDisconnect;
begin
  FileClose(FHandle);
  FHandle := INVALID_HANDLE;
end;

function TmnOSCommStream.GetConnected: Boolean;
begin
  Result := FHandle <> INVALID_HANDLE;
end;

function TmnOSCommStream.InternalWrite(const Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  E: Cardinal;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  Check(Result);//Error if -1
end;

function TmnOSCommStream.InternalRead(var Buffer; Count: Integer): Integer;
begin
  Result := FileRead(FHandle, Buffer, Count);
  Check(Result);
end;

function TmnOSCommStream.Check(R: Integer): Boolean;
var
  E: Integer;
begin
  Result := R = INVALID_HANDLE;
  if Result then
  begin
    E := __errno_location^;
    Raise ECommError.Create('Error ' + IntToStr(E));
  end;
end;

procedure TmnOSCommStream.Created;
begin
  inherited Created;
  FHandle := INVALID_HANDLE;
end;

function TmnOSCommStream.DoWaitWrite: Boolean;
begin
  Result := True;
end;

function TmnOSCommStream.DoWaitRead: Boolean;
var
  FDSet: TFDSet;
  T: TTimeVal;
  P: PTimeVal;
  R: Integer;
  C: Integer;
begin
  //Check if there is a Data
  C := 0;
  R := ioctl(integer(FHandle), FIONREAD, @C);
  Check(R);
  Result := C > 0;
  if not Result then
  begin
    if Timeout = -1 then
      P := nil
    else
    begin
      T.tv_usec := (Timeout mod 1000) * 1000;
      T.tv_sec := Timeout div 1000;
      P := @T;
    end;
    FD_ZERO(FDSet);
    FD_SET(integer(FHandle), FDSet);
    R := select(integer(FHandle) + 1, @FDSet, nil, nil, P);
    Check(R);
    Result := R > 0;
  end;
end;

function TmnOSCommStream.GetInQue: Integer;
var
  C: Integer;
  R: Integer;
begin
  //Check if there is a Data
  C := 0;
  R := ioctl(integer(FHandle), FIONREAD, @C);
  Check(R);
  Result := C;
end;

procedure TmnOSCommStream.Flush;
begin
  inherited;
  Check(tcdrain(FHandle));
end;

procedure TmnOSCommStream.Purge;
begin
  inherited;
  Check(ioctl(FHandle, TCFLSH, TCIOFLUSH));
end;

end.

