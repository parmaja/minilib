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
  public
    function WaitRead: Boolean; override;
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
  t: termios;
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

  cfmakeraw(t);
  term.c_cflag := term.c_cflag or CREAD;
  term.c_cflag := term.c_cflag or CLOCAL;
  term.c_cflag := term.c_cflag or HUPCL;

  with GetFlowControlFlags do
  begin
    //hardware handshake

    if (dcb.flags and dcb_RtsControlHandshake) > 0 then
      term.c_cflag := term.c_cflag or CRTSCTS
    else
      term.c_cflag := term.c_cflag and (not CRTSCTS);
    //software handshake
    if (dcb.flags and dcb_OutX) > 0 then
      term.c_iflag := term.c_iflag or IXON or IXOFF or IXANY
    else
      term.c_iflag := term.c_iflag and (not (IXON or IXOFF or IXANY));
    //size of byte
    term.c_cflag := term.c_cflag and (not CSIZE);
    case dcb.bytesize of
      5:
        term.c_cflag := term.c_cflag or CS5;
      6:
        term.c_cflag := term.c_cflag or CS6;
      7:
        term.c_cflag := term.c_cflag or CS7fix;
      8:
        term.c_cflag := term.c_cflag or CS8;
    end;
    //parity
    if (dcb.flags and dcb_ParityCheck) > 0 then
      term.c_cflag := term.c_cflag or PARENB
    else
      term.c_cflag := term.c_cflag and (not PARENB);
    case dcb.parity of
      1: //'O'
        term.c_cflag := term.c_cflag or PARODD;
      2: //'E'
        term.c_cflag := term.c_cflag and (not PARODD);
    end;
    //stop bits
    if dcb.stopbits > 0 then
      term.c_cflag := term.c_cflag or CSTOPB
    else
      term.c_cflag := term.c_cflag and (not CSTOPB);
  end;
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

function TmnOSCommStream.WaitRead: Boolean;
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
begin
  Result := 0;
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

