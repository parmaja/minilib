unit mnLinuxCommStreams;
{**
 *  This file is part of the "Mini Comm"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * Some function ported from ComPort at sourceforge thanks for them
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
    function DoWrite(const Buffer; Count: Integer): Integer; override;
    function DoRead(var Buffer; Count: Integer): Integer; override;

    function Check(R: Integer): Boolean;
    procedure Created; override;
  public
    function WaitEvent(const Events: TComEvents): TComEvents; override;
    function GetInQue: Integer;
    procedure Flush; override;
    procedure Purge; override;
  end;

implementation

{ TmnOSCommStream }

procedure TmnOSCommStream.DoConnect;
var
   P: string;
   aMode: Integer;
begin
  P := '/dev/' + Port;//TODO we need convertor COM to TTY more flixeble :P
  aMode := O_SYNC;
  case ConnectMode of
    ccmReadWrite: aMode := O_RDWR;
    ccmRead: aMode := O_RDONLY;
    ccmWrite: aMode := O_WRONLY;
  end;
  FHandle := THandle(Libc.open(pchar(P), aMode));
  Check(FHandle);
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

function TmnOSCommStream.DoWrite(const Buffer; Count: Integer): Integer;
var
  Bytes: DWORD;
  E: Cardinal;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  Check(Result);//Error if -1
end;

function TmnOSCommStream.DoRead(var Buffer; Count: Integer): Integer;
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
    Raise ECommError.Create('Error when flush ' + IntToStr(E));
  end;
end;

procedure TmnOSCommStream.Created;
begin
  inherited Created;
  FHandle := INVALID_HANDLE;
end;

function TmnOSCommStream.WaitEvent(const Events: TComEvents): TComEvents;
var
  FDSet: TFDSet;
  T: TTimeVal;
  P: PTimeVal;
  R: Integer;
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
  if R > 0 then
    Result := [evRxFlag]
  else
    Result := [];
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

