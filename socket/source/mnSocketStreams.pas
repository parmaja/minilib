unit mnSocketStreams;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
 
{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  mnStreams,
  mnSockets;

const
  WaitForEver: Longint = -1;

type
  { TmnSocketStream }

  TmnSocketStream = class(TmnConnectionStream)
  private
    FSocket: TmnCustomSocket;
    procedure FreeSocket;
  protected
    function GetConnected: Boolean; override;
    function CreateSocket: TmnCustomSocket; virtual;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
  public
    constructor Create(vSocket: TmnCustomSocket = nil);
    destructor Destroy; override;
    procedure Connect; override;
    procedure Drop; override; //Shutdown
    procedure Disconnect; override;
    function WaitToRead(Timeout: Longint): Boolean; override; //select
    function WaitToWrite(Timeout: Longint): Boolean; override; //select
    property Socket: TmnCustomSocket read FSocket;
  end;

  { TmnConnectionStream }

implementation

const
  cDataBuffSize = 8192;
  cBufferSize = 1024;

{ TmnStream }

destructor TmnSocketStream.Destroy;
begin
  try
    Disconnect;
  finally
    inherited;
  end;
end;

function TmnSocketStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if not Connected then
    DoError('SocketStream not connected for write')
  else if WaitToWrite(Timeout) then
  begin
    if Socket.Send(Buffer, Count) >= erClosed then
    begin
      FreeSocket;
      Result := 0;
    end
    else
      Result := Count;
  end
  else
  begin
    FreeSocket;
    Result := 0;
  end
end;

function TmnSocketStream.DoRead(var Buffer; Count: Longint): Longint;
begin
  Result := 0;
  if not Connected then
    DoError('SocketStream not connected for read')
  else
  begin
    if WaitToRead(Timeout) then
    begin
      if (Socket = nil) or (Socket.Receive(Buffer, Count) >= erClosed) then
      begin
        FreeSocket;
        Result := 0;
      end
      else
        Result := Count;
    end
    else
    begin
      FreeSocket;
      Result := 0;
    end;
  end;
end;

constructor TmnSocketStream.Create(vSocket: TmnCustomSocket);
begin
  inherited Create;
  FSocket := vSocket;
end;

procedure TmnSocketStream.Disconnect;
begin
  if (Socket <> nil) and Socket.Connected then
    Drop; //may be not but in slow matchine disconnect to take as effects as need (POS in 98)
  FreeSocket;
end;

function TmnSocketStream.GetConnected: Boolean;
begin
  Result := (Socket <> nil) and (Socket.Connected);
end;

procedure TmnSocketStream.Connect;
begin
  if Connected then
    raise EmnStreamException.Create('Already connected');
  if FSocket <> nil then
    raise EmnStreamException.Create('Socket must be nil');
  FSocket := CreateSocket;
  if FSocket = nil then
    raise EmnStreamException.Create('Connected fail');
end;

function TmnSocketStream.CreateSocket: TmnCustomSocket;
begin
  Result := nil;//if server connect no need to create socket
end;

function TmnSocketStream.WaitToRead(Timeout: Integer): Boolean;
var
  err:TmnError;
begin
  err := Socket.Select(Timeout, slRead);
  Result := err = erNone;
end;

function TmnSocketStream.WaitToWrite(Timeout: Integer): Boolean;
begin
  Result := Socket.Select(Timeout, slWrite) = erNone;
end;

procedure TmnSocketStream.FreeSocket;
begin
  FreeAndNil(FSocket);
end;

procedure TmnSocketStream.Drop;
begin
  if Socket <> nil then
    Socket.Shutdown(sdBoth);
end;

end.

