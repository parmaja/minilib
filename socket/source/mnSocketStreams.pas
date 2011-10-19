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
  cReadTimeout = 15000;
  cDataBuffSize = 8192;
  cBufferSize = 1024;

type
  { TmnSocketStream }

  TmnSocketStream = class(TmnBufferStream)
  private
    FTimeout: Integer;
    FSocket: TmnCustomSocket;
    function GetConnected: Boolean;
    procedure FreeSocket;
  protected
    function IsActive: Boolean; override;
    function CreateSocket: TmnCustomSocket; virtual;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
  public
    constructor Create(vSocket: TmnCustomSocket = nil); virtual;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Shutdown;
    function WaitToRead(Timeout: Longint = -1): Boolean; //select
    function WaitToWrite(Timeout: Longint = -1): Boolean; //select
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property Socket: TmnCustomSocket read FSocket;
    property Timeout: Integer read FTimeout write FTimeout;
    property Connected: Boolean read GetConnected;
  end;

  { TmnConnectionStream }

implementation

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
    DoError('SocketStream not connected')
  else if WaitToWrite(FTimeout) then
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
    DoError('SocketStream not connected')
  else
  begin
    if WaitToRead(FTimeout) then
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

function TmnSocketStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
{$IFDEF FPC}
  Result := 0;
{$ENDIF}  
  raise Exception.Create('not supported')
end;

constructor TmnSocketStream.Create(vSocket: TmnCustomSocket);
begin
  inherited Create;
  FSocket := vSocket;
  FTimeout := cReadTimeout;
end;

function TmnSocketStream.WaitToRead(Timeout: Integer): Boolean;
var
  err:TmnError;
begin
  err := Socket.Select(Timeout, slRead); 
  Result := err = erNone;
end;

procedure TmnSocketStream.Disconnect;
begin
  if (Socket <> nil) and Socket.Connected then
    Shutdown; //may be not but in slow matchine disconnect to take as effects as need (POS in 98)
  FreeSocket;
end;

function TmnSocketStream.GetConnected: Boolean;
begin
  Result := (Socket <> nil);
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

function TmnSocketStream.WaitToWrite(Timeout: Integer): Boolean;
begin
  Result := Socket.Select(Timeout, slWrite) = erNone;
end;

procedure TmnSocketStream.FreeSocket;
begin
  FreeAndNil(FSocket);
end;

function TmnSocketStream.IsActive: Boolean;
begin
  Result := Connected;
end;

procedure TmnSocketStream.Shutdown;
begin
  if Socket <> nil then
    Socket.Shutdown(sdBoth);
end;

end.

