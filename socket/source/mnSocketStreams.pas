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
    FOptions: TmnsoOptions;
    procedure FreeSocket;
  protected
    function GetConnected: Boolean; override;
    function CreateSocket: TmnCustomSocket; virtual;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
    procedure DoCloseWrite; override;
    procedure DoCloseRead; override;
  public
    constructor Create(vSocket: TmnCustomSocket = nil);
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function WaitToRead(vTimeout: Longint): TmnConnectionError; override; //select
    function WaitToWrite(vTimeout: Longint): TmnConnectionError; override; //select
    property Socket: TmnCustomSocket read FSocket;
    property Options: TmnsoOptions read FOptions write FOptions;
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
  if not Connected then
  begin
    FreeSocket;
    Result := 0;
    //DoError('Write: SocketStream not connected.') //we can't decide if it is error or disconnected gracefully, you need to check connected before write, maybe socket shutdown for write only
  end
  else if WaitToWrite(Timeout) = cerSuccess then //TODO WriteTimeout
  begin
    if Socket.Send(Buffer, Count) >= erTimeout then //yes in send we take timeout as error, we cant try again
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

procedure TmnSocketStream.DoCloseWrite;
begin
  inherited;
  if Socket <> nil then
    Socket.Shutdown([sdSend]);
end;

procedure TmnSocketStream.DoCloseRead;
begin
  inherited;
  if Socket <> nil then
    Socket.Shutdown([sdReceive]);
end;

function TmnSocketStream.DoRead(var Buffer; Count: Longint): Longint;
var
  err: TmnError;
  werr: TmnConnectionError;
begin
  Result := 0;
  if not Connected then
    ReadError //set EOF or raise error, not sure about raising error
  else
  begin
    werr := WaitToRead(Timeout);
    if (werr = cerSuccess) or ((werr = cerTimeout) and (soSafeReadTimeout in Options)) then
    begin
      if (Socket = nil) then
        Result := 0
      else
      begin
        err := Socket.Receive(Buffer, Count);
        if not ((err = erSuccess) or ((err = erTimeout) and (soSafeReadTimeout in Options))) then
        begin
          FreeSocket;
          Result := 0;
        end
        else
          Result := Count;
      end;
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
  FOptions := [soNoDelay, soWaitBeforeRead];
  FSocket := vSocket;
end;

procedure TmnSocketStream.Disconnect;
begin
  if (Socket <> nil) and Socket.Connected then
    Close; //may be not but in slow matchine disconnect to take as effects as need (POS in 98)
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
    if soSafeConnect in Options then
      exit
    else
      raise EmnStreamException.Create('Connected fail');
end;

function TmnSocketStream.CreateSocket: TmnCustomSocket;
begin
  Result := nil;//if server connect no need to create socket
end;

function TmnSocketStream.WaitToRead(vTimeout: Longint): TmnConnectionError;
var
  err: TmnError;
begin
  err := Socket.Select(vTimeout, slRead);
  if err = erSuccess then
    Result := cerSuccess
  else if (err = erTimeout) and (soSafeReadTimeout in Options) then
    Result := cerTimeout
  else	
  	Result := cerError;
end;

function TmnSocketStream.WaitToWrite(vTimeout: Longint): TmnConnectionError;
var
  err: TmnError;
begin
  err := Socket.Select(vTimeout, slWrite);
  if err = erSuccess then
    Result := cerSuccess
  else
  	Result := cerError;
end;

procedure TmnSocketStream.FreeSocket;
begin
  FreeAndNil(FSocket);
end;

end.

