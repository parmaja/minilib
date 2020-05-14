unit mnSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$M+}
{$H+}

interface

uses
  Classes,
  SysUtils,
  mnStreams;

type
  EmnException = class(Exception);
  TmnShutdown = (sdReceive, sdSend);
  TmnShutdowns = set of TmnShutdown;
  TmnError = (erSuccess, erTimeout, erClosed, erInvalid);
  TSelectCheck = (slRead, slWrite);

  TmnsoOption = (
    soReuseAddr,
    soKeepAlive,
    soNoDelay,
    //soBroadcast, soDebug, soDontLinger, soDontRoute, soOOBInLine, soAcceptConn
    soSetReadTimeout, //Set socket read timeout
    soWaitBeforeRead, //Wait for data come before read, that double the time wait if you set SetReadTimeout if no data come
    soWaitBeforeWrite, //Wait for ready before write, idk what for
    soConnectTimeout, //Connect will use Timeout to wait it
    soSafeReadTimeout //Keep socket connected if read timeout without error
    );
  TmnsoOptions = set of TmnsoOption;

  //maybe we should name it TmnSocket

  { TmnCustomSocket }

  TmnCustomSocket = class abstract(TObject)
  private
    FShutdownState: TmnShutdowns;
    function GetConnected: Boolean;
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure CheckActive; //this will force exception, cuz you should not use socket in api implmentation without active socket, i meant use it in api section only
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; virtual; abstract;
    function DoShutdown(How: TmnShutdowns): TmnError; virtual; abstract;
    function DoListen: TmnError; virtual; abstract;
    function DoSend(const Buffer; var Count: Longint): TmnError; virtual; abstract;
    function DoReceive(var Buffer; var Count: Longint): TmnError; virtual; abstract;
    property ShutdownState: TmnShutdowns read FShutdownState;
  public
    constructor Create;
    destructor Destroy; override;
    function Shutdown(How: TmnShutdowns): TmnError;
    function Close: TmnError; virtual; abstract;
    function Send(const Buffer; var Count: Longint): TmnError;
    function Receive(var Buffer; var Count: Longint): TmnError;
    function Select(Timeout: Integer; Check: TSelectCheck): TmnError;
    function Listen: TmnError;
    function Accept: TmnCustomSocket; virtual; abstract;
    property Active: Boolean read GetActive;
    property Connected: Boolean read GetConnected;
    function GetLocalAddress: string; virtual; abstract;
    function GetRemoteAddress: string; virtual; abstract;
    function GetLocalName: string; virtual; abstract;
    function GetRemoteName: string; virtual; abstract;
  end;

  { TmnCustomWallSocket }

  TmnCustomWallSocket = class abstract(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Bind(Options: TmnsoOptions; const Port: string; const Address: string = ''): TmnCustomSocket; virtual; abstract;
    function Connect(Options: TmnsoOptions; Timeout: Integer; const Port: string; const Address: string = ''): TmnCustomSocket; virtual; abstract;
  end;

  { Streams
    You can use mnClient.TmnClientSocketStrean or mnServer also
  }


  { TmnSocketStream }

  TmnSocketStream = class (TmnConnectionStream)
  private
    FSocket: TmnCustomSocket;
    FOptions: TmnsoOptions;
  protected
    procedure FreeSocket; virtual;
    function GetConnected: Boolean; override;
    function CreateSocket: TmnCustomSocket; virtual;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
    procedure DoCloseWrite; override;
    procedure DoCloseRead; override;
  public
    constructor Create; overload;
    constructor Create(vSocket: TmnCustomSocket); overload;
    destructor Destroy; override;
    procedure Connect; override;
    procedure Disconnect; override;
    function WaitToRead(vTimeout: Longint): TmnConnectionError; override; //select
    function WaitToWrite(vTimeout: Longint): TmnConnectionError; override; //select
    property Socket: TmnCustomSocket read FSocket;
    property Options: TmnsoOptions read FOptions write FOptions;
  end;

const
  WaitForEver: Longint = -1;

function WallSocket: TmnCustomWallSocket;

implementation

uses
  {$ifdef FPC}
    {$ifdef WINDOWS} //Win32 and WinCE
     mnWinSockets
    {$else}
    {$ifdef LINUX}
     mnLinuxSockets
    {$endif}
    {$endif};
  {$else}
    {$if DEFINED(MSWINDOWS)} //Win32 and WinCE
     mnWinSockets //delphi is only Win32
    {$elseif DEFINED(LINUX)}
     mnLinuxSockets
    {$else}
     mnPosixSockets
    {$endif};
  {$endif}
var
  FmnWallSocket: TmnCustomWallSocket = nil;

function WallSocket: TmnCustomWallSocket;
begin
  if FmnWallSocket = nil then
    FmnWallSocket := TmnWallSocket.Create;
  Result := FmnWallSocket;
end;

{ TmnCustomWallSocket }

constructor TmnCustomWallSocket.Create;
begin
  inherited;
end;

destructor TmnCustomWallSocket.Destroy;
begin
  inherited;
end;

{ TmnCustomSocket }

procedure TmnCustomSocket.CheckActive;
begin
  if (Self = nil) or (not Active) then
  begin
    Close;
    raise EmnException.Create('Socket is inactive');
  end
end;

constructor TmnCustomSocket.Create;
begin
  inherited;
end;

destructor TmnCustomSocket.Destroy;
begin
  if Active then
  begin
    Close;
  end;
  inherited;
end;

function TmnCustomSocket.GetConnected: Boolean;
begin
  Result := Active and ([sdReceive, sdSend] <> FShutdownState)
end;

function TmnCustomSocket.Listen: TmnError;
begin
  Result := DoListen;
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Receive(var Buffer; var Count: Longint): TmnError;
begin
  Result := DoReceive(Buffer, Count);
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Select(Timeout: Integer; Check: TSelectCheck): TmnError;
begin
  Result := DoSelect(Timeout, Check);
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Send(const Buffer; var Count: Longint): TmnError;
begin
  Result := DoSend(Buffer, Count);
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Shutdown(How: TmnShutdowns): TmnError;
begin
  if How <> [] then
  begin
    Result := DoShutdown(How);
    if Result = erSuccess then
      FShutdownState := FShutdownState + How
    else
      if Result > erTimeout then
        Close;
  end
  else
    Result := erSuccess;
end;

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
  else if not (soWaitBeforeWrite in Options) or (WaitToWrite(Timeout) = cerSuccess) then //TODO WriteTimeout
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
    if soWaitBeforeRead in Options then
      werr := WaitToRead(Timeout)
    else
      werr := cerSuccess;

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
  FOptions := [soNoDelay, soWaitBeforeRead, soWaitBeforeWrite];
  FSocket := vSocket;
end;

constructor TmnSocketStream.Create;
begin
  Create(nil);
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

initialization
finalization
  FreeAndNil(FmnWallSocket);
end.
