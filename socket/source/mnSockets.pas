unit mnSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

 {
    TODO: SIO_TCP_SET_ACK_FREQUENCY

 }

{
  https://stackoverflow.com/questions/3897883/how-to-detect-an-incoming-ssl-https-handshake-ssl-wire-format

  SSL 3.0 or TLS 1.0, 1.1 and 1.2
  +-------+------------------+------------------+--------+------
  | 0x16  | 2 bytes version  |  2 bytes length  |  0x01  |  etc.
  +-------+------------------+------------------+--------+------
      b[0] == 0x16 (message type "SSL handshake")
      b[1] should be 0x03 (currently newest major version, but who knows in future?)
      b[5] must be 0x01 (handshake protocol message "HelloClient")
}


{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$M+}
{$H+}

interface

uses
  Classes,
  SysUtils,
  mnOpenSSL,
  mnStreams;

const
  WaitForEver: Longint = -1;

type
  EmnException = class(Exception);
  EmnSocketException = class(Exception);
  TmnSocketState = (sdReceive, sdSend, sdClose);
  TmnSocketStates = set of TmnSocketState;
  TmnError = (erSuccess, erTimeout, erClosed, erInvalid);

  TSocketHandle = NativeInt;

  TSelectCheck = (slRead, slWrite);

  TmnsoOption = (
    soReuseAddr,
    soKeepAlive,
    soNagle, //TODO
    soNoDelay, //deprecated, Nagle's algorithm use it for faster communication, do not wait until ACK for previously sent data and accumulate data in send buffer...
    //soOOBINLINE todo SO_OOBINLINE = 10;
    soQuickAck, //SIO_TCP_SET_ACK_FREQUENCY fo windows, TCP_QUICKACK for Linux
    //soCORK, //not exist in windows //Don't send any data (partial frames) smaller than the MSS until the application says so or until 200ms later; is opposite of soNoDelay. The former forces packet-accumulation delay
    //soBroadcast, soDebug, soDontLinger, soDontRoute, soOOBInLine, soAcceptConn
    //MSG_PUSH_IMMEDIATE push in read, idk what for
    soWaitBeforeRead, //Wait for data come before read, that double the time wait if you set set ReadTimeout if no data come
    soWaitBeforeWrite, //Wait for ready before write, idk what for
    soCloseTimeout, //close socket if read timeout
    soSSL,  //Use OpenSSL 1.1.1
    soSSLDebug //* TODO
  );

  TmnsoOptions = set of TmnsoOption;

  TmnSocketParams = record //TODO
    Options: TmnsoOptions;
    ReadTimeout: Integer;
    WriteTimeout: Integer;
  end;

  TSocketKind = (skClient, skServer, skListener);

  //maybe we should name it TmnSocket

  { TmnCustomSocket }

  TmnCustomSocket = class abstract(TObject)
  private
    FOptions: TmnsoOptions;
    FKind: TSocketKind;
    FHostAddress: string;
		FHostName: string;
    function GetConnected: Boolean;
  protected
    FStates: TmnSocketStates;
    FHandle: TSocketHandle; //following OpenSSL handle of socket
    FPrepared: Boolean;

    ContextOwned: Boolean; //if not referenced
    SSL: TSSL;

    function GetActive: Boolean; virtual; abstract;
    procedure CheckActive; //this will force exception, cuz you should not use socket in api implmentation without active socket, i meant use it in api section only
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; virtual; abstract;
    function DoShutdown(How: TmnSocketStates): TmnError; virtual; abstract;
    function DoListen: TmnError; virtual; abstract;
    function DoSend(const Buffer; var Count: Longint): TmnError; virtual; abstract;
    function DoReceive(var Buffer; var Count: Longint): TmnError; virtual; abstract;
    function DoPeek(var Buffer; var Count: Longint): TmnError; virtual; abstract;
    function DoPending: Boolean; virtual; abstract;
    function DoClose: TmnError; virtual; abstract;
    property Kind: TSocketKind read FKind;
    property States: TmnSocketStates read FStates;
  public
    Context: TContext; //Maybe Reference to Listener CTX or external CTX

    constructor Create(AHandle: Integer; AOptions: TmnsoOptions; AKind: TSocketKind; AHostAddress: string = ''; AHostName: string = '');
    destructor Destroy; override;
    procedure EnableSSL;
    procedure Prepare; virtual; //TODO rename Connect;
    function Shutdown(How: TmnSocketStates): TmnError;
    function Close: TmnError;
    function Receive(var Buffer; var Count: Longint): TmnError;
    function Peek(var Buffer; var Count: Longint): TmnError;
    function Send(const Buffer; var Count: Longint): TmnError;
    function Select(Timeout: Integer; Check: TSelectCheck): TmnError;

    function Pending: Boolean; //now checking it for SSL only
    //function Flush: Boolean; //TODO, no flush for TCP, bad design OSs

    function Listen: TmnError;
    function Accept(Options: TmnsoOptions; ReadTimeout: Integer): TmnCustomSocket;
    property Active: Boolean read GetActive;
    property Connected: Boolean read GetConnected;
    function GetLocalAddress: string; virtual; abstract;
    function GetRemoteAddress: string; virtual; abstract;
    function GetLocalName: string; virtual; abstract;
    function GetRemoteName: string; virtual; abstract;
    property Options: TmnsoOptions read FOptions;
    //property Handle: TSocketHandle read FHandle; //I prefer not public it, but i need it into OpenSSL

    property HostAddress: string read FHostAddress;
		property HostName: string read FHostName;
  end;

  { TmnCustomWallSocket }

  TmnCustomWallSocket = class abstract(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    class constructor Init;

    function GetSocketError(Handle: TSocketHandle): Integer; virtual;

    //Bind used by Listener of server
    procedure Bind(Options: TmnsoOptions; ListenTimeout: Integer; var Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); virtual; abstract;
    procedure Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer); virtual; abstract;
    //Connect used by clients
    procedure Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: string; const Address: string; const BindAddress: string; out vSocket: TmnCustomSocket; out vErr: Integer); overload; virtual; abstract;
    function ResolveIP(const Address: string): string; virtual;
  end;

  { Streams
    You can use mnClient.TmnClientSocketStrean or mnServer also
  }

  { TmnSocketStream }

  TmnSocketStream = class(TmnConnectionStream)
  private
    FSocket: TmnCustomSocket;
    FOptions: TmnsoOptions;
  protected
    procedure FreeSocket; virtual;
    function GetConnected: Boolean; override;
    function CreateSocket(out vErr: Integer): TmnCustomSocket; virtual;
    function DoRead(var Buffer; Count: Longint): Longint; override;
    function DoWrite(const Buffer; Count: Longint): Longint; override;
    procedure DoCloseWrite; override;
    procedure DoCloseRead; override;
    procedure DoHandleError(var Handle: Boolean; AError: Integer); virtual;
    function HandleError(AError: Integer): Boolean;
  public
    constructor Create; overload;
    constructor Create(vSocket: TmnCustomSocket); overload;
    destructor Destroy; override;
    procedure EnableSSL;
    procedure Prepare; override;
    procedure Connect; override;
    //Disconnect can be called from out of thread like listener
    procedure Disconnect; override;
    function WaitToRead(vTimeout: Longint): TmnConnectionError; override; //select
    function WaitToWrite(vTimeout: Longint): TmnConnectionError; override; //select
    //This will peek raw data not OpenSSL data
    function Peek(var Buffer; var Count: Longint): Boolean; override;
    property Socket: TmnCustomSocket read FSocket;
    property Options: TmnsoOptions read FOptions write FOptions;
  end;

function WallSocket: TmnCustomWallSocket;

implementation

uses
  {$ifdef FPC}
    {$ifdef WINDOWS}
     mnWinSockets
    {$else}
      {$ifdef LINUX}
       mnFPSockets
      {$endif}
    {$endif};
  {$else}
    {$if DEFINED(MSWINDOWS)}
     mnWinSockets //delphi is only Win32
    {$elseif DEFINED(LINUX)}
     mnPosixSockets
    {$else}
     mnPosixSockets
    {$ifend};
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

function TmnCustomWallSocket.GetSocketError(Handle: TSocketHandle): Integer;
begin
  Result := 0;
end;

class constructor TmnCustomWallSocket.Init;
begin
  WallSocket; //create wall socket in case multi server
end;

function TmnCustomWallSocket.ResolveIP(const Address: string): string;
begin
  Result := '';
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

constructor TmnCustomSocket.Create(AHandle: Integer; AOptions: TmnsoOptions; AKind: TSocketKind; AHostAddress: string; AHostName: string);
begin
  inherited Create;
  FOptions := AOptions;
  FKind := AKind;
  FHandle := AHandle;
  FHostName := AHostName;
  FHostAddress := AHostAddress;
end;

destructor TmnCustomSocket.Destroy;
begin
  SSL.Free;
  if ContextOwned then
    FreeAndNil(Context);
  if Active then
    Close;
  inherited;
end;

procedure TmnCustomSocket.EnableSSL;
begin
  if (FKind in [skClient, skServer]) and not SSL.Active then
  begin
    FOptions :=  FOptions + [soSSL, soWaitBeforeRead];
    if Context = nil then
    begin
      if Kind = skServer then
        Context := TContext.Create(TTLS_SSLServerMethod)
      else
        Context := TContext.Create(TTLS_SSLClientMethod);
      ContextOwned := True;
      //Context.SetVerifyLocation('C:\Programs\curl\bin');
      //Context.SetVerifyFile('D:\temp\ssl\sni.cloudflaressl.com.crt');
    end;

    SSL := TSSL.Init(Context);

    if (HostName <> '') then
      SSL.SetHostName(HostName);

    SSL.SetSocket(FHandle);

    if Kind = skServer then
    begin
      if not SSL.ServerHandshake then
        Close;
    end
    else
    begin
      if not SSL.ClientHandshake then
      begin
        raise EmnSocketException.Create('SSL Connect failed');
      end;
    end;
  end;
end;

procedure TmnCustomSocket.Prepare;
begin
  FPrepared := True;
  if (soSSL in FOptions) then //Listener socket have no OpenSSL
  begin
    EnableSSL;
  end;
end;

function TmnCustomSocket.GetConnected: Boolean;
begin
  Result := Active and not (sdClose in FStates);
end;

function TmnCustomSocket.Listen: TmnError;
begin
  Result := DoListen;
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Accept(Options: TmnsoOptions; ReadTimeout: Integer): TmnCustomSocket;
var
  aErr: Integer;
begin
  CheckActive;
  WallSocket.Accept(FHandle, Options, ReadTimeout, Result, aErr);
end;

function TmnCustomSocket.Receive(var Buffer; var Count: Longint): TmnError;
var
  ReadSize: Integer;
  ret: TsslError;
begin
  //CheckActive; //no i do not want exeption in loop of buffer
  if not Active then
  begin
    Result := erClosed;
    Count := 0;
  end
  else
  begin
    if SSL.Active then
    begin
      ret := SSL.Read(Buffer, Count, ReadSize);
      case ret of
        seTimeout: Result := erTimeout;
        seClosed: Result := erClosed;
        seInvalid: Result := erInvalid;
        else
          Result := erSuccess;
      end;

      if Result=erSuccess then
        Count := ReadSize
      else
        Count := 0;

    end
    else
      Result := DoReceive(Buffer, Count);
    if Result > erTimeout then
      Close
    else if (Result = erTimeout) and (Count = 0) then
      Count := -1;
  end;
end;

function TmnCustomSocket.Select(Timeout: Integer; Check: TSelectCheck): TmnError;
begin
  Result := DoSelect(Timeout, Check);
  if Result > erTimeout then
    Close;
end;

function TmnCustomSocket.Peek(var Buffer; var Count: Longint): TmnError;
begin
  Result := DoPeek(Buffer, Count);
end;

function TmnCustomSocket.Pending: Boolean;
begin
  if SSL.Active then
    Result := SSL.Pending
  else
    Result := DoPending;
end;

function TmnCustomSocket.Send(const Buffer; var Count: Longint): TmnError;
var
  WriteSize: Integer;
  ret: Boolean;
begin
  //CheckActive; //no i do not want exeption in loop of buffer
  if not Active then
  begin
    Result := erClosed;
    Count := 0;
  end
  else
  begin
    if SSL.Active then
    begin
      ret := SSL.Write(Buffer, Count, WriteSize);
      if ret then
      begin
        Count := WriteSize;
        Result := erSuccess;
        if WriteSize <> Count then
          raise EmnSocketException.Create('Ops WriteSize <> Count we should care about real size')
      end
      else
      begin
        Count := 0;
        Result := erInvalid;
      end;
    end
    else
      Result := DoSend(Buffer, Count);
    if Result > erTimeout then
      Close
    else if (Result = erTimeout) and (Count = 0) then
      Count := -1; //Timeout result -1
  end;
end;

function TmnCustomSocket.Shutdown(How: TmnSocketStates): TmnError;
begin
  if How <> [] then
  begin
    {if SSL.Active then
      SSL.ShutDown;} //nop
    Result := DoShutdown(How);

    if Result = erSuccess then
      FStates := FStates + How
    else if Result > erTimeout then
      Close;
  end
  else
    Result := erSuccess;
end;

function TmnCustomSocket.Close: TmnError;
begin
  if Connected then
  begin
    if SSL.Active then //Active do not use soSSL
    begin
      SSL.ShutDown;
//      SSL.Free; //wrong, in waiting handshaking, will crash
    end;
    FStates := FStates + [sdClose];//before, in fail of read it close it, so not close twice
    Result := DoClose;
  end
  else
    Result := erSuccess;
    //or raise exception?
end;

{ TmnStream }

destructor TmnSocketStream.Destroy;
begin
  try
    Disconnect;
    FreeSocket;
  finally
    inherited;
  end;
end;

function TmnSocketStream.Peek(var Buffer; var Count: Longint): Boolean;
begin
  Result := Socket.Peek(Buffer, Count) in [erSuccess, erTimeout];
end;

procedure TmnSocketStream.Prepare;
begin
  Socket.Prepare;
  inherited;
end;

function TmnSocketStream.DoWrite(const Buffer; Count: Longint): Longint;
begin
  if not Connected then
  begin
    FreeSocket;
    Result := 0;
    //DoError('Write: SocketStream not connected.') //we can't decide if it is error or disconnected gracefully, you need to check connected before write, maybe socket shutdown for write only
  end
  else if not (soWaitBeforeWrite in Options) or (WaitToWrite(WriteTimeout) = cerSuccess) then //TODO WriteTimeout
  begin
    if Socket.Send(Buffer, Count) >= erTimeout then //yes in send we take timeout as error, we cant try again
    begin
      Disconnect;
      Result := 0;
    end
    else
      Result := Count;
  end
  else
  begin
    Disconnect;
    Result := 0;
  end
end;

procedure TmnSocketStream.EnableSSL;
begin
  Socket.EnableSSL;
end;

procedure TmnSocketStream.DoCloseWrite;
begin
  inherited;
{  if Socket <> nil then
    Socket.Shutdown([sdSend]);}
end;

procedure TmnSocketStream.DoCloseRead;
begin
  inherited;
{  if Socket <> nil then
    Socket.Shutdown([sdReceive]);}
end;

procedure TmnSocketStream.DoHandleError(var Handle: Boolean; AError: Integer);
begin
end;

function TmnSocketStream.HandleError(AError: Integer): Boolean;
begin
  Result := False;
  DoHandleError(Result, AError);
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
      werr := WaitToRead(ReadTimeout) //useing select, if data in TCP buffer it will back immediately
    else
      werr := cerSuccess;

    if (werr = cerTimeout) then
    begin
      if soCloseTimeout in Options then
        Disconnect;
      Result := 0;
    end
    else if (werr = cerSuccess) then
    begin
      err := Socket.Receive(Buffer, Count);
      if ((err = erTimeout) and (soCloseTimeout in Options)) or (err >= erClosed) then
      begin
        Disconnect;
        Result := 0;
      end
      else
        Result := Count;
    end
    else
    begin
      Disconnect;
      Result := 0;
    end;
  end;
end;

constructor TmnSocketStream.Create(vSocket: TmnCustomSocket);
begin
  inherited Create;
  //FOptions := [soNoDelay];
  FOptions := [];
  FSocket := vSocket;
end;

constructor TmnSocketStream.Create;
begin
  Create(nil);
end;

procedure TmnSocketStream.Disconnect;
begin
  if (Socket <> nil) and Socket.Connected then
  begin
    Close; //may be not but in slow matchine disconnect to take as effects as need (POS in 98)
    Socket.Close; //without it lag when blocking reading
  end;
//  FreeSocket; //Do not free it maybe it is closing from other thread while socket is reading
end;

function TmnSocketStream.GetConnected: Boolean;
begin
  Result := (Socket <> nil) and (Socket.Connected);
end;

procedure TmnSocketStream.Connect;
var
  aErr: Integer;
begin
  if Connected then
    raise EmnStreamException.Create('Already connected');

  if FSocket <> nil then
    raise EmnStreamException.Create('Socket must be nil');

  FSocket := CreateSocket(aErr);

  if FSocket = nil then
  begin
    if not HandleError(aErr) then
      raise EmnSocketException.CreateFmt('Connect failed [#%d]', [aErr]);
  end
  else
  begin
    try
      //check ssl
      FSocket.Prepare;
    except
      Disconnect;
      raise;
    end;
  end;
end;

function TmnSocketStream.CreateSocket(out vErr: Integer): TmnCustomSocket;
begin
  Result := nil;//if server connect no need to create socket
end;

function TmnSocketStream.WaitToRead(vTimeout: Longint): TmnConnectionError;
var
  err: TmnError;
begin
  if Socket.SSL.Active then //testing
  begin
    if Socket.SSL.Pending then
      exit(cerSuccess);
  end;

  err := Socket.Select(vTimeout, slRead);
  if err = erSuccess then
    Result := cerSuccess
  else if (err = erTimeout) then
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
