unit mnLinuxSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{$ifdef fpc}
{$mode delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils,
  netdb, sockets, Termio,
  mnSockets;

type

  { TmnSocket }

  TmnSocket = class(TmnCustomSocket)
  private
  protected
    function GetActive: Boolean; override;

    function DoReceive(var Buffer; var Count: Longint): TmnError; override;
    function DoSend(const Buffer; var Count: Longint): TmnError; override;
    //Timeout millisecond
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; override;
    function DoShutdown(How: TmnShutdowns): TmnError; override;
    function DoListen: TmnError; override;
    function DoClose: TmnError; override;
    function DoPending: Boolean; override;
  public
    function GetLocalAddress: string; override;
    function GetRemoteAddress: string; override;
    function GetLocalName: string; override;
    function GetRemoteName: string; override;
  end;

  { TmnWallSocket }

  TmnWallSocket = class(TmnCustomWallSocket)
  private
    function LookupPort(Port: string): Word;
  protected
    procedure FreeSocket(var vHandle: TSocketHandle; out vErr: integer);
    function Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSocketError(Handle: Integer): Integer; override;
    procedure Bind(Options: TmnsoOptions; ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: ansistring; const Address: AnsiString; out vSocket: TmnCustomSocket; out vErr: Integer); override;
  end;

implementation

uses
  BaseUnix;

const
  cBacklog = 5;
  INVALID_SOCKET		= TSocketHandle(NOT(0));
  SOCKET_ERROR			= -1;
  SO_TRUE:Longbool=True;
//  SO_FALSE:Longbool=False;
  TCP_QUICKACK = 12; //Some one said it is work on windows too

function InitSocketOptions(Handle: Integer; Options: TmnsoOptions; ReadTimeout: Integer): Integer;  //return error number
var
  t: Longint;
begin
  Result := 0;
  if (soNoDelay in Options) and not (soNagle in Options) then
  //if not (soNagle in Options) then //TODO
    fpsetsockopt(Handle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
  if soKeepAlive in Options then
    fpsetsockopt(Handle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
  if soQuickAck in Options then
    Result := fpsetsockopt(Handle, SOL_SOCKET, TCP_QUICKACK, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
    //ret := WSAIoctl(sock, SIO_TCP_SET_ACK_FREQUENCY, &freq, sizeof(freq), NULL, 0, &bytes, NULL, NULL);

  if not (soWaitBeforeRead in Options) then
  begin
    if ReadTimeout <> -1 then
    begin
      t := ReadTimeout;
      //* https://stackoverflow.com/questions/2876024/linux-is-there-a-read-or-recv-from-socket-with-timeout
      Result := fpsetsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @t, SizeOf(t));
    end;
  end;
end;

{ TmnSocket }

function TmnSocket.DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError;
begin
  Result := (WallSocket as TmnWallSocket).Select(FHandle, Timeout, Check);
end;

function TmnSocket.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

function TmnSocket.DoClose: TmnError;
var
  err: Longint;
begin
  if Active then
  begin
    err := closesocket(FHandle);
    if err = 0 then
      Result := erSuccess
    else
      Result := erInvalid;
    FHandle := INVALID_SOCKET;
  end
  else
    Result := erClosed;
end;

function TmnSocket.DoPending: Boolean;
var
  Count: Cardinal;
begin
  Count := 0;
  if FpIOCtl(FHandle, FIONREAD, @Count) = SOCKET_ERROR then  //  //ioctl(fd,FIONREAD,&bytes_available)
    Result := False //TODO
  else
    Result := Count > 0;
end;

function TmnSocket.DoShutdown(How: TmnShutdowns): TmnError;
var
  c: Integer;
  iHow: Integer;
begin
  if [sdReceive, sdSend] = How then
    iHow := 2 //SD_BOTH
  else if sdSend in How then
    iHow := 1 //SD_RECEIVE
  else if sdReceive in How then
    iHow := 0 //SD_SEND
  else
  begin
    Result := erInvalid;
    exit;
  end;

  CheckActive;
  c := fpshutdown(FHandle, iHow);
  if c = SOCKET_ERROR then
    Result := erInvalid
  else
    Result := erSuccess;
end;

function TmnSocket.DoListen: TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := fplisten(FHandle, cBacklog);
  if c = SOCKET_ERROR then
    Result := erInvalid
  else
    Result := erSuccess;
end;

function TmnSocket.DoReceive(var Buffer; var Count: Longint): TmnError;
var
  ret: Integer;
  errno: longint;
begin
  ret := fprecv(FHandle, @Buffer, Count, MSG_NOSIGNAL);
  if ret = 0 then
  begin
    Count := 0;
    Result := erClosed;
  end
  else if ret = SOCKET_ERROR then
  begin
    Count := 0;
    //CheckError not directly here
    if soWaitBeforeRead in Options then
      Result := erInvalid
    else
    begin
      errno := SocketError;
      if errno = EsockENOTSOCK then //or EsockEAGAIN
        Result := erTimeout //maybe closed, but we will pass it as timeout, the caller will close it depend on options
      else
        Result := erInvalid;
    end;
  end
  else
  begin
    Count := ret;
    Result := erSuccess;
  end;
end;

function TmnSocket.DoSend(const Buffer; var Count: Longint): TmnError;
var
  ret: Integer;
begin
  ret := fpsend(FHandle, @Buffer, Count, MSG_NOSIGNAL);
  if ret = 0 then
  begin
    Result := erClosed;
    Count := 0;
  end
  else if ret = SOCKET_ERROR then
  begin
    Count := 0;
    Result := erInvalid;
  end
  else
  begin
    Count := ret;
    Result := erSuccess;
  end;
end;

function TmnSocket.GetRemoteAddress: string;
var
  SockAddr: TSockAddr;
  aSize: Integer;
begin
  CheckActive;
  aSize := SizeOf(SockAddr);
  Initialize(SockAddr);
  if fpGetPeerName(FHandle, @SockAddr, @aSize) = 0 then
//    Result := NetAddrToStr(SockAddr.in_addr)
    Result := String(NetAddrToStr(sockaddr_in(SockAddr).sin_addr))
  else
    Result := '';
end;

function TmnSocket.GetRemoteName: string;
var
  SockAddr: TSockAddr;
  Size: Integer;
  s: ansistring;
begin
  CheckActive;
  Size := SizeOf(SockAddr);
  Initialize(SockAddr);
  if fpgetpeername(FHandle, @SockAddr, @Size) = 0 then
  begin
    s := '';//temp
    //gethostbyaddr(@SockAddr.sin_addr.s_addr, 4, AF_INET);
  end
  else
    s := '';
  Result := s;
end;

function TmnSocket.GetLocalAddress: string;
var
  SockAddr: TSockAddr;
  aSize: Integer;
begin
  CheckActive;
  aSize := SizeOf(SockAddr);
  if fpGetSockName(FHandle, @SockAddr, @aSize) = 0 then
//    Result := NetAddrToStr(SockAddr)
  else
    Result := '';
end;

function TmnSocket.GetLocalName: string;
var
  s: ansistring;
begin
  CheckActive;
  SetLength(s, 250);
//  fpgethostname(PChar(s), Length(s));
  s := '';//temp
  Result := s;
end;

{ TmnWallSocket }

function TmnWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

procedure TmnWallSocket.Bind(Options: TmnsoOptions; ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aSockAddr : TINetSockAddr;
  aHostEnt: PHostEntry;
begin
  aHandle := fpsocket(AF_INET, SOCK_STREAM, 0{IPPROTO_TCP});

  if aHandle <> INVALID_SOCKET then
  begin
    //fpsetsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));



    vErr := InitSocketOptions(aHandle, Options, ReadTimeout);

    if soReuseAddr in Options then
      fpsetsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_TRUE), SizeOf(SO_TRUE));

    aSockAddr.sin_family := AF_INET;
    aSockAddr.sin_port := htons(LookupPort(Port));
    if (Address = '') or (Address = '0.0.0.0') then
      aSockAddr.sin_addr.s_addr := INADDR_ANY
    else
      aSockAddr.sin_addr := StrToNetAddr(Address);

    if fpbind(aHandle,@aSockAddr, Sizeof(aSockAddr)) <> 0 then
    begin
      FreeSocket(aHandle, vErr);
    end;
  end;

  if aHandle <> INVALID_SOCKET then
    vSocket := TmnSocket.Create(aHandle, Options, skListener)
  else
    vSocket := nil;
end;

procedure TmnWallSocket.Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
begin
  aHandle := fpaccept(ListenerHandle, nil, nil); //aHandle := fpaccept(ListenerHandle, @aAddr, @aSize); //aAddr : TINetSockAddr;
  if aHandle < 0 then
  begin
    vSocket := nil;
    vErr := -1;
  end
  else
  begin
    InitSocketOptions(aHandle, Options, ReadTimeout);
    vSocket := TmnSocket.Create(aHandle, Options, skServer);
    vErr := 0;
  end;
end;

function TmnWallSocket.GetSocketError(Handle: Integer): Integer;
var
  errno: Longint;
  l: Integer;
begin
  l := SizeOf(errno);
  if fpgetsockopt(Handle, SOL_SOCKET, SO_ERROR, @errno, @l) <> 0 then
    Result := errno
  else
    Result := 0;
end;

procedure TmnWallSocket.FreeSocket(var vHandle: TSocketHandle; out vErr: integer );
begin
  vErr := SocketError;
  closesocket(vHandle);
  vHandle := INVALID_SOCKET;
end;

function TmnWallSocket.Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
var
  FSet: TFDSet;
  PSetRead, PSetWrite: PFDSet;
  c: Integer;
begin
  //CheckActive; no need select will return error for it, as i tho
  if vHandle = INVALID_SOCKET then
    Result := erClosed
  else
  begin
    fpfd_zero(FSet);
    fpfd_set(vHandle, FSet);
    if Check = slRead then
    begin
      PSetRead := @FSet;
      PSetWrite := nil;
    end
    else
    begin
      PSetRead := nil;
      PSetWrite := @FSet;
    end;

    c := fpselect(vHandle + 1, PSetRead, PSetWrite, PSetRead, Timeout);
    if (c = SOCKET_ERROR) then
      Result := erInvalid
    else if (c = 0) then
      Result := erTimeout
    else
      Result := erSuccess;
  end;
end;

procedure TmnWallSocket.Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: ansistring; const Address: AnsiString; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aAddr : TINetSockAddr;
  aHost: THostEntry;
  aMode: longint;
  ret: cint;
  time: ttimeval;
  DW: Integer;
begin
  //nonblick connect  https://stackoverflow.com/questions/1543466/how-do-i-change-a-tcp-socket-to-be-non-blocking
  //https://stackoverflow.com/questions/14254061/setting-time-out-for-connect-function-tcp-socket-programming-in-c-breaks-recv
  aHandle := fpsocket(AF_INET, SOCK_STREAM{TODO: for nonblock option: or O_NONBLOCK}, 0{IPPROTO_TCP});
  if aHandle <> INVALID_SOCKET then
  begin
    vErr := InitSocketOptions(aHandle, Options, ReadTimeout);

    if ConnectTimeout <> -1 then
    begin
      aMode := 1;
      ret := FpIOCtl(aHandle, FIONBIO, @aMode);
      if ret = Longint(SOCKET_ERROR) then
      begin
        FreeSocket(aHandle, vErr);
      end;
    end;

    if aHandle <> TSocketHandle(SOCKET_ERROR) then
    begin
      aAddr.sin_family := AF_INET;
      aAddr.sin_port := htons(StrToIntDef(Port, 0));

      if Address = '' then
        aAddr.sin_addr.s_addr := INADDR_ANY
      else
      begin
        aAddr.sin_addr := StrToNetAddr(Address);
        if (aAddr.sin_addr.s_addr = 0) then
        begin
          if ResolveHostByName(Address, aHost) then
          begin
            aAddr.sin_addr.s_addr := aHost.Addr.s_addr;
          end;
        end;
      end;
      ret := fpconnect(aHandle, @aAddr, SizeOf(aAddr));
      if ret = -1 then
      begin
        if (ConnectTimeout <> -1) and (SocketError = EsockEWOULDBLOCK) then
        begin
          aMode := 0;
          ret := FpIOCtl(aHandle, Longint(FIONBIO), @aMode);
          if ret = Longint(SOCKET_ERROR) then
            FreeSocket(aHandle, vErr)
          else if Select(aHandle, ConnectTimeout, slWrite) <> erSuccess then
            FreeSocket(aHandle, vErr);
        end
        else
          FreeSocket(aHandle, vErr);
      end;
    end;
  end;

  if aHandle <> INVALID_SOCKET then
    vSocket := TmnSocket.Create(aHandle, Options, skClient)
  else
    vSocket := nil;
end;

constructor TmnWallSocket.Create;
begin
  inherited;
end;

destructor TmnWallSocket.Destroy;
begin
  inherited;
end;

end.
