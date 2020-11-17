unit mnWinSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 * Mini Socket for Windows, but it compile in Windows fpc/delphi
 *
 *}

{$ifdef fpc}
{$mode delphi}
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}{$IFDEF WINDOWS} WinSock2,{$ELSE} sockets,{$ENDIF}{$ELSE DELPHI}WinSock2,{$ENDIF}
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
    function GetRemoteAddress: string; override;
    function GetRemoteName: string; override;
    function GetLocalAddress: string; override;
    function GetLocalName: string; override;
  end;

  { TmnWallSocket }

  TmnWallSocket = class(TmnCustomWallSocket)
  private
    FWSAData: TWSAData;
    FCount: Integer;
    function LookupPort(Port: string): Word;
  protected
    procedure FreeSocket(var vHandle: TSocketHandle);
    function Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSocketError(Handle: TSocketHandle): Integer; override;
    procedure Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Bind(Options: TmnsoOptions; ListenTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); override;

    procedure Startup;
    procedure Cleanup;
  end;

implementation

const
  cBacklog = 5;
  INVALID_SOCKET: Integer = -1;
  SO_TRUE: Longbool = True;
//  SO_FALSE:Longbool=False;
  TCP_QUICKACK = 12; //Some one said it is work on windows too

function InitSocketOptions(Handle: Integer; Options: TmnsoOptions; ReadTimeout: Integer): Integer;  //return error number
var
  t: Longint;
begin
  Result := 0;
  if (soNoDelay in Options) and not (soNagle in Options) then
  //if not (soNagle in Options) then //TODO
    Result := setsockopt(Handle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
  if soKeepAlive in Options then
    Result := setsockopt(Handle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
  if soQuickAck in Options then
    Result := setsockopt(Handle, SOL_SOCKET, TCP_QUICKACK, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
    //ret := WSAIoctl(sock, SIO_TCP_SET_ACK_FREQUENCY, &freq, sizeof(freq), NULL, 0, &bytes, NULL, NULL);

  if not (soWaitBeforeRead in Options) then
  begin
    if ReadTimeout <> -1 then
    begin
      t := ReadTimeout;
      //* https://stackoverflow.com/questions/2876024/linux-is-there-a-read-or-recv-from-socket-with-timeout
      Result := setsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, @t, SizeOf(t));
    end;
  end;
end;

{ TmnSocket }

function TmnSocket.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

function TmnSocket.DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError;
begin
  Result := (WallSocket as TmnWallSocket).Select(FHandle, Timeout, Check);
end;

function TmnSocket.DoClose: TmnError;
var
  err: Longint;
begin
  if Active then
  begin
    err := WinSock2.CloseSocket(FHandle);
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
  if ioctlsocket(FHandle, FIONREAD, Count) = SOCKET_ERROR then
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
    iHow := SD_BOTH
  else if sdReceive in How then
    iHow := SD_RECEIVE
  else if sdSend in How then
    iHow := SD_SEND
  else
  begin
    Result := erInvalid;
    exit;
  end;

  CheckActive;
  c := WinSock2.Shutdown(FHandle, iHow);
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
  c := WinSock2.listen(FHandle, cBacklog);
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
  ret := WinSock2.recv(FHandle, Buffer, Count, 0);
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
      errno := WSAGetLastError(); //not work with OpenSSL because it reset error to 0, now readtimeout in socket options not usefull
      if errno = WSAETIMEDOUT then
        Result := erTimeout //the caller will close it depend on options
      else
        Result := erInvalid
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
  ret := WinSock2.send(FHandle, (@Buffer)^, Count, 0);
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
  SockAddrIn: {$ifdef FPC}TSockAddrIn;{$else} TSockAddr;{$endif}
  Size: Integer;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
  {$ifdef FPC} Initialize(SockAddrIn); {$endif}
  if getpeername(FHandle, SockAddrIn, Size) = 0 then
    //Result := inet_ntoa(SockAddrIn.sin_addr)
    Result := String(inet_ntoa(sockaddr_in(SockAddrIn).sin_addr))
  else
    Result := '';
end;

function TmnSocket.GetRemoteName: string;
var
  SockAddrIn: {$ifdef FPC}TSockAddrIn;{$else}TSockAddr;{$endif}
  Size: Integer;
  aHostEnt: PHostEnt;
  s: ansistring;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
  {$ifdef FPC}Initialize(SockAddrIn);{$endif}
  if getpeername(FHandle, SockAddrIn, Size) = 0 then
  begin
    {$ifdef FPC}
    aHostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
    {$else}
    aHostEnt := gethostbyaddr(sockaddr_in(SockAddrIn).sin_addr.s_addr, 4, PF_INET);
    {$endif}

    if aHostEnt <> nil then
      s := aHostEnt.h_name
    else
      s := '';
  end
  else
    s := '';
  Result := string(s);
end;

function TmnSocket.GetLocalAddress: string;
var
  aName: AnsiString;
  aAddr: PAnsiChar;
  sa: TInAddr;
  aHostEnt: PHostEnt;
begin
  CheckActive;
  {$IFDEF FPC}
  aName := '';
  {$endif}
  SetLength(aName, 250);
  WinSock2.gethostname(PAnsiChar(aName), Length(aName));
  aName := PAnsiChar(aName);
  aHostEnt := WinSock2.gethostbyname(PAnsiChar(aName));
  if aHostEnt <> nil then
  begin
    aAddr := aHostEnt^.h_addr_list^;
    if aAddr <> nil then
    begin
      {$ifdef FPC}
      sa.S_un_b.s_b1 := aAddr[0];
      sa.S_un_b.s_b2 := aAddr[1];
      sa.S_un_b.s_b3 := aAddr[2];
      sa.S_un_b.s_b4 := aAddr[3];
      {$else}
      sa.S_un_b.s_b1 := Byte(aAddr[0]);
      sa.S_un_b.s_b2 := Byte(aAddr[1]);
      sa.S_un_b.s_b3 := Byte(aAddr[2]);
      sa.S_un_b.s_b4 := Byte(aAddr[3]);
      {$endif}
      Result := String(inet_ntoa(sa))
    end
    else
      Result := '';
  end
  else
    Result := '';
end;

function TmnSocket.GetLocalName: string;
var
  s: ansistring;
begin
  CheckActive;
  {$IFDEF FPC}
  s := '';
  {$endif}
  SetLength(s, 250);
  WinSock2.gethostname(PAnsiChar(s), Length(s));
  s := PAnsiChar(s);
  Result := string(s);
end;

{ TmnWallSocket }

constructor TmnWallSocket.Create;
begin
  inherited;
  Startup;
end;

destructor TmnWallSocket.Destroy;
begin
  inherited;
  Cleanup;
end;

function TmnWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

function TmnWallSocket.GetSocketError(Handle: TSocketHandle): Integer;
var
  errno: Longint;
  l: Integer;
begin
  l := SizeOf(errno);
  if getsockopt(Handle, SOL_SOCKET, SO_ERROR, @errno, l) = 0 then
    Result := errno
  else
    Result := -1;
end;

procedure TmnWallSocket.FreeSocket(var vHandle: TSocketHandle);
begin
  if (vHandle <> 0) or (vHandle <>  INVALID_SOCKET) then
    WinSock2.CloseSocket(vHandle);
  vHandle := INVALID_SOCKET;
end;

procedure TmnWallSocket.Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
begin
  aHandle := WinSock2.Accept(ListenerHandle, nil, nil);
  if aHandle = INVALID_SOCKET then
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

function TmnWallSocket.Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
var
  FSet: TFDSet;
  PSetRead, PSetWrite: PFDSet;
  TimeVal: TTimeVal;
  TimeValPtr: PTimeVal;
  c: Integer;
begin
  //CheckActive; no need select will return error for it, as i tho
  if vHandle = INVALID_SOCKET then
    Result := erClosed
  else
  begin
    {$ifdef FPC}
    Initialize(FSet);
    FD_ZERO(FSet);
    FD_SET(vHandle, FSet);
    {$else}
    FD_ZERO(FSet);
    _FD_SET(vHandle, FSet);
    {$endif}
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

    if Timeout = -1 then
      TimeValPtr := nil
    else
    begin
      TimeVal.tv_sec := Timeout div 1000;
      TimeVal.tv_usec := (Timeout mod 1000) * 1000;
      TimeValPtr := @TimeVal;
    end;

    c := WinSock2.select(0, PSetRead, PSetWrite, nil, TimeValPtr);
    if (c = SOCKET_ERROR) then
      Result := erInvalid
    else if (c = 0) then
      Result := erTimeout
    else
      Result := erSuccess;
  end
end;

procedure TmnWallSocket.Bind(Options: TmnsoOptions; ListenTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aSockAddr: {$ifdef FPC}TSockAddr;{$else}TSockAddrIn;{$endif}
  aHostEnt: PHostEnt;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);

  if aHandle <> INVALID_SOCKET then
  begin
    //https://stackoverflow.com/questions/55034112/c-disable-delayed-ack-on-windows
    //aFreq := 1; // can be 1..255, default is 2
    //aErr := ioctlsocket(sock, SIO_TCP_SET_ACK_FREQUENCY, &freq);

    vErr := InitSocketOptions(aHandle, Options, ListenTimeout);

    if soReuseAddr in Options then
      WinSock2.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));

    aSockAddr.sin_family := AF_INET;
    aSockAddr.sin_port := htons(LookupPort(Port));
    if (Address = '') or (Address = '0.0.0.0') then
      aSockAddr.sin_addr.s_addr := INADDR_ANY
    else
    begin
      aSockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(Address)));
      if ((aSockAddr.sin_addr.s_addr) = u_long(INADDR_NONE)) or (aSockAddr.sin_addr.s_addr = u_long(SOCKET_ERROR)) then
      begin
        aHostEnt := gethostbyname(PAnsiChar(AnsiString(Address)));
        if aHostEnt <> nil then
        begin
          {$ifdef FPC}
          aSockAddr.sin_addr.S_un_b.s_b1 := aHostEnt.h_addr^[0];
          aSockAddr.sin_addr.S_un_b.s_b2 := aHostEnt.h_addr^[1];
          aSockAddr.sin_addr.S_un_b.s_b3 := aHostEnt.h_addr^[2];
          aSockAddr.sin_addr.S_un_b.s_b4 := aHostEnt.h_addr^[3];
          {$else}
          aSockAddr.sin_addr.S_un_b.s_b1 := Byte(aHostEnt.h_addr^[0]);
          aSockAddr.sin_addr.S_un_b.s_b2 := Byte(aHostEnt.h_addr^[1]);
          aSockAddr.sin_addr.S_un_b.s_b3 := Byte(aHostEnt.h_addr^[2]);
          aSockAddr.sin_addr.S_un_b.s_b4 := Byte(aHostEnt.h_addr^[3]);
          {$endif}
          aSockAddr.sin_family := aHostEnt.h_addrtype;
        end;
      end;
    end;
    {$IFDEF FPC}
    if WinSock2.bind(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
    {$ELSE}
    if WinSock2.bind(aHandle, TSockAddr(aSockAddr), SizeOf(aSockAddr)) = SOCKET_ERROR then
    {$ENDIF}
    begin
      vErr := WSAGetLastError;
      FreeSocket(aHandle);
    end;
  end;

  if aHandle <> INVALID_SOCKET then
    vSocket := TmnSocket.Create(aHandle, Options, skListener)
  else
    vSocket := nil;
end;

procedure TmnWallSocket.Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aAddr: {$ifdef FPC}TSockAddr;{$else}TSockAddrIn;{$endif}
  aHost: PHostEnt;
  ret: Longint;
  aMode: u_long;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle <> INVALID_SOCKET then
  begin
    vErr := InitSocketOptions(aHandle, Options, ReadTimeout);

    if ConnectTimeout <> -1 then
    begin
      aMode := 1;
      ret := ioctlsocket(aHandle, Longint(FIONBIO), aMode);
      if ret = Longint(SOCKET_ERROR) then
      begin
        vErr := WSAGetLastError;
        FreeSocket(aHandle);
      end;
    end;

    if aHandle <> TSocketHandle(SOCKET_ERROR) then
    begin
      aAddr.sin_family := AF_INET;
      aAddr.sin_port := htons(LookupPort(Port));

      if Address = '' then
        aAddr.sin_addr.s_addr := INADDR_ANY
      else
      begin
        aAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(Address)));
        if (aAddr.sin_addr.s_addr = 0) or (aAddr.sin_addr.s_addr = u_long(SOCKET_ERROR)) then
        begin
          aHost := gethostbyname(PAnsiChar(AnsiString(Address)));
          if aHost <> nil then
          begin
            {$ifdef FPC}
            aAddr.sin_addr.S_un_b.s_b1 := aHost.h_addr^[0];
            aAddr.sin_addr.S_un_b.s_b2 := aHost.h_addr^[1];
            aAddr.sin_addr.S_un_b.s_b3 := aHost.h_addr^[2];
            aAddr.sin_addr.S_un_b.s_b4 := aHost.h_addr^[3];
            {$else}
            aAddr.sin_addr.S_un_b.s_b1 := Byte(aHost.h_addr^[0]);
            aAddr.sin_addr.S_un_b.s_b2 := Byte(aHost.h_addr^[1]);
            aAddr.sin_addr.S_un_b.s_b3 := Byte(aHost.h_addr^[2]);
            aAddr.sin_addr.S_un_b.s_b4 := Byte(aHost.h_addr^[3]);
            {$endif}
            aAddr.sin_family := aHost.h_addrtype;
          end;
        end;
      end;
      if aHandle <> TSocketHandle(SOCKET_ERROR) then
      begin
      {$IFDEF FPC}
        ret := WinSock2.connect(aHandle, aAddr, SizeOf(aAddr));
      {$ELSE}
        ret := WinSock2.connect(aHandle, TSockAddr(aAddr), SizeOf(aAddr));
      {$ENDIF}
        if (ret = SOCKET_ERROR) then
        begin
          vErr := WSAGetLastError;
          if (ConnectTimeout <> -1) and ((vErr = WSAEWOULDBLOCK) or (vErr = WSAEINPROGRESS)) then
          begin
            aMode := 0;
            ret := ioctlsocket(aHandle, Longint(FIONBIO), aMode);
            if ret = Longint(SOCKET_ERROR) then
            begin
              vErr := WSAGetLastError;
              FreeSocket(aHandle)
            end
            else if Select(aHandle, ConnectTimeout, slWrite) <> erSuccess then
            begin
              vErr := WSAGetLastError;
              FreeSocket(aHandle);
            end;
          end
          else
            FreeSocket(aHandle);
        end;
      end;
    end;
  end;

  if aHandle <> INVALID_SOCKET then
    vSocket := TmnSocket.Create(aHandle, Options, skClient)
  else
    vSocket := nil;
end;

procedure TmnWallSocket.Startup;
var
  e: Integer;
begin
  if FCount = 0 then
  begin
    e := WSAStartup($0202, FWSAData);
    if e <> 0 then
      raise EmnException.Create('Failed to initialize WinSocket,error #' + IntToStr(e));
  end;
  Inc(FCount)
end;

procedure TmnWallSocket.Cleanup;
begin
  Dec(FCount);
  if FCount = 0 then
    WSACleanup;
end;

end.
