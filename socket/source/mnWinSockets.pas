unit mnWinSockets;
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
{$IFDEF FPC}
  {$IFDEF WINDOWS}
    WinSock2,
  {$ELSE}
    sockets,
  {$ENDIF}
{$ELSE} // DELPHI
  WinSock,
{$ENDIF}
  mnSockets;

type
  TmnSocket = class(TmnCustomSocket)
  private
    FHandle: TSocket;
    FAddress: TSockAddr;
  protected
    function Valid(Value: Integer; WithZero: Boolean = False): Boolean;
    function Check(Value: Integer; WithZero: Boolean = False): Boolean;
    function GetActive: Boolean; override;
    //Timeout millisecond
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; override;
    function DoShutdown(How: TmnShutdown): TmnError; override;
  public
    constructor Create(vHandle: TSocket);
    procedure Close; override;
    function Accept: TmnCustomSocket; override;
    function Listen: TmnError; override;
    function Receive(var Buffer; var Count: Longint): TmnError; override;
    function Send(const Buffer; var Count: Longint): TmnError; override;
    function GetLocalAddress: string; override;
    function GetRemoteAddress: string; override;
    function GetLocalName: string; override;
    function GetRemoteName: string; override;
  end;

  { TmnWallSocket }

  TmnWallSocket = class(TmnCustomWallSocket)
  private
    FWSAData: TWSAData;
    FCount: Integer;
    function LookupPort(Port: string): Word;
  protected
    function Select(vHandle: TSocket; Timeout: Integer; Check: TSelectCheck): TmnError;
  public
    constructor Create; override;
    destructor Destroy; override;
    //Connect used by servers
    function Bind(Options: TmnsoOptions; const Port: string; const Address: string = ''): TmnCustomSocket; override;
    //Connect used by clients
    function Connect(Options: TmnsoOptions; Timeout: Integer; const Port: string; const Address: string = ''): TmnCustomSocket; override;
    procedure Startup;
    procedure Cleanup;
  end;

implementation

{ TmnSocket }

function TmnSocket.Receive(var Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
{$IFDEF FPC}
  c := WinSock2.recv(FHandle, Buffer, Count, 0);
{$ELSE}
  c := WinSock.recv(FHandle, Buffer, Count, 0);
{$ENDIF}
  if c = 0 then
  begin
    Count := 0;
    Result := erClosed;
    Close;
  end
  else if not Check(c) then
  begin
    Count := 0;
    Result := erInvalid;
    Error;
  end
  else
  begin
    Count := c;
    Result := erNone;
  end;
end;

function TmnSocket.Send(const Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
{$IFDEF FPC}
  c := WinSock2.send(FHandle, (@Buffer)^, Count, 0);
{$ELSE}
  c := WinSock.send(FHandle, (@Buffer)^, Count, 0);
{$ENDIF}
  if c = 0 then
  begin
    Result := erClosed;
    Count := 0;
    Close;
  end
  else if not Check(c) then
  begin
    Count := 0;
    Result := erInvalid;
    Error;
  end
  else
  begin
    Count := c;
    Result := erNone;
  end;
end;

function TmnSocket.DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError;
begin
  Result := (WallSocket as TmnWallSocket).Select(FHandle, Timeout, Check);
  if Result = erFail then
    Error;
end;

function TmnSocket.Valid(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := Check(Value, WithZero);
  if not Result then
    Error;
end;

function TmnSocket.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TmnSocket.Close;
begin
  if Active then
  begin
  {$IFDEF FPC}
    WinSock2.CloseSocket(FHandle);
  {$ELSE}
    WinSock.CloseSocket(FHandle);
  {$ENDIF}
    FHandle := INVALID_SOCKET;
  end;
end;

function TmnSocket.DoShutdown(How: TmnShutdown): TmnError;
const
  cHow: array[TmnShutdown] of Integer = (0, SD_RECEIVE, SD_SEND, SD_BOTH);
var
  c: Integer;
begin
  CheckActive;
{$IFDEF FPC}
  c := WinSock2.Shutdown(FHandle, cHow[How]);
{$ELSE}
  c := WinSock.Shutdown(FHandle, cHow[How]);
{$ENDIF}
  if c = SOCKET_ERROR then
  begin
    Result := erFail;
    RaiseLastOSError;
  end
  else
    Result := erNone;
end;

function TmnSocket.Accept: TmnCustomSocket;
var
  aHandle: TSocket;
  AddrSize: Integer;
begin
  CheckActive;
  AddrSize := SizeOf(FAddress);
{$IFDEF FPC}
  aHandle := WinSock2.Accept(FHandle, @FAddress, @AddrSize);
{$ELSE}
  aHandle := WinSock.Accept(FHandle, @FAddress, @AddrSize);
{$ENDIF}
  if aHandle = INVALID_SOCKET then
    Result := nil
  else
    Result := TmnSocket.Create(aHandle);
end;

constructor TmnSocket.Create(vHandle: TSocket);
begin
  inherited Create;
  FHandle := vHandle;
end;

function TmnSocket.Listen: TmnError;
var
  c: Integer;
begin
  CheckActive;
{$IFDEF FPC}
  c := WinSock2.listen(FHandle, 5);
{$ELSE}
  c := WinSock.listen(FHandle, 5);
{$ENDIF}
  if c = SOCKET_ERROR then
  begin
    Error;
    Result := erFail;
  end
  else
    Result := erNone;
end;

function TmnSocket.Check(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := not ((Value = SOCKET_ERROR) or (WithZero and (Value = 0)));
end;

function TmnSocket.GetRemoteAddress: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
  {$ifdef FPC}
  Finalize(SockAddrIn);
  {$endif}
  if getpeername(FHandle, SockAddrIn, Size) = 0 then
    Result := inet_ntoa(SockAddrIn.sin_addr)
  else
    Result := '';
end;

function TmnSocket.GetRemoteName: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
  aHostEnt: PHostEnt;
  s: ansistring;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
  {$ifdef FPC}
  Finalize(SockAddrIn);
  {$endif}
  if getpeername(FHandle, SockAddrIn, Size) = 0 then
  begin
    aHostEnt := gethostbyaddr(@SockAddrIn.sin_addr.s_addr, 4, PF_INET);
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
  SetLength(aName, 250);
{$IFDEF FPC}
  WinSock2.gethostname(PChar(aName), Length(aName));
{$ELSE}
  WinSock.gethostname(PAnsiChar(aName), Length(aName));
{$ENDIF}
  aName := PAnsiChar(aName);
{$IFDEF FPC}
  aHostEnt := WinSock2.gethostbyname(PChar(aName));
{$ELSE}
  aHostEnt := WinSock.gethostbyname(PAnsiChar(aName));
{$ENDIF}
  if aHostEnt <> nil then
  begin
    aAddr := aHostEnt^.h_addr_list^;
    if aAddr <> nil then
    begin
      sa.S_un_b.s_b1 := aAddr[0];
      sa.S_un_b.s_b2 := aAddr[1];
      sa.S_un_b.s_b3 := aAddr[2];
      sa.S_un_b.s_b4 := aAddr[3];
      Result := inet_ntoa(sa)
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
  SetLength(s, 250);
{$IFDEF FPC}
  WinSock2.gethostname(PChar(s), Length(s));
{$ELSE}
  WinSock.gethostname(PAnsiChar(s), Length(s));
{$ENDIF}
  s := PAnsiChar(s);
  Result := string(s);
end;

{ TmnWallSocket }

procedure TmnWallSocket.Cleanup;
begin
  Dec(FCount);
  if FCount = 0 then
    WSACleanup;
end;

constructor TmnWallSocket.Create;
begin
  inherited;
  Startup;
end;

function TmnWallSocket.Bind(Options: TmnsoOptions; const Port: string; const Address: string): TmnCustomSocket;
const
  SO_TRUE: Longbool = True;
var
  aHandle: TSocket;
  aSockAddr: TSockAddr;
  aHostEnt: PHostEnt;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    if soSafeConnect in Options then
      exit(nil)
    else
      raise EmnException.Create('Failed to create a socket, Error #' + Inttostr(WSAGetLastError));

  if soNoDelay in Options then
    setsockopt(aHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));

  if soReuseAddr in Options then
  begin
    {$IFDEF FPC}
      {$IFNDEF WINCE}
        WinSock2.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_TRUE), SizeOf(SO_TRUE));
      {$ENDIF}
    {$ELSE}
      WinSock.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
    {$ENDIF}
  end;

  aSockAddr.sin_family := AF_INET;
  aSockAddr.sin_port := htons(LookupPort(Port));
  if Address = '' then
    aSockAddr.sin_addr.s_addr := INADDR_ANY
  else
  begin
    aSockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(AnsiString(Address)));
    if ((aSockAddr.sin_addr.s_addr) = u_long(INADDR_NONE)) or (aSockAddr.sin_addr.s_addr = u_long(SOCKET_ERROR)) then
    begin
      aHostEnt := gethostbyname(PAnsiChar(AnsiString(Address)));
      if aHostEnt <> nil then
      begin
        aSockAddr.sin_addr.S_un_b.s_b1 := aHostEnt.h_addr^[0];
        aSockAddr.sin_addr.S_un_b.s_b2 := aHostEnt.h_addr^[1];
        aSockAddr.sin_addr.S_un_b.s_b3 := aHostEnt.h_addr^[2];
        aSockAddr.sin_addr.S_un_b.s_b4 := aHostEnt.h_addr^[3];
        aSockAddr.sin_family := aHostEnt.h_addrtype;
      end;
    end;
  end;
{$IFDEF FPC}
  if WinSock2.bind(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ELSE}
  if WinSock.bind(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ENDIF}
  if soSafeConnect in Options then
    exit(nil)
  else
    raise EmnException.Create('failed to bind the socket, error #' + IntToStr(WSAGetLastError) + '.'#13#13'another server is already use the same port (' + Port + ').');
  Result := TmnSocket.Create(aHandle);
end;

destructor TmnWallSocket.Destroy;
begin
  inherited;
  Cleanup;
end;

function TmnWallSocket.Select(vHandle: TSocket; Timeout: Integer; Check: TSelectCheck): TmnError;
var
  FSet: TFDSet;
  PSetRead, PSetWrite: PFDSet;
  TimeVal: TTimeVal;
  c: Integer;
begin
  //CheckActive; no need select will return error for it, as i tho
  if vHandle = INVALID_SOCKET then
    Result := erClosed
  else
  begin
    {$ifdef FPC}
    Finalize(FSet);
    {$endif}
    FD_ZERO(FSet);
    FD_SET(vHandle, FSet);
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
    begin
    {$IFDEF FPC}
      c := WinSock2.select(0, PSetRead, PSetWrite, nil, nil)
    {$ELSE}
      c := WinSock.select(0, PSetRead, PSetWrite, nil, nil)
    {$ENDIF}
    end
    else
    begin
      TimeVal.tv_sec := Timeout div 1000;
      TimeVal.tv_usec := (Timeout mod 1000) * 1000;
    {$IFDEF FPC}
      c := WinSock2.select(0, PSetRead, PSetWrite, nil, @TimeVal);
    {$ELSE}
      c := WinSock.select(0, PSetRead, PSetWrite, nil, @TimeVal);
    {$ENDIF}
    end;
    if (c = SOCKET_ERROR) then
      Result := erFail
    else if (c = 0) then
      Result := erTimout
    else
      Result := erNone;
  end
end;

function TmnWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
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

function TmnWallSocket.Connect(Options: TmnsoOptions; Timeout: Integer; const Port: string; const Address: string): TmnCustomSocket;
const
  SO_TRUE: Longbool = True;
var
  aHandle: TSocket;
  aAddr: TSockAddr;
  aHost: PHostEnt;
  ret: Longint;
  aMode: u_long;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    if soSafeConnect in Options then
      exit(nil)
    else
      raise EmnException.Create('Failed to connect socket, Error #' + Inttostr(WSAGetLastError));

  if soNoDelay in Options then
    setsockopt(aHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));

//http://support.microsoft.com/default.aspx?kbid=140325
  if soKeepAlive in Options then
    setsockopt(aHandle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));

  if soConnectTimeout in Options then
  begin
    aMode := 1;
    ret := ioctlsocket(aHandle, {$ifdef FPC}Longint(FIONBIO){$else}FIONBIO{$endif}, aMode);
    if ret = Longint(SOCKET_ERROR) then
      if soSafeConnect in Options then
        exit(nil)
      else
        raise EmnException.Create('Failed to set nonblock socket, Error #' + Inttostr(WSAGetLastError));
  end;

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
        aAddr.sin_addr.S_un_b.s_b1 := aHost.h_addr^[0];
        aAddr.sin_addr.S_un_b.s_b2 := aHost.h_addr^[1];
        aAddr.sin_addr.S_un_b.s_b3 := aHost.h_addr^[2];
        aAddr.sin_addr.S_un_b.s_b4 := aHost.h_addr^[3];
        aAddr.sin_family := aHost.h_addrtype;
      end;
    end;
  end;
{$IFDEF FPC}
  ret := WinSock2.connect(aHandle, aAddr, SizeOf(aAddr));
{$ELSE}
  ret := WinSock.connect(aHandle, aAddr, SizeOf(aAddr));
{$ENDIF}
  if (ret = SOCKET_ERROR) and not ((soConnectTimeout in Options) and (WSAGetLastError = WSAEWOULDBLOCK)) then
    if soSafeConnect in Options then
      exit(nil)
    else
      raise EmnException.Create('Failed to connect the socket, error #' + IntToStr(WSAGetLastError) + '.'#13'Address "' + Address +'" Port "' + Port + '".');

  if soConnectTimeout in Options then
  begin
    aMode := 0;
    ret := ioctlsocket(aHandle, {$ifdef FPC}Longint(FIONBIO){$else}FIONBIO{$endif}, aMode);
    if ret = Longint(SOCKET_ERROR) then
      if soSafeConnect in Options then
        exit(nil)
      else
        raise EmnException.Create('Failed to set nonblock socket, Error #' + Inttostr(WSAGetLastError));

    if Select(aHandle, Timeout, slWrite) <> erNone then
      if soSafeConnect in Options then
      exit(nil)
    else
      raise EmnException.Create('Failed to connect nonblock socket, Error #' + Inttostr(WSAGetLastError));
  end;
  Result := TmnSocket.Create(aHandle)
end;

end.
