unit mnPosixSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{$IFDEF FPC}
{$mode delphi}
{$M+}
{$H+}
{$endif}

interface

uses
  Classes,
  Posix.SysSelect,
  Posix.SysSocket,
  Posix.Unistd,
  Posix.ArpaInet,
  Posix.NetDB,
  Posix.SysTime,
  Posix.NetinetIn,
  SysUtils,
  mnSockets;

const
  IPPROTO_IP = 0;
  IPPROTO_TCP = 6;
  TCP_NODELAY = 1;
  INADDR_ANY  = 0;
  INADDR_NONE = $ffffffff;


  {$IF defined(MACOS) or defined(IOS)}
  //MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
  MSG_NOSIGNAL  = 0;  // Do not generate SIGPIPE.
                           // Works under MAC OS X, but is undocumented,
                           // So FPC doesn't include it

  FIONREAD = $4004667F; // oSX FIONREAD        = Posix.StrOpts.FIONREAD;

  FIONBIO	 = $8004667E; //OSX FIONBIO         = Posix.StrOpts.FIONBIO;
  FIOASYNC = $8004667D; //OSX  FIOASYNC        = Posix.StrOpts.FIOASYNC;  // not defined in XE2
  {$ELSE}
   MSG_NOSIGNAL  = $4000; // Do not generate SIGPIPE.
  {$ENDIF}

type
  TSocket = integer;

  TaddrIP4 = packed record
    case boolean of
       true: (s_addr  : int32);
       false: (s_bytes : packed array[1..4] of byte);
  end;

  sockaddr_in = record
    {$IF defined(OSX) or defined(IOS)}
		sin_len: UInt8;
    {$endif}

    sin_family: sa_family_t;
    sin_port: word;
    sin_addr: TaddrIP4;
    sin_zero: packed array [0..7] of Byte;
  end;

  TSockAddr = packed record
    case integer of
      0: (addr: sockaddr);
      1: (addr_in: sockaddr_in);
      //2: (addr_in6: sockaddr_in6)
  end;

  TmnSocket = class(TmnCustomSocket)
  private
    FHandle: TSocket;
    FAddress: TSockAddr;
  protected
    function Valid(Value: Integer; WithZero: Boolean = False): Boolean;
    function Check(Value: Integer; WithZero: Boolean = False): Boolean;
    function GetActive: Boolean; override;
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; override;
    function DoShutdown(How: TmnShutdown): TmnError; override;
  public
    constructor Create(Handle: TSocket);
    procedure Close; override;
    function Accept: TmnCustomSocket; override;
    function Receive(var Buffer; var Count: Longint): TmnError; override;
    function Send(const Buffer; var Count: Longint): TmnError; override;
    function Listen: TmnError; override;
    function GetLocalAddress: string; override;
    function GetRemoteAddress: string; override;
    function GetLocalName: string; override;
    function GetRemoteName: string; override;
  end;

  { TmnWallSocket }

  TmnWallSocket = class(TmnCustomWallSocket)
  private
    function LookupPort(Port: string): Word;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Bind(Options: TmnsoOptions; const Port: string; const Address: string = ''): TmnCustomSocket; override;
    function Connect(Options: TmnsoOptions; Timeout: Integer; const Port: string; const Address: string = ''): TmnCustomSocket; override;
  end;

implementation

uses
  mnUtils;

const
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

procedure StrToNetAddr(S: string; var addr: TaddrIP4);
var
  l: string;
begin
  l := S;
  addr.s_bytes[1] := StrToIntDef(Fetch(S, '.'), 0);
  addr.s_bytes[2] := StrToIntDef(Fetch(S, '.'), 0);
  addr.s_bytes[3] := StrToIntDef(Fetch(S, '.'), 0);
  addr.s_bytes[4] := StrToIntDef(Fetch(S, '.'), 0);
end;

{ TmnSocket }

function TmnSocket.Receive(var Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := Posix.SysSocket.Recv(FHandle, Buffer, Count, MSG_NOSIGNAL);
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
  c := Posix.SysSocket.Send(FHandle, Buffer, Count, MSG_NOSIGNAL);

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
var
  FSet: fd_set;
  c: Integer;

  LTime: TimeVal;
  LTimePtr: PTimeVal;
begin

  //CheckActive; no need select will return error for it, as i tho
  if FHandle = INVALID_SOCKET then
    Result := erClosed
  else
  begin
    if Timeout = -1 then
    begin
      LTimePtr := nil;
    end
    else
    begin
      LTime.tv_sec := Timeout div 1000;
      LTime.tv_usec := (Timeout mod 1000) * 1000;
      LTimePtr := @LTime;
    end;

    fd_zero(FSet);
    //_FD_SET(FHandle, FSet);
    _FD_SET(0, FSet);
    if Check = slRead then
      c := Posix.SysSelect.Select(FD_SETSIZE, @FSet, nil, nil, LTimePtr)
    else
      c := Posix.SysSelect.Select(FD_SETSIZE, nil, @FSet, nil, LTimePtr);
    {if Check = slRead then
      c := Posix.SysSelect.Select(FD_SETSIZE, @FSet, nil, nil, LTimePtr)
    else
      c := Posix.SysSelect.Select(FD_SETSIZE, nil, @FSet, nil, LTimePtr);}

    if (c = 0) or (c = SOCKET_ERROR) then
    begin
      Error;
      Result := erFail;
    end
    else
      Result := erNone;
  end;
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
    __close(FHandle);
    FHandle := INVALID_SOCKET;
  end;
end;

function TmnSocket.DoShutdown(How: TmnShutdown): TmnError;
const
  cHow: array[TmnShutdown] of Integer = (0, SHUT_RD, SHUT_WR, SHUT_RDWR);
var
  c: Integer;
begin
  CheckActive;
  c := Posix.SysSocket.shutdown(FHandle, cHow[How]);
  if c = SOCKET_ERROR then
  begin
    Result := erFail;
//    RaiseLastOSError; do not raise an error, maybe it is disconnected by the other side
  end
  else
    Result := erNone;
end;

function TmnSocket.Accept: TmnCustomSocket;
var
  aHandle: TSocket;
  aSize: Cardinal;
begin
  CheckActive;
  aSize := SizeOf(FAddress);
  aHandle := Posix.SysSocket.accept(FHandle, FAddress.addr, aSize);
  if aHandle < 0 then
    Result := nil
  else
    Result := TmnSocket.Create(aHandle);
end;

constructor TmnSocket.Create(Handle: TSocket);
begin
  inherited Create;
  FHandle := Handle;
end;

function TmnSocket.Listen: TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := Posix.SysSocket.listen(FHandle, 5);
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
  SockAddr: TSockAddr;
  aSize: Integer;
begin
  CheckActive;
  Result := '';
end;

function TmnSocket.GetRemoteName: string;
var
  SockAddr: TSockAddr;
  Size: Integer;
begin
  CheckActive;
  Result := '';
end;

function TmnSocket.GetLocalAddress: string;
var
  SockAddr: TSockAddr;
  aSize: Integer;
begin
  CheckActive;
  Result := '';
end;

function TmnSocket.GetLocalName: string;
var
  s: string;
begin
  CheckActive;
  SetLength(s, 250);
  s := '';
  Result := s;
end;

{ TmnWallSocket }

constructor TmnWallSocket.Create;
begin
  inherited;

end;

const
  SO_TRUE:Longbool=True;
//  SO_FALSE:Longbool=False;

function TmnWallSocket.Bind(Options: TmnsoOptions; const Port: string; const Address: string): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TSockAddr;
begin
  aHandle := Posix.SysSocket.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to create a socket');

  if soReuseAddr in Options then
    Posix.SysSocket.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, SO_TRUE, SizeOf(SO_TRUE));

  if soNoDelay in Options then
    Posix.SysSocket.setsockopt(aHandle, IPPROTO_IP, TCP_NODELAY, SO_TRUE, SizeOf(SO_TRUE));

 //  fpsetsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));

  aAddr.addr_in.sin_family := AF_INET;
  aAddr.addr_in.sin_port := StrToIntDef(Port, 0);
  StrToNetAddr(Address, aAddr.addr_in.sin_addr);
  If Posix.SysSocket.bind(aHandle, aAddr.addr, Sizeof(aAddr)) <> 0 then
    raise EmnException.Create('failed to bind the socket, maybe another server is already use the same port (' + Port + ').');
  Result := TmnSocket.Create(aHandle);
end;

destructor TmnWallSocket.Destroy;
begin
  inherited;
end;

function TmnWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

function TmnWallSocket.Connect(Options: TmnsoOptions; Timeout: Integer; const Port: string; const Address: string): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TSockAddr;
  ret: integer;
  h: Phostent;

  LHints: AddrInfo;
  LRetVal: Integer;
  LAddrInfo: pAddrInfo;
begin
  //nonblick connect  https://stackoverflow.com/questions/1543466/how-do-i-change-a-tcp-socket-to-be-non-blocking
  //https://stackoverflow.com/questions/14254061/setting-time-out-for-connect-function-tcp-socket-programming-in-c-breaks-recv
  aHandle := Posix.SysSocket.socket(AF_INET, SOCK_STREAM{TODO: for nonblock option: or O_NONBLOCK}, 0{IPPROTO_TCP});
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to connect socket');

  //if soNoDelay in Options then
    //setsockopt(aHandle, IPPROTO_TCP, TCP_NODELAY, SO_TRUE, SizeOf(SO_TRUE));

//http://support.microsoft.com/default.aspx?kbid=140325
  if soKeepAlive in Options then
    setsockopt(aHandle, SOL_SOCKET, SO_KEEPALIVE, SO_TRUE, SizeOf(SO_TRUE));

//  fpsetsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));


  aAddr.addr_in.sin_family := AF_INET;
  aAddr.addr_in.sin_port := htons(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.addr_in.sin_addr.s_addr := INADDR_ANY
  else
  begin
    StrToNetAddr(Address, aAddr.addr_in.sin_addr);
    if (aAddr.addr_in.sin_addr.s_addr = 0) then
    begin
      //h := gethostbyname(TMarshal.AsAnsi(Address));
      //aAddr.addr_in.sin_addr.s_addr := UInt32(h.h_addr_list);

      FillChar(LHints, SizeOf(LHints), 0);
      LHints.ai_family := AF_INET;
      LHints.ai_socktype := SOCK_STREAM;
      LAddrInfo := nil;

      LRetVal := getaddrinfo(MarshaledAString(TMarshal.AsAnsi(Address)), nil, LHints, LAddrInfo);
      if LRetVal = 0 then
      begin
        aAddr.addr_in.sin_addr.s_addr := Psockaddr_in(LAddrInfo^.ai_addr).sin_addr.s_addr;
      end
      else
        raise EmnException.CreateFmt('Failed Get IP [%s]', [Address]);
    end;
  end;
  ret := Posix.SysSocket.connect(aHandle, aAddr.addr, SizeOf(aAddr));

  (*LAddrIPv4.sin_family := AF_INET;
  LAddrIPv4.sin_port := htons(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.addr_in.sin_addr.s_addr := INADDR_ANY
  else
  begin
    StrToNetAddr(Address, LAddrIPv4.sin_addr);
    if (aAddr.addr_in.sin_addr.s_addr = 0) then
    begin
      //getaddrinfo()
{      if ResolveHostByName(Address, aHost) then //DNS server
      begin
        aAddr.sin_addr.s_addr := aHost.Addr.s_addr;
      end;}
    end;
  end;
  ret := Posix.SysSocket.connect(aHandle, LAddr, SizeOf(LAddrIPv4));*)
  if ret = -1 then
    raise EmnException.Create('Failed to connect the socket, error #' + IntToStr(ret) + '.'#13'Address "' + Address +'" Port "' + Port + '".');
  Result := TmnSocket.Create(aHandle)
end;

end.

//StrToHostAddr
