unit mnLinuxSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$mode delphi}
{$M+}
{$H+}

interface

uses
  Classes,
  SysUtils,
  sockets,
  mnSockets;

type
  TmnSocket = class(TmnCustomSocket)
  private
    FHandle: TSocket;
    FAddress: TINetSockAddr;
  protected
    function Valid(Value: Integer; WithZero: Boolean = False): Boolean;
    function Check(Value: Integer; WithZero: Boolean = False): Boolean;
    function GetActive: Boolean; override;
    function DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError; override;
    function DoShutdown(How: TmnShutdown): TmnError; override;
  public
    constructor Create(Handle: TSocket);
    procedure Close; override;
    function Accept: TmnCustomSocket; override;
    function Receive(var Buffer; var Count: Longint): TmnError; override;
    function Send(const Buffer; var Count: Longint): TmnError; override;
    function Listen: TmnError; override;
    function GetLocalAddress: ansistring; override;
    function GetRemoteAddress: ansistring; override;
    function GetLocalName: string; override;
    function GetRemoteName: string; override;
  end;

  TmnWallSocket = class(TmnCustomWallSocket)
  private
    FCount: Integer;
    function LookupPort(Port: string): Word;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Bind(Options: TmnOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; override;
    function Connect(Options: TmnOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; override;
  end;

implementation

uses
  BaseUnix;

const
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

{ TmnSocket }

function TmnSocket.Receive(var Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := fprecv(FHandle, @Buffer, Count, MSG_NOSIGNAL);
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
  c := fpsend(FHandle, @Buffer, Count, MSG_NOSIGNAL);
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

function TmnSocket.DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError;
var
  FSet: TFDSet;
  PSetRead, PSetWrite: PFDSet;
  c: Integer;
begin
  CheckActive;
  fpfd_zero(FSet);
  fpfd_set(FHandle, FSet);
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
    Timeout := 0;
  c := fpselect(FHandle + 1, PSetRead, PSetWrite, PSetRead, Timeout); {$hint 'why FHandle + 1 not 1'}
  if (c = 0) or (c = SOCKET_ERROR) then
  begin
    Error;
    Result := erFail;
  end
  else
    Result := erNone;
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
    closesocket(FHandle);
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
  c := fpshutdown(FHandle, cHow[How]);
  if c = SOCKET_ERROR then
  begin
    Result := erFail;
//    RaiseLastOSError; do not raise an error, matbe it is disconnected by the other side
  end
  else
    Result := erNone;
end;

function TmnSocket.Accept: TmnCustomSocket;
var
  aHandle: TSocket;
  aSize: Integer;
begin
  CheckActive;
  aSize := SizeOf(FAddress);
  aHandle := fpaccept(FHandle, @FAddress, @aSize);
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
  c := fplisten(FHandle, 5);
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

function TmnSocket.GetRemoteAddress: ansistring;
var
  SockAddr: TSockAddr;
  aSize: Integer;
begin
  CheckActive;
  aSize := SizeOf(SockAddr);
  if fpGetPeerName(FHandle, @SockAddr, @aSize) = 0 then
//    Result := NetAddrToStr(SockAddr.in_addr)
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
  if fpgetpeername(FHandle, @SockAddr, @Size) = 0 then
  begin
    s := '';//temp
    //gethostbyaddr(@SockAddr.sin_addr.s_addr, 4, AF_INET);
  end
  else
    s := '';
  Result := s;
end;

function TmnSocket.GetLocalAddress: ansistring;
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

constructor TmnWallSocket.Create;
begin
  inherited;
end;

const
  SO_TRUE:Longbool=True;
  SO_FALSE:Longbool=False;

function TmnWallSocket.Bind(Options: TmnOptions; const Port: ansistring; const Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TINetSockAddr;
begin
  aHandle := fpsocket(AF_INET, SOCK_STREAM, 0{IPPROTO_TCP});
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to create a socket');

  if soReuseAddr in Options then
    fpsetsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_TRUE), SizeOf(SO_TRUE));

  if soKeepAlive in Options then
    fpsetsockopt(aHandle, SOL_SOCKET, SO_KEEPALIVE, PChar(@SO_TRUE), SizeOf(SO_TRUE));

//  fpsetsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));

  aAddr.sin_family := AF_INET;
  aAddr.sin_port := ShortHostToNet(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.sin_addr.s_addr := INADDR_ANY
  else
    aAddr.sin_addr := StrToNetAddr(Address);
  If  fpbind(aHandle,@aAddr, Sizeof(aAddr)) <> 0 then
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

function TmnWallSocket.Connect(Options: TmnOptions; const Port, Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TINetSockAddr;
begin
  aHandle := fpsocket(AF_INET, SOCK_STREAM, 0{IPPROTO_TCP});
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to connect socket');

//  fpsetsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));

  aAddr.sin_family := AF_INET;
  aAddr.sin_port := ShortHostToNet(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.sin_addr.s_addr := INADDR_ANY
  else
    aAddr.sin_addr := StrToNetAddr(Address);
  if fpconnect(aHandle, @aAddr, SizeOf(aAddr)) <> 0 then
    raise EmnException.Create('Failed to connect the socket, Address "' + Address +'" Port "' + Port + '".');
  Result := TmnSocket.Create(aHandle)
end;

end.

//StrToHostAddr