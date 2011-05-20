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
  TmnLinuxSocket = class(TmnCustomSocket)
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

  TmnLinuxWallSocket = class(TmnCustomWallSocket)
  private
    FCount: Integer;
    function LookupPort(Port: string): Word;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Bind(Options: TmnOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; override;
    function Connect(Options: TmnOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; override;
    procedure Startup; override;
    procedure Cleanup; override;
  end;

implementation

uses
  BaseUnix;

const
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

{ TmnLinuxSocket }

function TmnLinuxSocket.Receive(var Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := fprecv(FHandle, @Buffer, Count, 0);
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

function TmnLinuxSocket.Send(const Buffer; var Count: Integer): TmnError;
var
  c: Integer;
begin
  CheckActive;
  c := fpsend(FHandle, @Buffer, Count, 0);
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

function TmnLinuxSocket.DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError;
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

function TmnLinuxSocket.Valid(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := Check(Value, WithZero);
  if not Result then
    Error;
end;

function TmnLinuxSocket.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TmnLinuxSocket.Close;
begin
  if Active then
  begin
    closesocket(FHandle);
    FHandle := INVALID_SOCKET;
  end;
end;

function TmnLinuxSocket.DoShutdown(How: TmnShutdown): TmnError;
const
  cHow: array[TmnShutdown] of Integer = (SHUT_RD, SHUT_WR, SHUT_RDWR);
var
  c: Integer;
begin
  CheckActive;
  c := fpshutdown(FHandle, cHow[How]);
  if c = SOCKET_ERROR then
  begin
    Result := erFail;
    RaiseLastOSError;
  end
  else
    Result := erNone;
end;

function TmnLinuxSocket.Accept: TmnCustomSocket;
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
    Result := TmnLinuxSocket.Create(aHandle);
end;

constructor TmnLinuxSocket.Create(Handle: TSocket);
begin
  inherited Create;
  FHandle := Handle;
end;

function TmnLinuxSocket.Listen: TmnError;
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

function TmnLinuxSocket.Check(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := not ((Value = SOCKET_ERROR) or (WithZero and (Value = 0)));
end;

function TmnLinuxSocket.GetRemoteAddress: ansistring;
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

function TmnLinuxSocket.GetRemoteName: string;
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
    //gethostbyaddr(@SockAddr.sin_addr.s_addr, 4, PF_INET);
  end
  else
    s := '';
  Result := s;
end;

function TmnLinuxSocket.GetLocalAddress: ansistring;
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

function TmnLinuxSocket.GetLocalName: string;
var
  s: ansistring;
begin
  CheckActive;
  SetLength(s, 250);
//  fpgethostname(PChar(s), Length(s));
  s := '';//temp
  Result := s;
end;

{ TmnLinuxWallSocket }

procedure TmnLinuxWallSocket.Cleanup;
begin
  Dec(FCount);
end;

constructor TmnLinuxWallSocket.Create;
begin
  inherited;
end;

const
  SO_TRUE:Longbool=True;
  SO_FALSE:Longbool=False;

function TmnLinuxWallSocket.Bind(Options: TmnOptions; const Port: ansistring; const Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TINetSockAddr;
begin
  aHandle := fpsocket(PF_INET, SOCK_STREAM, 0);
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to create a socket');

  if soReuseAddr in Options then
  {$message hint 'use keepalive'}
    fpsetsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_TRUE), SizeOf(SO_TRUE));

  aAddr.sin_family := AF_INET;
  aAddr.sin_port := ShortHostToNet(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.sin_addr.s_addr := INADDR_ANY
  else
    aAddr.sin_addr := StrToNetAddr(Address);
  If  fpbind(aHandle,@aAddr, Sizeof(aAddr)) <> 0 then
    raise EmnException.Create('failed to bind the socket, maybe another server is already use the same port (' + Port + ').');
  Result := TmnLinuxSocket.Create(aHandle);
end;

destructor TmnLinuxWallSocket.Destroy;
begin
  inherited;
end;

function TmnLinuxWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

procedure TmnLinuxWallSocket.Startup;
begin
  if FCount = 0 then
  begin
    //init somthing
  end;
  Inc(FCount)
end;

function TmnLinuxWallSocket.Connect(Options: TmnOptions; const Port, Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aAddr : TINetSockAddr;
begin
  aHandle := fpsocket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to connect socket');

  aAddr.sin_family := AF_INET;
  aAddr.sin_port := ShortHostToNet(StrToIntDef(Port, 0));
  if Address = '' then
    aAddr.sin_addr.s_addr := INADDR_ANY
  else
    aAddr.sin_addr := StrToNetAddr(Address);
  if fpconnect(aHandle, @aAddr, SizeOf(aAddr)) <> 0 then
    raise EmnException.Create('Failed to connect the socket, Address "' + Address +'" Port "' + Port + '".');
  Result := TmnLinuxSocket.Create(aHandle)
end;

initialization                                                                                                  
  RegisterWallSocket(TmnLinuxWallSocket.Create);
end.

//StrToHostAddr