unit mnWin32Sockets;
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
  //mnWinSock2,
{$IFDEF FPC}
{$IFDEF WINDOWS}
  WinSock2,
{$ELSE}
  sockets,
{$ENDIF}
{$ELSE}
  WinSock,
{$ENDIF}
  mnSockets;

type
  TmnWinSocket = class(TmnCustomSocket)
  private
    FHandle: TSocket;
    FAddress: TSockAddr;
  protected
    function Valid(Value: Integer; WithZero: Boolean = False): Boolean;
    function Check(Value: Integer; WithZero: Boolean = False): Boolean;
    function GetActive: Boolean; override;
    function DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError; override;
  public
    constructor Create(Handle: TSocket);
    function RecvLength: Cardinal; override;
    procedure Close; override;
    function Accept: TmnCustomSocket; override;
    procedure Cancel; override;
    function Receive(var Buffer; var Count: Longint): TmnError; override;
    function Send(const Buffer; var Count: Longint): TmnError; override;
    function Shutdown(How: TmnShutdown): TmnError; override;
    function Listen: TmnError; override;
    function GetLocalAddress: ansistring; override;
    function GetRemoteAddress: ansistring; override;
    function GetLocalName: string; override;
    function GetRemoteName: string; override;
  end;

  TmnWinWallSocket = class(TmnCustomWallSocket)
  private
    FWSAData: TWSAData;
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

{ TmnWinSocket }

function TmnWinSocket.Receive(var Buffer; var Count: Integer): TmnError;
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

function TmnWinSocket.Send(const Buffer; var Count: Integer): TmnError;
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

function TmnWinSocket.DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError;
var
  FSet: TFDSet;
  PSetRead, PSetWrite: PFDSet;
  TimeVal: TTimeVal;
  c: Integer;
begin
  CheckActive;
  FD_ZERO(FSet);
  FD_SET(FHandle, FSet);
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
  if (c = 0) or (c = SOCKET_ERROR) then
  begin
    Error;
    Result := erFail;
  end
  else
    Result := erNone;
end;

function TmnWinSocket.Valid(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := Check(Value, WithZero);
  if not Result then
    Error;
end;

function TmnWinSocket.GetActive: Boolean;
begin
  Result := FHandle <> INVALID_SOCKET;
end;

procedure TmnWinSocket.Close;
begin
  if Active then
  begin
  {$IFDEF FPC}
    WinSock2.Closesocket(FHandle);
  {$ELSE}
    WinSock.Closesocket(FHandle);
  {$ENDIF}
    FHandle := INVALID_SOCKET;
  end;
end;

function TmnWinSocket.RecvLength: Cardinal;
var
  i: Integer;
{$IFDEF FPC}
  l:DWORD;
{$ELSE}
 l:Longint;
{$ENDIF}
begin
  CheckActive;
  l := 0;
  i := ioctlsocket(FHandle, FIONREAD, L);
  Result := l;
  if (i = SOCKET_ERROR) then
  begin
    Error;
  end;
end;

function TmnWinSocket.Shutdown(How: TmnShutdown): TmnError;
const
  cHow: array[TmnShutdown] of Integer = (SD_RECEIVE, SD_SEND, SD_BOTH);
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

function TmnWinSocket.Accept: TmnCustomSocket;
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
  begin
    Result := TmnWinSocket.Create(aHandle);
  end;
end;

constructor TmnWinSocket.Create(Handle: TSocket);
begin
  inherited Create;
  FHandle := Handle;
end;

function TmnWinSocket.Listen: TmnError;
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

function TmnWinSocket.Check(Value: Integer; WithZero: Boolean): Boolean;
begin
  Result := not ((Value = SOCKET_ERROR) or (WithZero and (Value = 0)));
end;

function TmnWinSocket.GetRemoteAddress: ansistring;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
  if getpeername(FHandle, SockAddrIn, Size) = 0 then
    Result := inet_ntoa(SockAddrIn.sin_addr)
  else
    Result := '';
end;

function TmnWinSocket.GetRemoteName: string;
var
  SockAddrIn: TSockAddrIn;
  Size: Integer;
  aHostEnt: PHostEnt;
  s: ansistring;
begin
  CheckActive;
  Size := SizeOf(SockAddrIn);
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
  Result := s;
end;

function TmnWinSocket.GetLocalAddress: ansistring;
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

function TmnWinSocket.GetLocalName: string;
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
  Result := s;
end;

procedure TmnWinSocket.Cancel;
begin
{$IFDEF FPC}
  {$IFNDEF WINCE}
    WinSock2.WSACancelBlockingCall;
  {$ENDIF}
{$ELSE}
  WinSock.WSACancelBlockingCall;
{$ENDIF}
end;

{ TmnWinWallSocket }

procedure TmnWinWallSocket.Cleanup;
begin
  Dec(FCount);
  if FCount = 0 then
    WSACleanup;
end;

constructor TmnWinWallSocket.Create;
begin
  inherited;
end;

const
  SO_TRUE:Longbool=True;
  SO_FALSE:Longbool=False;

function TmnWinWallSocket.Bind(Options: TmnOptions; const Port: ansistring; const Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aSockAddr: TSockAddr;
  aHostEnt: PHostEnt;
  aAddr: array[0..3] of AnsiChar;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to create a socket, Error #' + Inttostr(WSAGetLastError));

  if soReuseAddr in Options then
{$IFDEF FPC}
  {$IFNDEF WINCE}
    WinSock2.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PChar(@SO_TRUE), SizeOf(SO_TRUE));
  {$ENDIF}
{$ELSE}
    WinSock.setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_TRUE), SizeOf(SO_TRUE));
{$ENDIF}

  aSockAddr.sin_family := AF_INET;
  aSockAddr.sin_port := htons(LookupPort(Port));
  if Address = '' then
    aSockAddr.sin_addr.s_addr := INADDR_ANY
  else
  begin
    aSockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(Address));
    if aSockAddr.sin_addr.s_addr = INADDR_NONE then
    begin
      aHostEnt := gethostbyname(PAnsiChar(Address));
      if aHostEnt <> nil then
      begin
        Move(aHostEnt.h_addr^, aAddr, aHostEnt.h_length);
        aSockAddr.sin_addr.S_un_b.s_b1 := aAddr[0];
        aSockAddr.sin_addr.S_un_b.s_b2 := aAddr[1];
        aSockAddr.sin_addr.S_un_b.s_b3 := aAddr[2];
        aSockAddr.sin_addr.S_un_b.s_b4 := aAddr[3];
        aSockAddr.sin_family := aHostEnt.h_addrtype;
      end;
    end;
  end;
{$IFDEF FPC}
  if WinSock2.bind(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ELSE}
  if WinSock.bind(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ENDIF}
    raise EmnException.Create('failed to bind the socket, error #' + IntToStr(WSAGetLastError) + '.'#13#13'another server is already use the same port (' + Port + ').');
  Result := TmnWinSocket.Create(aHandle);
end;

destructor TmnWinWallSocket.Destroy;
begin
  inherited;
end;

function TmnWinWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

procedure TmnWinWallSocket.Startup;
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

function TmnWinWallSocket.Connect(Options: TmnOptions; const Port, Address: ansistring): TmnCustomSocket;
var
  aHandle: TSocket;
  aSockAddr: TSockAddr;
  aHostEnt: PHostEnt;
  aAddr: PAnsiChar;
begin
  aHandle := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  if aHandle = INVALID_SOCKET then
    raise EmnException.Create('Failed to connect socket, Error #' + Inttostr(WSAGetLastError));

  aSockAddr.sin_family := AF_INET;
  aSockAddr.sin_port := htons(LookupPort(Port));

  aSockAddr.sin_addr.s_addr := inet_addr(PAnsiChar(Address));
  if aSockAddr.sin_addr.s_addr = INADDR_NONE then
  begin
    aHostEnt := gethostbyname(PAnsiChar(Address));
    if aHostEnt <> nil then
    begin
      Move(aHostEnt.h_addr^, aAddr, aHostEnt.h_length);
      aSockAddr.sin_addr.S_un_b.s_b1 := aAddr[0];
      aSockAddr.sin_addr.S_un_b.s_b2 := aAddr[1];
      aSockAddr.sin_addr.S_un_b.s_b3 := aAddr[2];
      aSockAddr.sin_addr.S_un_b.s_b4 := aAddr[3];
      aSockAddr.sin_family := aHostEnt.h_addrtype;
    end;
  end;
{$IFDEF FPC}
  if WinSock2.connect(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ELSE}
  if WinSock.connect(aHandle, aSockAddr, SizeOf(aSockAddr)) = SOCKET_ERROR then
{$ENDIF}
      raise EmnException.Create('Failed to connect the socket, error #' + IntToStr(WSAGetLastError) + '.'#13'Address "' + Address +'" Port "' + Port + '".');
  Result := TmnWinSocket.Create(aHandle)
end;

initialization                                                                                                  
  RegisterWallSocket(TmnWinWallSocket.Create);
end.
