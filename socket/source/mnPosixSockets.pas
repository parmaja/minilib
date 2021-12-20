unit mnPosixSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Belal Hamad <https://github.com/belalhamed>
 *
 * Mini Socket for Linux, but it used by Delphi
 *
 *}

{$ifdef fpc}
{$mode delphi} //this file not compiled in FPC
{$endif}
{$M+}
{$H+}

interface

uses
  Classes, SysUtils,
  Posix.Base, Posix.SysSelect, Posix.SysSocket, Posix.Unistd, Posix.ArpaInet, Posix.NetDB, Posix.SysTime, Posix.NetinetIn, Posix.Fcntl, Posix.Errno,
  {$ifdef LINUX} Linuxapi.KernelIoctl, Linuxapi.KernelDefs, {$else}  {$endif}
  Posix.StrOpts, //
  mnSockets;

type

  { TmnSocket }

  TmnSocket = class(TmnCustomSocket)
  private
//    FAddress: TSockAddr;
  protected
    function GetActive: Boolean; override;
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; override;
    function DoShutdown(How: TmnSocketStates): TmnError; override;

    function InternalSend(vBuf: Pointer; vLen: Integer): Integer;
    function DoListen: TmnError; override;
    function DoReceive(var Buffer; var Count: Longint): TmnError; override;
    function DoSend(const Buffer; var Count: Longint): TmnError; override;
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
    function LookupPort(Port: string): Word;
    function TestGetAddrInfo(const AHostName, AServiceName: string; const AHints: AddrInfo): PAddrInfo;
  protected
    procedure FreeSocket(var vHandle: TSocketHandle);
    function Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
  public
    constructor Create; override;
    destructor Destroy; override;
    function GetSocketError(Handle: TSocketHandle): Integer; override;
    procedure Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Bind(Options: TmnsoOptions; ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); override;
    procedure Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer); override;
  end;

implementation

uses
  mnUtils;

const
  INVALID_SOCKET		= TSocketHandle(NOT(0));
  SOCKET_ERROR			= -1;

  IPPROTO_IP = 0;
//  IPPROTO_TCP = 6;
  TCP_NODELAY = 1;
  SO_TRUE: Longbool = True;
//  SO_FALSE:Longbool=False;
  TCP_QUICKACK = 12; //Some one said it is work on windows too

  INADDR_ANY  = 0;
  INADDR_NONE = $ffffffff;

  {$if defined(MACOS) or defined(IOS)}
  MSG_NOSIGNAL  = $20000;  // Do not generate SIGPIPE.
  //FIONREAD = $4004667F; // oSX FIONREAD        = Posix.StrOpts.FIONREAD;

  //FIONBIO	 = $8004667E; //OSX FIONBIO         = Posix.StrOpts.FIONBIO;
  //FIOASYNC = $8004667D; //OSX  FIOASYNC        = Posix.StrOpts.FIOASYNC;  // not defined in XE2
  {$else}
   //MSG_NOSIGNAL  = $4000; // Do not generate SIGPIPE.
  {$ifend}

  {$ifdef ANDROID}
  FIONBIO         = $5421;//from LinuxAPI
  {$endif}

type
  TaddrIP4 = packed record
    case boolean of
       true: (s_addr  : int32);
       false: (s_bytes : packed array[1..4] of byte);
  end;

  sockaddr_in = record
    {$if defined(OSX) or defined(IOS)}
    sin_len: UInt8;
    {$ifend}

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

function StrToNetAddr(S: string): TaddrIP4;
var
  l: string;
begin
  l := S;
  Result.s_bytes[1] := StrToIntDef(FetchStr(S, '.'), 0);
  Result.s_bytes[2] := StrToIntDef(FetchStr(S, '.'), 0);
  Result.s_bytes[3] := StrToIntDef(FetchStr(S, '.'), 0);
  Result.s_bytes[4] := StrToIntDef(FetchStr(S, '.'), 0);
end;

function NetAddrToStr(addr : TaddrIP4) : string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to 4 do
  begin
     Result := Result + IntToStr(addr.s_bytes[i]);
     if i < 4 then
       Result := Result + '.';
  end;
end;

function InitSocketOptions(Handle: Integer; Options: TmnsoOptions; ReadTimeout: Integer): Integer;  //return error number
var
  t: timeval;
begin
  Result := 0;
  if (soNoDelay in Options) and not (soNagle in Options) then
  //if not (soNagle in Options) then //TODO
    Result := setsockopt(Handle, IPPROTO_TCP, TCP_NODELAY, SO_TRUE, SizeOf(SO_TRUE));
  if soKeepAlive in Options then
    Result := setsockopt(Handle, SOL_SOCKET, SO_KEEPALIVE, SO_TRUE, SizeOf(SO_TRUE));
  if soQuickAck in Options then
    Result := setsockopt(Handle, SOL_SOCKET, TCP_QUICKACK, SO_TRUE, SizeOf(SO_TRUE));
    //ret := WSAIoctl(sock, SIO_TCP_SET_ACK_FREQUENCY, &freq, sizeof(freq), NULL, 0, &bytes, NULL, NULL);

  if not (soWaitBeforeRead in Options) then
  begin
    if ReadTimeout <> -1 then
    begin
      //t := ReadTimeout;
      //* https://stackoverflow.com/questions/2876024/linux-is-there-a-read-or-recv-from-socket-with-timeout
      //Result := setsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, t, SizeOf(t));
      t.tv_sec := ReadTimeout div 1000;
      t.tv_usec := (ReadTimeout mod 1000) * 1000;
      Result := setsockopt(Handle, SOL_SOCKET, SO_RCVTIMEO, t, SizeOf(t));

      //t := errno;
    end;
  end;
  //TODO setsockopt(aHandle, SOL_SOCKET, SO_NOSIGPIPE, PChar(@SO_TRUE), SizeOf(SO_TRUE));
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
    if Kind = skListener then
	    if not sdReceive in State then //good for listener
    	  DoShutdown([sdReceive, sdSend]);
    err := __close(FHandle);
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
  if ioctl(FHandle, FIONREAD, @Count) = SOCKET_ERROR then  //  //ioctl(fd,FIONREAD,&bytes_available)
    Result := False //TODO
  else
    Result := Count > 0;
end;

function TmnSocket.DoShutdown(How: TmnSocketStates): TmnError;
var
  c: Integer;
  iHow: Integer;
begin
  if [sdReceive, sdSend] = How then
    iHow := SHUT_RDWR
  else if sdReceive in How then
    iHow := SHUT_RD
  else if sdSend in How then
    iHow := SHUT_WR
  else
  begin
    Result := erInvalid;
    exit;
  end;

  //CheckActive;

  c := Posix.SysSocket.shutdown(FHandle, iHow);

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
  c := Posix.SysSocket.listen(FHandle, 5);
  if c = SOCKET_ERROR then
    Result := erInvalid
  else
    Result := erSuccess;
end;

function TmnSocket.DoReceive(var Buffer; var Count: Longint): TmnError;
var
  ret: Integer;
begin
  CheckActive;

  //ret := Posix.SysSocket.Recv(FHandle, Buffer, Count, MSG_NOSIGNAL); //MSG_NOSIGNAL fail on mac and ios :)
  ret := Posix.SysSocket.Recv(FHandle, Buffer, Count, 0);
  if ret = 0 then
  begin
    Count := 0;
    Result := erClosed;
  end
  else if ret = SOCKET_ERROR then
  begin
    Count := 0;
    //TODO copy it from windows
    //Result := erTimout; //maybe closed, but we will pass it as timeout, the caller will close it depend on options
    Result := erInvalid;
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

  CheckActive;
  //ret := Posix.SysSocket.Send(FHandle, Buffer, Count, MSG_NOSIGNAL); //MSG_NOSIGNAL fail on mac and ios :)
  ret := Posix.SysSocket.Send(FHandle, Buffer, Count, 0);
  //c := InternalSend(@Buffer, Count);

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

function TmnSocket.InternalSend(vBuf: Pointer; vLen: Integer): Integer;
var
  aBuf: PByte;
  aSent, aError: Integer;
begin
  Result := 0;

  aBuf := vBuf;
  while (Result < vLen) do
  begin
    aSent := Posix.SysSocket.Send(FHandle, aBuf^, vLen - Result, MSG_NOSIGNAL); //MSG_NOSIGNAL fail on mac and ios :)

    if (aSent < 0) then
    begin
      aError := GetLastError;

      if (aError = EINTR) then
        Continue
      else if (aError = EAGAIN) or (aError = EWOULDBLOCK) then
        Break
      else
        Exit(-1);
    end;

    Inc(Result, aSent);
    Inc(aBuf, aSent);
  end;
end;

function TmnSocket.GetRemoteAddress: string;
var
  aSockAddr: SockAddr;
  aSize: {$ifdef ANDROID32} Integer;{$else}Cardinal;{$endif}
begin
  CheckActive;
  aSize := SizeOf(aSockAddr);
  //Initialize(aSockAddr);
  if getpeername(FHandle, aSockAddr, aSize) = 0 then
    Result := NetAddrToStr(sockaddr_in(aSockAddr).sin_addr)
  else
    Result := '';
end;

function TmnSocket.GetRemoteName: string;
//var
//  SockAddr: TSockAddr;
//  Size: Integer;
begin
  CheckActive;
  Result := '';
end;

function TmnSocket.GetLocalAddress: string;
var
  aSockAddr: SockAddr;
  aSize: {$ifdef ANDROID32} Integer;{$else}Cardinal;{$endif}
begin
  CheckActive;
  aSize := SizeOf(aSockAddr);
  //Initialize(aSockAddr);
  if GetSockName(FHandle, aSockAddr, aSize) = 0 then
    Result := NetAddrToStr(sockaddr_in(aSockAddr).sin_addr)
  else
    Result := '';
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

destructor TmnWallSocket.Destroy;
begin
  inherited;
end;

function SetNonBlock(ASocket: TSocketHandle; ANonBlock: Boolean): Integer;
var
  LFlag: Cardinal;
begin
  LFlag := fcntl(ASocket, F_GETFL);
  if ANonBlock then
    LFlag := LFlag and not O_SYNC or O_NONBLOCK
  else
    LFlag := LFlag and not O_NONBLOCK or O_SYNC;
  Result := fcntl(ASocket, F_SETFL, LFlag);
end;

function TmnWallSocket.TestGetAddrInfo(const AHostName, AServiceName: string; const AHints: AddrInfo): PAddrInfo;
var
  M: TMarshaller;
  LHost, LService: Pointer;
  LAddrInfo: PAddrInfo;
begin
  if (AHostName <> '') then
    LHost := M.AsAnsi(AHostName).ToPointer
  else
    LHost := nil;

  if (AServiceName <> '') then
    LService := M.AsAnsi(AServiceName).ToPointer
  else
    LService := nil;

  if Posix.NetDB.getaddrinfo(LHost, LService, AHints, Paddrinfo(LAddrInfo))=0 then
    Result := LAddrInfo
  else
    Result := nil;
end;

function TmnWallSocket.GetSocketError(Handle: TSocketHandle): Integer;
var
  errno: Longint;
  l: {$ifdef ANDROID32} Integer;{$else}Cardinal;{$endif}
begin
  l := SizeOf(errno);
  if getsockopt(Handle, SOL_SOCKET, SO_ERROR, errno, l) = 0 then
    Result := errno
  else
    Result := -1;
end;

function TmnWallSocket.LookupPort(Port: string): Word;
begin
  Result := StrToIntDef(Port, 0);
end;

procedure TmnWallSocket.FreeSocket(var vHandle: TSocketHandle);
begin
  if (vHandle <> 0) or (vHandle <>  INVALID_SOCKET) then
    __close(vHandle);
  vHandle := INVALID_SOCKET;
end;

procedure TmnWallSocket.Accept(ListenerHandle: TSocketHandle; Options: TmnsoOptions; ReadTimeout: Integer; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aSize: {$ifdef ANDROID32}Integer;{$else}Cardinal;{$endif}
  aAddress: TSockAddr;
begin
  aSize := SizeOf(aAddress);
  aHandle := Posix.SysSocket.accept(ListenerHandle, aAddress.addr, aSize);
  //aHandle := Posix.SysSocket.Accept(ListenerHandle, nil, nil); <- that the correct

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

function TmnWallSocket.Select(vHandle: TSocketHandle; Timeout: Integer; Check: TSelectCheck): TmnError;
var
  FSet: fd_set;
  PSetRead, PSetWrite: Pfd_set;
  c: Integer;
  LTime: TimeVal;
  LTimePtr: PTimeVal;
begin
  //CheckActive; no need select will return error for it, as i tho
  if vHandle = INVALID_SOCKET then
    Result := erClosed
  else
  begin
    fd_zero(FSet);
    _FD_SET(vHandle, FSet);

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
      LTimePtr := nil
    else
    begin
      LTime.tv_sec := Timeout div 1000;
      LTime.tv_usec := (Timeout mod 1000) * 1000;
      LTimePtr := @LTime;
    end;

    c := Posix.SysSelect.Select(vHandle + 1, PSetRead, PSetWrite, nil, LTimePtr);

    if (c = SOCKET_ERROR) then
      Result := erInvalid
    else if (c = 0) then
      Result := erTimeout
    else
      Result := erSuccess;
  end;
end;

procedure TmnWallSocket.Bind(Options: TmnsoOptions; ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aAddr : TSockAddr;
begin
  aHandle := Posix.SysSocket.socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);

  if aHandle <> INVALID_SOCKET then
  begin
    vErr := InitSocketOptions(aHandle, Options, ReadTimeout);

    if soReuseAddr in Options then
      vErr := setsockopt(aHandle, SOL_SOCKET, SO_REUSEADDR, SO_TRUE, SizeOf(SO_TRUE));

    aAddr.addr_in.sin_family := AF_INET;
    aAddr.addr_in.sin_port := htons(LookupPort(Port));
    if (Address = '') or (Address = '0.0.0.0') then
      aAddr.addr_in.sin_addr.s_addr := INADDR_ANY
    else
      aAddr.addr_in.sin_addr := StrToNetAddr(Address);

    If Posix.SysSocket.bind(aHandle, aAddr.addr, Sizeof(aAddr)) <> 0 then
    begin
      vErr := errno; //GetSocketError(aHandle);
      FreeSocket(aHandle);
    end;
  end;

  if aHandle<>INVALID_SOCKET then
    vSocket := TmnSocket.Create(aHandle, Options, skListener)
  else
    vSocket := nil;
end;

procedure TmnWallSocket.Connect(Options: TmnsoOptions; ConnectTimeout, ReadTimeout: Integer; const Port: string; const Address: string; out vSocket: TmnCustomSocket; out vErr: Integer);
var
  aHandle: TSocketHandle;
  aAddr : TSockAddr;
  ret: integer;

  LHints: AddrInfo;
  LRetVal: Integer;
  LAddrInfo: pAddrInfo;
  aInfo: AddrInfo;
  aMode: UInt32;
begin
  //nonblick connect  https://stackoverflow.com/questions/1543466/how-do-i-change-a-tcp-socket-to-be-non-blocking
  //https://stackoverflow.com/questions/14254061/setting-time-out-for-connect-function-tcp-socket-programming-in-c-breaks-recv

  FillChar(aInfo, SizeOf(AddrInfo), 0);
  aInfo.ai_family := AF_UNSPEC;
  aInfo.ai_socktype := SOCK_STREAM;
  aInfo.ai_protocol := IPPROTO_TCP;
  LAddrInfo := TestGetAddrInfo(Address, Port, aInfo); //Zaher: IDK what is it, but cool

  if LAddrInfo = nil then //TODO
  begin
    vSocket := nil;
    vErr := 0;
    exit;
  end;
  //aHandle := Posix.SysSocket.socket(AF_INET, SOCK_STREAM{TODO: for nonblock option: or O_NONBLOCK}, IPPROTO_TCP);
  aHandle := Posix.SysSocket.socket(LAddrInfo.ai_family, LAddrInfo.ai_socktype, LAddrInfo.ai_protocol);

  if aHandle <> INVALID_SOCKET then
  begin
    vErr := InitSocketOptions(aHandle, Options, ReadTimeout);

    if ConnectTimeout <> -1 then
    begin
      aMode := 1;
      ret := IOCtl(aHandle, FIONBIO, @aMode);
      if ret = Longint(SOCKET_ERROR) then
      begin
        vErr := errno; //GetSocketError(aHandle);
        FreeSocket(aHandle);
      end;
    end;

    if aHandle <> TSocketHandle(SOCKET_ERROR) then
    begin
      aAddr.addr_in.sin_family := AF_INET;
      aAddr.addr_in.sin_port := htons(LookupPort(Port));

      if (Address = '') or (Address = '0.0.0.0') then
        aAddr.addr_in.sin_addr.s_addr := INADDR_ANY
      else
      begin
        aAddr.addr_in.sin_addr := StrToNetAddr(Address);
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
          begin
            vErr := errno; //GetSocketError(aHandle);
            FreeSocket(aHandle);
          end;
        end;
      end;

      if aHandle <> TSocketHandle(SOCKET_ERROR) then
      begin
        ret := Posix.SysSocket.connect(aHandle, aAddr.addr, SizeOf(aAddr));

        if ret = -1 then
        begin
          vErr := errno; //GetSocketError(aHandle);
          if (ConnectTimeout <> -1) and ((vErr = EWOULDBLOCK) or (vErr = EINPROGRESS)) then //Need to wait
          begin
            aMode := 0;
            ret := IOCtl(aHandle, Longint(FIONBIO), @aMode);
            if ret = Longint(SOCKET_ERROR) then
            begin
              vErr := errno;//GetSocketError(aHandle);
              FreeSocket(aHandle)
            end
            else if Select(aHandle, ConnectTimeout, slWrite) <> erSuccess then
            begin
              vErr := errno; //GetSocketError(aHandle);
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

end.
