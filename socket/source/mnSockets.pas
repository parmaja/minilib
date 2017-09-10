unit mnSockets;
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
  SysUtils;

type
  EmnException = class(Exception);
  TmnShutdown = (sdNone, sdReceive, sdSend, sdBoth);
  TmnError = (erNone, erTimout, erFail, erClosed, erInvalid);
  TSelectCheck = (slRead, slWrite);

  TmnsoOption = (soBroadcast, soDebug, soDontLinger, soDontRoute, soKeepAlive, soOOBInLine, soReuseAddr, soNoDelay, soBlocking, soAcceptConn);
  TmnsoOptions = set of TmnsoOption;

  { TmnCustomSocket }

  TmnCustomSocket = class(TObject)
  private
    FCloseWhenError: Boolean;
    FClosing: Boolean;
    FShutdownState: TmnShutdown;
    function GetConnected: Boolean;
  protected
    procedure Error;
    function GetActive: Boolean; virtual; abstract;
    procedure CheckActive;
    function DoSelect(Timeout: Int64; Check: TSelectCheck): TmnError; virtual; abstract;
    function DoShutdown(How: TmnShutdown): TmnError; virtual; abstract;
    property ShutdownState: TmnShutdown read FShutdownState;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    function Shutdown(How: TmnShutdown): TmnError;
    procedure Close; virtual; abstract;
    function Send(const Buffer; var Count: Longint): TmnError; virtual; abstract;
    function Receive(var Buffer; var Count: Longint): TmnError; virtual; abstract;
    function Select(Timeout: Int64; Check: TSelectCheck): TmnError;
    function Listen: TmnError; virtual; abstract;
    function Accept: TmnCustomSocket; virtual; abstract;
    property Active: Boolean read GetActive;
    property Connected: Boolean read GetConnected;
    property CloseWhenError: Boolean read FCloseWhenError write FCloseWhenError default True;
    function GetLocalAddress: ansistring; virtual; abstract;
    function GetRemoteAddress: ansistring; virtual; abstract;
    function GetLocalName: string; virtual; abstract;
    function GetRemoteName: string; virtual; abstract;
  end;

  TmnCustomWallSocket = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Bind(Options: TmnsoOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; virtual; abstract;
    function Connect(Options: TmnsoOptions; const Port: ansistring; const Address: ansistring = ''): TmnCustomSocket; virtual; abstract;
  end;

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
    mnWinSockets; //delphi is only Win32
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
  FCloseWhenError := True;
end;

destructor TmnCustomSocket.Destroy;
begin
  if Active then
  begin
    Close;
  end;
  inherited;
end;

procedure TmnCustomSocket.Error;
begin
  if FCloseWhenError then
    Close;
end;

function TmnCustomSocket.GetConnected: Boolean;
begin
  Result := Active and (FShutdownState = sdNone)
end;

function TmnCustomSocket.Select(Timeout: Int64; Check: TSelectCheck): TmnError;
begin
  Result := DoSelect(Timeout, Check);
  if (Result = erNone) and FClosing then
    Result := erClosed;
end;

procedure TmnCustomSocket.Terminate;
begin
  FClosing := True;
  Shutdown(sdBoth);
  Close;
end;

function TmnCustomSocket.Shutdown(How: TmnShutdown): TmnError;
begin
  if How > sdNone then
  begin
    Result := DoShutdown(How);
    if Result = erNone then
      FShutdownState := How;
  end
  else
    Result := erNone;
end;

initialization
finalization
  FreeAndNil(FmnWallSocket);
end.
