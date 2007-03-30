unit mnSockets;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

uses
  Classes,
  SysUtils;

type
  EmnException = class(Exception);
  TmnShutdown = (sdReceive, sdSend, sdBoth);
  TmnError = (erNone, erTimout, erFail, erClosed, erInvalid);
  TSelectCheck = (slRead, slWrite);

{  TmnState = (ssInvalidState, ssOpened, ssBound, ssConnecting, ssConnected, ssAccepting, ssListening, ssClosed); }
  TmnOption = (soBroadcast, soDebug, soDontLinger, soDontRoute, soKeepAlive, soOOBInLine, soReuseAddr, soNoDelay, soBlocking, soAcceptConn);
  TmnOptions = set of TmnOption;

  TmnCustomSocket = class(TObject)
  private
    FClosing: Boolean;
  protected
    procedure FatalError(const Msg: string);
    procedure Error;
    function GetActive: Boolean; virtual; abstract;
    procedure CheckActive;
    function DoSelect(Timeout: Integer; Check: TSelectCheck): TmnError; virtual; abstract;
  public
    destructor Destroy; override;
    function RecvLength: Cardinal; virtual; abstract;
    procedure Terminate;
    procedure Close; virtual; abstract;
    function Shutdown(How: TmnShutdown): TmnError; virtual; abstract;
    function Send(const Buffer; var Count: Longint): TmnError; virtual; abstract;
    function Receive(var Buffer; var Count: Longint): TmnError; virtual; abstract;
    function Select(Timeout: Integer; Check: TSelectCheck): TmnError;
    function Listen: TmnError; virtual; abstract;
    function Accept: TmnCustomSocket; virtual; abstract;
    procedure Cancel; virtual; abstract;
    property Active: Boolean read GetActive;
    function GetLocalAddress: string; virtual; abstract;
    function GetLocalName: string; virtual; abstract;
    function GetRemoteAddress: string; virtual; abstract;
    function GetRemoteName: string; virtual; abstract;
  end;

  TmnCustomWallSocket = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Bind(Options: TmnOptions; const Port: string; const Address: string = ''): TmnCustomSocket; virtual; abstract;
    function Connect(Options: TmnOptions; const Port: string; const Address: string = ''): TmnCustomSocket; virtual; abstract;
    procedure Startup; virtual; abstract;
    procedure Cleanup; virtual; abstract;
  end;

function WallSocket: TmnCustomWallSocket;
function IsWallSocketRegistered: Boolean;
procedure RegisterWallSocket(WallSocket: TmnCustomWallSocket);

implementation

var
  FmnWallSocket: TmnCustomWallSocket = nil;

function IsWallSocketRegistered: Boolean;
begin
  Result := FmnWallSocket <> nil;
end;

function WallSocket: TmnCustomWallSocket;
begin
  if FmnWallSocket = nil then
    raise EmnException.Create('no WallSocket registerd');
  Result := FmnWallSocket;
end;

procedure RegisterWallSocket(WallSocket: TmnCustomWallSocket);
begin
  if FmnWallSocket <> nil then
    raise EmnException.Create('Already a WallSocket registerd');
  FmnWallSocket := WallSocket;
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
    FatalError('Socket is inactive');
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
  Close;
end;

procedure TmnCustomSocket.FatalError(const Msg: string);
begin
  Close;
  raise EmnException.Create(Msg);
end;

function TmnCustomSocket.Select(Timeout: Integer;
  Check: TSelectCheck): TmnError;
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

initialization
finalization
  FreeAndNil(FmnWallSocket);
end.

