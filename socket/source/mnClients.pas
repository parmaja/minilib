unit mnClients;
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
  mnSockets,
  mnSocketStreams, 
  mnConnections;

type
{ TmnClient }

  TmnClient = class;
  TmnCaller = class;

  TmnClientSocketStream = class(TmnSocketStream)
  private
    FAddress: string;
    FPort: string;
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
  protected
    function CreateSocket: TmnCustomSocket; override;
  public
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
  end;

  TmnClientStream = class(TmnSocketStream)
  private
    FAddress: string;
    FPort: string;
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
  protected
    function CreateSocket: TmnCustomSocket; override;
  public
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
  end;

  TmnClientConnection = class(TmnConnection)
  private
    FCaller: TmnCaller;
    procedure SetCaller(const Value: TmnCaller);
  protected
    procedure Execute; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    property Caller: TmnCaller read FCaller write SetCaller;
  end;

  TmnClientConnectionClass = class of TmnClientConnection;

  TmnOnLog = procedure(Connection: TmnConnection; const S: string) of object;
  TmnOnCallerNotify = procedure(Caller: TmnCaller) of object;

  TmnCaller = class(TmnLockThread) // thread to watch for incoming requests
  private
    FPort: string;
    FAddress: string;
    FList: TmnConnectionList;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnCallerNotify;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
    function GetCount: Integer;
  protected
    FOptions: TmnOptions;
    function CreateConnection(Socket: TmnCustomSocket): TmnClientConnection; virtual;
    procedure Shutdown;
    procedure Execute; override;
    procedure Changed;
    procedure Remove(Connection: TmnClientConnection);
    procedure Add(Connection: TmnClientConnection);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {$ifndef FPC} //already found in FPC 2.4.4
    procedure Start;
    {$endif}
    procedure Stop;
    procedure Log(Connection: TmnConnection; S: string);
    function AddConnection(vPort: string; vAddress: string): TmnClientConnection;
    property Connected: Boolean read GetConnected;
    property Count: Integer read GetCount;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnCallerNotify read FOnChanged write FOnChanged;
  end;

  TmnClient = class(TComponent)
  private
    FActive: Boolean;
    FPort: string;
    FAddress: string;
    FCaller: TmnCaller;
    FOnBeforeOpen: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnCallerNotify;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
  protected
    function CreateCaller: TmnCaller; virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoAfterClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Start;
    procedure Stop;
    procedure Open;
    procedure Close;
  published
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
    property Active: boolean read FActive write SetActive default False;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnCallerNotify read FOnChanged write FOnChanged;
  end;

function mnClient: TmnClient;

implementation

var
  FmnClient: TmnClient = nil;

function mnClient: TmnClient;
begin
  if FmnClient = nil then
    FmnClient := TmnClient.Create(nil);
  Result := FmnClient;
end;

{ TmnClientConnection }

constructor TmnClientConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  FreeOnTerminate := True;
end;

destructor TmnClientConnection.Destroy;
begin
  inherited;
end;

procedure TmnClientConnection.Execute;
begin
  inherited;
  Caller := nil;
end;

procedure TmnClientConnection.SetCaller(const Value: TmnCaller);
begin
  if FCaller <> Value then
  begin
    if FCaller <> nil then
    begin
      FCaller.Remove(Self);
    end;
    FCaller := Value;
    if FCaller <> nil then
    begin
      FCaller.Add(Self);
    end;
  end;
end;

procedure TmnClient.SetActive(const Value: boolean);
begin
  if not (csDesigning in ComponentState) then
  begin
    FActive := Value;
  end
  else
  begin
    if Value and not FActive then
      Start
    else if not Value and FActive then
      Stop;
  end;
end;

{ TmnClient }

procedure TmnClient.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FActive then
      Start;
  end;
end;

{ TmnCaller }

procedure TmnCaller.Add(Connection: TmnClientConnection);
begin
  Enter;
  try
    FList.Add(Connection);
    Changed;
  finally
    Leave;
  end;
end;

procedure TmnCaller.Changed;
begin
  if Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TmnCaller.Connect;
begin
end;

constructor TmnCaller.Create;
begin
  inherited;
  FList := TmnConnectionList.Create;
end;

function TmnCaller.CreateConnection(Socket: TmnCustomSocket): TmnClientConnection;
begin
  Result := TmnClientConnection.Create(Socket);
end;

destructor TmnCaller.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TmnCaller.Disconnect;
begin
  if Connected then
  begin
    WallSocket.Cleanup;
  end;
end;

procedure TmnCaller.Execute;
begin
  Connect;
//  Suspend; deprecated in FPC 2.4.4
  Shutdown;
  Disconnect;
end;

function TmnCaller.GetConnected: Boolean;
begin
  Result := Terminated;
end;

function TmnCaller.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TmnCaller.Log(Connection: TmnConnection; S: string);
begin
  if Assigned(FOnLog) then
  begin
    Enter;
    try
      FOnLog(Connection, S);
    finally
      Leave;
    end;
  end;
end;

procedure TmnCaller.Remove(Connection: TmnClientConnection);
begin
  Enter;
  try
    if Connection.FreeOnTerminate then
      FList.Remove(Connection);
    Changed;
  finally
    Leave;
  end;
end;

{$ifndef FPC} //already found in FPC 2.4.4
procedure TmnCaller.Start;
begin
  Resume;
end;
{$endif}

procedure TmnCaller.Stop;
begin
  Terminate;
//  Resume; deprecated
end;

procedure TmnCaller.Shutdown;
var
  i: Integer;
begin
  Enter;
  try
    for i := 0 to FList.Count - 1 do
    begin
      FList[i].FreeOnTerminate := False;
      FList[i].Terminate;
    end;
  finally
    Leave;
  end;
  try
    while FList.Count > 0 do
    begin
//      WaitForSingleObject(FList[0].Handle, INFINITE);
      FList[0].WaitFor;
      FList[0].Free;
      FList.Delete(0);
    end;
    Changed;
  finally
  end;
end;

function TmnCaller.AddConnection(vPort: string; vAddress: string): TmnClientConnection;
var
  aSocket: TmnCustomSocket;
begin
  if vPort = '' then
    vPort := FPort;
  if vAddress = '' then
    vAddress := FAddress;
  aSocket := WallSocket.Connect([], vPort, vAddress);
  Result := CreateConnection(aSocket);
  Result.Caller := Self;
  Result.Start;
end;

{ TmnClient }

constructor TmnClient.Create(AOwner: TComponent);
begin
  inherited;
  FAddress := '0.0.0.0';
  if not (csDesigning in ComponentState) then
    WallSocket.Startup;
end;

destructor TmnClient.Destroy;
begin
  if not (csDesigning in ComponentState) then
    WallSocket.Cleanup;
  inherited;
end;

procedure TmnClient.Start;
begin
//  CheckInactive;
  if (FCaller = nil) then // if its already active, dont start again
  begin
    try
      DoBeforeOpen;
      try
        FCaller := CreateCaller;
        FCaller.OnLog := OnLog;
        FCaller.OnChanged := OnChanged;
        FCaller.FPort := FPort;
        FCaller.FAddress := Address;
        FCaller.Start;
        FActive := True;
      except
        FreeAndNil(FCaller);
        WallSocket.Cleanup;
        raise;
      end
    finally
    end;
  end;
end;

procedure TmnClient.Stop;
begin
  if (FActive) then
  begin
    FCaller.Stop;
    FCaller.WaitFor;
    FreeAndNil(FCaller);
    FActive := False;
    DoAfterClose;
  end;
end;

function TmnClient.CreateCaller: TmnCaller;
begin
  Result := TmnCaller.Create;
end;

procedure TmnClient.SetAddress(const Value: string);
begin
  if Active then
    raise EmnException.Create('Can not change Address value when active');
  FAddress := Value;
end;

procedure TmnClient.SetPort(const Value: string);
begin
  if Active then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

procedure TmnClient.DoBeforeOpen;
begin
  if Assigned(FOnBeforeOpen) then
    FOnBeforeOpen(Self);
end;

procedure TmnClient.DoAfterClose;
begin
  if Assigned(FOnAfterClose) then
    FOnAfterClose(Self);
end;

procedure TmnClient.Close;
begin
  Stop;
end;

procedure TmnClient.Open;
begin
  Start;
end;

{ TmnClientStream }

function TmnClientStream.CreateSocket: TmnCustomSocket;
begin
  Result := WallSocket.Connect([], Port, Address)
end;

procedure TmnClientStream.SetAddress(const Value: string);
begin
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FAddress := Value;
end;

procedure TmnClientStream.SetPort(const Value: string);
begin
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

{ TmnClientSocketStream }

function TmnClientSocketStream.CreateSocket: TmnCustomSocket;
begin
  Result := WallSocket.Connect([], Port, Address)
end;

procedure TmnClientSocketStream.SetAddress(const Value: string);
begin
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FAddress := Value;
end;

procedure TmnClientSocketStream.SetPort(const Value: string);
begin
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

end.

