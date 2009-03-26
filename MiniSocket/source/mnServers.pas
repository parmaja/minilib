unit mnServers;
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
{$MODE delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  mnSockets,
  mnConnections;

type
  TmnServer = class;
  TmnListener = class;

  TmnServerConnection = class(TmnConnection)
  private
    FListener: TmnListener;
    procedure SetListener(const Value: TmnListener);
  protected
    procedure Execute; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    property Listener: TmnListener read FListener write SetListener;
  end;

  TmnServerConnectionClass = class of TmnServerConnection;

  TmnOnLog = procedure(Connection: TmnConnection; const S: string) of object;
  TmnOnListenerNotify = procedure(Listener: TmnListener) of object;

  TmnListener = class(TmnLockThread) // thread to watch for incoming requests
  private
    FAttempt: Integer;
    FSocket: TmnCustomSocket;
    FPort: string;
    FAddress: string;
    FList: TmnConnectionList;
    FOnLog: TmnOnLog;
    FServer: TmnServer;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
    function GetCount: Integer;
  protected
    FOptions: TmnOptions;
    function CreateConnection(Socket: TmnCustomSocket): TmnServerConnection; virtual;
    procedure Prepare; virtual; // called before add a new connection
    procedure Shutdown; virtual;
    procedure Execute; override;
    procedure Changed; virtual;
    procedure Remove(Connection: TmnServerConnection); virtual;
    procedure Add(Connection: TmnServerConnection); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Log(Connection: TmnConnection; S: string);
    property Server: TmnServer read FServer;
    property Connected: Boolean read GetConnected;
    property Socket: TmnCustomSocket read FSocket;
    property Count: Integer read GetCount;
    property Options: TmnOptions read FOptions;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
  end;

  TmnServer = class(TComponent)
  private
    FPort: string;
    FActive: Boolean;
    FListener: TmnListener;
    FAddress: string;
    FOnBeforeOpen: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnListenerNotify;
    FOnPrepare: TmnOnListenerNotify;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
    function GetCount: Integer;
  protected
    function CreateListener: TmnListener; virtual;
    procedure DoChanged(Listener: TmnListener); virtual;
    procedure DoPrepare(Listener: TmnListener); virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoAfterClose; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Start;
    procedure Stop;
    procedure Open;
    procedure Close;
    property Listener: TmnListener read FListener;
    property Count: Integer read GetCount;
  published
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
    property Active: boolean read FActive write SetActive default False;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnListenerNotify read FOnChanged write FOnChanged;
    property OnPrepare: TmnOnListenerNotify read FOnPrepare write FOnPrepare;
  end;

implementation

{ TmnServerConnection }

constructor TmnServerConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  FreeOnTerminate := True;
end;

destructor TmnServerConnection.Destroy;
begin
  inherited;
end;

procedure TmnServerConnection.Execute;
begin
  inherited;
  Listener := nil;
end;

procedure TmnServerConnection.SetListener(const Value: TmnListener);
begin
  if FListener <> Value then
  begin
    if FListener <> nil then
    begin
      FListener.Remove(Self);
    end;
    FListener := Value;
    if FListener <> nil then
    begin
      FListener.Add(Self);
    end;
  end;
end;

procedure TmnServer.SetActive(const Value: boolean);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
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

{ TmnServer }

procedure TmnServer.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    if FActive then
      Start;
  end;
end;

procedure TmnServer.DoChanged(Listener: TmnListener);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnChanged) then
      FOnChanged(Listener);
  end;
end;

procedure TmnServer.DoPrepare(Listener: TmnListener);
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnPrepare) then
      FOnPrepare(Listener);
  end;
end;

procedure TmnServer.DoBeforeClose;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnBeforeClose) then
      FOnBeforeClose(Self);
  end;
end;

function TmnServer.GetCount: Integer;
begin
  if Listener <> nil then
    Result := Listener.Count
  else
    Result := 0;
end;

procedure TmnServer.DoAfterOpen;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnAfterOpen) then
      FOnAfterOpen(Self);
  end;
end;

{ TmnListener }

procedure TmnListener.Add(Connection: TmnServerConnection);
begin
  Enter;
  try
    FList.Add(Connection);
    Changed;
  finally
    Leave;
  end;
end;

procedure TmnListener.Changed;
begin
  if FServer <> nil then
    FServer.DoChanged(Self);
end;

procedure TmnListener.Connect;
begin
  FSocket := WallSocket.Bind(FOptions, FPort, FAddress);
  if Connected then
    Socket.Listen;
end;

constructor TmnListener.Create;
begin
  inherited;
  FList := TmnConnectionList.Create;
  FAttempt := 3; // 3 times
end;

function TmnListener.CreateConnection(Socket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TmnServerConnection.Create(Socket);
end;

destructor TmnListener.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TmnListener.Disconnect;
begin
  if Connected then
  begin
    Socket.Close;
  end;
  FreeAndNil(FSocket);
end;

procedure TmnListener.Execute;
var
  aSocket: TmnCustomSocket;
  aConnection: TmnServerConnection;
begin
  Connect;
  while Connected and not Terminated do
  begin
    try
      begin
        aSocket := Socket.Accept;
        if (aSocket = nil) then
        begin
          //must attempt for new socket 3 times
          if not Terminated then
          begin
            if FAttempt > 0 then
            begin
              FAttempt := FAttempt - 1;
              Connect;
            end
            else
              Socket.Shutdown(sdBoth);
          end;
        end
        else
        begin
          try
            Enter; //because we add connection to a thread list
            try
              Prepare;
              aConnection := CreateConnection(aSocket);
              aConnection.Listener := Self;
//              aConnection.Start;
            finally
              Leave;
            end;
            aConnection.Start; //moved here need some test
          finally
          end;
        end;
      end
    finally
    end;
  end;
  Shutdown;
  Disconnect;
end;

function TmnListener.GetConnected: Boolean;
begin
  Result := (FSocket <> nil) and FSocket.Active;
end;

function TmnListener.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TmnListener.Log(Connection: TmnConnection; S: string);
begin
  if Assigned(FOnLog) then
  begin
//    Enter;
    try
      FOnLog(Connection, S);
    finally
//      Leave;
    end;
  end;
end;

procedure TmnListener.Prepare;
begin
  if FServer <> nil then
    FServer.DoPrepare(Self);
end;

procedure TmnListener.Remove(Connection: TmnServerConnection);
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

procedure TmnListener.Start;
begin
  Resume;
end;

procedure TmnListener.Shutdown;
var
  i: Integer;
begin
  Enter;
  try
    for i := 0 to FList.Count - 1 do
    begin
      FList[i].FreeOnTerminate := False;
      FList[i].Stop;
    end;
  finally
    Leave;
  end;
  try
    while FList.Count > 0 do
    begin
      FList[0].WaitFor;
      FList[0].Free;
      FList.Delete(0);
    end;
    Changed;
  finally
  end;
end;

{ TmnServer }

constructor TmnServer.Create(AOwner: TComponent);
begin
  inherited;
  FAddress := '0.0.0.0';
  if not (csDesigning in ComponentState) then
    WallSocket.Startup;
end;

destructor TmnServer.Destroy;
begin
  Stop;
  if not (csDesigning in ComponentState) then
    WallSocket.Cleanup;
  inherited;
end;

procedure TmnServer.Start;
begin
//  CheckInactive;
  if (FListener = nil) then // if its already active, dont start again
  begin
    try
      DoBeforeOpen;
      try
        FListener := CreateListener;
        FListener.OnLog := OnLog;
        FListener.FServer := Self;
        FListener.FPort := FPort;
        FListener.FAddress := Address;
        FListener.Start;
        FActive := True;
      except
        FreeAndNil(FListener);
        raise;
      end;
      DoAfterOpen;
    finally
    end;
  end;
end;

procedure TmnListener.Stop;
begin
  Enter;
  try
    Terminate;
    Socket.Cancel;
//    Socket.Shutdown(sdBoth); //if thread in accept()
    Socket.Close; //if thread in accept()
  finally
    Leave;
  end;
end;

procedure TmnServer.Stop;
begin
  if (FListener <> nil) then
  begin
    DoBeforeClose;
    FListener.Stop;
    FListener.WaitFor;
    FreeAndNil(FListener);
    FActive := False;
    DoAfterClose;
  end;
end;

function TmnServer.CreateListener: TmnListener;
begin
  Result := TmnListener.Create;
end;

procedure TmnServer.SetAddress(const Value: string);
begin
  if Active then
    raise EmnException.Create('Can not change Address value when active');
  FAddress := Value;
end;

procedure TmnServer.SetPort(const Value: string);
begin
  if Active then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

procedure TmnServer.DoBeforeOpen;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnBeforeOpen) then
      FOnBeforeOpen(Self);
  end;
end;

procedure TmnServer.DoAfterClose;
begin
  if not (csDestroying in ComponentState) then
  begin
    if Assigned(FOnAfterClose) then
      FOnAfterClose(Self);
  end;
end;

procedure TmnServer.Close;
begin
  Stop;
end;

procedure TmnServer.Open;
begin
  Start;
end;

initialization
finalization
end.

