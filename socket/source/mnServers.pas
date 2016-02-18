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
{$ELSE}
{$ENDIF}

{.$define Synchronize}
{.$define NoLog}

interface

uses
  Classes,
  SysUtils,
  mnSockets, mnSocketStreams, mnHttpClient,
  mnConnections;

type
  TmnServer = class;
  TmnListener = class;

  { TmnServerConnection }

  TmnServerConnection = class(TmnConnection)
  private
    function GetListener: TmnListener;
  protected
    procedure Execute; override;
  public
    constructor Create(vConnector: TmnConnector; Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    property Listener: TmnListener read GetListener;
  end;

  TmnServerConnectionClass = class of TmnServerConnection;

  TmnOnLog = procedure(const S: string) of object;
  TmnOnListenerNotify = procedure(Listener: TmnListener) of object;

  { TmnListener }

  TmnListener = class(TmnConnector) // thread to watch for incoming requests
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
    FLogMessage: string;
    procedure SyncLog;
    procedure SyncChanged;
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; virtual;
    procedure DropConnections; virtual;
    procedure Prepare; virtual;
    procedure Execute; override;
    procedure Changed; virtual;
    procedure Remove(Connection: TmnServerConnection); virtual;
    procedure Add(Connection: TmnServerConnection); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    {$ifndef FPC} //already found in FPC 2.4.4
    procedure Start;
    {$endif}
    procedure Stop;
    procedure Log(S: string);
    property Server: TmnServer read FServer;
    property Connected: Boolean read GetConnected;
    property Socket: TmnCustomSocket read FSocket;
    property Count: Integer read GetCount;
    property Options: TmnOptions read FOptions;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    //if listener connection down by network it reconnect again
    property Attempt: Integer read FAttempt write FAttempt;
  end;

  { TmnServer }

  TmnServer = class(TObject)
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
    IsDestroying: Boolean;
    function CreateListener: TmnListener; virtual;
    procedure DoChanged(vListener: TmnListener); virtual;
    procedure DoPrepare(vListener: TmnListener); virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoAfterClose; virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Start(vListener: TmnListener = nil);
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

constructor TmnServerConnection.Create(vConnector: TmnConnector; Socket: TmnCustomSocket);
begin
  inherited;
  FreeOnTerminate := True;
  if Listener <> nil then
  begin
    Listener.Add(Self);
  end;
end;

destructor TmnServerConnection.Destroy;
begin
  inherited;
end;

function TmnServerConnection.GetListener: TmnListener;
begin
  Result := Connector as TmnListener;
end;

procedure TmnServerConnection.Execute;
begin
  inherited;
  if Listener <> nil then
  begin
    Listener.Remove(Self);
  end;
end;

procedure TmnServer.SetActive(const Value: Boolean);
begin
  if Value and not FActive then
    Start
  else if not Value and FActive then
    Stop;
end;

{ TmnServer }

procedure TmnServer.DoChanged(vListener: TmnListener);
begin
  if not (IsDestroying) then
  begin
    if Assigned(FOnChanged) then
      FOnChanged(vListener);
  end;
end;

procedure TmnServer.DoPrepare(vListener: TmnListener);
begin
  if not (IsDestroying) then
  begin
    if Assigned(FOnPrepare) then
      FOnPrepare(vListener);
  end;
end;

procedure TmnServer.DoBeforeClose;
begin
  if not (IsDestroying) then
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
  if not (IsDestroying) then
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
  {$ifndef NoLog}
  {$ifdef Synchrcmd
  onize}
  Synchronize(Self, SyncChanged);
  {$else}
  SyncChanged;
  {$endif}
  {$endif NoLog}
end;

procedure TmnListener.Connect;
begin
  if not Terminated then
  begin
    FSocket := WallSocket.Bind(FOptions, FPort, FAddress);
    FSocket.CloseWhenError := False;
    if Connected then
      Socket.Listen;
  end;
end;

constructor TmnListener.Create;
begin
  inherited;
  FList := TmnConnectionList.Create;
  FAttempt := 0; // 3 times
end;

function TmnListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TmnServerConnection.Create(Self, vSocket);
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
        if (Socket.Select(10000, slRead) = erNone) and not Terminated then
          aSocket := Socket.Accept
        else
          aSocket := nil;
        Enter;
        try
          //Just a stop to finish proc outside
        finally
          Leave;
        end;
        if not Terminated then
        begin
          if (aSocket = nil) then
          begin
            //must attempt for new socket 3 times
            if (FAttempt > 0) and (not Socket.Active) then
            begin
              FAttempt := FAttempt - 1;
              Connect;
            end;
          end
          else
          begin
            try
              Enter; //because we add connection to a thread list
              try
                if FServer <> nil then
                  FServer.DoPrepare(Self);
                aConnection := CreateConnection(aSocket);
              finally
                Leave;
              end;
              aConnection.Start; //moved here need some test
            finally
            end;
          end;
        end;
      end
    finally
    end;
  end;
  DropConnections;
  Disconnect;
  Changed;
end;

function TmnListener.GetConnected: Boolean;
begin
  Result := (FSocket <> nil) and FSocket.Active;
end;

function TmnListener.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TmnListener.Log(S: string);
begin
  {$ifndef NoLog}
  FLogMessage := S;
  {$ifdef Synchronize}
  Synchronize(Self, SyncLog);
  {$else}
  SyncLog;
  {$endif}
  {$endif NoLog}
end;

procedure TmnListener.Prepare;
begin
end;

procedure TmnListener.Remove(Connection: TmnServerConnection);
begin
  Enter;
  try
    if Connection.FreeOnTerminate then
      FList.Remove(Connection);
  finally
    Leave;
  end;
  Changed;
end;

{$ifndef FPC} //already found in FPC 2.4.4
procedure TmnListener.Start;
begin
  Resume;
end;
{$endif}

procedure TmnListener.DropConnections;
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
  finally
  end;
end;

{ TmnServer }

constructor TmnServer.Create;
begin
  inherited Create;
  FAddress := '0.0.0.0';
end;

destructor TmnServer.Destroy;
begin
  IsDestroying := True;
  Stop;
  inherited;
end;

procedure TmnServer.Start(vListener: TmnListener);
begin
  if (FListener = nil) then // if its already active, dont start again
  begin
    try
      DoStart;
      DoBeforeOpen;
      try
        FListener := vListener;
        if FListener = nil then
          FListener := CreateListener;
        FListener.OnLog := OnLog;
        FListener.FServer := Self;
        FListener.FPort := FPort;
        FListener.FAddress := Address;
        FListener.Prepare;
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
    if Socket <> nil then
    begin
      {$ifdef FPC}
      {$ifndef WINDOWS}
      {$hint 'Why need to Shutdown to stop Accept?'}
      Socket.Shutdown(sdBoth); //stop the accept from waiting
      {$endif}
      {$endif}
      Socket.Close;
    end;
  finally
    Leave;
  end;
end;

procedure TmnListener.SyncChanged;
begin
  if FServer <> nil then
    FServer.DoChanged(Self);
end;

procedure TmnListener.SyncLog;
begin
  if Assigned(FOnLog) then
    FOnLog(FLogMessage);
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
  DoStop;
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
  if not (IsDestroying) then
  begin
    if Assigned(FOnBeforeOpen) then
      FOnBeforeOpen(Self);
  end;
end;

procedure TmnServer.DoAfterClose;
begin
  if not (IsDestroying) then
  begin
    if Assigned(FOnAfterClose) then
      FOnAfterClose(Self);
  end;
end;

procedure TmnServer.DoStart;
begin
end;

procedure TmnServer.DoStop;
begin
end;

procedure TmnServer.Close;
begin
  Stop;
end;

procedure TmnServer.Open;
begin
  Start;
end;

end.

