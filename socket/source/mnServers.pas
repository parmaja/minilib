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
  mnSockets,
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
    constructor Create(vOwner: TmnConnections; Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    property Listener: TmnListener read GetListener;
  end;

  TmnServerConnectionClass = class of TmnServerConnection;

  TmnOnLog = procedure(const S: string) of object;
  TmnOnListenerNotify = procedure(Listener: TmnListener) of object;

  { TmnListener }

  TmnListener = class(TmnConnections) // thread to watch for incoming requests
  private
    FTimeout: Integer;
    FAttempts: Integer;
    FTries: Integer;
    FSocket: TmnCustomSocket; //Listner socket waiting by call "select"
    FServer: TmnServer;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
  protected
    FOptions: TmnsoOptions;
    FMessage: string;
    procedure SyncLog;
    procedure SyncChanged;
    procedure DropConnections; virtual;
    procedure LogMessage(S: string); virtual;
    procedure Changed; virtual;

    procedure Prepare; virtual;
    procedure Execute; override;
    procedure Remove(Connection: TmnServerConnection); virtual;
    procedure Add(Connection: TmnServerConnection); virtual;
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop; override;
    procedure Log(S: string); virtual;
    property Server: TmnServer read FServer;
    property Connected: Boolean read GetConnected;
    property Socket: TmnCustomSocket read FSocket;
    property Options: TmnsoOptions read FOptions;
    //if listener connection down by network it will reconnect again
    property Attempts: Integer read FAttempts write FAttempts;
    property Timeout: Integer read FTimeout write FTimeout default -1;
  end;

  {**
    mnServer in the future can manage more than listener
    So put shared info into the Server
  *}
  { TmnServer }

  TmnServer = class(TObject)
  private
    FActive: Boolean;
    FPort: string;
    FAddress: string;
    FListener: TmnListener;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
    function GetCount: Integer;
  protected
    IsDestroying: Boolean;
    function DoCreateListener: TmnListener; virtual;
    function CreateListener: TmnListener; virtual;
    procedure DoLog(const S: string); virtual;
    procedure DoChanged(vListener: TmnListener); virtual;
    procedure DoAccepted(vListener: TmnListener); virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoAfterClose; virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    procedure Open;
    procedure Close;
    property Listener: TmnListener read FListener;
    property Count: Integer read GetCount;

    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;

    property Active: boolean read FActive write SetActive default False;

  end;

  { TmnEventServer }

  TmnEventServer = class(TmnServer)
  private
    FOnBeforeOpen: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnListenerNotify;
    FOnAccepted: TmnOnListenerNotify;
  protected
    procedure DoLog(const S: string); override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoAccepted(vListener: TmnListener); override;
    procedure DoBeforeOpen; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeClose; override;
    procedure DoAfterClose; override;
  published
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose write FOnBeforeClose;
    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnListenerNotify read FOnChanged write FOnChanged;
    property OnAccepted: TmnOnListenerNotify read FOnAccepted write FOnAccepted;
  end;

implementation

{ TmnEventServer }

procedure TmnEventServer.DoLog(const S: string);
begin
  inherited DoLog(S);
  if not (IsDestroying) then
  begin
    if Assigned(FOnLog) then
      FOnLog(S);
  end;
end;

procedure TmnEventServer.DoChanged(vListener: TmnListener);
begin
  inherited DoChanged(vListener);
  if not (IsDestroying) then
  begin
    if Assigned(FOnChanged) then
      FOnChanged(vListener);
  end;
end;

procedure TmnEventServer.DoAccepted(vListener: TmnListener);
begin
  inherited DoAccepted(vListener);
  if not (IsDestroying) then
  begin
    if Assigned(FOnAccepted) then
      FOnAccepted(vListener);
  end;
end;

procedure TmnEventServer.DoBeforeOpen;
begin
  inherited DoBeforeOpen;
  if not (IsDestroying) then
  begin
    if Assigned(FOnBeforeOpen) then
      FOnBeforeOpen(Self);
  end;
end;

procedure TmnEventServer.DoAfterOpen;
begin
  inherited DoAfterOpen;
  if not (IsDestroying) then
  begin
    if Assigned(FOnAfterOpen) then
      FOnAfterOpen(Self);
  end;
end;

procedure TmnEventServer.DoBeforeClose;
begin
  inherited DoBeforeClose;
  if not (IsDestroying) then
  begin
    if Assigned(FOnBeforeClose) then
      FOnBeforeClose(Self);
  end;
end;

procedure TmnEventServer.DoAfterClose;
begin
  inherited DoAfterClose;
  if not (IsDestroying) then
  begin
    if Assigned(FOnAfterClose) then
      FOnAfterClose(Self);
  end;
end;

{ TmnServerConnection }

constructor TmnServerConnection.Create(vOwner: TmnConnections; Socket: TmnCustomSocket);
begin
  inherited;
  FreeOnTerminate := True;
  if Listener <> nil then
    Listener.Add(Self);
end;

destructor TmnServerConnection.Destroy;
begin
  inherited;
end;

function TmnServerConnection.GetListener: TmnListener;
begin
  Result := Owner as TmnListener;
end;

procedure TmnServerConnection.Execute;
begin
  inherited;
  if Listener <> nil then
    Listener.Remove(Self);
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
end;

procedure TmnServer.DoAccepted(vListener: TmnListener);
begin
end;

procedure TmnServer.DoBeforeClose;
begin
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
end;

{ TmnListener }

procedure TmnListener.Add(Connection: TmnServerConnection);
begin
  Enter;
  try
    List.Add(Connection);
    Changed;
  finally
    Leave;
  end;
end;

procedure TmnListener.Changed;
begin
  {$ifndef NoLog}
  {$ifdef Synchronize}
  Synchronize(Self, SyncChanged);
  {$else}
  Enter;
  try
    SyncChanged;
  finally
    Leave;
  end;
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
  FAttempts := 0;
  FTimeout := -1;
end;

function TmnListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TmnServerConnection.Create(Self, vSocket);
end;

destructor TmnListener.Destroy;
begin
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
  FTries := FAttempts;
  Connect;
  while Connected and not Terminated do
  begin
    try
      if (Socket.Select(Timeout, slRead) = erNone) and not Terminated then
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
          //only if we need retry mode, attempt to connect new socket, for 3 times as example, that if socket disconnected for wiered reason
          if (not Connected) and (FAttempts > 0) and (FTries > 0) then
          begin
            FTries := FTries - 1;
            Connect;
          end; //else we will not continue look at "while" conditions
        end
        else
        begin
          try
            Enter; //because we add connection to a thread list
            try
              if FServer <> nil then
                FServer.DoAccepted(Self);
              aConnection := CreateConnection(aSocket);
            finally
              Leave;
            end;
            aConnection.Start; //moved here need some test
          finally
          end;
        end;
      end;
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

procedure TmnListener.Log(S: string);
begin
  {$ifndef NoLog}
  LogMessage(S);

  {$ifdef Synchronize}
  FMessage := S;
  Synchronize(Self, SyncLog);
  {$else}
  Enter;
  try
    FMessage := S;
    SyncLog;
  finally
    Leave;
  end;
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
      List.Remove(Connection);
  finally
    Leave;
  end;
  Changed;
end;

procedure TmnListener.DropConnections;
var
  i: Integer;
begin
  Enter;
  try
    for i := 0 to List.Count - 1 do
    begin
      List[i].FreeOnTerminate := False;
      List[i].Stop;
    end;
  finally
    Leave;
  end;
  try
    while List.Count > 0 do
    begin
      List[0].WaitFor;
      List[0].Free;
      List.Delete(0);
    end;
  finally
  end;
end;

procedure TmnListener.LogMessage(S: string);
begin
end;

{ TmnServer }

constructor TmnServer.Create;
begin
  inherited Create;
  FAddress := '0.0.0.0';
end;

procedure TmnServer.BeforeDestruction;
begin
  IsDestroying := True;
  inherited BeforeDestruction;
end;

destructor TmnServer.Destroy;
begin
  Stop;
  inherited;
end;

procedure TmnServer.Start;
begin
  if (FListener = nil) then // if its already active, dont start again
  begin
    try
      DoStart;
      DoBeforeOpen;
      try
        FListener := CreateListener;
        FListener.FServer := Self;
        FListener.FPort := FPort;
        FListener.FAddress := FAddress;
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
  if FServer <> nil then
    FServer.DoLog(FMessage);
  FMessage :='';
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

function TmnServer.DoCreateListener: TmnListener;
begin
  Result := TmnListener.Create;
end;

function TmnServer.CreateListener: TmnListener;
begin
  Result := DoCreateListener;
end;

procedure TmnServer.DoLog(const S: string);
begin
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
end;

procedure TmnServer.DoAfterClose;
begin
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

