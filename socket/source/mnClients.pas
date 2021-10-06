unit mnClients;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  Classes, {$ifndef FPC} Types, {$endif} SysUtils, SyncObjs,
  mnSockets, mnStreams, mnConnections;

type

{ TmnClient }

  TmnClients = class;

  { TmnClientSocketStream }

  TmnClientSocket = class(TmnSocketStream)
  private
    FAddress: string;
    FPort: string;
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
  protected
    function CreateSocket(out vErr: Integer): TmnCustomSocket; override;
  public
    constructor Create(const vAddress: string = ''; vPort: string = ''; vOptions: TmnsoOptions = [soNoDelay]);
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
  end;

  TmnClientSocketStream = TmnClientSocket;

  { TmnClientConnection }

  TmnClientConnection = class(TmnConnection) //this child object in Clients
  private
    function GetOwner: TmnClients;
  protected
  public
    constructor Create(vOwner: TmnConnections);
    destructor Destroy; override;
    property Owner: TmnClients read GetOwner;
  end;

  { TmnLogClient }

  TmnClient = class(TmnClientConnection) //with log pool, same as Listener
  private
    FLock: TCriticalSection; //only if have no owner
    FLogMessages: TStringList;
    function GetLock: TCriticalSection;
  protected
    procedure DoLog(s: string); virtual; //Outside of thread
    procedure PostLogs; //to Queue
    procedure Log(s: string);
    property LogMessages: TStringList read FLogMessages;
  public
    constructor Create(vOwner: TmnConnections);
    destructor Destroy; override;
    property Lock: TCriticalSection read GetLock;
  end;

  TmnJobClient = class(TmnClient)
  public
    constructor Create(vOwner: TmnConnections; JobObject: TObject);
  end;

  TmnClientConnectionClass = class of TmnClientConnection;

  TmnOnLog = procedure(Connection: TmnConnection; const S: string) of object;
  TmnOnCallerNotify = procedure(Caller: TmnClients) of object;

  { TmnClients }

  TmnClients = class(TmnConnections) // thread pooling to collect outgoing clients threads
  private
    FLock: TCriticalSection;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnCallerNotify;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
  protected
    function CreateConnection: TmnConnection; virtual; abstract;
  protected
    FOptions: TmnsoOptions;
    procedure Shutdown;
    procedure Execute; override;
    procedure Changed;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Remove(Connection: TmnConnection); override;
    procedure Add(Connection: TmnConnection); override;
    procedure Stop; override;
    procedure Log(Connection: TmnConnection; S: string);
    function AddConnection(vPort: string; vAddress: string): TmnClientConnection;
    property Connected: Boolean read GetConnected;

    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnCallerNotify read FOnChanged write FOnChanged;
    property Lock: TCriticalSection read FLock;
  end;

implementation

{ TmnClient }

function TmnClient.GetLock: TCriticalSection;
begin
  if FLock <> nil then
    Result := FLock
  else
    Result := Owner.FLock;
end;

procedure TmnClient.DoLog(s: string);
begin
end;

procedure TmnClient.PostLogs;
var
  b: Boolean;
  s: String;
begin
  repeat
    Lock.Enter;
    try
      b := LogMessages.Count > 0;
      if b then
      begin
        s := LogMessages[0];
        LogMessages.Delete(0);
      end
      else
        s := '';
    finally
      Lock.Leave;
    end;
    if b then
      DoLog(s);
  until not b;
end;

procedure TmnClient.Log(s: string);
begin
  Lock.Enter;
  try
    LogMessages.Add(S);
  finally
    Lock.Leave;
  end;
  Queue(PostLogs);
end;

constructor TmnClient.Create(vOwner: TmnConnections);
begin
  inherited;
  FLogMessages := TStringList.Create;
  if vOwner = nil then  // no owner we will make our Lock
    FLock := TCriticalSection.Create;
end;

destructor TmnClient.Destroy;
begin
  FreeAndNil(FLogMessages);
  FreeAndNil(FLock);
  inherited;
end;

{ TmnClientConnection }

constructor TmnClientConnection.Create(vOwner: TmnConnections);
begin
  inherited;
  FreeOnTerminate := True;
  if Owner <> nil then
    Owner.Add(Self);
end;

destructor TmnClientConnection.Destroy;
begin
  if Owner <> nil then
    Owner.Remove(Self);
  inherited;
end;

function TmnClientConnection.GetOwner: TmnClients;
begin
  Result := (inherited Owner) as TmnClients;
end;

{ TmnClients }

procedure TmnClients.Add(Connection: TmnConnection);
begin
  inherited;
  Changed;
end;

procedure TmnClients.Changed;
begin
  if Assigned(FOnChanged) then
  begin
    FOnChanged(Self);
  end;
end;

procedure TmnClients.Connect;
begin
end;

constructor TmnClients.Create;
begin
  inherited;
  FLock := TCriticalSection.Create; //only if have no owner
  //FOptions := [soNoDelay]; //you can use soKeepAlive
  FOptions := [];
end;

destructor TmnClients.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

procedure TmnClients.Disconnect;
begin
  if Connected then
  begin
  end;
end;

procedure TmnClients.Execute;
begin
  Connect;
  Shutdown;
  Disconnect;
end;

function TmnClients.GetConnected: Boolean;
begin
  Result := not Terminated;
end;

procedure TmnClients.Log(Connection: TmnConnection; S: string);
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

procedure TmnClients.Remove(Connection: TmnConnection);
begin
  if Connection.FreeOnTerminate then
    inherited;
end;

procedure TmnClients.Stop;
begin
  Terminate;
end;

procedure TmnClients.Shutdown;
var
  i: Integer;
begin
  Enter;
  try
    for i := 0 to List.Count - 1 do
    begin
      List[i].FreeOnTerminate := False;
      List[i].Terminate;
    end;
  finally
    Leave;
  end;
  try
    while List.Count > 0 do
    begin
//      WaitForSingleObject(List[0].Handle, INFINITE);
      List[0].WaitFor;
      List[0].Free;
      List.Delete(0);
    end;
    Changed;
  finally
  end;
end;

function TmnClients.AddConnection(vPort: string; vAddress: string): TmnClientConnection;
begin
  if vPort = '' then
    vPort := FPort;
  if vAddress = '' then
    vAddress := FAddress;
  Result := CreateConnection as TmnClientConnection;
  Result.Start;
end;

{ TmnClientSocket }

function TmnClientSocket.CreateSocket(out vErr: Integer): TmnCustomSocket;
begin
  WallSocket.Connect(Options, ConnectTimeout, ReadTimeout, Port, Address, Result, vErr);
end;

constructor TmnClientSocket.Create(const vAddress: string; vPort: string; vOptions: TmnsoOptions);
begin
  inherited Create;
  FAddress := vAddress;
  FPort := vPort;
  Options := vOptions;
end;

procedure TmnClientSocket.SetAddress(const Value: string);
begin
  if FAddress = Value then Exit;
  if Connected then
    raise EmnException.Create('Can not change Address value when active');
  FAddress := Value;
end;

procedure TmnClientSocket.SetPort(const Value: string);
begin
  if FPort =Value then Exit;
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

{ TmnJobClient }

constructor TmnJobClient.Create(vOwner: TmnConnections; JobObject: TObject);
begin
end;

end.
