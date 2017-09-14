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
  {$ifndef FPC}
  Types,
  {$endif}
  SysUtils,
  mnSockets,
  mnSocketStreams, 
  mnConnections;

type

{ TmnClient }

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

  TmnClientStream = class(TmnClientSocketStream)
  end deprecated;

  { TmnClientConnection }

  TmnClientConnection = class(TmnConnection) //this child object in Caller
  private
    function GetCaller: TmnCaller;
  protected
    procedure Execute; override;
  public
    constructor Create(vConnector: TmnConnections; vSocket: TmnCustomSocket); override;
    destructor Destroy; override;
    property Caller: TmnCaller read GetCaller;
  end;

  TmnClientConnectionClass = class of TmnClientConnection;

  TmnOnLog = procedure(Connection: TmnConnection; const S: string) of object;
  TmnOnCallerNotify = procedure(Caller: TmnCaller) of object;

  { TmnCaller }

  TmnCaller = class(TmnConnections) // thread pooling to watch for outgoing requests
  private
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnCallerNotify;
    procedure Connect;
    procedure Disconnect;
    function GetConnected: Boolean;
  protected
    FOptions: TmnsoOptions;
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
    {$if CompilerVersion < 18} // Delphi 2007 or later {$ifend}
    procedure Start;
    {$endif}
    {$endif}
    procedure Stop; override;
    procedure Log(Connection: TmnConnection; S: string);
    function AddConnection(vPort: string; vAddress: string): TmnClientConnection;
    property Connected: Boolean read GetConnected;

    property OnLog: TmnOnLog read FOnLog write FOnLog;
    property OnChanged: TmnOnCallerNotify read FOnChanged write FOnChanged;
  end;

implementation

{ TmnClientConnection }

constructor TmnClientConnection.Create(vConnector: TmnConnections; vSocket: TmnCustomSocket);
begin
  inherited;
  FreeOnTerminate := True;
  if Caller <> nil then
  begin
    Caller.Add(Self);
  end;
end;

destructor TmnClientConnection.Destroy;
begin
  inherited;
end;

procedure TmnClientConnection.Execute;
begin
  inherited;
  if Caller <> nil then
  begin
    Caller.Remove(Self);
  end;
end;

function TmnClientConnection.GetCaller: TmnCaller;
begin
  Result := Connector as TmnCaller;
end;

{ TmnCaller }

procedure TmnCaller.Add(Connection: TmnClientConnection);
begin
  Enter;
  try
    List.Add(Connection);
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
  FOptions := [soNoDelay]; //you can use soKeepAlive
end;

function TmnCaller.CreateConnection(Socket: TmnCustomSocket): TmnClientConnection;
begin
  Result := TmnClientConnection.Create(Self, Socket);
end;

destructor TmnCaller.Destroy;
begin
  inherited;
end;

procedure TmnCaller.Disconnect;
begin
  if Connected then
  begin
  end;
end;

procedure TmnCaller.Execute;
begin
  Connect;
  Shutdown;
  Disconnect;
end;

function TmnCaller.GetConnected: Boolean;
begin
  Result := Terminated;
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
      List.Remove(Connection);
    Changed;
  finally
    Leave;
  end;
end;

{$ifndef FPC} //already found in FPC 2.4.4
{$if CompilerVersion < 18} // Delphi 2007 or later {$ifend}
procedure TmnCaller.Start;
begin
  Resume;
end;
{$endif}
{$endif}

procedure TmnCaller.Stop;
begin
  Terminate;
end;

procedure TmnCaller.Shutdown;
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
  Result.Start;
end;

{ TmnClientSocketStream }

function TmnClientSocketStream.CreateSocket: TmnCustomSocket;
begin
  Result := WallSocket.Connect([soReuseAddr, soNoDelay], Port, Address)
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
