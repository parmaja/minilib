unit mnServers;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ELSE}
{$ENDIF}

{.$define NoLog}

interface

uses
  Classes, SysUtils,
  mnUtils, mnOpenSSL,
  mnSockets, mnStreams, mnConnections;

const
  cListenerTimeout = 3000;
  cIdleInterval = 1 * 60 * 1000;

type

{ TmnServerSocket }

{
  TmnServerSocket Class is for beginner to play simple example of socket server, it accept one connection only
  If you want multiple connection, use TmnServer
}

  TmnServerSocket = class(TmnSocketStream)
  private
    FContext: TContext;
    FAddress: string;
    FPort: string;
    FListenerSocket: TmnCustomSocket;
    function GetFullAddress: string;
    procedure SetAddress(Value: string);
    procedure SetFullAddress(AValue: string);
    procedure SetPort(Value: string);
  protected
    procedure FreeSocket; override;
    function CreateSocket(out vErr: Integer): TmnCustomSocket; override;
  public
    PrivateKeyFile: string;
    CertificateFile: string;
    constructor Create(const vAddress, vPort: string; vOptions: TmnsoOptions = [soNoDelay]);
    destructor Destroy; override;
    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
    property FullAddress: string read GetFullAddress write SetFullAddress;
  end;

{
   Server and Listener classes
}
{ Server }

  TmnServer = class;
  TmnListener = class;

  { TmnServerConnection }

  TmnServerConnection = class(TmnConnection)
  private
    FRemoteIP: string;
    FStream: TmnConnectionStream;
    function GetListener: TmnListener;
  protected
    function GetConnected: Boolean; override;
    procedure Prepare; override;
    procedure TerminatedSet; override;
    procedure Execute; override;
  public
    constructor Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
    destructor Destroy; override;
    procedure Disconnect; virtual;
    property Stream: TmnConnectionStream read FStream;
    property Listener: TmnListener read GetListener;
    property RemoteIP: string read FRemoteIP;
  end;

  TmnServerConnectionClass = class of TmnServerConnection;

  TmnOnLog = procedure(const S: string) of object;
  TmnOnListenerNotify = procedure(Listener: TmnListener) of object;
  TmnOnListenerAcceptNotify = procedure(Listener: TmnListener; vConnection: TmnServerConnection) of object;

  { TmnListener }

  TmnListener = class(TmnConnections) // thread to watch for incoming requests
  private
    FServer: TmnServer;
    FTimeout: Integer;
    FAttempts: Integer;
    FTries: Integer;
    FSocket: TmnCustomSocket; //Listner socket waiting by call "select"
    FLogMessages: TStringList;
    FOptions: TmnsoOptions;
    FLastCheck: UInt64;
    function GetConnected: Boolean;
    procedure SetOptions(AValue: TmnsoOptions);
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; virtual;
    function CreateConnection(vSocket: TmnCustomSocket): TmnConnection;
    function CreateStream(vSocket: TmnCustomSocket): TmnConnectionStream;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); virtual;

    property LogMessages: TStringList read FLogMessages;

  protected //OpenSSL
    Context: TContext;
    //You can use full path
    FullChain: Boolean;
    PrivateKeyFile: string;
    CertificateFile: string;
    CertificateFileDate: TDateTime;

    procedure Connect;
    procedure Disconnect;
    function Accept: TmnCustomSocket;
    procedure UpdateChanged;
  protected
    procedure PostLogs; //run in main thread by queue
    procedure PostChanged; //run in main thread by queue
    procedure Changed; virtual;

    procedure Prepare; virtual;
    procedure Execute; override;
    procedure Unprepare; virtual;
    procedure DropConnections; virtual;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Connection: TmnConnection); override;
    procedure Remove(Connection: TmnConnection); override;
    // Use this function when you are in a thread do not use Server.Log
    procedure Log(S: string); virtual;
    property Server: TmnServer read FServer;
    property Connected: Boolean read GetConnected;
    property Socket: TmnCustomSocket read FSocket;
    property Options: TmnsoOptions read FOptions write SetOptions;
    //if listener connection down by network it will reconnect again
    property Attempts: Integer read FAttempts write FAttempts;
    //it is ListenerTimeout not ReadTimeOut
    property Timeout: Integer read FTimeout write FTimeout default cListenerTimeout;
    procedure ReloadContext;
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
    FLogging: Boolean;
    FUseSSL: Boolean;
    FIdleTick: UInt64;
    FIdleInterval: Int64;
    procedure SetActive(const Value: Boolean);
    procedure SetAddress(const Value: string);
    procedure SetPort(const Value: string);
    function GetCount: Integer;
    function GetConnected: Boolean;
  protected
    IsDestroying: Boolean;
    IsStopping: Boolean;
    function DoCreateListener: TmnListener; virtual;
    function CreateListener: TmnListener; virtual;
    procedure DoLog(const S: string); virtual;
    procedure DoChanged(vListener: TmnListener); virtual;
    procedure DoAccepted(vListener: TmnListener; vConnection: TmnServerConnection); virtual;
    procedure DoIdle; virtual; //no connection found after time out:)
    procedure DoBeforeOpen; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoAfterClose; virtual;
    procedure DoStart; virtual;
    procedure DoStop; virtual;
    procedure DoCheckSynchronize;
    //Idle is in Listener thread not in main thread
    procedure Idle(vListener: TmnListener);
  public
    FullChain: Boolean;
    PrivateKeyFile: string;
    CertificateFile: string;

    constructor Create;
    procedure BeforeDestruction; override;
    destructor Destroy; override;
    //Server.Log This called from outside of any threads, i mean you should be in the main thread to call it, if not use Listener.Log
    procedure Log(const S: string);

    procedure Start;
    procedure Wait;
    procedure Restart;
    procedure Disconnect; //* Disconnect stop listener connection
    procedure Stop;

    property Listener: TmnListener read FListener;
    property Count: Integer read GetCount;

    property Port: string read FPort write SetPort;
    property Address: string read FAddress write SetAddress;
    property UseSSL: Boolean read FUseSSL write FUseSSL;

    property Active: boolean read FActive write SetActive default False;
    property Logging: Boolean read FLogging write FLogging default false;
    property Connected: Boolean read GetConnected;
    property IdleInterval: Int64 read FIdleInterval write FIdleInterval default cIdleInterval;
  end;

//TODO move to another unit SimpleClientServer
{--------------------------------------------------------------------------------------------------
        Simple Server
}
  TmnServerExecuteProc = procedure(Stream: TmnConnectionStream);

  { TmnSimpleServerConnection }

  TmnSimpleServerConnection = class(TmnServerConnection)
  protected
    procedure Process; override;
  end;

  { TmnSimpleListener }

  TmnSimpleListener = class(TmnListener)
  protected
    ExecuteProc: TmnServerExecuteProc; //Referenced to Server proc
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
  end;

  { TmnSimpleServer }

  TmnSimpleServer = class(TmnServer)
  protected
    ExecuteProc: TmnServerExecuteProc;
    function DoCreateListener: TmnListener; override;
  public
    constructor Create(AutoStart: Boolean; AAddress: string; APort: String; AReadTimeOut: Integer = -1);
    destructor Destroy; override;
  end;

//--------------------------------------------------------------------------------------------------

  { TmnEventServer }

  TmnEventServer = class(TmnServer)
  private
    FOnBeforeOpen: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnAfterClose: TNotifyEvent;
    FOnLog: TmnOnLog;
    FOnChanged: TmnOnListenerNotify;
    FOnAccepted: TmnOnListenerAcceptNotify;
  protected
    procedure DoLog(const S: string); override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoAccepted(vListener: TmnListener; vConnection: TmnServerConnection); override;
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
    property OnAccepted: TmnOnListenerAcceptNotify read FOnAccepted write FOnAccepted;
  end;

implementation

{ TmnSimpleServerConnection }

procedure TmnSimpleServerConnection.Process;
begin
  inherited Process;
  (Listener as TmnSimpleListener).ExecuteProc(Stream);
end;

{ TmnSimpleListener }

function TmnSimpleListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnSimpleServerConnection.Create(Self, vStream);
end;

{ TmnSimpleServer }

function TmnSimpleServer.DoCreateListener: TmnListener;
begin
  Result := TmnSimpleListener.Create;
  (Result as TmnSimpleListener).ExecuteProc := ExecuteProc;
end;

constructor TmnSimpleServer.Create(AutoStart: Boolean; AAddress: string; APort: String; AReadTimeOut: Integer);
begin
  inherited Create;
  Address := AAddress;
  Port := APort;
  if AutoStart then
    Start;

  //SimpleServers.Add(Self); //TODO
end;

destructor TmnSimpleServer.Destroy;
begin
  //SimpleServers.Remove(Self); //TODO
  inherited Destroy;
end;

{ TmnEventServer }

procedure TmnEventServer.DoLog(const S: string);
begin
  inherited;
  if not (IsDestroying) then
  begin
    if Assigned(FOnLog) then
      FOnLog(S);
  end;
end;

procedure TmnEventServer.DoChanged(vListener: TmnListener);
begin
  inherited;
  if not (IsDestroying) then
  begin
    if Assigned(FOnChanged) then
      FOnChanged(vListener);
  end;
end;

procedure TmnEventServer.DoAccepted(vListener: TmnListener; vConnection: TmnServerConnection);
begin
  inherited;
  if not (IsDestroying) then
  begin
    if Assigned(FOnAccepted) then
      FOnAccepted(vListener, vConnection);
  end;
end;

procedure TmnEventServer.DoBeforeOpen;
begin
  inherited;
  if not (IsDestroying) then
  begin
    if Assigned(FOnBeforeOpen) then
      FOnBeforeOpen(Self);
  end;
end;

procedure TmnEventServer.DoAfterOpen;
begin
  inherited;
  if not (IsDestroying) then
  begin
    if Assigned(FOnAfterOpen) then
      FOnAfterOpen(Self);
  end;
end;

procedure TmnEventServer.DoBeforeClose;
begin
  inherited;
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

constructor TmnServerConnection.Create(vOwner: TmnConnections; vStream: TmnConnectionStream);
begin
  inherited Create(vOwner);
  FStream := vStream;
  FreeOnTerminate := True;
end;

destructor TmnServerConnection.Destroy;
begin
  Disconnect;
  FreeAndNil(FStream);
  inherited;
end;

function TmnServerConnection.GetListener: TmnListener;
begin
  Result := Owner as TmnListener;
end;

function TmnServerConnection.GetConnected: Boolean;
begin
  Result := (FStream <> nil) and FStream.Connected;
end;

procedure TmnServerConnection.Disconnect;
begin
  if (FStream <> nil) and (FStream.Connected) then
    FStream.Disconnect;
end;

procedure TmnServerConnection.Prepare;
begin
  FStream.Prepare;
  inherited Prepare;
end;

procedure TmnServerConnection.TerminatedSet;
begin
  Disconnect;
  inherited;
end;

procedure TmnServerConnection.Execute;
begin
  inherited Execute;
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

procedure TmnServer.DoCheckSynchronize;
begin
  CheckSynchronize
end;

procedure TmnServer.DoAccepted(vListener: TmnListener; vConnection: TmnServerConnection);
begin
end;

procedure TmnServer.DoBeforeClose;
begin
end;

function TmnServer.GetConnected: Boolean;
begin
  Result := (FListener<>nil) and (FListener.Connected);
end;

function TmnServer.GetCount: Integer;
begin
  if Listener <> nil then
  begin
    Listener.Enter;
    try
      Result := Listener.Count;
    finally
      Listener.Leave;
    end;
  end
  else
    Result := 0;
end;

procedure TmnServer.Idle(vListener: TmnListener);
begin
  if (TThread.GetTickCount64-FIdleTick)>IdleInterval then
  begin
    FIdleTick := TThread.GetTickCount64;
    vListener.Queue(DoIdle);
  end;
end;

procedure TmnServer.DoAfterOpen;
begin
end;

{ TmnListener }

procedure TmnListener.Add(Connection: TmnConnection);
begin
  inherited;
  //Log('Add: #' + IntToStr(Connection.ID));
  Changed;
end;

procedure TmnListener.Changed;
begin
  Queue(nil, PostChanged);
end;

procedure TmnListener.Connect;
var
  aErr: Integer;
begin
  if not Terminated then
  begin
    WallSocket.Bind(FOptions, Timeout, FPort, FAddress, FSocket, aErr);
    if Connected then
    begin
      Socket.Prepare;
      Socket.Listen;
    end
    else
      raise EmnStreamException.CreateFmt('Bind fail [%d]', [aErr]);
  end;
end;

constructor TmnListener.Create;
begin
  inherited Create;
  FreeOnTerminate := False;
  FLogMessages := TStringList.Create;
  FAttempts := 0;
  FTimeout := cListenerTimeout;
end;

function TmnListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnServerConnection.Create(Self, vStream);
end;

function TmnListener.CreateConnection(vSocket: TmnCustomSocket): TmnConnection;
begin
  Result := DoCreateConnection(CreateStream(vSocket));
  Result.ID := NewID;
end;

function TmnListener.CreateStream(vSocket: TmnCustomSocket): TmnConnectionStream;
begin
  Result := nil;
  DoCreateStream(Result, vSocket);
end;

procedure TmnListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  Result := TmnSocketStream.Create(vSocket);
  //TmnSocketStream(Result).Options := TmnSocketStream(Result).Options + Options;
  TmnSocketStream(Result).Options := Options;
  if FServer.UseSSL then
    TmnSocketStream(Result).Options := TmnSocketStream(Result).Options + [soSSL]; //TODO i think it should in listener options too
end;

function TmnListener.Accept: TmnCustomSocket;
begin
  Result := Socket.Accept(Options, Timeout);
end;

procedure TmnListener.PostLogs;
var
  b: Boolean;
  s: String;
begin
  if FServer <> nil then
  repeat
    Enter;
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
      Leave;
    end;
    if b then
      FServer.DoLog(s);
  until not b;
end;

destructor TmnListener.Destroy;
begin
  FreeAndNil(FLogMessages);
  inherited;
end;

procedure TmnListener.Disconnect;
begin
  if Connected then
    Socket.Close;
  FreeAndNil(FSocket);
end;

procedure TmnListener.Execute;
var
  aSocket: TmnCustomSocket;
  aConnection: TmnServerConnection;
begin

  try
    FTries := FAttempts;
    Connect;
    if Connected then
      Changed;
    while Connected and not Terminated do
    begin
      try
        if (Socket.Select(Timeout, slRead) = erSuccess) and not Terminated then
        begin
          aSocket := Accept;
          if aSocket <> nil then
          begin
            UpdateChanged;
            aSocket.Context := Context;
            //aSocket.Prepare;
          end
        end
        else
        begin
          aSocket := nil;
        end;

        {Enter; //todo remove it;
        try
          //Just a stop to finish some procedures outside, or make terminated get new value before continue
        finally
          Leave;
        end;}

        //Yield;//todo test:

        if not Terminated then
        begin
          //w.Start;
          if Connected and (Server <> nil) then
            Server.Idle(Self);

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
                aConnection := CreateConnection(aSocket) as TmnServerConnection;
                aConnection.FRemoteIP := aSocket.GetRemoteAddress;
                //aConnection.Stream.ReadTimeout ////hmmmm
                //aConnection.Prepare
                if FServer <> nil then
                  FServer.DoAccepted(Self, aConnection);
              finally
                Leave;
              end;
              //Log('Starting: ' + aConnection.ClassName);
              aConnection.Start; //moved here need some test
            finally
            end;
          end;
          {w.Stop;
          var i := w.ElapsedMilliseconds;
          if i>1 then
            OutputDebugString(PChar('next select' + i.ToString));
          w.Reset;}
        end;
      finally
      end;
    end;
    DropConnections;
    Enter;
    try
      Disconnect;
    finally
      Leave;
    end;
    Unprepare;
    Changed;
  except
    on E: Exception  do
    begin
      Log(E.Message);
      raise;
    end;
  end;
end;

procedure TmnListener.Unprepare;
begin
  if soSSL in Options then
    FreeAndNil(Context);
end;

procedure TmnListener.UpdateChanged;
const
  sChangeInterval = 10 * 6000; //60s
var
  aDate: TDateTime;

begin
  if (GetTickCount64-FLastCheck)>sChangeInterval then
  begin
    FLastCheck := GetTickCount64;
    FileAge(CertificateFile, aDate, True);
    if CertificateFileDate<>aDate then
    begin
      ReloadContext;
    end;
  end;
end;

function TmnListener.GetConnected: Boolean;
begin
  Result := (FSocket <> nil) and FSocket.Active;
end;

procedure TmnListener.SetOptions(AValue: TmnsoOptions);
begin
  if FOptions =AValue then Exit;
  FOptions :=AValue;
  //TODO check if not connected
end;

procedure TmnListener.Log(S: string);
begin
  if Server.Logging then
  begin
    Enter;
    try
      LogMessages.Add(S);
    finally
      Leave;
    end;
    Queue(nil, PostLogs); //nil = queue not linked with this thread "RemoveQueuedEvents"
  end;
end;

procedure TmnListener.Prepare;
begin
  if soSSL in Options then
  begin
    Context := TContext.Create(TTLS_SSLServerMethod, [coNoCompressing, coServer]);
    ReloadContext;
  end;
end;

procedure TmnListener.ReloadContext;
begin
  if soSSL in Options then
  begin
    FileAge(CertificateFile, CertificateFileDate, True);
    Context.LoadPrivateKeyFile(UTF8Encode(PrivateKeyFile));
    Context.LoadCertFile(UTF8Encode(CertificateFile), FullChain);
    Context.CheckPrivateKey; //do not use this
    //Context.SetVerifyNone;
  end;
end;

procedure TmnListener.Remove(Connection: TmnConnection);
begin
  //Log('Removed: #' + IntToStr(Connection.ID));
  inherited;
  Changed;
end;

procedure TmnListener.DropConnections;
var
  i: Integer;
  aConnection: TmnConnection;
begin
  Enter;
  try
    for i := 0 to List.Count - 1 do
    begin
      List[i].FreeOnTerminate := False; //I will kill you
//      List[i].Terminate;
    end;
  finally
    Leave;
  end;

  try
    while List.Count > 0 do
    begin
      aConnection := List[0];
      aConnection.Terminate;
      aConnection.WaitFor;
      //Log('Connection Stopped #' + IntToStr(aConnection.ID));
      aConnection.Free;
      List.Delete(0);
    end;
  finally
  end;
end;

procedure TmnListener.TerminatedSet;
begin
  inherited;
  Enter;
  try
    if Socket <> nil then
    begin
      Socket.Shutdown([sdReceive, sdSend]);//stop the accept from waiting

      //in linux close will cause lag on select
      //Shutdown worked in windows
      //need check on mac and ios

      //Socket.Close();
      Sleep(1); //for breathing signals in system os

    end;
    //Log('before finally: TmnListener.TerminatedSet');
  finally
    //Log('finally: TmnListener.TerminatedSet');
    Leave;
  end;
end;

{ TmnServer }

constructor TmnServer.Create;
begin
  inherited Create;
  FAddress := '0.0.0.0';
  //FAddress := '';
  PrivateKeyFile := 'privatekey.pem';
  CertificateFile := 'certificate.pem';
  IdleInterval := cIdleInterval;
  FIdleTick := TThread.GetTickCount64;
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

procedure TmnServer.Disconnect;
begin
  if Listener <> nil then
    Listener.Disconnect;
end;

procedure TmnServer.Log(const S: string);
begin
  DoLog(S);
end;

procedure TmnServer.Restart;
begin
  Stop;
  Start;
end;

procedure TmnServer.Start;
begin
  if (FListener = nil) then // if its already active, dont start again
  begin
    try
      if UseSSL then
        InitOpenSSL;
      DoBeforeOpen;
      FListener := CreateListener;
      try
        FListener.FServer := Self;
        FListener.FPort := FPort;
        FListener.FAddress := FAddress;
        if UseSSL then
          FListener.FOptions := FListener.FOptions + [soSSL];
        FListener.FullChain := FullChain;
        FListener.PrivateKeyFile := PrivateKeyFile;
        FListener.CertificateFile := CertificateFile;

        FListener.Prepare;
        //FListener.Timeout := 500;
        DoStart;
        FListener.Start;
        Log('Server starting at port: ' + Port);
        FActive := True;
      except
        FreeAndNil(FListener); //case error because delphi call terminateset on free
        raise;
      end;
      DoAfterOpen;
    finally
    end;
  end;
end;

procedure TmnListener.PostChanged;
begin
  if FServer <> nil then
    FServer.DoChanged(Self);
end;

procedure TmnServer.Stop;
begin
  if (FListener <> nil) then
  begin
    if not IsStopping then
    begin
      IsStopping := True;
      DoBeforeClose;
      FListener.Terminate;
      FListener.WaitFor;
      Log('Listener Stopped');

      //to process all queues
      //in case of service ThreadID<>MainThreadID :)
      TThread.Synchronize(nil, DoCheckSynchronize);

      FreeAndNil(FListener);
      FActive := False;
      DoAfterClose;
      IsStopping := False;
      Log('Server stopped');
    end;
  end;
  DoStop;
  //TThread.Synchronize(nil, DoCheckSynchronize);
end;

procedure TmnServer.Wait;
begin
  if Listener <> nil then
    Listener.WaitFor;
end;

function TmnServer.DoCreateListener: TmnListener;
begin
  Result := TmnListener.Create;
end;

procedure TmnServer.DoIdle;
begin
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

{ TmnServerSocket }

procedure TmnServerSocket.SetAddress(Value: string);
begin
  if FAddress = Value then Exit;
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FAddress := Value;
end;

function TmnServerSocket.GetFullAddress: string;
begin
  Result := FAddress + ':' + FPort;
end;

procedure TmnServerSocket.SetFullAddress(AValue: string);
var
  aPort: string;
begin
  FAddress := AValue;
  aPort := SubStr(FAddress, ':', 1);
  if aPort <> '' then
  begin
    FPort := aPort;
    FAddress := SubStr(FAddress, ':', 0)
  end;
end;

procedure TmnServerSocket.SetPort(Value: string);
begin
  if FPort =Value then Exit;
  if Connected then
    raise EmnException.Create('Can not change Port value when active');
  FPort := Value;
end;

procedure TmnServerSocket.FreeSocket;
begin
  inherited;
  FreeAndNil(FListenerSocket);
  FreeAndNil(FContext);
end;

function TmnServerSocket.CreateSocket(out vErr: Integer): TmnCustomSocket;
begin
  if soSSL in Options then
  begin
    FContext := TContext.Create(TTLS_SSLServerMethod, [coServer, coNoCompressing]);
    FContext.LoadCertFile(CertificateFile);
    FContext.LoadPrivateKeyFile(PrivateKeyFile);
    FContext.CheckPrivateKey; //do not use this
    //Context.SetVerifyNone;
  end;

  WallSocket.Bind(Options, ReadTimeout, Port, Address, FListenerSocket, vErr);
  if FListenerSocket <> nil then
  begin
//    FListenerSocket.Context := FContext;
    FListenerSocket.Prepare;
    FListenerSocket.Listen;
    Result := FListenerSocket.Accept(Options, ReadTimeout);
    if Result = nil then
      FreeAndNil(FListenerSocket)
    else
    begin
      Result.Context := FContext;
      //Result.Prepare; connect will do that
    end;
  end
  else
    Result := nil;
end;

constructor TmnServerSocket.Create(const vAddress, vPort: string; vOptions: TmnsoOptions);
begin
  inherited Create;
  FAddress := vAddress;
  FPort := SubStr(FAddress, ':', 1);
  if FPort <> '' then
    FAddress := SubStr(FAddress, ':', 0)
  else
    FPort := vPort;
  Options := vOptions;
end;

destructor TmnServerSocket.Destroy;
begin
  inherited Destroy;
end;

end.

