unit dbgpServers;
{$mode objfpc}{$H+}
{**
 * Mini Edit
 *
 * @license    GPL 2 (http://www.gnu.org/licenses/gpl.html)
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{

}

{$ifdef WINDOWS}
{.$DEFINE SAVELOG}
{$endif}

interface

uses
  SysUtils, StrUtils, Classes, Contnrs, Dialogs, Variants,
  mnSockets, mnStreams, mnConnections, mnServers, mnXMLUtils, mnXMLBase64,
  mnXMLRttiProfile, mnXMLNodes, SyncObjs, IniFiles;

type
  EdbgpException = class(Exception);
  TdbgpServer = class;
  TdbgpConnection = class;
  TdbgpConnectionClass = class of TdbgpConnection;

  TdbgpRespond = class(TmnXMLNodes)
  public
    Source: string;
  end;

  TdbgpActionFlag = (dbgpafSend, dbgpafCheckError, dbgpafStopOnError);
  TdbgpActionFlags = set of TdbgpActionFlag;

  { TdbgpAction }

  TdbgpAction = class(TObject)
  private
    FConnection: TdbgpConnection;
    FKeepAlive: Boolean;
    FKey: string;
    FFlags: TdbgpActionFlags;
    FEvent: TEvent; //must be nil until we need one
  protected
    FTransactionID: integer;
    procedure CheckError(Respond: TdbgpRespond);
    function GetCommand: string; virtual;
    function GetData: string; virtual;
    procedure Process(Respond: TdbgpRespond); virtual;
    procedure Created; virtual; //after create it
    procedure Prepare; virtual; //after pop from spool
    function Stay: boolean; virtual;
    function Enabled: boolean; virtual;
    function Accept: boolean; virtual;
    property Key: string read FKey;
    property Connection: TdbgpConnection read FConnection;
    property Flags: TdbgpActionFlags read FFlags write FFlags;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CreateEvent; virtual;
    procedure FreeEvent; virtual;
    property Event: TEvent read FEvent;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive; //do no free it
  end;

  TdbgpActionClass = class of TdbgpAction;

  TdbgpSpool = class(TObjectList)
  private
    function GetItem(Index: integer): TdbgpAction;
    procedure SetItem(Index: integer; const Value: TdbgpAction);
  public
    function Add(Action: TdbgpAction): integer; virtual;
    property Items[Index: integer]: TdbgpAction read GetItem write SetItem; default;
  end;

  TdbgpConnectionSpool = class(TdbgpSpool)
  private
    FConnection: TdbgpConnection;
  public
    function Add(Action: TdbgpAction): integer; override;
  end;

  TdbgpInit = class(TdbgpAction)
  protected
    procedure Created; override;
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  { TdbgpFeatureSet }

  TdbgpFeatureSet = class(TdbgpAction)
  protected
    FName: string;
    FValue: string;
  public
    constructor CreateBy(vName, vValue: string);
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpGetCurrent = class(TdbgpAction)
  private
    FCurKey: string;
    FCurFile: string;
    FCurLine: integer;
    procedure ShowFile;
  public
    procedure Created; override;
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpStepOver = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpStepInto = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpStepOut = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpRun = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  { TdbgpDetach }

  TdbgpDetach = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
    destructor Destroy; override;
  end;

  TdbgpStop = class(TdbgpAction)
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpCustomGet = class(TdbgpAction)
  public
    VariableType: string;
    VariableName: string;
    VariableValue: variant;
  end;
  // Watches

  TdbgpCustomGetWatch = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpGetWatch = class(TdbgpCustomGetWatch)
  protected
  public
    Index: integer;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  { TdbgpEval }

  TdbgpEval = class(TdbgpCustomGet)
  protected
  public
    function GetCommand: string; override;
    function GetData: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  { TdbgpGetWatchInstance }

  TdbgpGetWatchInstance = class(TdbgpCustomGetWatch)
  protected
  public
    destructor Destroy; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpGetWatches = class(TdbgpCustomGetWatch)
  protected
  public
    Current: integer;
    function Stay: boolean; override;
    function Enabled: boolean; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  // Breakpoints

  TdbgpSetBreakpoint = class(TdbgpAction)
  protected
  public
    BreakpointID: cardinal;
    FileName: string;
    FileLine: integer;
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  TdbgpSetBreakpoints = class(TdbgpSetBreakpoint)
  protected
  public
    Current: integer;
    function Enabled: boolean; override;
    function Stay: boolean; override;
  end;

  TdbgpRemoveBreakpoint = class(TdbgpAction)
  protected
  public
    BreakpointID: integer;
    function GetCommand: string; override;
    procedure Process(Respond: TdbgpRespond); override;
  end;

  { TdbgpConnection }

  TdbgpConnection = class(TmnServerConnection)
  private
    FLocalSpool: TdbgpConnectionSpool;
    FKey: string;
    function GetServer: TdbgpServer;
  public
    FTransactionID: integer;
  protected
    function NewTransactionID: integer;
{$IFDEF SAVELOG}
    procedure SaveLog(s: string);
{$ENDIF}
    function ReadRespond: TdbgpRespond;
    function PopAction: TdbgpAction;
    function SendCommand(Command: string; Data: string): integer;
    procedure Prepare; override;
    procedure DoProcess;
    procedure Process; override;
    procedure Unprepare; override;
  public
    constructor Create(Socket: TmnCustomSocket); override;
    destructor Destroy; override;
    procedure Stop; override;
    property Key: string read FKey;
    property Server: TdbgpServer read GetServer;
  published
  end;

  TmnDBGListener = class(TmnListener)
  private
    FAddress: string;
    FPort: integer;
  protected
    function CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Port: integer read FPort write FPort;
    property Address: string read FAddress write FAddress;
  end;

  TdbgpWatch = class(TObject)
  private
    FHandle: integer;
    FVariableName: string;
    FValue: variant;
    FVariableType: string;
  public
    property Handle: integer read FHandle write FHandle;
  published
    property VariableName: string read FVariableName write FVariableName;
    property VariableType: string read FVariableType write FVariableType;
    property Value: variant read FValue write FValue;
  end;

  TdbgpWatches = class(TObjectList)
  private
    FServer: TdbgpServer;
    CurrentHandle: integer;
    function GetItem(Index: integer): TdbgpWatch;
    procedure SetItem(Index: integer; const Value: TdbgpWatch);
    function GetValues(Name: string): variant;
    procedure SetValues(Name: string; const Value: variant);
  protected
    property Server: TdbgpServer read FServer;
  public
    function Find(Name: string): TdbgpWatch;
    function Add(Watch: TdbgpWatch): integer; overload;
    function Add(VariableName: string; Value: variant): integer; overload;
    procedure AddWatch(Name: string);
    procedure RemoveWatch(Name: string);
    procedure Clean;
    property Values[Name: string]: variant read GetValues write SetValues;
    property Items[Index: integer]: TdbgpWatch read GetItem write SetItem; default;
  end;

  TdbgpBreakpoint = class(TObject)
  private
    FID: integer;
    FLine: integer;
    FHandle: integer;
    FFileName: string;
  public
    constructor Create;
    property Handle: integer read FHandle write FHandle;
    property ID: integer read FID write FID;
  published
    property FileName: string read FFileName write FFileName;
    property Line: integer read FLine write FLine;
  end;

  TdbgpBreakpoints = class(TObjectList)
  private
    CurrentHandle: integer;
    FServer: TdbgpServer;
    function GetItem(Index: integer): TdbgpBreakpoint;
    procedure SetItem(Index: integer; const Value: TdbgpBreakpoint);
  protected
    property Server: TdbgpServer read FServer;
  public
    function Add(Breakpoint: TdbgpBreakpoint): integer; overload;
    function Remove(Breakpoint: TdbgpBreakpoint): integer; overload;
    procedure Remove(Handle: integer); overload;
    function Add(FileName: string; Line: integer): integer; overload;
    function Find(Name: string; Line: integer): TdbgpBreakpoint;
    procedure Toggle(FileName: string; Line: integer);
    property Items[Index: integer]: TdbgpBreakpoint read GetItem write SetItem; default;
  end;

  TdbgpOnServerEvent = procedure(Sender: TObject; Socket: TdbgpConnection) of object;
  TdbgpOnShowFile = procedure(const Key, FileName: string; Line: integer) of object;

  { TdbgpServer }

  TdbgpServer = class(TmnServer)
  private
    FBreakOnFirstLine: Boolean;
    FSpool: TdbgpSpool;
    FWatches: TdbgpWatches;
    FBreakpoints: TdbgpBreakpoints;
    FRunCount: Integer;
    function GetIsRuning: Boolean;
  protected
    procedure Notification(AComponent: TComponent; operation: TOperation); override;
    function CreateListener: TmnListener; override;
    procedure DoChanged(vListener: TmnListener); override;
    procedure DoStart; override;
    procedure DoStop; override;
    property Spool: TdbgpSpool read FSpool;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Resume;
    procedure AddAction(Action: TdbgpAction); overload;
    procedure AddAction(ActionClass: TdbgpActionClass); overload;
    procedure RemoveAction(Action: TdbgpAction);
    procedure ExtractAction(Action: TdbgpAction);
    procedure Clear;
    property RunCount: Integer read FRunCount;
    property IsRuning: Boolean read GetIsRuning;
    property Watches: TdbgpWatches read FWatches;
    property Breakpoints: TdbgpBreakpoints read FBreakpoints;
    property BreakOnFirstLine: Boolean read FBreakOnFirstLine write FBreakOnFirstLine default False;
  published
  end;

  { TdbgpManager }

  TdbgpManager = class(TObject)
  private
    FOnShowFile: TdbgpOnShowFile;
   public
    Lock: TCriticalSection;
    Event: TEvent;
    constructor Create;
    destructor Destroy; override;
    procedure ShowFile(const Key, FileName: string; Line: integer = -1);
    property OnShowFile: TdbgpOnShowFile read FOnShowFile write FOnShowFile;
  end;

function DBGP: TdbgpManager;

implementation

var
  FDBGP: TdbgpManager = nil;

function DBGP: TdbgpManager;
begin
  if FDBGP = nil then
    FDBGP := TdbgpManager.Create;
  Result := FDBGP;
end;

{ TdbgpEval }

function TdbgpEval.GetCommand: string;
begin
  Result := 'eval';
end;

function TdbgpEval.GetData: string;
begin
  Result := 'echo ' + VariableName;
end;

procedure TdbgpEval.Process(Respond: TdbgpRespond);
begin
  inherited Process(Respond);
end;

{ TdbgpFeatureSet }

constructor TdbgpFeatureSet.CreateBy(vName, vValue: string);
begin
  Create;
  FName := vName;
  FValue:= vValue;
end;

function TdbgpFeatureSet.GetCommand: string;
begin
  //Result := 'feature_set -n show_hidden -v 1';
  Result := 'feature_set -n ' + FName + ' -v '+ FValue;
end;

procedure TdbgpFeatureSet.Process(Respond: TdbgpRespond);
begin
  inherited Process(Respond);
end;

{ TdbgpManager }

constructor TdbgpManager.Create;
begin
  inherited;
  Lock := TCriticalSection.Create;
  Event := TEvent.Create(nil, False, False, '');
end;

destructor TdbgpManager.Destroy;
begin
  FreeAndNil(Event);
  FreeAndNil(Lock);
  inherited;
end;

procedure TdbgpManager.ShowFile(const Key, FileName: string; Line: integer);
begin
  if Assigned(FOnShowFile) then
    FOnShowFile(Key, FileName, Line);
end;

constructor TdbgpServer.Create(AOwner: TComponent);
begin
  inherited;
  FSpool := TdbgpSpool.Create(True);
  Port := '9000';
  FWatches := TdbgpWatches.Create;
  FWatches.FServer := Self;
  FBreakpoints := TdbgpBreakpoints.Create;
  FBreakpoints.FServer := Self;
  FBreakOnFirstLine := False;
end;

destructor TdbgpServer.Destroy;
begin
  FreeAndNil(FWatches);
  FreeAndNil(FBreakpoints);
  FreeAndNil(FSpool);
  inherited;
end;

function TdbgpServer.GetIsRuning: Boolean;
begin
  Result := RunCount > 0;
end;

procedure TdbgpServer.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

constructor TdbgpConnection.Create(Socket: TmnCustomSocket);
begin
  inherited;
  FLocalSpool := TdbgpConnectionSpool.Create;
  FLocalSpool.FConnection := Self;
  KeepAlive := True;
  Stream.Timeout := 5000;
end;

destructor TdbgpConnection.Destroy;
begin
  FreeAndNil(FLocalSpool);
  inherited;
end;

{ TdbgpConnection }

function TdbgpConnection.NewTransactionID: integer;
begin
  Inc(FTransactionID);
  Result := FTransactionID;
end;

procedure TdbgpGetCurrent.ShowFile; //this function must Synchronize
begin
  DBGP.ShowFile(FCurKey, FCurFile, FCurLine);
end;

procedure TdbgpConnection.DoProcess;
var
  aAction: TdbgpAction;
  aRespond: TdbgpRespond;
  aCommand: string;
  aKeep: Boolean;
begin
  aAction := PopAction;
  if aAction <> nil then
  begin
    if not aAction.Enabled then
      FLocalSpool.Remove(aAction)
    else
      try
        aCommand := aAction.GetCommand;
        if (dbgpafSend in aAction.Flags) and (aCommand <> '') then
          aAction.FTransactionID := SendCommand(aCommand, aAction.GetData);
        if aAction.Accept and Connected then
        begin
          aRespond := ReadRespond;
          try
            if (aRespond <> nil) and (aRespond.Root <> nil) then
            begin
              if (aRespond.GetAttribute('response', 'status') = 'stopping') then
                Disconnect
              else if (aRespond.GetAttribute('response', 'status') = 'stoped') then
              begin
                //Disconnect;
              end
              else
              begin
                try
                  if (aRespond <> nil) and Connected and (aRespond.Root <> nil) then
                    aAction.Process(aRespond);
                finally
                end;
              end;
            end;
          finally
            FreeAndNil(aRespond);
          end;
        end;
      finally
        if not aAction.Stay then
        begin
          aKeep := (aAction.Event <> nil) or aAction.KeepAlive;
          if aAction.Event <> nil then
            aAction.Event.SetEvent;
          if aKeep then
            FLocalSpool.Extract(aAction)
          else
            FLocalSpool.Remove(aAction);
        end;
      end;
  end;
end;

procedure TdbgpConnection.Unprepare;
begin
  inherited Unprepare;
end;

{ TdbgpSocketServer }

function TdbgpServer.CreateListener: TmnListener;
begin
  Result := TmnDBGListener.Create;
end;

procedure EnumDirList(const Path: string; Strings: TStrings);
var
  I: integer;
  SearchRec: TSearchRec;
begin
  try
    I := FindFirst(Path, faDirectory, SearchRec);
    while I = 0 do
    begin
      if ((SearchRec.Attr and faDirectory) > 0) and (SearchRec.Name[1] <> '.') then
        Strings.Add(SearchRec.Name);
      I := FindNext(SearchRec);
    end;
    FindClose(SearchRec);
  except
  end;
end;

function TdbgpConnection.ReadRespond: TdbgpRespond;
var
  Reader: TmnXMLNodeReader;
  s: string;
  aMatched: boolean;
begin
  Result := nil;
  Stream.ReadUntil(#0, s, aMatched);
  if Connected and aMatched and (S <> '') then
  begin
    Result := TdbgpRespond.Create;
    Stream.ReadUntil(#0, s, aMatched);
    s := Trim(s);
    {$IFDEF SAVELOG}
    SaveLog(s);
    {$ENDIF}
    Result.Source := s;
    Reader := TmnXMLNodeReader.Create;
    try
      Reader.Start;
      Reader.Nodes := Result;
      Reader.ParseLine(s, 0);
    finally
      Reader.Free;
    end;
  end;
end;

{$IFDEF SAVELOG}
procedure TdbgpConnection.SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\xdebug_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;

{$ENDIF}

function TdbgpConnection.SendCommand(Command: string; Data: string): integer;
var
  s: string;
begin
  Result := NewTransactionID;
  s := Command + ' -i ' + IntToStr(Result);
  if Data <> '' then
    s := s + ' -- ' + Data;
  Stream.WriteLine(s, #0);
{$IFDEF SAVELOG}
  SaveLog(s);
{$ENDIF}
end;

function TdbgpConnection.GetServer: TdbgpServer;
begin
  Result := (Listener.Server as TdbgpServer);
end;

function TdbgpConnection.PopAction: TdbgpAction;
var
  aAction: TdbgpAction;
  i: integer;
begin
  if FLocalSpool.Count = 0 then
  begin
    InterLockedIncrement(Server.FRunCount);
    DBGP.Event.WaitFor(INFINITE); //wait the ide to make resume
    InterLockedDecrement(Server.FRunCount);

    DBGP.Lock.Enter;
    try
      i := 0;
      while i < Server.Spool.Count do
      begin
        aAction := Server.Spool.Extract(Server.Spool[i]) as TdbgpAction;
        //        if aAction.Key = Key then
        FLocalSpool.Add(aAction);
        //        else
        //        inc(i);
      end;
    finally
      DBGP.Lock.Leave;
    end;
  end;
  Result := nil;
  while not Terminated and ((FLocalSpool.Count > 0) and (Result = nil)) do
  begin
    Result := FLocalSpool[0];
    Result.Prepare;
  end;
end;

procedure TdbgpConnection.Prepare;
begin
  inherited;
  FLocalSpool.Add(TdbgpInit.Create);
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('show_hidden', '1'));
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('max_depth', '1'));
  FLocalSpool.Add(TdbgpFeatureSet.CreateBy('max_children', '1'));
  FLocalSpool.Add(TdbgpSetBreakpoints.Create);
  if Server.BreakOnFirstLine then
  begin
    FLocalSpool.Add(TdbgpStepInto.Create);
    FLocalSpool.Add(TdbgpGetCurrent.Create);
  end
  else
  begin
    FLocalSpool.Add(TdbgpRun.Create);
    FLocalSpool.Add(TdbgpGetWatches.Create);
    FLocalSpool.Add(TdbgpGetCurrent.Create);
end;
end;

procedure TdbgpConnection.Process;
begin
  //Allow one connection to process
  //Listener.Enter;
  try
    DoProcess;
  finally
    //Listener.Leave;
  end;
end;

procedure TdbgpConnection.Stop;
begin
  inherited;
  DBGP.Event.SetEvent;
end;

{ TmnDBGListener }

function TmnDBGListener.CreateConnection(vSocket: TmnCustomSocket): TmnServerConnection;
begin
  Result := TdbgpConnection.Create(vSocket);
end;

constructor TmnDBGListener.Create;
begin
  inherited;
  FOptions := FOptions + [soReuseAddr];
end;

destructor TmnDBGListener.Destroy;
begin
  inherited;
end;

procedure TdbgpServer.DoStart;
begin
  Spool.Clear;
  inherited;
end;

procedure TdbgpServer.DoStop;
begin
  inherited;
  if FSpool <> nil then //DoStop class when Server free
    FSpool.Clear;
end;

procedure TdbgpServer.DoChanged(vListener: TmnListener);
begin
  inherited;
  if vListener.Count = 0 then //TODO: i am not sure in Linux
    DBGP.ShowFile('', '');
end;

{ TdbgpAction }

function TdbgpAction.GetCommand: string;
begin
  Result := '';
end;

function TdbgpAction.GetData: string;
begin
  Result := '';
end;

function TdbgpAction.Stay: boolean;
begin
  Result := False;
end;

procedure TdbgpAction.Process(Respond: TdbgpRespond);
begin

end;

function TdbgpAction.Accept: boolean;
begin
  Result := True;
end;

procedure TdbgpAction.CreateEvent;
begin
  if FEvent <> nil then
    raise EdbgpException.Create('Event already exists');
  FEvent := TEvent.Create(nil, True, False, '');
end;

procedure TdbgpAction.FreeEvent;
begin
  FreeAndNil(FEvent);
end;

function TdbgpAction.Enabled: boolean;
begin
  Result := True;
end;

procedure TdbgpAction.Prepare;
begin
end;

procedure TdbgpAction.CheckError(Respond: TdbgpRespond);
begin
  if (Respond.Root <> nil) then
    if StrToIntDef(Respond.GetAttribute('response', 'transaction_id'), -1) <> FTransactionID then
      raise Exception.Create('transaction_id is not same with command.'#13 + Respond.Source);
end;

constructor TdbgpAction.Create;
begin
  inherited Create;
  Created;
end;

destructor TdbgpAction.Destroy;
begin
  FreeEvent;
  inherited;
end;

procedure TdbgpAction.Created;
begin
  Flags := [dbgpafSend];
end;

{ TdbgpSpool }

function TdbgpSpool.Add(Action: TdbgpAction): integer;
begin
  Result := inherited Add(Action);
end;

function TdbgpSpool.GetItem(Index: integer): TdbgpAction;
begin
  Result := inherited Items[Index] as TdbgpAction;
end;

procedure TdbgpSpool.SetItem(Index: integer; const Value: TdbgpAction);
begin
  inherited Items[Index] := Value;
end;

{ TdbgpStepOver }

function TdbgpStepOver.GetCommand: string;
begin
  Result := 'step_over';
end;

procedure TdbgpStepOver.Process(Respond: TdbgpRespond);
begin
  inherited;

end;

{ TdbgpStepInto }

function TdbgpStepInto.GetCommand: string;
begin
  Result := 'step_into';
end;

procedure TdbgpStepInto.Process(Respond: TdbgpRespond);
begin
  inherited;
end;

procedure TdbgpServer.Resume;
begin
  DBGP.Event.SetEvent;
end;

{ TdbgpInit }

procedure TdbgpInit.Created;
begin
  inherited;
  Flags := Flags - [dbgpafSend];
end;

function TdbgpInit.GetCommand: string;
begin
  Result := 'init';
end;

procedure TdbgpInit.Process(Respond: TdbgpRespond);
begin
  inherited;
  DBGP.Lock.Enter;
  try
    Connection.Server.Watches.Clean;
    Connection.FKey := Respond.Root.Attributes['idekey'];
  finally
    DBGP.Lock.Leave;
  end;
end;

{ TdbgpGetCurrent }

function TdbgpGetCurrent.GetCommand: string;
begin
  Result := 'stack_get';
end;

procedure TdbgpGetCurrent.Process(Respond: TdbgpRespond);
begin
  inherited;
  if Respond.Root.Items.Count > 0 then
  begin
    FCurFile := URIToFileName(Respond.GetAttribute('stack', 'filename'));
    if FCurFile <> '' then
    begin
      FCurKey := Connection.Key;
      FCurLine := StrToIntDef(Respond.GetAttribute('stack', 'lineno'), 0);
      try
        //Dont do any lock here
        Connection.Synchronize(@ShowFile);
      finally
      end;
    end;
  end;
end;

procedure TdbgpGetCurrent.Created;
begin
  inherited;
  Flags := Flags + [dbgpafCheckError];
end;

{ TdbgpRun }

function TdbgpRun.GetCommand: string;
begin
  Result := 'run';
end;

procedure TdbgpRun.Process(Respond: TdbgpRespond);
begin
  inherited;
end;

{ TdbgpDetach }

function TdbgpDetach.GetCommand: string;
begin
  Result := 'detach';
end;

procedure TdbgpDetach.Process(Respond: TdbgpRespond);
begin
  inherited;
  Connection.Disconnect;
end;

destructor TdbgpDetach.Destroy;
begin
  inherited Destroy;
end;

{ TdbgpStop }

function TdbgpStop.GetCommand: string;
begin
  Result := 'stop';
end;

procedure TdbgpStop.Process(Respond: TdbgpRespond);
begin
  inherited;
  Connection.Disconnect;
end;

{ TdbgpStepOut }

function TdbgpStepOut.GetCommand: string;
begin
  Result := 'step_out';
end;

procedure TdbgpStepOut.Process(Respond: TdbgpRespond);
begin
  inherited;
end;

{ TdbgpWatches }

function TdbgpWatches.Add(Watch: TdbgpWatch): integer;
begin
  Result := inherited Add(Watch);
end;

function TdbgpWatches.Add(VariableName: string; Value: variant): integer;
var
  aWatch: TdbgpWatch;
begin
  Inc(CurrentHandle);
  aWatch := TdbgpWatch.Create;
  aWatch.Handle := CurrentHandle;
  aWatch.VariableName := VariableName;
  aWatch.VariableType := '';
  aWatch.Value := Value;
  Result := Add(aWatch);
end;

procedure TdbgpWatches.AddWatch(Name: string);
var
  aIndex: integer;
begin
  DBGP.Lock.Enter;
  try
    aIndex := Add(Name, '');
  finally
    DBGP.Lock.Leave;
  end;
  if Server.IsRuning then
  begin
    DBGP.Lock.Enter;
    try
      Server.Spool.Add(TdbgpGetWatches.Create);
      Server.Spool.Add(TdbgpGetCurrent.Create);
    finally
      DBGP.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TdbgpWatches.Clean;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    VarClear(Items[i].FValue);
  end;
end;

function TdbgpWatches.Find(Name: string): TdbgpWatch;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i].VariableName = Name then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TdbgpWatches.GetItem(Index: integer): TdbgpWatch;
begin
  Result := inherited Items[Index] as TdbgpWatch;
end;

function TdbgpWatches.GetValues(Name: string): variant;
var
  aWatch: TdbgpWatch;
begin
  aWatch := Find(Name);
  if aWatch <> nil then
    Result := aWatch.Value
  else
    VarClear(Result);
end;

procedure TdbgpWatches.RemoveWatch(Name: string);
var
  i: integer;
  Founded: Boolean;
begin
  Founded:=False;
  for i := 0 to Count - 1 do
  begin
    if Items[i].VariableName = Name then
    begin
      Delete(i);
      Founded:=True;
      break;
    end;
  end;
  if Server.IsRuning then
  begin
    DBGP.Lock.Enter;
    try
      Server.Spool.Add(TdbgpGetWatches.Create);
      Server.Spool.Add(TdbgpGetCurrent.Create);
    finally
      DBGP.Lock.Leave;
    end;
    Server.Resume;
  end;
end;

procedure TdbgpWatches.SetItem(Index: integer; const Value: TdbgpWatch);
begin
  inherited Items[Index] := Value;
end;

procedure TdbgpWatches.SetValues(Name: string; const Value: variant);
begin

end;

{ TdbgpGetWatch }

procedure TdbgpGetWatch.Process(Respond: TdbgpRespond);
begin
  inherited;
  DBGP.Lock.Enter;
  try
    Connection.Server.Watches[Index].Value := VariableValue;
    Connection.Server.Watches[Index].VariableType := VariableType;
  finally
    DBGP.Lock.Leave;
  end;
end;

{ TdbgpBreakpoints }

function TdbgpBreakpoints.Add(Breakpoint: TdbgpBreakpoint): integer;
begin
  Result := inherited Add(Breakpoint);
end;

function TdbgpBreakpoints.Add(FileName: string; Line: integer): integer;
var
  aBreakpoint: TdbgpBreakpoint;
begin
  Inc(CurrentHandle);
  aBreakpoint := TdbgpBreakpoint.Create;
  aBreakpoint.Handle := CurrentHandle;
  aBreakpoint.FileName := FileName;
  aBreakpoint.Line := Line;
  Result := Add(aBreakpoint);
end;

function TdbgpBreakpoints.Find(Name: string; Line: integer): TdbgpBreakpoint;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].line = Line) and SameText(Items[i].FileName, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TdbgpBreakpoints.GetItem(Index: integer): TdbgpBreakpoint;
begin
  Result := inherited Items[Index] as TdbgpBreakpoint;
end;

function TdbgpBreakpoints.Remove(Breakpoint: TdbgpBreakpoint): integer;
begin
  Result := inherited Remove(Breakpoint);
end;

procedure TdbgpBreakpoints.Remove(Handle: integer);
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Handle = Handle then
    begin
      Delete(i);
      break;
    end;
  end;
end;

procedure TdbgpBreakpoints.SetItem(Index: integer; const Value: TdbgpBreakpoint);
begin
  inherited Items[Index] := Value;
end;

procedure TdbgpBreakpoints.Toggle(FileName: string; Line: integer);
var
  aBreakpoint: TdbgpBreakpoint;
  aSetBreakpoint: TdbgpSetBreakpoint;
  aRemoveBreakpoint: TdbgpRemoveBreakpoint;
begin
  aBreakpoint := Find(FileName, Line);
  if aBreakpoint <> nil then
  begin
    Remove(aBreakpoint);
    if Server.IsRuning then
    begin
      if aBreakpoint.ID <> 0 then
      begin
        aRemoveBreakpoint := TdbgpRemoveBreakpoint.Create;
        aRemoveBreakpoint.BreakpointID := aBreakpoint.ID;
        Server.Spool.Add(aRemoveBreakpoint);
      end;
    end;
  end
  else
  begin
    Add(FileName, Line);
    if Server.IsRuning then
    begin
      aSetBreakpoint := TdbgpSetBreakpoint.Create;
      aSetBreakpoint.FileName := FileName;
      aSetBreakpoint.FileLine := Line;
      Server.Spool.Add(aSetBreakpoint);
    end;
  end;
end;

{ TdbgpGetWatches }

function TdbgpGetWatches.Stay: boolean;
begin
  DBGP.Lock.Enter;
  try
    Inc(Current);
    Result := Current < Connection.Server.Watches.Count;
  finally
    DBGP.Lock.Leave;
  end;
end;

procedure TdbgpGetWatches.Process(Respond: TdbgpRespond);
begin
  inherited;
  DBGP.Lock.Enter;
  try
    Connection.Server.Watches[Current].Value := VariableValue;
    Connection.Server.Watches[Current].VariableType := VariableType;
  finally
    DBGP.Lock.Leave;
  end;
end;

function TdbgpGetWatches.Enabled: boolean;
begin
  DBGP.Lock.Enter;
  try
    Result := Current < Connection.Server.Watches.Count;
    if Result then
      VariableName := Connection.Server.Watches[Current].VariableName;
  finally
    DBGP.Lock.Leave;
  end;
end;

{ TdbgpSetBreakpoints }

function TdbgpSetBreakpoints.Enabled: boolean;
begin
  DBGP.Lock.Enter;
  try
    Result := Current < Connection.Server.Breakpoints.Count;
    if Result then
    begin
      FileName := Connection.Server.Breakpoints[Current].FileName;
      FileLine := Connection.Server.Breakpoints[Current].Line;
    end;
  finally
    DBGP.Lock.Leave;
  end;
end;

function TdbgpSetBreakpoints.Stay: boolean;
begin
  DBGP.Lock.Enter;
  try
    Connection.Server.Breakpoints[Current].ID := BreakpointID;
    Inc(Current);
    Result := Current < Connection.Server.Breakpoints.Count;
  finally
    DBGP.Lock.Leave;
  end;
end;

{ TdbgpSetBreakpoint }

function TdbgpSetBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_set -t line -n ' + IntToStr(FileLine) + ' -f ' + FileNameToURI(FileName) + '';
end;

procedure TdbgpSetBreakpoint.Process(Respond: TdbgpRespond);
begin
  inherited;
  CheckError(Respond);
  BreakpointID := StrToInt(Respond.Root.Attributes['id']);
end;

{ TdbgpRemoveBreakpoint }

function TdbgpRemoveBreakpoint.GetCommand: string;
begin
  Result := 'breakpoint_remove -d ' + IntToStr(BreakpointID);
end;

procedure TdbgpRemoveBreakpoint.Process(Respond: TdbgpRespond);
begin
  inherited;
end;

{ TdbgpGetWatchInstance }

procedure TdbgpGetWatchInstance.Process(Respond: TdbgpRespond);
begin
  inherited;
end;

destructor TdbgpGetWatchInstance.Destroy;
begin
  inherited;
end;

{ TdbgpConnectionSpool }

function TdbgpConnectionSpool.Add(Action: TdbgpAction): integer;
begin
  Result := inherited Add(Action);
  Action.FConnection := FConnection;
end;

{ TdbgpBreakpoint }

constructor TdbgpBreakpoint.Create;
begin
  inherited;
end;

{$IFDEF SAVELOG}
procedure SaveLog(s: string);
var
  aStrings: TStringList;
  aStream: TFileStream;
  i: integer;
const
  sFile = 'c:\lock_server.log';
begin
  aStrings := TStringList.Create;
  aStrings.Text := s;
  if FileExists(sFile) then
  begin
    aStream := TFileStream.Create(sFile, fmOpenWrite);
    aStream.Seek(0, soFromEnd);
  end
  else
    aStream := TFileStream.Create(sFile, fmCreate);

  try
    for i := 0 to aStrings.Count - 1 do
    begin
      s := aStrings[i] + #13;
      aStream.Write(s[1], Length(s));
    end;
  finally
    aStream.Free;
    aStrings.Free;
  end;
end;
{$ENDIF}

procedure TdbgpServer.AddAction(Action: TdbgpAction);
begin
  DBGP.Lock.Enter;
  try
    Spool.Add(Action);
  finally
    DBGP.Lock.Leave;
  end;
end;

procedure TdbgpServer.AddAction(ActionClass: TdbgpActionClass);
begin
  AddAction(ActionClass.Create);
end;

procedure TdbgpServer.RemoveAction(Action: TdbgpAction);
begin
  DBGP.Lock.Enter;
  try
    Spool.Remove(Action);
  finally
    DBGP.Lock.Leave;
  end;
end;

procedure TdbgpServer.ExtractAction(Action: TdbgpAction);
begin
  DBGP.Lock.Enter;
  try
    Spool.Extract(Action);
  finally
    DBGP.Lock.Leave;
  end;
end;

procedure TdbgpServer.Clear;
begin
  DBGP.Lock.Enter;
  try
    Spool.Clear;
  finally
    DBGP.Lock.Leave;
  end;
end;

{ TdbgpCustomGetWatch }

function TdbgpCustomGetWatch.GetCommand: string;
begin
  Result := 'property_value -n "' + VariableName + '" -m 1024';
  //Result := 'property_get -n "' + VariableName + '" -m 1024';
end;

procedure TdbgpCustomGetWatch.Process(Respond: TdbgpRespond);
const
  //sCmd = 'property';
  sCmd = 'response';
begin
  inherited;
  if Respond[sCmd] <> nil then
  begin
    if Respond[sCmd].Attributes['encoding'] = 'base64' then
      VariableValue := Base64Decode(Respond[sCmd].Value)
    else
      VariableValue := Respond[sCmd].Value;

    VariableType := Respond[sCmd].Attributes['type'];
  end
  else
  begin
    VariableType := '[ERROR]';
    VariableValue := '';
  end;
end;

initialization
finalization
  FreeAndNil(FDBGP);
end.

