unit mnModules;
{$M+}{$H+}
{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

interface

uses
  SysUtils, Classes,
  mnClasses, mnStreams, mnFields,
  mnSockets, mnConnections, mnServers;

type
  TmnCommandException = class(Exception);
  TmnCommandConnection = class;
  TmnCommandConnectionClass = class of TmnCommandConnection;
  TmnCommandConnectionState = (hcRequest, hcHeader, hcPostedData); //TODO remove it

  TmnCommand = class;

  TmnRequest = record
    Module: string;
    Command: string;
    Method: string;
    Path: string;
    Version: string;
    Protcol: string;
    OriginalRequest: string; //Full of first line of header
  end;

  TmnModule = class;

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  TmnCommandState = (cmdsHeadSent, cmdsHeaderSent, cmdsContentsSent, cmdsEnd);
  TmnCommandStates = set of TmnCommandState;

  { TmnCommand }

  TmnCommand = class(TmnObject)
  private
    FRequest: TmnRequest;
    FModule: TmnModule;
    FLocking: Boolean;
    FRaiseExceptions: Boolean;
    FRequestHeader: TmnFields;
    FRespondHeader: TmnFields;
    FRequestStream: TmnBufferStream;
    FRespondStream: TmnBufferStream;
    FStates: TmnCommandStates;
    procedure SetModule(const Value: TmnModule); virtual;
    procedure SetRequestHeader(const Value: TmnFields);
    function GetActive: Boolean;
  protected
    FWorking: Boolean;

    procedure Prepare; virtual;
    procedure Execute; virtual;
    procedure SendHeader; virtual;
    procedure Respond; virtual;
    procedure Unprepare; virtual; //Shutdown it;

    property RequestStream: TmnBufferStream read FRequestStream;
    property RespondStream: TmnBufferStream read FRespondStream;
    procedure SendHead(AHead: string); virtual;
    procedure PostHeader(AName, AValue: string); virtual;
  public
    constructor Create(AModule: TmnModule; RequestStream: TmnBufferStream = nil; RespondStream: TmnBufferStream = nil); virtual;
    destructor Destroy; override;

    property Active: Boolean read GetActive;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmnModule read FModule write SetModule;
    property Request: TmnRequest read FRequest;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    //Lock the server listener when execute the command
    property Locking: Boolean read FLocking write FLocking default False; //deprecated;
    //Prepare called after created in lucking mode
    property RequestHeader: TmnFields read FRequestHeader write SetRequestHeader;
    property RespondHeader: TmnFields read FRespondHeader;
    property States: TmnCommandStates read FStates;
  end;

  TmnCommandClass = class of TmnCommand;

  TmnCommandClassItem = class(TmnNamedObject)
  private
    FCommandClass: TmnCommandClass;
  public
    property CommandClass: TmnCommandClass read FCommandClass;
  end;

  { TmnCommandClasses }

  TmnCommandClasses = class(TmnNamedObjectList<TmnCommandClassItem>)
  private
  public
    function Add(const Name: string; CommandClass: TmnCommandClass): Integer;
  end;

  {
    Module will do simple protocol before execute command
    Module have protocol name must match when parse request, before selecting
  }

  TmnModules = class;

  { TmnModule }

  TmnModule = class(TmnNamedObject)
  private
    FCommands: TmnCommandClasses;
    FModules: TmnModules;
    FParams: TStringList;
  protected
    DefaultCommand: TmnCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: string): TmnCommandClass; virtual;
    procedure Created; override;
    procedure CreateCommands; virtual;
  public
    constructor Create(AModules: TmnModules); virtual;
    destructor Destroy; override;
    function Match(ARequest: TmnRequest): Boolean; virtual;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass; ADefaultCommand: Boolean = False): Integer; overload;
    function CreateCommand(CommandName: string; ARequest: TmnRequest; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil): TmnCommand;
    procedure ExecuteCommand(CommandName: string; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil; RequestString: TArray<String> = nil);
    procedure ParseHeader(RequestHeader: TmnFields; Stream: TmnBufferStream); virtual;
    property Commands: TmnCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Params: TStringList read FParams;
    property Modules: TmnModules read FModules;
  end;

  TmnModules = class(TmnNamedObjectList<TmnModule>)
  private
  protected
  public
    function ParseRequest(const Request: string): TmnRequest; virtual;
    function Add(const Name: string; AModule:TmnModule): Integer;
  end;

//--- Server ---

  TmnCommandServer = class;

  { TmnCommandConnection }

  TmnCommandConnection = class(TmnServerConnection)
  private
    FCommand: TmnCommand;
  public
  protected
    procedure Process; override;
  public
    destructor Destroy; override;
  published
  end;

  { TmnCommandListener }

  TmnCommandListener = class(TmnListener)
  private
    function GetServer: TmnCommandServer;
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
    property Server: TmnCommandServer read GetServer;
  public
  end;

  TmnCommandServer = class(TmnEventServer)
  private
    FModules: TmnModules;
  protected
    function DoCreateListener: TmnListener; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Modules: TmnModules read FModules;
  end;

implementation

uses
  mnUtils;

{ TmnCommandListener }

constructor TmnCommandServer.Create;
begin
  inherited;
  FModules := TmnModules.Create;
  Port := '81';
end;

destructor TmnCommandServer.Destroy;
begin
  FreeAndNil(FModules);
  inherited;
end;

destructor TmnCommandConnection.Destroy;
begin
  FreeAndNil(FCommand);
  inherited;
end;

{ TmnCommandConnection }

procedure TmnCommandConnection.Process;
var
  aRequestLine: string;
  aRequest: TmnRequest;
  aModule: TmnModule;
begin
  inherited;
  if Connected then
  begin
    if FCommand = nil then
    begin
      if Connected then
      begin
        aRequestLine := Stream.ReadLine;
        aRequest := (Listener.Server as TmnCommandServer).Modules.ParseRequest(aRequestLine);
        aModule := (Listener.Server as TmnCommandServer).Modules.Find(aRequest.Module);
        if aModule = nil then
          aModule := (Listener.Server as TmnCommandServer).Modules[0]; //fall back

        Listener.Enter;
        try
          FCommand := aModule.CreateCommand(aRequest.Command, aRequest, Stream, Stream);
          FCommand.Prepare;
        finally
          Listener.Leave;
        end;
      end;

      if FCommand <> nil then
      begin
        try
          FCommand.FWorking := True; //TODO
          if FCommand.Locking then
            Listener.Enter;
          try
            FCommand.Execute;
          finally
            FCommand.FWorking := False;
            if FCommand.Locking then
              Listener.Leave;
          end;
        except
          if FCommand.RaiseExceptions then
            raise;
        end;
        if Stream.Connected then
          Stream.Disconnect;
        FreeAndNil(FCommand);
      end
      else
        Stream.Disconnect;
    end;
  end;
end;

{ TGuardSocketServer }

function TmnCommandServer.DoCreateListener: TmnListener;
begin
  Result := TmnCommandListener.Create;
end;

{ TmnCustomCommandListener }

function TmnCommandListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnCommandConnection.Create(Self, vStream);
end;

procedure TmnCommandListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.Timeout := -1;
end;

function TmnCommandListener.GetServer: TmnCommandServer;
begin
  Result := inherited Server as TmnCommandServer;
end;

{ TmnCommand }

constructor TmnCommand.Create(AModule: TmnModule; RequestStream: TmnBufferStream; RespondStream: TmnBufferStream);
begin
  inherited Create;
  FLocking := False;
  FModule := Module;
  FRequestStream := RequestStream;
  FRespondStream := FRespondStream;
  FRequestHeader := TmnFields.Create;
  FRespondHeader := TmnFields.Create;
  Created;
end;

destructor TmnCommand.Destroy;
begin
  FreeAndNil(FRequestHeader);
  FreeAndNil(FRespondHeader);
  inherited;
end;

procedure TmnCommand.Prepare;
begin
end;

procedure TmnCommand.SendHeader;
begin
end;

procedure TmnCommand.Respond;
begin
end;

procedure TmnCommand.Unprepare;
begin
end;

procedure TmnCommand.SendHead(AHead: string);
begin
  RespondStream.WriteLine(AHead);
  FStates := FStates + [cmdsHeadSent];
end;

procedure TmnCommand.PostHeader(AName, AValue: string);
begin
  if cmdsHeadSent in FStates then
    raise Exception.Create('Header is sent');
end;

procedure TmnCommand.Execute;
begin
  SendHeader;
  FStates := FStates + [cmdsHeaderSent];
  {$ifdef DEBUG_MODE}
//    Server.Listener.Log(Connection, GetCommandName + ': Started on port ' + Server.Port);
  try
  {$endif}
    Respond;
  {$ifdef DEBUG_MODE}
  except
    on E:Exception do
    begin
//      Server.Listener.Log(Connection, GetCommandName + ': Error ' + E.Message);
      raise;
    end;
  end;
//    Server.Listener.Log(Connection, GetCommandName + ': Finished');
  {$endif}
end;

function TmnCommand.GetActive: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmnCommand.SetModule(const Value: TmnModule);
begin
  FModule := Value;
end;

procedure TmnCommand.SetRequestHeader(const Value: TmnFields);
begin
  if FRequestHeader <> Value then
  begin
    FreeAndNil(FRequestHeader);
    FRequestHeader := Value;
  end;
end;

function TmnCommandClasses.Add(const Name: string; CommandClass: TmnCommandClass): Integer;
var
  aItem: TmnCommandClassItem;
begin
  aItem := TmnCommandClassItem.Create;
  aItem.Name := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

{ TmnModule }

function TmnModule.CreateCommand(CommandName: string; ARequest: TmnRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmnCommand;
var
  aClass: TmnCommandClass;
begin
  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self);
    Result.FModule := Self;
    Result.FRequest := ARequest;
    Result.FRequestStream := ARequestStream;
    Result.FRespondStream := ARespondStream;
  end
  else
    Result := nil;
  if Result <> nil then
    ParseHeader(Result.RequestHeader, ARequestStream);
end;

function TmnModule.GetCommandClass(var CommandName: string): TmnCommandClass;
var
  aItem: TmnCommandClassItem;
begin
  aItem := Commands.Find(CommandName);
  if aItem <> nil then
  begin
    CommandName := aItem.Name;
    Result := aItem.CommandClass;
  end
  else
    Result := DefaultCommand;
end;

procedure TmnModule.ParseHeader(RequestHeader: TmnFields; Stream: TmnBufferStream);
var
  line: string;
begin
  if Stream <> nil then
  begin
    while not Stream.EOF do
    begin
      line := Stream.ReadLineRawByte;
      if line = '' then
        break
      else
      begin
        RequestHeader.AddItem(line, ':');
      end;
    end;
  end;
end;

procedure TmnModule.Created;
begin
  inherited;
  FParams := TStringList.Create;
  FCommands := TmnCommandClasses.Create;
  CreateCommands;
end;

procedure TmnModule.CreateCommands;
begin

end;

constructor TmnModule.Create(AModules: TmnModules);
begin
  inherited Create;
  FModules := AModules;
end;

destructor TmnModule.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCommands);
  inherited;
end;

function TmnModule.Match(ARequest: TmnRequest): Boolean;
begin
end;

procedure TmnModule.ExecuteCommand(CommandName: string; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; RequestString: TArray<String>);
var
  Command: TmnCommand;
  ARequest: TmnRequest;
begin
  Finalize(ARequest);
  Command := CreateCommand(CommandName, ARequest, ARequestStream, ARespondStream);
  if Command <> nil then //todo
  begin
    Command.Execute;
  end;
end;

function TmnModule.GetActive: Boolean;
begin
  Result := True; //todo
end;

function TmnModule.RegisterCommand(vName: string; CommandClass: TmnCommandClass; ADefaultCommand: Boolean): Integer;
begin
{  if Active then
    raise TmnCommandException.Create('Server is Active');}
  if FCommands.Find(vName) <> nil then
    raise TmnCommandException.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
  if ADefaultCommand then
    DefaultCommand := CommandClass;
end;

{ TmnModules }

function TmnModules.Add(const Name: string; AModule:TmnModule): Integer;
begin
  AModule.Name := Name;
  Result := inherited Add(AModule);
end;

function TmnModules.ParseRequest(const Request: string): TmnRequest;
var
  aRequests: TStringList;
begin
  Finalize(Result);
  aRequests := TStringList.Create;
  try
    StrToStrings(Request, aRequests, [' '], []);
    if aRequests.Count > 0 then
    begin
      Result.Module := aRequests[0];
      Result.Method := Result.Module;
    end;
    if aRequests.Count > 1 then
      Result.Path := aRequests[1];
    if aRequests.Count > 2 then
      Result.Version := aRequests[2];
  finally
    aRequests.Free;
  end;
end;

end.
