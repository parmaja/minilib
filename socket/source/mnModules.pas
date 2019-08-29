unit mnModules;
{$M+}{$H+}
{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *
 *  https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
 *
 *}

interface

uses
  SysUtils, Classes,
  mnClasses, mnStreams, mnFields,
  mnSockets, mnConnections, mnServers;

type
  TmnModuleException = class(Exception);

  TmnModuleConnection = class;
  TmnModuleConnectionClass = class of TmnModuleConnection;

  TmnCommand = class;

  TmnRequest = record
    Command: string;

    Module: string;
    Method: string;
    URI: string;
    Protcol: string;
    Raw: string; //Full of first line of header
  end;

  TmnModule = class;

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  TmnCommandState = (
    cmdsRespondSent, //reposnd line, first line before header
    cmdsHeaderSent,
    cmdsContentsSent,
    cmdsEnd
  );

  TmnCommandStates = set of TmnCommandState;

  { TmnCommand }

  TmnCommand = class(TmnObject)
  private
    FRequest: TmnRequest;
    FModule: TmnModule;
    FRaiseExceptions: Boolean;
    FRequestHeader: TmnFields;
    FRespondHeader: TmnFields;
    FRequestStream: TmnConnectionStream;
    FRespondStream: TmnConnectionStream;
    FStates: TmnCommandStates;
    procedure SetModule(const Value: TmnModule); virtual;
    procedure SetRequestHeader(const Value: TmnFields);
    function GetActive: Boolean;
  protected
    procedure Prepare; virtual;
    procedure Execute;
    procedure Respond; virtual;
    procedure Unprepare; virtual; //Shutdown it;

    property RequestStream: TmnConnectionStream read FRequestStream;
    property RespondStream: TmnConnectionStream read FRespondStream;
    procedure SendRespond(ALine: string); virtual;
    procedure PostHeader(AName, AValue: string); virtual;
    procedure SendHeader; virtual;
  public
    constructor Create(AModule: TmnModule; RequestStream: TmnConnectionStream = nil; RespondStream: TmnConnectionStream = nil); virtual;
    destructor Destroy; override;

    property Active: Boolean read GetActive;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmnModule read FModule write SetModule;
    property Request: TmnRequest read FRequest;
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    //Lock the server listener when execute the command
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
    FProtcol: string;
  protected
    DefaultCommand: TmnCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: string): TmnCommandClass; virtual;
    procedure Created; override;
    procedure CreateCommands; virtual;

    procedure SendHeader(ACommand: TmnCommand); virtual;

    function CreateCommand(CommandName: string; ARequest: TmnRequest; ARequestStream: TmnConnectionStream = nil; ARespondStream: TmnConnectionStream = nil): TmnCommand; overload;

    procedure ParseHeader(RequestHeader: TmnFields; Stream: TmnConnectionStream); virtual;
    procedure ParseRequest(var ARequest: TmnRequest); virtual;
    function Match(ARequest: TmnRequest): Boolean; virtual;

    procedure ExecuteCommand(CommandName: string; ARequestStream: TmnConnectionStream = nil; ARespondStream: TmnConnectionStream = nil; RequestString: TArray<String> = nil); deprecated;

  public
    constructor Create(AName: string; AModules: TmnModules; AProtcol: string); virtual;
    destructor Destroy; override;
    function Execute(ARequest: TmnRequest; ARequestStream: TmnConnectionStream = nil; ARespondStream: TmnConnectionStream = nil): Boolean;
    function RegisterCommand(vName: string; CommandClass: TmnCommandClass; ADefaultCommand: Boolean = False): Integer; overload;

    property Commands: TmnCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Params: TStringList read FParams;
    property Modules: TmnModules read FModules;
    property Protcol: string read FProtcol;
  end;

  { TmnModules }

  TmnModules = class(TmnNamedObjectList<TmnModule>)
  private
    FEndOfLine: string;
    FEOFOnError: Boolean;
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetEndOfLine(AValue: string);
    procedure SetEOFOnError(AValue: Boolean);
  protected
    function GetActive: Boolean; virtual;
    procedure Created; override;
  public
    function ParseRequest(const Request: string): TmnRequest; virtual;
    function Match(ARequest: TmnRequest): TmnModule; virtual;
    function Add(const Name: string; AModule:TmnModule): Integer; overload;

    property Active: Boolean read GetActive write SetActive;

    property EndOfLine: string read FEndOfLine write SetEndOfLine;
    property EOFOnError: Boolean read FEOFOnError write SetEOFOnError default True;
  end;

//--- Server ---

  TmnModuleServer = class;

  { TmnModuleConnection }

  TmnModuleConnection = class(TmnServerConnection)
  private
    FCommand: TmnCommand;
  public
  protected
    procedure Process; override;
  public
    destructor Destroy; override;
  published
  end;

  { TmnModuleListener }

  TmnModuleListener = class(TmnListener)
  private
    function GetServer: TmnModuleServer;
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
    property Server: TmnModuleServer read GetServer;
  public
  end;

  { TmnModuleServer }

  TmnModuleServer = class(TmnEventServer)
  private
    FModules: TmnModules;
  protected
    function DoCreateListener: TmnListener; override;
    procedure StreamCreated(AStream: TmnConnectionStream); virtual;
    procedure DoStart; override;
    procedure DoStop; override;
  public
    constructor Create;
    destructor Destroy; override;
    property Modules: TmnModules read FModules;
  end;

function ParseURI(Request: string; out URIPath: string; URIParams: TStringList): Boolean;

implementation

uses
  mnUtils;

procedure ParamsCallBack(vObject: TObject; S: string);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  Name := Copy(s, 1, p - 1);
  Value := DequoteStr(Copy(s, p + 1, MaxInt));
  (vObject as TmnFields).Add(Name, Value);
end;

function ParseURI(Request: string; out URIPath: string; URIParams: TStringList): Boolean;
var
  I, J: Integer;
  aParams: string;
begin
  I := 1;
  while (I <= Length(Request)) and (Request[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(Request)) and (Request[I] <> ' ') do
    Inc(I);

  URIPath := Copy(Request, J, I - J);

  Inc(I);
  while (I <= Length(Request)) and (Request[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(Request)) and (Request[I] <> ' ') do
    Inc(I);

  if URIPath <> '' then
    if URIPath[1] = '/' then //Not sure
      Delete(URIPath, 1, 1);

  Result := URIPath <> '';

    { Find parameters }
  J := Pos('?', URIPath);
  if J <= 0 then
    aParams := ''
  else
  begin
    aParams := Copy(URIPath, J + 1, Length(URIPath));
    URIPath := Copy(URIPath, 1, J - 1);
    if URIParams <> nil then
      StrToStringsCallback(aParams, URIParams, @ParamsCallBack);
  end;
end;

{ TmnModuleListener }

constructor TmnModuleServer.Create;
begin
  inherited;
  FModules := TmnModules.Create;
  Port := '81';
end;

destructor TmnModuleServer.Destroy;
begin
  FreeAndNil(FModules);
  inherited;
end;

destructor TmnModuleConnection.Destroy;
begin
  FreeAndNil(FCommand);
  inherited;
end;

{ TmnModuleConnection }

procedure TmnModuleConnection.Process;
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
        aRequestLine := TrimRight(Stream.ReadLine);
        aRequest := (Listener.Server as TmnModuleServer).Modules.ParseRequest(aRequestLine);
        aModule := (Listener.Server as TmnModuleServer).Modules.Match(aRequest);
        if (aModule = nil) and ((Listener.Server as TmnModuleServer).Modules.Count > 0) then
          aModule := (Listener.Server as TmnModuleServer).Modules[0]; //fall back
        if (aModule = nil) then
        begin
          Stream.Disconnect; //if failed
          raise TmnModuleException.Create('Nothing todo!');
        end;

        try
          try
            aModule.Execute(aRequest, Stream, Stream);
          finally
          end;
        except
{          if FCommand.RaiseExceptions then
            raise;}
        end;
        if Stream.Connected then
          Stream.Disconnect;
      end;
    end;
  end;
end;

function TmnModuleServer.DoCreateListener: TmnListener;
begin
  Result := TmnModuleListener.Create;
end;

procedure TmnModuleServer.StreamCreated(AStream: TmnConnectionStream);
begin
  AStream.EndOfLine := Modules.EndOfLine;
  AStream.EOFOnError := Modules.EOFOnError;
end;

procedure TmnModuleServer.DoStart;
begin
  inherited DoStart;
  Modules.Active := True;
end;

procedure TmnModuleServer.DoStop;
begin
  Modules.Active := False;
  inherited DoStop;
end;

{ TmnCustomCommandListener }

function TmnModuleListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmnModuleConnection.Create(Self, vStream);
end;

procedure TmnModuleListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.Timeout := -1;
  Server.StreamCreated(Result);
end;

function TmnModuleListener.GetServer: TmnModuleServer;
begin
  Result := inherited Server as TmnModuleServer;
end;

{ TmnCommand }

constructor TmnCommand.Create(AModule: TmnModule; RequestStream: TmnConnectionStream; RespondStream: TmnConnectionStream);
begin
  inherited Create;
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
  if cmdsHeaderSent in FStates then
    raise TmnModuleException.Create('Header is sent');
  FStates := FStates + [cmdsHeaderSent];
  Module.SendHeader(Self);
end;

procedure TmnCommand.Respond;
begin
end;

procedure TmnCommand.Unprepare;
begin
end;

procedure TmnCommand.SendRespond(ALine: string);
begin
  if cmdsRespondSent in FStates then
    raise TmnModuleException.Create('Respond is sent');
  RespondStream.WriteLine(ALine);
  FStates := FStates + [cmdsRespondSent];
end;

procedure TmnCommand.PostHeader(AName, AValue: string);
begin
  if cmdsHeaderSent in FStates then
    raise TmnModuleException.Create('Header is sent');
  RespondHeader.Add(AName, AValue);
end;

procedure TmnCommand.Execute;
begin
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

function TmnModule.CreateCommand(CommandName: string; ARequest: TmnRequest; ARequestStream: TmnConnectionStream; ARespondStream: TmnConnectionStream): TmnCommand;
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

procedure TmnModule.ParseHeader(RequestHeader: TmnFields; Stream: TmnConnectionStream);
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
end;

procedure TmnModule.CreateCommands;
begin

end;

procedure TmnModule.SendHeader(ACommand: TmnCommand);
var
  item: TmnField;
begin
  for item in ACommand.RespondHeader do
  begin
     ACommand.RespondStream.WriteLine(item.GetFullString(': '));
  end;
  ACommand.RespondStream.WriteLine(Modules.EndOfLine);
end;

procedure TmnModule.ParseRequest(var ARequest: TmnRequest);
begin
  ARequest.Command := ARequest.Method;
end;

constructor TmnModule.Create(AName: string; AModules: TmnModules; AProtcol: string);
begin
  inherited Create;
  Name := AName;
  FModules := AModules;
  FModules.Add(Self);
  FParams := TStringList.Create;
  FCommands := TmnCommandClasses.Create;
  CreateCommands;
end;

destructor TmnModule.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCommands);
  inherited;
end;

function TmnModule.Match(ARequest: TmnRequest): Boolean;
begin
  Result := SameText(Protcol, ARequest.Protcol);
end;

function TmnModule.Execute(ARequest: TmnRequest; ARequestStream: TmnConnectionStream; ARespondStream: TmnConnectionStream): Boolean;
var
  aCMD: TmnCommand;
begin
  ParseRequest(ARequest);
  aCMD := CreateCommand(ARequest.Command, ARequest, ARequestStream, ARespondStream);
  if aCMD = nil then
    raise TmnModuleException.Create('Can not find command: ' + ARequest.Command);
  try
    aCMD.Prepare;
    aCMD.Execute;
    aCMD.Unprepare;
  finally
    FreeAndNil(aCMD);
  end;
end;

procedure TmnModule.ExecuteCommand(CommandName: string; ARequestStream: TmnConnectionStream; ARespondStream: TmnConnectionStream; RequestString: TArray<String>);
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
  Result := Modules.Active; //todo
end;

function TmnModule.RegisterCommand(vName: string; CommandClass: TmnCommandClass; ADefaultCommand: Boolean): Integer;
begin
{  if Active then
    raise TmnModuleException.Create('Server is Active');}
  if FCommands.Find(vName) <> nil then
    raise TmnModuleException.Create('Command already exists: ' + vName);
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

procedure TmnModules.SetEndOfLine(AValue: string);
begin
  if FEndOfLine =AValue then
    Exit;
  if Active then
    raise TmnModuleException.Create('You can''t change EOL while server is active');
  FEndOfLine :=AValue;
end;

procedure TmnModules.SetActive(AValue: Boolean);
begin
  FActive := true;
end;

function TmnModules.GetActive: Boolean;
begin

end;

procedure TmnModules.SetEOFOnError(AValue: Boolean);
begin
  if FEOFOnError =AValue then Exit;
  if Active then
    raise TmnModuleException.Create('You can''t change EOFOnError while server is active');
  FEOFOnError :=AValue;
end;

procedure TmnModules.Created;
begin
  inherited Created;
  FEOFOnError := True;
  FEndOfLine := sWinEndOfLine; //for http protocol
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
      Result.Method := aRequests[0];
    if aRequests.Count > 1 then
      Result.URI := aRequests[1];
    if aRequests.Count > 2 then
      Result.Protcol := aRequests[2];
  finally
    aRequests.Free;
  end;
  Result.Raw := Request;
end;

function TmnModules.Match(ARequest: TmnRequest): TmnModule;
var
  item: TmnModule;
begin
  Result := nil;
  for item in Self do
  begin
    if item.Match(ARequest) then
    begin
      Result := item;
      break;
    end;
  end;
end;

end.
