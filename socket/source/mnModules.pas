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
 *
 }
 {
           userinfo       host      port
          ┌──┴───┐ ┌──────┴──────┐ ┌┴┐
  https://john.doe@www.example.com:123/forum/questions/?tag=networking&order=newest#top
  └─┬─┘   └───────────┬──────────────┘└───────┬───────┘ └───────────┬─────────────┘ └┬┘
  scheme          authority                  path                 query           fragment

  https://en.wikipedia.org/wiki/Uniform_Resource_Identifier

  REST tools
  https://resttesttest.com
  https://httpbin.org
  http://dummy.restapiexample.com/

 }

interface

uses
  SysUtils, Classes,
  mnClasses, mnStreams, mnFields,
  mnSockets, mnConnections, mnServers;

const
  cDefaultKeepAliveTimeOut = 5000; //TODO move module

type
  TmodModuleException = class(Exception);

  TmodModuleConnection = class;
  TmodModuleConnectionClass = class of TmodModuleConnection;

  { TmodParams }

  TmodParams = class(TmnFields)
  private
    FSeperator: string;
    FDelimiter: Char;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetItem(Index: Integer): TmnField;
  public
    constructor Create;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property FieldByName; default;
    property Seperator: string read FSeperator write FSeperator; //value
    property Delimiter: Char read FDelimiter write FDelimiter; //eol
    property AsString: string read GetAsString write SetAsString;
    function ReadInteger(Name: string; Def: Integer = 0): Integer;
    function ReadString(Name: string; Def: String = ''): String;
    function ReadBoolean(Name: string; Def: Boolean = False): Boolean;
    property Items[Index: Integer]: TmnField read GetItem;
  end;

  TmodCommand = class;

  TmodRequest = record
    Method: string;
    URI: utf8string;
    Protcol: string;

    Path: string;
    Module: string;
    Command: string;

    Raw: string; //Full of first line of header
  end;

  TmodModule = class;

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  TmodCommandState = (
    cmdsRespondSent, //reposnd line, first line before header
    cmdsHeaderSent,
    cmdsContentsSent,
    cmdsEnd
  );

  TmodCommandStates = set of TmodCommandState;

  TmodeResult = (
    erSuccess,
    erKeepAlive //keep the stream connection alive, not the command
  );

  TmodeResults = set of TmodeResult;

  TmodExecuteResults = record
    Status: TmodeResults;
    Timout: Integer;
  end;

  { TmodCommand }

  TmodCommand = class(TmnObject)
  private
    FModule: TmodModule;
    FRaiseExceptions: Boolean;
    FRequestHeader: TmodParams;
    FRespondHeader: TmodParams;
    FRequestStream: TmnBufferStream;
    FRespondStream: TmnBufferStream;
    FContentSize: Int64;

    FStates: TmodCommandStates;
    procedure SetModule(const Value: TmodModule); virtual;
    procedure SetRequestHeader(const Value: TmodParams);
    function GetActive: Boolean;
  protected
    Request: TmodRequest;
    procedure Prepare(var Result: TmodExecuteResults); virtual;
    procedure RespondError(ErrorNumber: Integer; ErrorMessage: string); virtual;
    procedure Respond(var Result: TmodExecuteResults); virtual;
    function Execute: TmodExecuteResults; virtual;
    procedure Unprepare(var Result: TmodExecuteResults); virtual; //Shutdown it;

    property RequestStream: TmnBufferStream read FRequestStream;
    property RespondStream: TmnBufferStream read FRespondStream;
    procedure SendRespond(ALine: string); virtual;
    //Add new header, can dublicate
    procedure PostHeader(AName, AValue: string); virtual;
    //Update header by name
    procedure SetHeader(AName, AValue: string); virtual;
    //Update header by name but adding new value to old value
    procedure PutHeader(AName, AValue: string); virtual;
    procedure SendHeader; virtual;
  public
    constructor Create(AModule: TmodModule; RequestStream: TmnBufferStream = nil; RespondStream: TmnBufferStream = nil); virtual;
    destructor Destroy; override;

    property Active: Boolean read GetActive;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmodModule read FModule write SetModule;
    //Lock the server listener when execute the command
    //Prepare called after created in lucking mode
    property RequestHeader: TmodParams read FRequestHeader write SetRequestHeader;
    property RespondHeader: TmodParams read FRespondHeader;
    property ContentSize: Int64 read FContentSize write FContentSize; //todo
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    property States: TmodCommandStates read FStates;
  end;

  TmodCommandClass = class of TmodCommand;

  TmodCommandClassItem = class(TmnNamedObject)
  private
    FCommandClass: TmodCommandClass;
  public
    property CommandClass: TmodCommandClass read FCommandClass;
  end;

  { TmodCommandClasses }

  TmodCommandClasses = class(TmnNamedObjectList<TmodCommandClassItem>)
  private
  public
    function Add(const Name: string; CommandClass: TmodCommandClass): Integer;
  end;

  {
    Module will do simple protocol before execute command
    Module have protocol name must match when parse request, before selecting
  }

  TmodModules = class;

  { TmodModule }

  TmodModule = class(TmnNamedObject)
  private
    FCommands: TmodCommandClasses;
    FKeepAliveTimeOut: Integer;
    FModules: TmodModules;
    FParams: TStringList;
    FProtcol: string;
    FUseKeepAlive: Boolean;
  protected
    DefaultCommand: TmodCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: string): TmodCommandClass; virtual;
    procedure Created; override;
    procedure CreateCommands; virtual;

    procedure SendHeader(ACommand: TmodCommand); virtual;

    function CreateCommand(CommandName: string; ARequest: TmodRequest; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil): TmodCommand; overload;

    procedure ParseHeader(RequestHeader: TmodParams; Stream: TmnBufferStream); virtual;
    procedure ParseRequest(var ARequest: TmodRequest; ACommand: TmodCommand = nil); virtual;
    function Match(var ARequest: TmodRequest): Boolean; virtual;
  public
    constructor Create(AName: string; AProtcol: string; AModules: TmodModules); virtual;
    destructor Destroy; override;
    function Execute(ARequest: TmodRequest; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil): TmodExecuteResults;
    procedure ExecuteCommand(CommandName: string; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil; RequestString: TArray<String> = nil);
    function RegisterCommand(vName: string; CommandClass: TmodCommandClass; ADefaultCommand: Boolean = False): Integer; overload;

    property Commands: TmodCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Params: TStringList read FParams;
    property Modules: TmodModules read FModules;
    property Protcol: string read FProtcol;
    property KeepAliveTimeOut: Integer read FKeepAliveTimeOut write FKeepAliveTimeOut;
    property UseKeepAlive: Boolean read FUseKeepAlive write FUseKeepAlive default False;
  end;

  { TmodModules }

  TmodModules = class(TmnNamedObjectList<TmodModule>)
  private
    FEOFOnError: Boolean;
    FActive: Boolean;
    FEndOfLine: string;
    FDefaultModule: TmodModule;
    procedure SetActive(AValue: Boolean);
    procedure SetEndOfLine(AValue: string);
  protected
    function GetActive: Boolean; virtual;
    procedure Created; override;
  public
    function ParseRequest(const Request: string): TmodRequest; virtual;
    function Match(var ARequest: TmodRequest): TmodModule; virtual;

    function Add(const Name: string; AModule:TmodModule): Integer; overload;

    property Active: Boolean read GetActive write SetActive;
    property EndOfLine: string read FEndOfLine write SetEndOfLine; //TODO move to module
    property DefaultModule: TmodModule read FDefaultModule write FDefaultModule;
  end;

//--- Server ---

  TmodModuleServer = class;

  { TmodModuleConnection }

  TmodModuleConnection = class(TmnServerConnection)
  private
  public
  protected
    procedure Process; override;
  public
    destructor Destroy; override;
  published
  end;

  { TmodModuleListener }

  TmodModuleListener = class(TmnListener)
  private
    function GetServer: TmodModuleServer;
  protected
    function DoCreateConnection(vStream: TmnConnectionStream): TmnConnection; override;
    procedure DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket); override;
    property Server: TmodModuleServer read GetServer;
  public
  end;

  { TmodModuleServer }

  TmodModuleServer = class(TmnEventServer)
  private
    FModules: TmodModules;
  protected
    function DoCreateListener: TmnListener; override;
    procedure StreamCreated(AStream: TmnBufferStream); virtual;
    procedure DoBeforeOpen; override;
    procedure DoAfterClose; override;
    function CreateModules: TmodModules; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Modules: TmodModules read FModules;
  end;

function ParseURI(Request: string; out URIPath: string; URIParams: TmodParams): Boolean;
procedure ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TmodParams);

implementation

uses
  mnUtils;

procedure ParamsCallBack(Sender: Pointer; Index:Integer; S: string; var Resume: Boolean);
var
  Name, Value: string;
  p: Integer;
begin
  p := pos('=', s);
  if p >= 0 then
  begin
    Name := Copy(s, 1, p - 1);
    Value := DequoteStr(Copy(s, p + 1, MaxInt));
  end
  else
  begin
    Name := S;
    Value := '';
  end;
  (TObject(Sender) as TmodParams).Add(Name, Value);
end;

function ParseURI(Request: string; out URIPath: string; URIParams: TmodParams): Boolean;
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
      StrToStringsCallback(aParams, URIParams, @ParamsCallBack, ['&'], [' ']);
  end;
end;

procedure ParsePath(aRequest: string; out Name: string; out URIPath: string; URIParams: TmodParams);
begin
  ParseURI(aRequest, URIPath, URIParams);
  Name := SubStr(URIPath, '/', 0);
  URIPath := Copy(URIPath, Length(Name) + 1, MaxInt);
end;

{ TmodModuleListener }

constructor TmodModuleServer.Create;
begin
  inherited;
  FModules := CreateModules;
  Port := '81';
end;

function TmodModuleServer.CreateModules: TmodModules;
begin
  Result := TmodModules.Create
end;

destructor TmodModuleServer.Destroy;
begin
  FreeAndNil(FModules);
  inherited;
end;

destructor TmodModuleConnection.Destroy;
begin
  inherited;
end;

{ TmodModuleConnection }

procedure TmodModuleConnection.Process;
var
  aRequestLine: string;
  aRequest: TmodRequest;
  aModule: TmodModule;
  Result: TmodExecuteResults;
begin
  inherited;
  aRequestLine := TrimRight(Stream.ReadLineRawByte);
  if Connected and (aRequestLine <> '') then //aRequestLine empty when timeout but not disconnected
  begin
    aRequest := (Listener.Server as TmodModuleServer).Modules.ParseRequest(aRequestLine);
    aModule := (Listener.Server as TmodModuleServer).Modules.Match(aRequest);
    if (aModule = nil) and ((Listener.Server as TmodModuleServer).Modules.Count > 0) then
      aModule := (Listener.Server as TmodModuleServer).Modules.DefaultModule; //fall back

    if (aModule = nil) then
    begin
      Stream.Disconnect; //if failed
    end
    else
    try
      if aModule <> nil then
        Result := aModule.Execute(aRequest, Stream, Stream);
    finally
    end;

    if Stream.Connected then
    begin
      if (erKeepAlive in Result.Status) then
        Stream.Timeout := Result.Timout
      else
        Stream.Disconnect;
    end;
  end;
end;

function TmodModuleServer.DoCreateListener: TmnListener;
begin
  Result := TmodModuleListener.Create;
end;

procedure TmodModuleServer.StreamCreated(AStream: TmnBufferStream);
begin
  AStream.EndOfLine := Modules.EndOfLine;
  //AStream.EOFOnError := Modules.EOFOnError;
end;

procedure TmodModuleServer.DoBeforeOpen;
begin
  inherited;
  Modules.Active := True;
end;

procedure TmodModuleServer.DoAfterClose;
begin
  Modules.Active := False;
  inherited;
end;

{ TmodCustomCommandListener }

function TmodModuleListener.DoCreateConnection(vStream: TmnConnectionStream): TmnConnection;
begin
  Result := TmodModuleConnection.Create(Self, vStream);
end;

procedure TmodModuleListener.DoCreateStream(var Result: TmnConnectionStream; vSocket: TmnCustomSocket);
begin
  inherited;
  Result.Timeout := -1;
  Server.StreamCreated(Result);
end;

function TmodModuleListener.GetServer: TmodModuleServer;
begin
  Result := inherited Server as TmodModuleServer;
end;

{ TmodCommand }

constructor TmodCommand.Create(AModule: TmodModule; RequestStream: TmnBufferStream; RespondStream: TmnBufferStream);
begin
  inherited Create;
  FModule := AModule;
  FRequestStream := RequestStream; //do not free
  FRespondStream := FRespondStream; //do not free

  FRequestHeader := TmodParams.Create;
  FRespondHeader := TmodParams.Create;
end;

destructor TmodCommand.Destroy;
begin
  FreeAndNil(FRequestHeader);
  FreeAndNil(FRespondHeader);
  inherited;
end;

procedure TmodCommand.Prepare(var Result: TmodExecuteResults);
begin
  Module.ParseRequest(Request, Self);
end;

procedure TmodCommand.SendHeader;
begin
  if not (cmdsRespondSent in FStates) then
    raise TmodModuleException.Create('Respond line not sent');
  if cmdsHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');
  FStates := FStates + [cmdsHeaderSent];
  Module.SendHeader(Self);
end;

procedure TmodCommand.Respond(var Result: TmodExecuteResults);
begin
end;

procedure TmodCommand.RespondError(ErrorNumber: Integer; ErrorMessage: string);
begin
end;

procedure TmodCommand.Unprepare(var Result: TmodExecuteResults);
begin
end;

procedure TmodCommand.SendRespond(ALine: string);
begin
  if cmdsRespondSent in FStates then
    raise TmodModuleException.Create('Respond is sent');
  RespondStream.WriteLineUTF8(ALine);
  FStates := FStates + [cmdsRespondSent];
end;

procedure TmodCommand.PostHeader(AName, AValue: string);
begin
  if cmdsHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');
  RespondHeader.Add(AName, AValue);
end;

function TmodCommand.Execute: TmodExecuteResults;
begin
  {$ifdef DEBUG_MODE}
//    Server.Listener.Log(Connection, GetCommandName + ': Started on port ' + Server.Port);
  try
  {$endif}
    Result.Status := []; //default to be not keep alive, not sure, TODO
    Prepare(Result);
    Respond(Result);
    Unprepare(Result);
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

function TmodCommand.GetActive: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmodCommand.PutHeader(AName, AValue: string);
begin
  if cmdsHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');
  RespondHeader.Put(AName, AValue);
end;

procedure TmodCommand.SetHeader(AName, AValue: string);
begin
  if cmdsHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');
  RespondHeader.Put(AName, AValue);
end;

procedure TmodCommand.SetModule(const Value: TmodModule);
begin
  FModule := Value;
end;

procedure TmodCommand.SetRequestHeader(const Value: TmodParams);
begin
  if FRequestHeader <> Value then
  begin
    FreeAndNil(FRequestHeader);
    FRequestHeader := Value;
  end;
end;

function TmodCommandClasses.Add(const Name: string; CommandClass: TmodCommandClass): Integer;
var
  aItem: TmodCommandClassItem;
begin
  aItem := TmodCommandClassItem.Create;
  aItem.Name := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

{ TmodModule }

function TmodModule.CreateCommand(CommandName: string; ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmodCommand;
var
  aClass: TmodCommandClass;
begin
  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self);
    Result.FModule := Self;
    Result.Request := ARequest;
    Result.FRequestStream := ARequestStream;
    Result.FRespondStream := ARespondStream;
  end
  else
    Result := nil;
  if Result <> nil then
    ParseHeader(Result.RequestHeader, ARequestStream);
end;

function TmodModule.GetCommandClass(var CommandName: string): TmodCommandClass;
var
  aItem: TmodCommandClassItem;
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

procedure TmodModule.ParseHeader(RequestHeader: TmodParams; Stream: TmnBufferStream);
var
  line: string;
begin
  if Stream <> nil then
  begin
    while not Stream.Done do
    begin
      line := Stream.ReadLineRawByte;
      if line = '' then
        break
      else
      begin
        RequestHeader.AddItem(line, ':', true);
      end;
    end;
  end;
end;

procedure TmodModule.Created;
begin
  inherited;
end;

procedure TmodModule.CreateCommands;
begin

end;

procedure TmodModule.SendHeader(ACommand: TmodCommand);
var
  item: TmnField;
  s: string;
begin
  for item in ACommand.RespondHeader do
  begin
    s := item.GetFullString(': ');
    //WriteLn(s);
    ACommand.RespondStream.WriteLineUTF8(S);
  end;
  ACommand.RespondStream.WriteLineUTF8(UTF8String(''));
end;

procedure TmodModule.ParseRequest(var ARequest: TmodRequest; ACommand: TmodCommand);
begin
  ARequest.Command := ARequest.Method;
end;

constructor TmodModule.Create(AName: string; AProtcol: string; AModules: TmodModules);
begin
  inherited Create;
  Name := AName;
  FModules := AModules;
  FModules.Add(Self);
  FParams := TStringList.Create;
  FCommands := TmodCommandClasses.Create;
  FKeepAliveTimeOut := cDefaultKeepAliveTimeOut; //TODO move module
  CreateCommands;
end;

destructor TmodModule.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCommands);
  inherited;
end;

function TmodModule.Match(var ARequest: TmodRequest): Boolean;
begin
  Result := SameText(Protcol, ARequest.Protcol);
end;

function TmodModule.Execute(ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmodExecuteResults;
var
  aCMD: TmodCommand;
begin
  Result.Status := [erSuccess];
  ParseRequest(ARequest);
  aCMD := CreateCommand(ARequest.Command, ARequest, ARequestStream, ARespondStream);
  if aCMD = nil then
    raise TmodModuleException.Create('Can not find command: ' + ARequest.Command);
  try
    try
      Result := aCMD.Execute;
      Result.Status := Result.Status + [erSuccess];
    except
      on E: Exception do
        aCMD.RespondError(500, E.Message);
    end;
  finally
    FreeAndNil(aCMD);
  end;
end;

procedure TmodModule.ExecuteCommand(CommandName: string; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; RequestString: TArray<String>);
var
  ARequest: TmodRequest;
begin
  Finalize(ARequest);
  ARequest.Command := CommandName;
  Execute(ARequest, ARequestStream, ARespondStream);
end;

function TmodModule.GetActive: Boolean;
begin
  Result := Modules.Active; //todo
end;

function TmodModule.RegisterCommand(vName: string; CommandClass: TmodCommandClass; ADefaultCommand: Boolean): Integer;
begin
{  if Active then
    raise TmodModuleException.Create('Server is Active');}
  if FCommands.Find(vName) <> nil then
    raise TmodModuleException.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
  if ADefaultCommand then
    DefaultCommand := CommandClass;
end;

{ TmodModules }

function TmodModules.Add(const Name: string; AModule:TmodModule): Integer;
begin
  AModule.Name := Name;
  Result := inherited Add(AModule);
end;

procedure TmodModules.SetEndOfLine(AValue: string);
begin
  if FEndOfLine =AValue then
    Exit;
{  if Active then
    raise TmodModuleException.Create('You can''t change EOL while server is active');}
  FEndOfLine :=AValue;
end;

procedure TmodModules.SetActive(AValue: Boolean);
begin
  FActive := true;
end;

function TmodModules.GetActive: Boolean;
begin
  Result := True;
end;

procedure TmodModules.Created;
begin
  inherited;
  FEOFOnError := True;
  FEndOfLine := sWinEndOfLine; //for http protocol
end;

function TmodModules.ParseRequest(const Request: string): TmodRequest;
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

function TmodModules.Match(var ARequest: TmodRequest): TmodModule;
var
  item: TmodModule;
  SaveRequest: TmodRequest;
begin
  Result := nil;
  SaveRequest := ARequest;
  for item in Self do
  begin
    if item.Match(ARequest) then
    begin
      Result := item;
      break;
    end;
    ARequest := SaveRequest;
  end;
end;

{ TmodParams }

function TmodParams.GetAsString: string;
var
  item: TmnField;
begin
  Result := '';
  for item in Self do
  begin
    if Result <> '' then
      Result := Result + Delimiter;
    Result := Result + Item.Name + Seperator + ' ' + Item.AsString;
  end;
end;

function TmodParams.GetItem(Index: Integer): TmnField;
begin
  Result := (inherited GetItem(Index)) as TmnField;
end;

procedure TmodParams.LoadFromStream(Stream: TStream);
var
  Strings: TStringList;
  Line: string;
begin
  Strings := TStringList.Create;
  try
    Strings.LoadFromStream(Stream);
    Clear;
    for line in Strings do
    begin
      AddItem(Line, Strings.NameValueSeparator, True);
    end;
  finally
    Strings.Free;
  end;
end;

procedure TmodParams.SaveToStream(Stream: TStream);
var
  Strings: TStringList;
  i: Integer;
begin
  Strings := TStringList.Create;
  try
    for i := 0 to Count - 1 do
      Strings.Add(Self.Items[i].GetFullString);
    Strings.SaveToStream(Stream);
  finally
    Strings.Free;
  end;
end;

procedure TmodParams.SetAsString(const Value: string);
begin
  StrToStringsCallback(Value, Self, @ParamsCallBack, [Self.Delimiter], [' ']);
end;

constructor TmodParams.Create;
begin
  inherited Create;
  Seperator := '=';
  Delimiter := #13;
end;

function TmodParams.ReadInteger(Name: string; Def: Integer): Integer;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsInteger
  else
    Result := Def;
end;

function TmodParams.ReadString(Name: string; Def: String): String;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsString
  else
    Result := Def;
end;

function TmodParams.ReadBoolean(Name: string; Def: Boolean): Boolean;
var
  Field: TmnField;
begin
  Field := FindField(Name);
  if Field <> nil then
    Result := Field.AsBoolean
  else
    Result := Def;
end;

end.
