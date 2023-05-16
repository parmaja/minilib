unit mnModules;

{$M+}{$H+}
{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *  https://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html
 *
 *
 }
 {  HEAD:
              userinfo       host      port
              ┌──┴───┐ ┌──────┴──────┐ ┌┴┐
  GET https://john.doe@www.example.com:123/forum/questions/?tag=networking&order=newest#top HTTP/1.1
  └┬┘    └─┬─┘   └───────────┬───────────┘└───────┬───────┘└───────────┬─────────────┘ └┬─┘ └─────┬┘
  method scheme          authority               path                query             fragment   protocol
  └┬┘                                     └──┬──┘
  Command                                  Module                     Params

  https://en.wikipedia.org/wiki/Uniform_Resource_Identifier

  REST tools
  https://resttesttest.com
  https://httpbin.org
  http://dummy.restapiexample.com/

 }

interface

uses
  SysUtils, Classes, StrUtils, Types,
  mnClasses, mnStreams, mnFields, mnParams,
  mnSockets, mnConnections, mnServers;

const
  cDefaultKeepAliveTimeOut = 5000; //TODO move module
  URLPathDelim  = '/';

type
  TmodModuleException = class(Exception);

  TmodModuleConnection = class;
  TmodModuleConnectionClass = class of TmodModuleConnection;

  TmodCommand = class;

  { TmnHeaderField }

  TmnHeaderField = class(TmnParam)
    function GetFullString: String; override;
  end;

  { TmnHeader }

  TmnHeader = class(TmnParams)
  protected
    function CreateField: TmnField; override;
  public
    constructor Create;
  end;

  TmodRequestInfo = record
    Raw: String; //Full of first line of header

    //from raw :) raw = Method + URI + Protocol
    Method: Utf8string;
    Protocol: Utf8string;
    URI: Utf8string;

    //from URI :) URI = Address + Query
    Address: Utf8string;
    Query: Utf8string;

    Path: Utf8string;


    Module: String;
    Command: String;
    Client: String;
  end;

  { TmodRequest }

  TmodRequest = class(TObject)
  private
    FHeader: TmnHeader;
    FStream: TmnBufferStream;
    FContentLength: Integer;
    procedure SetRequestHeader(AValue: TmnHeader);
  protected
    Info: TmodRequestInfo;
  public
    constructor Create(AStream: TmnBufferStream);
    destructor Destroy; override;
    procedure  Clear;
    property Raw: String read Info.Raw write Info.Raw;

    //from raw
    property Method: Utf8string read Info.Method write Info.Method;
    property URI: Utf8string read Info.URI write Info.URI;
    property Protocol: Utf8string read Info.Protocol write Info.Protocol;
    property Address: Utf8string read Info.Address write Info.Address;
    property Query: Utf8string read Info.Query write Info.Query;
    //for module
    property Path: Utf8string read Info.Path write Info.Path;
    property Module: String read Info.Module write Info.Module;
    property Command: String read Info.Command write Info.Command;
    property Client: String read Info.Client write Info.Client;

    property ContentLength: Integer read FContentLength write FContentLength;
    property Stream: TmnBufferStream read FStream write FStream;
    property Header: TmnHeader read FHeader write SetRequestHeader;
    function CollectURI: string;
  end;

  TmodeResult = (
    erSuccess,
    erKeepAlive //keep the stream connection alive, not the command
    );

  TmodeResults = set of TmodeResult;

  TmodRespondResult = record
    Status: TmodeResults;
    Timout: Integer;
  end;

  TmodRespondState = (
    resHeadSent, //reposnd line, first line before header
    resHeaderSent,
    resContentsSent,
    //resSuccess,
    //resKeepAlive,
    resEnd
    );

  TmodRespondStates = set of TmodRespondState;

  { TmodRespond }

  TmodRespond = class(TObject)
  private
    FStates: TmodRespondStates;
    FHeader: TmnHeader;
    //FRespondResult: TmodRespondResult;
    FStream: TmnBufferStream;
  protected
    function HeadText: string; virtual;
    procedure DoSendHeader; virtual;
    procedure DoHeaderSent; virtual;
    procedure SendHead;
  public
    constructor Create;
    destructor Destroy; override;
    property Header: TmnHeader read FHeader;
    property Stream: TmnBufferStream read FStream write FStream;
    property States: TmodRespondStates read FStates;
    //property RespondResult: TmodRespondResult read FRespondResult;

    //Add new header, can dublicate
    procedure AddHeader(AName, AValue: String); virtual;
    //Update header by name but adding new value to old value
    procedure PutHeader(AName, AValue: String);

    procedure SendHeader;
  end;

  TmodModule = class;

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  { TmodCommand }

  TmodCommand = class(TmnObject)
  private
    FModule: TmodModule;
    FRespond: TmodRespond;
    FRequest: TmodRequest;
    FRaiseExceptions: Boolean;
    FContentSize: Int64;

    procedure SetModule(const Value: TmodModule); virtual;
    function GetActive: Boolean;
  protected
    procedure Prepare(var Result: TmodRespondResult); virtual;
    procedure RespondResult(var Result: TmodRespondResult); virtual;
    function Execute: TmodRespondResult; virtual;
    procedure Unprepare(var Result: TmodRespondResult); virtual; //Shutdown it;
    function CreateRespond: TmodRespond; virtual;

    procedure Log(S: String); virtual;
  public
    constructor Create(AModule: TmodModule; ARequest: TmodRequest; RequestStream: TmnBufferStream = nil; RespondStream: TmnBufferStream = nil); virtual;
    destructor Destroy; override;

    property Active: Boolean read GetActive;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmodModule read FModule write SetModule;
    //Lock the server listener when execute the command
    //Prepare called after created in lucking mode
    property ContentSize: Int64 read FContentSize write FContentSize; //todo
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    property Request: TmodRequest read FRequest;
    property Respond: TmodRespond read FRespond;

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
    function Add(const Name: String; CommandClass: TmodCommandClass): Integer;
  end;

  {
    Module will do simple protocol before execute command
    Module have protocol name must match when parse request, before selecting
  }

  TmodModules = class;

  { TmodModule }

  TmodModule = class(TmnNamedObject)
  private
    FAliasName: String;
    FCommands: TmodCommandClasses;
    FKeepAliveTimeOut: Integer;
    FModules: TmodModules;
    FParams: TStringList;
    FProtocols: TArray<String>;
    FUseKeepAlive: Boolean;
    FUseCompressing: Boolean;
    procedure SetAliasName(AValue: String);
  protected
    FFallbackCommand: TmodCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: String): TmodCommandClass; virtual;
    procedure Created; override;
    procedure DoCreateCommands; virtual;
    procedure CreateCommands;

    function CreateCommand(CommandName: String; ARequest: TmodRequest; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil): TmodCommand; overload;

    procedure ParseHeader(RequestHeader: TmnParams; Stream: TmnBufferStream); virtual;
    procedure ParseHead(ARequest: TmodRequest); virtual;
    function RequestCommand(ARequest: TmodRequest; ARequestStream, ARespondStream: TmnBufferStream): TmodCommand; virtual;
    function Match(const ARequest: TmodRequest): Boolean; virtual;
    procedure Log(S: String); virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Reload; virtual;
    procedure Init; virtual;
    procedure Idle; virtual;
    procedure InternalError(ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; var Handled: Boolean); virtual;
  public
    constructor Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules); virtual;
    destructor Destroy; override;
    function Execute(ARequest: TmodRequest; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil): TmodRespondResult;
    procedure ExecuteCommand(CommandName: String; ARequestStream: TmnBufferStream = nil; ARespondStream: TmnBufferStream = nil; RequestString: TArray<String> = nil);
    function RegisterCommand(vName: String; CommandClass: TmodCommandClass; AFallback: Boolean = False): Integer; overload;

    property Commands: TmodCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Params: TStringList read FParams;
    property Modules: TmodModules read FModules;
    property Protocols: TArray<String> read FProtocols;
    property KeepAliveTimeOut: Integer read FKeepAliveTimeOut write FKeepAliveTimeOut;
    property UseKeepAlive: Boolean read FUseKeepAlive write FUseKeepAlive default False;
    property UseCompressing: Boolean read FUseCompressing write FUseCompressing;
    property AliasName: String read FAliasName write SetAliasName;
  end;

  TmodModuleClass = class of TmodModule;

  TmodModuleServer = class;

  { TmodModules }

  TmodModules = class(TmnNamedObjectList<TmodModule>)
  private
    FEOFOnError: Boolean;
    FActive: Boolean;
    FEndOfLine: String;
    FDefaultModule: TmodModule;
    FDefaultProtocol: String;
    FInit: Boolean;
    FServer: TmodModuleServer;
    procedure SetEndOfLine(AValue: String);
  protected
    function GetActive: Boolean; virtual;
    procedure Created; override;
    procedure Start;
    procedure Stop;
    procedure Init;
    procedure Idle;
    property Server: TmodModuleServer read FServer;
  public
    constructor Create(AServer: TmodModuleServer);
    procedure ParseHead(ARequest: TmodRequest; const RequestLine: String); virtual;
    function Match(ARequest: TmodRequest): TmodModule; virtual;
    property DefaultProtocol: String read FDefaultProtocol write FDefaultProtocol;

    function  Find<T: Class>: T; overload;
    function  Find(const ModuleClass: TmodModuleClass): TmodModule; overload;

    function Add(const Name, AliasName: String; AModule: TmodModule): Integer; overload;
    procedure Log(S: String); virtual;

    property Active: Boolean read GetActive;
    property EndOfLine: String read FEndOfLine write SetEndOfLine; //TODO move to module
    property DefaultModule: TmodModule read FDefaultModule write FDefaultModule;
  end;

  //--- Server ---

  { TmodModuleConnection }

  TmodModuleConnection = class(TmnServerConnection)
  private
  protected
    procedure Process; override;
    procedure Prepare; override;
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
    procedure DoStart; override;
    procedure DoStop; override;
    procedure DoIdle; override;
    function Module<T: class>: T;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Modules: TmodModules read FModules;
  end;

  { TmnFieldHelper }

  TmnFieldHelper = class helper for TmnField
  public
    function Have(AValue: String; vSeperators: TSysCharSet = [';']): Boolean;
  end;

function ParseRaw(const Raw: String; out Method, Protocol, URI: Utf8string): Boolean;
function ParseURI(const URI: String; out Address, Params: Utf8string): Boolean;
procedure ParseParams(const Params: String; mnParams: TmnParams);


function ParseAddress(const Request: String; out URIPath: Utf8string; out URIQuery: Utf8string): Boolean; overload;
function ParseAddress(const Request: String; out URIPath: Utf8string; out URIParams: Utf8string; URIQuery: TmnParams): Boolean; overload;
procedure ParsePath(const aRequest: String; out Name: String; out URIPath: Utf8string; out URIParams: Utf8string; URIQuery: TmnParams);

implementation

uses
  mnUtils;

function ParseRaw(const Raw: String; out Method, Protocol, URI: Utf8string): Boolean;
var
  aRequests: TStringList;
begin
  aRequests := TStringList.Create;
  try
    StrToStrings(Raw, aRequests, [' '], []);
    if aRequests.Count > 0 then
      Method := aRequests[0];
    if aRequests.Count > 1 then
      URI := aRequests[1];
    if aRequests.Count > 2 then
      Protocol := aRequests[2];
  finally
    FreeAndNil(aRequests);
  end;
  Result := True;
end;

function ParseURI(const URI: String; out Address, Params: Utf8string): Boolean;
var
  I, J: Integer;
begin
  J := Pos('?', URI);
  if J > 0 then
  begin
    Address := Copy(URI, 1, J - 1);
    Params := Copy(URI, J + 1, Length(URI));
  end
  else
  begin
    Address := URI;
    Params := '';
  end;
end;

procedure ParseParams(const Params: String; mnParams: TmnParams);
begin
  StrToStringsCallback(Params, mnParams, @ParamsCallBack, ['&'], [' ']);
end;

function ParseAddress(const Request: String; out URIPath: Utf8string; out URIQuery: Utf8string): Boolean;
var
  I, J: Integer;
begin
  {if Request <> '' then
    if Request[1] = URLPathDelim then //Not sure
      Delete(Request, 1, 1);}

  I := 1;
  while (I <= Length(Request)) and (Request[I] = ' ') do
    Inc(I);
  J := I;
  while (I <= Length(Request)) and (Request[I] <> ' ') do
    Inc(I);

  URIPath := Copy(Request, J, I - J);

  if URIPath <> '' then
    if URIPath[1] = URLPathDelim then //Not sure
      Delete(URIPath, 1, 1);

  Result := URIPath <> '';

  { Find parameters }
  {belal taskeej getting params in case of params has space}
  J := Pos('?', URIPath);
  if J > 0 then
  begin
    URIQuery := Copy(Request, J + 1, Length(Request));
    URIPath := Copy(URIPath, 1, J - 1);
  end
  else
  begin
    URIQuery := '';
  end;
end;

function ParseAddress(const Request: String; out URIPath: Utf8string; out URIParams: Utf8string; URIQuery: TmnParams): Boolean;
begin
  Result := ParseAddress(Request, URIPath, URIParams);
  if Result then
    if URIQuery <> nil then
      //ParseParams(aParams, False);
      StrToStringsCallback(URIParams, URIQuery, @ParamsCallBack, ['&'], [' ']);
end;

procedure ParsePath(const aRequest: String; out Name: String; out URIPath: Utf8string; out URIParams: Utf8string; URIQuery: TmnParams);
begin
  ParseAddress(aRequest, URIPath, URIParams, URIQuery);
  Name := SubStr(URIPath, URLPathDelim, 0);
  URIPath := Copy(URIPath, Length(Name) + 1, MaxInt);
end;

{ TmodRespond }

constructor TmodRespond.Create;
begin
  inherited;
  FHeader := TmnHeader.Create;
end;

destructor TmodRespond.Destroy;
begin
  FreeAndNil(FHeader);
  inherited;
end;

procedure TmodRespond.DoHeaderSent;
begin

end;

procedure TmodRespond.DoSendHeader;
begin
end;

function TmodRespond.HeadText: string;
begin
  Result := '';
end;

procedure TmodRespond.AddHeader(AName, AValue: String);
begin
  if resContentsSent in FStates then
    raise TmodModuleException.Create('Content is sent');

  Header.Add(AName, AValue);
end;

procedure TmodRespond.PutHeader(AName, AValue: String);
begin
  if resHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');

  Header.Put(AName, AValue);
end;

procedure TmodRespond.SendHeader;
var
  item: TmnField;
  s: String;
begin
  if resHeaderSent in FStates then
    raise TmodModuleException.Create('Header is sent');

  SendHead;

  for item in Header do
  begin
    s := item.GetNameValue(': ');
    //WriteLn(s);
    Stream.WriteLineUTF8(S);
  end;

  DoSendHeader; //enter after

  Stream.WriteLineUTF8(Utf8string(''));
  FStates := FStates + [resHeaderSent];

  DoHeaderSent;

  {s := '';
  for item in Header do
  begin
    s := s + item.GetNameValue(': ')+Stream.EndOfLine;
  end;
 // s := s + Stream.EndOfLine;
  Stream.WriteLineUTF8(Utf8string(s));}
end;

procedure TmodRespond.SendHead;
begin
  if resHeadSent in FStates then
    raise TmodModuleException.Create('Respond is sent');
  if HeadText='' then
    raise TmodModuleException.Create('head not set');

  Stream.WriteLineUTF8(HeadText);
  FStates := FStates + [resHeadSent];
end;

{ TmodRequest }

procedure TmodRequest.SetRequestHeader(AValue: TmnHeader);
begin
  if FHeader <> AValue then
  begin
    FreeAndNil(FHeader);
    FHeader := AValue;
  end;
end;

function TmodRequest.CollectURI: string;
begin
  if Query<>'' then
    Result := Address+'?'+Query
  else
    Result := Address;
end;

constructor TmodRequest.Create(AStream: TmnBufferStream);
begin
  inherited Create;
  FHeader := TmnHeader.Create;
  FStream := AStream;
end;

destructor TmodRequest.Destroy;
begin
  FreeAndNil(FHeader);
  inherited Destroy;
end;

procedure TmodRequest.Clear;
begin
  Initialize(Info);
end;

{ TmnHeaderField }

function TmnHeaderField.GetFullString: String;
begin
  Result := GetNameValue(': ');
end;

{ TmnHeader }

constructor TmnHeader.Create;
begin
  inherited Create;
  AutoRemove := True;
end;

function TmnHeader.CreateField: TmnField;
begin
  Result := TmnHeaderField.Create;
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
  Result := TmodModules.Create(Self);
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

procedure TmodModuleConnection.Prepare;
begin
  inherited;
  (Listener.Server as TmodModuleServer).Modules.Init;
end;

procedure TmodModuleConnection.Process;
var
  aRequestLine: String;
  aRequest: TmodRequest;
  aModule: TmodModule;
  Result: TmodRespondResult;
begin
  inherited;
  aRequestLine := TrimRight(Stream.ReadLineUTF8);
  if Connected and (aRequestLine <> '') then //aRequestLine empty when timeout but not disconnected
  begin
    aRequest := TmodRequest.Create(Stream);
    try
      (Listener.Server as TmodModuleServer).Modules.ParseHead(aRequest, aRequestLine);
      aModule := (Listener.Server as TmodModuleServer).Modules.Match(aRequest);

      if (aModule = nil) and ((Listener.Server as TmodModuleServer).Modules.Count > 0) then
        aModule := (Listener.Server as TmodModuleServer).Modules.DefaultModule; //fall back

      if (aModule = nil) then
      begin
        Stream.Disconnect; //if failed
      end
      else
        try
          aRequest.Client := RemoteIP;
          Result := aModule.Execute(aRequest, Stream, Stream);
        finally
        end;
    finally
      FreeAndNil(aRequest); //if create command then aRequest change to nil
    end;

    if Stream.Connected then
    begin
      if (erKeepAlive in Result.Status) then
        Stream.ReadTimeout := Result.Timout
      else
        Stream.Disconnect;
    end;
  end;
end;

function TmodModuleServer.DoCreateListener: TmnListener;
begin
  Result := TmodModuleListener.Create;
end;

procedure TmodModuleServer.DoIdle;
begin
  inherited;
  Modules.Idle;
end;

procedure TmodModuleServer.DoStart;
begin
  inherited;
  Modules.Start;
end;

procedure TmodModuleServer.DoStop;
begin
  if Modules <> nil then
    Modules.Stop;
  inherited;
end;

function TmodModuleServer.Module<T>: T;
var
  i: Integer;
begin
  if Modules<>nil then
    for I := 0 to Modules.Count-1 do
      if Modules[i] is T then
        Exit(Modules[i] as T);

  Result := nil;
end;

procedure TmodModuleServer.StreamCreated(AStream: TmnBufferStream);
begin
  AStream.EndOfLine := Modules.EndOfLine;
  //AStream.EOFOnError := Modules.EOFOnError;
end;

procedure TmodModuleServer.DoBeforeOpen;
begin
  inherited;
end;

procedure TmodModuleServer.DoAfterClose;
begin
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
  Result.ReadTimeout := -1;
  Server.StreamCreated(Result);
end;

function TmodModuleListener.GetServer: TmodModuleServer;
begin
  Result := inherited Server as TmodModuleServer;
end;

{ TmodCommand }

constructor TmodCommand.Create(AModule: TmodModule; ARequest: TmodRequest; RequestStream: TmnBufferStream; RespondStream: TmnBufferStream);
begin
  inherited Create;
  FModule := AModule;
  FRequest := ARequest; //do not free

  FRespond := CreateRespond;
  FRespond.Stream := RequestStream;
end;

destructor TmodCommand.Destroy;
begin
  FreeAndNil(FRespond);
  inherited;
end;

procedure TmodCommand.Prepare(var Result: TmodRespondResult);
begin
end;

procedure TmodCommand.Log(S: String);
begin
  Module.Log(S);
end;

procedure TmodCommand.RespondResult(var Result: TmodRespondResult);
begin
end;

procedure TmodCommand.Unprepare(var Result: TmodRespondResult);
begin
end;

function TmodCommand.CreateRespond: TmodRespond;
begin
  Result := TmodRespond.Create;
end;

function TmodCommand.Execute: TmodRespondResult;
begin
  Result.Status := []; //default to be not keep alive, not sure, TODO
  Prepare(Result);
  RespondResult(Result);
  Unprepare(Result);
end;

function TmodCommand.GetActive: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmodCommand.SetModule(const Value: TmodModule);
begin
  FModule := Value;
end;

function TmodCommandClasses.Add(const Name: String; CommandClass: TmodCommandClass): Integer;
var
  aItem: TmodCommandClassItem;
begin
  aItem := TmodCommandClassItem.Create;
  aItem.Name := UpperCase(Name);
  aItem.FCommandClass := CommandClass;
  Result := inherited Add(aItem);
end;

{ TmodModule }

function TmodModule.CreateCommand(CommandName: String; ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmodCommand;
var
  //  aName: string;
  aClass: TmodCommandClass;
begin
  //aName := GetCommandName(ARequest, ARequestStream);

  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self, ARequest, ARequestStream, ARespondStream);
  end
  else
    Result := nil;
end;

function TmodModule.GetCommandClass(var CommandName: String): TmodCommandClass;
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
    Result := FFallbackCommand;
end;

procedure TmodModule.Idle;
begin

end;

procedure TmodModule.Init;
begin

end;

procedure TmodModule.InternalError(ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; var Handled: Boolean);
begin

end;

procedure TmodModule.ParseHead(ARequest: TmodRequest);
var
  aAddress: string;
begin
  aAddress := ARequest.Address;
  if (aAddress<>'') and StartsText(URLPathDelim, aAddress) then
    aAddress := Copy(aAddress, 2, MaxInt);

  ARequest.Module := SubStr(aAddress, URLPathDelim, 0);
  ARequest.Path := Copy(aAddress, Length(ARequest.Module) + 1, MaxInt);
end;

procedure TmodModule.ParseHeader(RequestHeader: TmnParams; Stream: TmnBufferStream);
var
  line: String;
begin
  if Stream <> nil then
  begin
    while not (cloRead in Stream.Done) do
    begin
      line := Stream.ReadLineUTF8;
      if line = '' then
        break
      else
      begin
        RequestHeader.AddItem(line, ':', True);
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
  if Commands.Count = 0 then
    DoCreateCommands;
end;

procedure TmodModule.Start;
begin

end;

procedure TmodModule.Stop;
begin

end;

function TmodModule.RequestCommand(ARequest: TmodRequest; ARequestStream, ARespondStream: TmnBufferStream): TmodCommand;
begin
  Result := CreateCommand(ARequest.Command, ARequest, ARequestStream, ARespondStream);
end;

constructor TmodModule.Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules);
begin
  inherited Create;
  Name := AName;
  FAliasName := AAliasName;
  FProtocols := AProtocols;
  FModules := AModules;
  FModules.Add(Self);
  FParams := TStringList.Create;
  FCommands := TmodCommandClasses.Create(True);
  FKeepAliveTimeOut := cDefaultKeepAliveTimeOut; //TODO move module
end;

destructor TmodModule.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FCommands);
  inherited;
end;

procedure TmodModule.DoCreateCommands;
begin
end;

function TmodModule.Match(const ARequest: TmodRequest): Boolean;
begin
  //Result := SameText(AliasName, ARequest.Module) and ((Protocols = nil) or StrInArray(ARequest.Protocol, Protocols));
  Result := StartsText('/'+AliasName, ARequest.Address) and ((Protocols = nil) or StrInArray(ARequest.Protocol, Protocols));
end;

procedure TmodModule.Log(S: String);
begin
end;

function TmodModule.Execute(ARequest: TmodRequest; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream): TmodRespondResult;
var
  aCMD: TmodCommand;
  aHandled: Boolean;
begin
  CreateCommands;

  Result.Status := [erSuccess];


  ParseHead(ARequest);
  ParseHeader(ARequest.Header, ARequestStream);

  aCmd := RequestCommand(ARequest, ARequestStream, ARespondStream);

  if aCMD <> nil then
  begin
    try
      Result := aCMD.Execute;
      Result.Status := Result.Status + [erSuccess];
    finally
      FreeAndNil(aCMD);
    end;
  end
  else
  begin
    aHandled := False;
    InternalError(ARequest, ARequestStream, ARespondStream, aHandled);

    if not aHandled then
      raise TmodModuleException.Create('Can not find command or fallback command: ' + ARequest.Command);
  end;
end;

procedure TmodModule.ExecuteCommand(CommandName: String; ARequestStream: TmnBufferStream; ARespondStream: TmnBufferStream; RequestString: TArray<String>);
var
  ARequest: TmodRequest;
begin
  ARequest.Command := CommandName;
  Execute(ARequest, ARequestStream, ARespondStream);
end;

procedure TmodModule.SetAliasName(AValue: String);
begin
  if FAliasName = AValue then
    Exit;
  FAliasName := AValue;
end;

function TmodModule.GetActive: Boolean;
begin
  Result := Modules.Active; //todo
end;

function TmodModule.RegisterCommand(vName: String; CommandClass: TmodCommandClass; AFallback: Boolean): Integer;
begin
{  if Active then
    raise TmodModuleException.Create('Server is Active');}
  if FCommands.Find(vName) <> nil then
    raise TmodModuleException.Create('Command already exists: ' + vName);
  Result := FCommands.Add(vName, CommandClass);
  if AFallback then
    FFallbackCommand := CommandClass;
end;

procedure TmodModule.Reload;
begin

end;

{ TmodModules }

function TmodModules.Add(const Name, AliasName: String; AModule: TmodModule): Integer;
begin
  AModule.Name := Name;
  AModule.AliasName := AliasName;
  Result := inherited Add(AModule);
end;

procedure TmodModules.SetEndOfLine(AValue: String);
begin
  if FEndOfLine = AValue then
    Exit;
{  if Active then
    raise TmodModuleException.Create('You can''t change EOL while server is active');}
  FEndOfLine := AValue;
end;

procedure TmodModules.Start;
var
  aModule: TmodModule;
begin
  for aModule in Self do
    aModule.Start;
  FActive := True;
end;

procedure TmodModules.Stop;
var
  aModule: TmodModule;
begin
  FActive := False;
  for aModule in Self do
    aModule.Stop;
end;

function TmodModules.GetActive: Boolean;
begin
  Result := FActive;
end;

procedure TmodModules.Idle;
var
  aModule: TmodModule;
begin
  for aModule in Self do
    aModule.Idle;
end;

procedure TmodModules.Init;
var
  aModule: TmodModule;
begin
  if not FInit then
  begin
    FInit := True;
    for aModule in Self do
      aModule.Init;
  end;
end;

procedure TmodModules.Log(S: String);
begin
  Server.Listener.Log(S);
end;

constructor TmodModules.Create(AServer: TmodModuleServer);
begin
  inherited Create;
  FServer := AServer;
end;

procedure TmodModules.Created;
begin
  inherited;
  FEOFOnError := True;
  FEndOfLine := sWinEndOfLine; //for http protocol
end;

procedure TmodModules.ParseHead(ARequest: TmodRequest; const RequestLine: String);
begin
  ARequest.Clear;
  ARequest.Raw := RequestLine;
  ParseRaw(RequestLine, ARequest.Info.Method, ARequest.Info.Protocol, ARequest.Info.URI);
  ParseURI(ARequest.URI, ARequest.Info.Address, ARequest.Info.Query);
end;

function TmodModules.Match(ARequest: TmodRequest): TmodModule;
var
  item: TmodModule;
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

function TmodModules.Find<T>: T;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i] is T then
    begin
      Result := Items[i] as T;
      break;
    end;
  end;
end;

function TmodModules.Find(const ModuleClass: TmodModuleClass): TmodModule;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if Items[i] is ModuleClass then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

{ TmnFieldHelper }

function TmnFieldHelper.Have(AValue: String; vSeperators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
begin
  if Self = nil then
    Result := False
  else
  begin
    SubValues := TStringList.Create;
    try
      StrToStrings(AsString, SubValues, vSeperators, [' ']);
      Result := SubValues.IndexOf(AValue) >= 0;
    finally
      SubValues.Free;
    end;
  end;
end;

end.
