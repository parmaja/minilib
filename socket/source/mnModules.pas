unit mnModules;
{$M+}{$H+}
{$IFDEF FPC}{$MODE delphi}{$ENDIF}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
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
  SysUtils, Classes, StrUtils, Types, DateUtils,
  Generics.Defaults, mnStreamUtils,
  mnClasses, mnStreams, mnFields, mnParams,
  mnSockets, mnConnections, mnServers;

const
  cDefaultKeepAliveTimeOut = 50000; //TODO move module
  URLPathDelim  = '/';

type
  TmodModuleException = class(Exception);

  TmodModuleConnection = class;
  TmodModuleConnectionClass = class of TmodModuleConnection;

  TmodCommand = class;

  TmodHeaderState = (
    resHeaderSending,
    resHeadSent, //reposnd line, first line before header
    resHeaderSent,
    //resSuccess,
    //resKeepAlive,
    resEnd
    );

  TmodHeaderStates = set of TmodHeaderState;

  TmodHeader = class(TmnHeader)
  public
    FStates: TmodHeaderStates;
  public
    function Domain: string;
    function Origin: string;
    property States: TmodHeaderStates read FStates;
  end;

  TmodCustomCommand = class;

  TmodCommunicate = class abstract(TmnObject)
  private
    FHead: string;
    FHeader: TmodHeader;
    FStream: TmnBufferStream; //*todo TmnConnectionStream
    FCookies: TStrings;
    FWritingStarted: Boolean;
    FContentLength: Integer;
    FCommand: TmodCustomCommand;
    procedure SetHead(const Value: string);
  protected
    procedure OnWriting(vCount: Longint);

    procedure SendHead;

    procedure DoPrepareHeader; virtual;
    procedure DoSendHeader; virtual;
    procedure DoWriteCookies; virtual;
    procedure DoHeaderSent; virtual;
    procedure DoReceiveHeader; virtual;
    procedure DoHeaderReceived; virtual;
  public
    constructor Create(ACommand: TmodCustomCommand);
    destructor Destroy; override;
    procedure SetStream(AStream: TmnConnectionStream; TriggerHeader: Boolean); virtual;
    property Stream: TmnBufferStream read FStream;
    property Header: TmodHeader read FHeader;
    property Cookies: TStrings read FCookies;
    procedure SetCookie(const vNameSpace, vName, Value: string);
    function GetCookie(const vNameSpace, vName: string): string;

    procedure ClearHeader;
    procedure ReceiveHeader; virtual;
    procedure SendHeader; virtual;

    //Add new header, can dublicate
    procedure AddHeader(const AName: string; AValue: TDateTime); overload;
    procedure AddHeader(const AName, AValue: String); overload; virtual;
    procedure AddHeader(const AName: string; Values: TStringDynArray); overload;
    //Update header by name but adding new value to old value
    procedure PutHeader(AName, AValue: String);

    property Head: string read FHead write SetHead;
    property Command: TmodCustomCommand read FCommand;
    property ContentLength: Integer read FContentLength write FContentLength;
  end;

  TmodRequestInfo = record
    Raw: String; //Full of first line of header

    //from raw :) raw = Method + URI + Protocol
    Method: string;
    Protocol: string;
    URI: string;

    //from URI :) URI = Address + Query
    Address: string;
    Query: string;

    Command: String;
    Client: String;
  end;

  TmnRoute = class(TStringList)
  private
    function GetRoute(vIndex: Integer): string;
  public
    property Route[vIndex: Integer]: string read GetRoute; default;
  end;

  { TmodRequest }

  TmodRequest = class(TmodCommunicate)
  private
    FParams: TmnFields;
    FRoute: TmnRoute;
    FPath: String;
  protected
    Info: TmodRequestInfo;
    procedure DoWriteCookies; override;
    procedure Created; override;
  public
    destructor Destroy; override;
    procedure  Clear;

    property Raw: String read Info.Raw write Info.Raw;

    //from raw
    property Method: string read Info.Method write Info.Method;
    property URI: string read Info.URI write Info.URI;
    property Protocol: string read Info.Protocol write Info.Protocol;
    property Address: string read Info.Address write Info.Address;
    property Query: string read Info.Query write Info.Query;
    //for module
    property Command: String read Info.Command write Info.Command;
    property Client: String read Info.Client write Info.Client;
    property Path: String read FPath write FPath;

    property Route: TmnRoute read FRoute write FRoute;
    property Params: TmnFields read FParams;

    function CollectURI: string;
  end;

  TmodeResult = (
    mrSuccess,
    mrKeepAlive //keep the stream connection alive, not the command
    );

  TmodeResults = set of TmodeResult;

  TmodRespondResult = record
    Status: TmodeResults;
    Timout: Integer;
  end;

  TmodKeepAlive = (klvUndefined, klvClose, klvKeepAlive);

  { TmodRespond }

  TmodRespond = class(TmodCommunicate)
  private
    FKeepAlive: Boolean;
  protected
    procedure DoWriteCookies; override;
    procedure DoReceiveHeader; override;
  public
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
  end;

  TmodModule = class;

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  { TmodCustomCommand }

  TmodCustomCommand = class(TmnNamedObject)
  private
    FRaiseExceptions: Boolean;
  protected
    FRespond: TmodRespond;
    FRequest: TmodRequest;

    FCompressClass: TmnCompressStreamProxyClass;
    FCompressProxy: TmnCompressStreamProxy;

    procedure SetCompressClass(AValue: TmnCompressStreamProxyClass);
    procedure SetsCompressProxy(AValue: TmnCompressStreamProxy);

    procedure DoPrepareHeader(Sender: TmodCommunicate); virtual;
    procedure DoSendHeader(Sender: TmodCommunicate); virtual;
    procedure DoHeaderSent(Sender: TmodCommunicate); virtual;

    procedure DoHeaderReceived(Sender: TmodCommunicate); virtual;

    function CreateRequest: TmodRequest; virtual;
    function CreateRespond: TmodRespond; virtual;
    procedure Created; override;
  public
    UseKeepAlive: TmodKeepAlive;
    UseCompressing: Boolean;
    UseWebSocket: Boolean;
    KeepAliveTimeOut: Integer;
    constructor Create(ARequest: TmodRequest; aStream: TmnConnectionStream = nil);
    destructor Destroy; override;

    //Compress on the fly, now we use deflate
    property CompressClass: TmnCompressStreamProxyClass read FCompressClass write SetCompressClass;
    property CompressProxy: TmnCompressStreamProxy read FCompressProxy write SetsCompressProxy;

    //Prepare called after created in lucking mode
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    property Request: TmodRequest read FRequest;
    property Respond: TmodRespond read FRespond;
  end;

  TmodCustomCommandClass = class of TmodCustomCommand;

  TmodCommand = class(TmodCustomCommand)
  private
    FModule: TmodModule;
    function GetActive: Boolean;
  protected
    procedure SetModule(const Value: TmodModule); virtual;
    procedure Log(S: String); virtual;

    procedure Prepare(var Result: TmodRespondResult); virtual;
    procedure RespondResult(var Result: TmodRespondResult); virtual;
    function Execute: TmodRespondResult;
    procedure Unprepare(var Result: TmodRespondResult); virtual; //Shutdown it
  public
    constructor Create(AModule: TmodModule; ARequest: TmodRequest; aStream: TmnConnectionStream = nil); virtual;
    //GetCommandName: make name for command when register it, useful when log the name of it
    property Module: TmodModule read FModule write SetModule;
    property Active: Boolean read GetActive;
    //Lock the server listener when execute the command
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
    FLevel: Integer;
    FModules: TmodModules;
    FProtocols: TArray<String>;
    FKeepAliveTimeOut: Integer;
    FUseKeepAlive: TmodKeepAlive;
    FUseCompressing: Boolean;
    FUseWebSocket: Boolean;
    procedure SetAliasName(AValue: String);
  protected
    CommandRegistered: Boolean;
    FFallbackCommand: TmodCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: String): TmodCommandClass; virtual;
    procedure Created; override;
    procedure DoRegisterCommands; virtual;
    procedure RegisterCommands;
    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); virtual;
    procedure DoPrepareRequest(ARequest: TmodRequest); virtual;

    function Match(const ARequest: TmodRequest): Boolean; virtual;
    procedure PrepareRequest(ARequest: TmodRequest);
    function CreateCommand(CommandName: String; ARequest: TmodRequest; AStream: TmnConnectionStream = nil): TmodCommand; overload;

    procedure DoReceiveHeader(ARequest: TmodRequest); virtual;
    procedure ReceiveHeader(ARequest: TmodRequest; Stream: TmnBufferStream);

    function RequestCommand(ARequest: TmodRequest; AStream: TmnConnectionStream): TmodCommand; virtual;
    procedure Log(S: String); virtual;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Reload; virtual;
    procedure Init; virtual;
    procedure Idle; virtual;
    procedure InternalError(ARequest: TmodRequest; AStream: TmnBufferStream; var Handled: Boolean); virtual;
  public
    //Default fallback module should have no alias name
    //Protocols all should lowercase
    constructor Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules = nil); virtual;
    destructor Destroy; override;
    function RegisterCommand(vName: String; CommandClass: TmodCommandClass; AFallback: Boolean = False): Integer; overload;

    //* Run in Connection Thread
    function Execute(ARequest: TmodRequest; AStream: TmnConnectionStream = nil): TmodRespondResult;

    property Commands: TmodCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Modules: TmodModules read FModules;
    //* use lower case in Protocols
    property Protocols: TArray<String> read FProtocols;
    property KeepAliveTimeOut: Integer read FKeepAliveTimeOut write FKeepAliveTimeOut;
    property UseKeepAlive: TmodKeepAlive read FUseKeepAlive write FUseKeepAlive default klvUndefined;
    property UseCompressing: Boolean read FUseCompressing write FUseCompressing;
    property UseWebSocket: Boolean read FUseWebSocket write FUseWebSocket;
    property AliasName: String read FAliasName write SetAliasName;
    property Level: Integer read FLevel write FLevel;
  end;

  TmodModuleClass = class of TmodModule;

  TmodModuleServer = class;

  { TmodModules }

  TmodModules = class(TmnNamedObjectList<TmodModule>)
  private
//    FEOFOnError: Boolean;
    FActive: Boolean;
    FEndOfLine: String;
    FDefaultProtocol: String;
    FInit: Boolean;
    FServer: TmodModuleServer;
    procedure SetEndOfLine(AValue: String);
  protected
    function CheckRequest(const ARequest: string): Boolean; virtual;
    function GetActive: Boolean; virtual;
    procedure Created; override;
    procedure Start;
    procedure Stop;
    procedure Init;
    procedure Idle;
    property Server: TmodModuleServer read FServer;
    procedure Added(Item: TmodModule); override;
  public
    constructor Create(AServer: TmodModuleServer);
    procedure ParseHead(ARequest: TmodRequest; const RequestLine: String); virtual;
    function Match(ARequest: TmodRequest): TmodModule; virtual;
    property DefaultProtocol: String read FDefaultProtocol write FDefaultProtocol;

    function ServerUseSSL: Boolean;
    function Find<T: Class>: T; overload;
    function Find<T: Class>(const AName: string): T; overload;
    function Find(const ModuleClass: TmodModuleClass): TmodModule; overload;

    function Add(const Name, AliasName: String; AModule: TmodModule): Integer; overload;
    procedure Log(S: String); virtual;

    property Active: Boolean read GetActive;
    property EndOfLine: String read FEndOfLine write SetEndOfLine; //TODO move to module
  end;

  //--- Server ---

  { TmodModuleConnection }

  TmodModuleConnection = class(TmnServerConnection)
  private
  protected
    function ModuleServer: TmodModuleServer;
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

function ParseRaw(const Raw: String; out Method, Protocol, URI: string): Boolean;
function ParseURI(const URI: String; out Address, Params: string): Boolean;
procedure ParseQuery(const Query: String; mnParams: TmnFields);
procedure ParseParamsEx(const Params: String; mnParams: TmnParams);


function ParseAddress(const Request: string; out URIPath: string; out URIQuery: string): Boolean; overload;
function ParseAddress(const Request: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams): Boolean; overload;
procedure ParsePath(const aRequest: string; out Name: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams);
function FormatHTTPDate(vDate: TDateTime): string;
function ExtractDomain(const URI: string): string;
function DeleteSubPath(const SubKey, Path: string): string;

implementation

uses
  mnUtils;

function ParseRaw(const Raw: String; out Method, Protocol, URI: string): Boolean;
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

function ParseURI(const URI: String; out Address, Params: string): Boolean;
var
  J: Integer;
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
  Result := True;
end;

procedure ParseParamsEx(const Params: String; mnParams: TmnParams);
begin
  StrToStringsCallback(Params, mnParams, @FieldsCallBack, ['/'], [' ']);
end;

procedure ParseQuery(const Query: String; mnParams: TmnFields);
begin
  StrToStringsCallback(Query, mnParams, @FieldsCallBack, ['&'], [' ']);
end;

function ParseAddress(const Request: String; out URIPath: string; out URIQuery: string): Boolean;
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

function ParseAddress(const Request: String; out URIPath: string; out URIParams: string; URIQuery: TmnParams): Boolean;
begin
  Result := ParseAddress(Request, URIPath, URIParams);
  if Result then
    if URIQuery <> nil then
      //ParseParams(aParams, False);
      StrToStringsCallback(URIParams, URIQuery, @FieldsCallBack, ['&'], [' ']);
end;

procedure ParsePath(const aRequest: String; out Name: String; out URIPath: string; out URIParams: string; URIQuery: TmnParams);
begin
  ParseAddress(aRequest, URIPath, URIParams, URIQuery);
  Name := SubStr(URIPath, URLPathDelim, 0);
  URIPath := Copy(URIPath, Length(Name) + 1, MaxInt);
end;

var
  DefFormatSettings : TFormatSettings;

function FormatHTTPDate(vDate: TDateTime): string;
var
  aDate: TDateTime;
begin
  {$ifdef FPC}
  aDate := NowUTC;
  {$else}
  aDate := TTimeZone.Local.ToUniversalTime(vDate);
  {$endif}
  Result := FormatDateTime('ddd, dd mmm yyyy hh:nn:ss', aDate, DefFormatSettings) + ' GMT';
end;

function ExtractDomain(const URI: string): string;
var
  aStart, aCount, p: Integer;
begin
  if URI<>'' then
  begin
    aStart := 1;
    //TODO use pose for ://
    if StartsText('http://', URI) then
      Inc(aStart, 7);
    if StartsText('https://', URI) then
      Inc(aStart, 8);

    aCount := Length(URI) - aStart + 1;

    p := Pos(':', URI, aStart); //check for port
    if (p<>0) then aCount := p - aStart;

    p := Pos('/', URI, aStart);
    if (p<>0) and ((p - aStart)<aCount) then aCount := p - aStart;


    if aCount>0 then
    begin
      Exit(Copy(URI, aStart, aCount));
    end;
  end;

  Result := '';
end;

function DeleteSubPath(const SubKey, Path: string): string;
begin
  if StartsText(URLPathDelim, Path) then
    Result := Copy(Path, Length(URLPathDelim) + Length(SubKey) + 1, MaxInt)
  else
    Result := Copy(Path, Length(SubKey) + 1, MaxInt);;
end;

{ TmodRespond }

procedure TmodCommunicate.AddHeader(const AName: string; Values: TStringDynArray);
var
  s, t: string;
begin
  t := '';
  for s in Values do
  begin
    if t<>'' then
      t := t + ', ';
    t := t + s;
  end;

  AddHeader(AName, t);
end;

{ TmodRespond }

procedure TmodRespond.DoReceiveHeader;
begin
  inherited;
  if (Header.Field['Connection'].IsExists) then
    FKeepAlive := SameText(Header['Connection'], 'Keep-Alive');
end;

procedure TmodRespond.DoWriteCookies;
var
  s: string;
begin
  inherited;
  if Cookies.Count <> 0 then
  begin
    for s in Cookies do
      Stream.WriteUTF8Line('Set-Cookie: ' + s);
  end;
end;

{ TmodRequest }

function TmodRequest.CollectURI: string;
begin
  Result := URLPathDelim + Address;
  if Query<>'' then
    Result := Result+'?'+Query
end;

procedure TmodRequest.Created;
begin
  inherited;
  FRoute := TmnRoute.Create;
  FParams := TmnFields.Create;
end;

destructor TmodRequest.Destroy;
begin
  FreeAndNil(FRoute);
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TmodRequest.DoWriteCookies;
var
  s: UTF8String;
begin
  inherited;
  Cookies.Delimiter := ';';
  s := UTF8Encode(Cookies.DelimitedText);
  if s <> '' then
    Stream.WriteUTF8Line('Cookie: ' + s);
end;

procedure TmodRequest.Clear;
begin
  Initialize(Info);
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

function TmodModuleConnection.ModuleServer: TmodModuleServer;
begin
  Result :=  (Listener.Server as TmodModuleServer);
end;

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
  //need support peek :( for check request
  Stream.ReadUTF8Line(aRequestLine);
  aRequestLine := TrimRight(aRequestLine); //* TODO do we need UTF8ToString?
  if Connected and (aRequestLine <> '') then //aRequestLine empty when timeout but not disconnected
  begin

    if not ModuleServer.Modules.CheckRequest(aRequestLine) then //check ssl connection on not ssl server need support peek :(
    begin
      Stream.Disconnect;
      Exit;
    end;

    aRequest := TmodRequest.Create(nil);
    try
      ModuleServer.Modules.ParseHead(aRequest, aRequestLine);
      aModule := ModuleServer.Modules.Match(aRequest);

      if (aModule = nil) then
      begin
        Stream.Disconnect; //if failed
      end
      else
        try
          aRequest.SetStream(Stream, False);
          aRequest.Client := RemoteIP;
          Result := aModule.Execute(aRequest, Stream);
        finally
          if Stream.Connected then
          begin
            if (mrKeepAlive in Result.Status) then
              Stream.ReadTimeout := Result.Timout
            else
              Stream.Disconnect;
          end;
        end;
    finally
      FreeAndNil(aRequest); //if create command then aRequest change to nil
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
  if Modules<>nil then //not stoped
  begin
    Modules.Idle;
  end;
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

{ TmodCustomCommand }

procedure TmodCustomCommand.DoSendHeader(Sender: TmodCommunicate);
begin
end;

procedure TmodCustomCommand.SetCompressClass(AValue: TmnCompressStreamProxyClass);
begin
  if FCompressClass <> nil then
    raise TmodModuleException.Create('Compress class is already set!');
  FCompressClass := AValue;
end;

procedure TmodCustomCommand.SetsCompressProxy(AValue: TmnCompressStreamProxy);
begin
  if FCompressProxy <> nil then
    raise TmodModuleException.Create('Compress proxy is already set!');
  FCompressProxy :=AValue;
end;

destructor TmodCustomCommand.Destroy;
begin
  FreeAndNil(FRespond);
  inherited;
end;

constructor TmodCustomCommand.Create(ARequest: TmodRequest; aStream: TmnConnectionStream);
begin
  inherited Create;
  FRequest := ARequest; //do not free
  if FRequest <> nil then
    FRequest.FCommand := Self
  else
  begin
    FRequest := CreateRequest;
    FRequest.SetStream(AStream, False);
  end;

  FRespond := CreateRespond;
  FRespond.SetStream(AStream, True);
end;

procedure TmodCustomCommand.Created;
begin
  inherited;
  UseKeepAlive := klvUndefined;
  UseCompressing := True;
  UseWebSocket := True;
end;

function TmodCustomCommand.CreateRequest: TmodRequest;
begin
  Result := TmodRequest.Create(Self);
end;

function TmodCustomCommand.CreateRespond: TmodRespond;
begin
  Result := TmodRespond.Create(Self);
end;

procedure TmodCustomCommand.DoHeaderReceived(Sender: TmodCommunicate);
begin
end;

procedure TmodCustomCommand.DoHeaderSent(Sender: TmodCommunicate);
begin
  if CompressClass <> nil then
  begin
    if CompressProxy = nil then
    begin
      CompressProxy := CompressClass.Create([cprsWrite], 9);
      Sender.Stream.AddProxy(CompressProxy);
    end
    else
      CompressProxy.Enable;
  end;
end;

procedure TmodCustomCommand.DoPrepareHeader(Sender: TmodCommunicate);
begin
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

{ TmodCommand }

function TmodCommand.Execute: TmodRespondResult;
begin
  Result.Status := []; //default to be not keep alive, not sure, TODO
  Prepare(Result);
  RespondResult(Result);
  Unprepare(Result);
end;

procedure TmodCommand.Prepare(var Result: TmodRespondResult);
begin
end;

procedure TmodCommand.RespondResult(var Result: TmodRespondResult);
begin
end;

procedure TmodCommand.Unprepare(var Result: TmodRespondResult);
begin
end;

procedure TmodCommand.Log(S: String);
begin
  Module.Log(S);
end;

function TmodCommand.GetActive: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmodCommand.SetModule(const Value: TmodModule);
begin
  FModule := Value;
end;

constructor TmodCommand.Create(AModule: TmodModule; ARequest: TmodRequest; aStream: TmnConnectionStream = nil);
begin
  inherited Create(ARequest, aStream);
  FModule := AModule;
end;

{ TmodModule }

function TmodModule.CreateCommand(CommandName: String; ARequest: TmodRequest; AStream: TmnConnectionStream = nil): TmodCommand;
var
  //  aName: string;
  aClass: TmodCommandClass;
begin
  //aName := GetCommandName(ARequest, ARequestStream);

  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self, ARequest, AStream);
    Result.Name := CommandName;
    Result.KeepAliveTimeOut := KeepAliveTimeOut;
    Result.UseKeepAlive := UseKeepAlive;
    Result.UseCompressing := UseCompressing;
    Result.UseWebSocket := UseWebSocket;
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

procedure TmodModule.InternalError(ARequest: TmodRequest; AStream: TmnBufferStream; var Handled: Boolean);
begin

end;

procedure TmodModule.ReceiveHeader(ARequest: TmodRequest; Stream: TmnBufferStream);
begin
  if Stream <> nil then
  begin
    ARequest.ReceiveHeader;
    DoReceiveHeader(ARequest);
  end;
end;

procedure TmodModule.Created;
begin
  inherited;
end;

procedure TmodModule.RegisterCommands;
begin
  if Commands.Count = 0 then
  begin
    DoRegisterCommands;
  end;
  CommandRegistered := True;
end;

procedure TmodModule.Start;
begin

end;

procedure TmodModule.Stop;
begin

end;

function TmodModule.RequestCommand(ARequest: TmodRequest; AStream: TmnConnectionStream): TmodCommand;
begin
  Result := CreateCommand(ARequest.Command, ARequest, AStream);
end;

constructor TmodModule.Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules);
begin
  inherited Create;
  Name := AName;
  FAliasName := AAliasName;
  FProtocols := AProtocols;
  if AModules <> nil then
  begin
    FModules := AModules;//* nope, Add will assign it
    FModules.Add(Self);
  end;
  FCommands := TmodCommandClasses.Create(True);
  FKeepAliveTimeOut := cDefaultKeepAliveTimeOut; //TODO move module
end;

destructor TmodModule.Destroy;
begin
  FreeAndNil(FCommands);
  inherited;
end;

procedure TmodModule.DoRegisterCommands;
begin
end;

procedure TmodModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  if (AliasName <> '') then
    ARequest.Path := DeleteSubPath(ARequest.Route[0], ARequest.Path);
end;

procedure TmodModule.DoReceiveHeader(ARequest: TmodRequest);
begin
end;

procedure TmodModule.DoMatch(const ARequest: TmodRequest; var vMatch: Boolean);
begin
  vMatch := (AliasName<>'') and (ARequest.Route[0] = AliasName);
end;

function TmodModule.Match(const ARequest: TmodRequest): Boolean;
begin
  if not CommandRegistered then
    RegisterCommands;
  //Result := SameText(AliasName, ARequest.Module) and ((Protocols = nil) or StrInArray(ARequest.Protocol, Protocols));
  Result := False;
  if ((Protocols = nil) or StrInArray(LowerCase(ARequest.Protocol), Protocols)) then
  begin
    DoMatch(ARequest, Result);
  end;
end;

procedure TmodModule.Log(S: String);
begin
end;

function TmodModule.Execute(ARequest: TmodRequest; AStream: TmnConnectionStream): TmodRespondResult;
var
  aCommand: TmodCommand;
  aHandled: Boolean;
begin
  Result.Status := [mrSuccess];

  ReceiveHeader(ARequest, AStream);

  aCommand := RequestCommand(ARequest, AStream);

  if aCommand <> nil then
  begin
    try
      Result := aCommand.Execute;
      Result.Status := Result.Status + [mrSuccess];
    finally
      FreeAndNil(aCommand);
    end;
  end
  else
  begin
    aHandled := False;
    InternalError(ARequest, AStream, aHandled);

    if not aHandled then
      raise TmodModuleException.Create('Can not find command or fallback command: ' + ARequest.Command);
  end;
end;

procedure TmodModule.PrepareRequest(ARequest: TmodRequest);
begin
  ARequest.Params.Clear;
  ParseQuery(ARequest.Query, ARequest.Params);

  ARequest.Params['Module'] := AliasName;
  DoPrepareRequest(ARequest);
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

function TmodModules.ServerUseSSL: Boolean;
begin
  if Server<>nil then
    Result := Server.UseSSL
  else
    Result := False;
end;

procedure TmodModules.SetEndOfLine(AValue: String);
begin
  if FEndOfLine = AValue then
    Exit;
{  if Active then
    raise TmodModuleException.Create('You can''t change EOL while server is active');}
  FEndOfLine := AValue;
end;

function ModuleCompareLevel(Item1, Item2: Pointer): Integer;
begin
  Result := TmodModule(Item1).Level - TmodModule(Item2).Level;
end;

procedure TmodModules.Start;
var
  aModule: TmodModule;
begin
  {$ifdef FPC}
  Sort(ModuleCompareLevel); //* sort it before run
  {$else}
  Sort(TComparer<TmodModule>.Construct(function(const Left, Right: TmodModule): Integer
  begin
    Result := Left.Level-Right.Level;
  end)); //* sort it before run
  {$endif}
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

procedure TmodModules.Added(Item: TmodModule);
begin
  inherited Added(Item);
  Item.FModules := Self;
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

function TmodModules.CheckRequest(const ARequest: string): Boolean;
begin
  Result := True;
end;

constructor TmodModules.Create(AServer: TmodModuleServer);
begin
  inherited Create;
  FServer := AServer;
end;

procedure TmodModules.Created;
begin
  inherited;
//  FEOFOnError := True;
  FEndOfLine := sWinEndOfLine; //for http protocol
end;

procedure TmodModules.ParseHead(ARequest: TmodRequest; const RequestLine: String);
begin
  ARequest.Clear;
  ARequest.Raw := RequestLine;
  ParseRaw(RequestLine, ARequest.Info.Method, ARequest.Info.Protocol, ARequest.Info.URI);
  ParseURI(ARequest.URI, ARequest.Info.Address, ARequest.Info.Query);

  //zaher: @zaher,belal, I dont like it
  if (ARequest.Address <> '') and (ARequest.Address <> '/') and StartsText(URLPathDelim, ARequest.Address) then
    ARequest.Path := Copy(ARequest.Address, 2, MaxInt)
  else
    ARequest.Path := ARequest.Address;

  StrToStrings(ARequest.Path, ARequest.Route, ['/']);


{  if (ARequest.Address<>'') and (ARequest.Address<>'/') then
  begin
    if StartsText(URLPathDelim, ARequest.Address) then
      StrToStrings(Copy(ARequest.Address, 2, MaxInt), ARequest.Route, ['/'])
    else
      StrToStrings(ARequest.Address, ARequest.Route, ['/']);
  end;}

end;

function TmodModules.Match(ARequest: TmodRequest): TmodModule;
var
  item, aModule, aLast: TmodModule;
begin
  Result := nil;
  aModule := nil;
  aLast := nil;
  for item in Self do
  begin
    if (item.AliasName <> '') and item.Match(ARequest) then
    begin
      aModule := item;
      break;
    end
    else if (item.AliasName = '') then //* find fallback module without aliasname
      aLast := item;
  end;

  if aModule = nil then
    aModule := aLast;

  if aModule <> nil then
  begin
    //item.PrepareRequest(ARequest); //always have params
    aModule.PrepareRequest(ARequest);
    Result := aModule;
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

function TmodModules.Find<T>(const AName: string): T;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i] is T) and SameText(AName, Items[i].Name) then
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

{ TmodCommunicate }

constructor TmodCommunicate.Create(ACommand: TmodCustomCommand);
begin
  inherited Create;
  FCommand := ACommand;
  FHeader := TmodHeader.Create;
  FCookies := TStringList.Create;
  FCookies.Delimiter := ';';
end;

destructor TmodCommunicate.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FCookies);
  SetStream(nil, False);
  inherited;
end;

function TmodCommunicate.GetCookie(const vNameSpace, vName: string): string;
begin
  if vNameSpace<>'' then
    Result := Cookies.Values[vNameSpace+'.'+vName]
  else
    Result := Cookies.Values[vName];
end;

procedure TmodCommunicate.ReceiveHeader;
begin
  Header.ReadHeader(Stream);
  Cookies.DelimitedText := Header['Cookie'];
  if (Header.Field['Content-Length'].IsExists) then
    ContentLength := Header.Field['Content-Length'].AsInt64;
  DoReceiveHeader;
  DoHeaderReceived;
  if Command <> nil then
    Command.DoHeaderReceived(Self);
end;

procedure TmodCommunicate.SendHead;
begin
  if resHeadSent in Header.States then
    raise TmodModuleException.Create('Head is sent');

  if Head = '' then
    raise TmodModuleException.Create('Head not set');

  Stream.WriteUTF8Line(Head);
  Header.FStates := Header.FStates + [resHeadSent];
end;

procedure TmodCommunicate.DoHeaderReceived;
begin

end;

procedure TmodCommunicate.DoHeaderSent;
begin

end;

procedure TmodCommunicate.DoPrepareHeader;
begin

end;

procedure TmodCommunicate.DoReceiveHeader;
begin
end;

procedure TmodCommunicate.DoSendHeader;
begin
end;

procedure TmodCommunicate.DoWriteCookies;
begin
end;

procedure TmodCommunicate.SetCookie(const vNameSpace, vName, Value: string);
begin
  if vNameSpace<>'' then
    Cookies.Values[vNameSpace+'.'+vName] := Value
  else
    Cookies.Values[vName] := Value;
end;

procedure TmodCommunicate.SetHead(const Value: string);
begin
  FHead := Value;
end;

procedure TmodCommunicate.ClearHeader;
begin
  FHeader.Clear;
end;

procedure TmodCommunicate.SendHeader;
var
  item: TmnField;
  s: String;
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is sent');

  Header.FStates := Header.FStates + [resHeaderSending];

  DoPrepareHeader;
  if Command <> nil then
    Command.DoPrepareHeader(Self);

  SendHead;

  if (ContentLength > 0) then
    PutHeader('Content-Length: ', IntToStr(ContentLength));

  for item in Header do
  begin
    s := item.GetNameValue(': ');
    //WriteLn(s);
    Stream.WriteUTF8Line(s);
  end;

  DoWriteCookies;
  DoSendHeader; //enter after

  if Command <> nil then
    Command.DoSendHeader(Self);

  Stream.WriteUTF8Line(Utf8string(''));
  Header.FStates := Header.FStates + [resHeaderSent];

  DoHeaderSent;
  if Command <> nil then
    Command.DoHeaderSent(Self);
end;

procedure TmodCommunicate.AddHeader(const AName: string; AValue: TDateTime);
begin
  AddHeader(AName, FormatHTTPDate(AValue));
end;

procedure TmodCommunicate.AddHeader(const AName, AValue: String);
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is already sent');

  Header.Add(AName, AValue);
end;

procedure TmodCommunicate.PutHeader(AName, AValue: String);
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is sent');

  Header.Put(AName, AValue);
end;

procedure TmodCommunicate.OnWriting(vCount: Longint);
begin
  if not FWritingStarted then
  begin
    FWritingStarted := True;
    if not (resHeaderSending in Header.FStates) and not (resHeaderSent in Header.FStates) then
      SendHeader;
  end;
end;

procedure TmodCommunicate.SetStream(AStream: TmnConnectionStream; TriggerHeader: Boolean);
begin
  if (FStream <> nil) and (FStream is TmnConnectionStream) then
    (FStream as TmnConnectionStream).OnWriting := nil;
  FStream := AStream;
  if (FStream <> nil) and TriggerHeader then
    AStream.OnWriting := OnWriting;
end;

{ TmnRoute }

function TmnRoute.GetRoute(vIndex: Integer): string;
begin
  if vIndex<Count then
    Result := Strings[vIndex]
  else
    Result := '';
end;

{ TmodHeader }

function TmodHeader.Domain: string;
var
  s: string;
begin
  s := Origin;
  if s<>'*' then
    Result := ExtractDomain(s)
  else
    Result := '';
end;

function TmodHeader.Origin: string;
begin
  Result := Self['Origin'];
  if Result='' then
    Result := '*';
end;

initialization
  DefFormatSettings := TFormatSettings.Invariant;
end.
