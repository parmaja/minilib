unit mnModules;
{$IFDEF FPC}
{$mode delphi}
{$modeswitch prefixedattributes}
{$modeswitch arrayoperators}
{$modeswitch arraytodynarray}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$ENDIF}
{$H+}{$M+}
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
  └┬┘                                     └─┬─┘ └─┬─┘
  Command                                Module  Alias              Params

  https://en.wikipedia.org/wiki/Uniform_Resource_Identifier

  REST tools
    https://resttesttest.com
    https://httpbin.org
    http://dummy.restapiexample.com/

  echo
    ws://ws.vi-server.org/mirror
}

interface

uses
  SysUtils, Classes, StrUtils, Types, DateUtils, {$ifdef FPC}ZStream,{$else}ZLib,{$endif}
  Generics.Defaults, mnStreamUtils, SyncObjs,
  mnClasses, mnStreams, mnFields, mnParams, mnMIME,
  mnSockets, mnConnections, mnServers;

const
  cDefaultKeepAliveTimeOut = 50000; //TODO move module

type
  TmodModuleException = class(Exception);

  TmodModuleConnection = class;
  TmodModuleConnectionClass = class of TmodModuleConnection;

  TmodCommand = class;

  TmodHeaderState = (
    resHeaderSending,
    resHeadSent, //reposnd line, first line before header
    resHeaderSent,
    //resLatch, //raise exception when writing to stream , i don't like it!
    //resBodySent,
    //resSuccess,
    //resKeepAlive,
    resEnd
    );

  TmodHeaderStates = set of TmodHeaderState;

  TmodAnswer = (
    hrNone,
    hrOK,
    hrNoContent,
    hrUnauthorized,
    hrForbidden, //403
    hrError, //500 Internal Error
    hrRedirect, //302
    hrNotModified,
    hrMovedTemporarily, //307
    hrMovedPermanently,
    hrNotFound,
    hrSwitchingProtocols,
    hrServiceUnavailable
  );

  TmodAnswerHelper = record helper for TmodAnswer
    function ToString: string; //HTTP
  end;

  TmodHeader = class(TmnHeader)
  private
    FStates: TmodHeaderStates;
  public
    function Domain: string;
    function Origin: string;
    function Host: string;
    property States: TmodHeaderStates read FStates;
    procedure Clear; override;
  end;

  TmodCommunicate = class;

  { TmodCommunicateStreamControl }

  TmodCommunicateStreamControl =  class(TmnStreamControl)
  private
    FCommunicate: TmodCommunicate;
  public
    procedure Writing(Count: Longint); override;
    procedure Reading(Count: Longint); override;
    property Communicate: TmodCommunicate read FCommunicate;
  end;

  { TmnwCookie }

  TmnwCookie = class(TmnNameValueObject)
  private
    FDomain: string;
    FPath: string;
    FAge: Integer;
    FChanged: Boolean;
    FDeleting: Boolean;
    procedure SetAge(const AValue: Integer);
    procedure SetDomain(const AValue: string);
    procedure SetPath(const AValue: string);
  protected
    procedure SetValue(const AValue: string); override;
    procedure Created; override;
  public
    Stricted: Boolean; //SameSite
    Secured: Boolean; //HTTPS only
    function GetText: string;
    function GenerateValue: string;
    procedure Delete;
    procedure SetChanged;
    procedure ResetChanged;
    property Path: string read FPath write SetPath;
    property Domain: string read FDomain write SetDomain;
    property Age: Integer read FAge write SetAge;
    property Changed: Boolean read FChanged;
  end;

  { TmnwCookies }

  TmnwCookies = class(TmnNameValueObjectList<TmnwCookie>)
  public
    procedure SetRequestText(S: string);
    function GetRequestText: string;
  end;

  TmnCustomCommand = class;

  TmodCommunicate = class abstract(TmnObject)
  private
    FHead: string;
    FHeader: TmodHeader;
    FKeepAlive: Boolean;
    FCookies: TmnwCookies;
    FWritingStarted: Boolean;
    FContentLength: Int64;
    FParent: TmnCustomCommand;
    FStreamControl: TmodCommunicateStreamControl;

    FStamp: string;
    //function GetLatch: Boolean;
    //procedure SetLatch(const AValue: Boolean);
    procedure SetHead(const Value: string);
  protected
    procedure DoWriting(vCount: Longint);
    procedure DoReading(vCount: Longint);

    procedure SendHead;
    procedure ReceiveHead;

    procedure DoPrepareHeader; virtual;
    procedure DoSendHeader; virtual;
    procedure DoWriteCookies; virtual;
    procedure DoHeaderSent; virtual;

    procedure InitProtocol; virtual;
    procedure DoHeaderReceived; virtual;
    function GetStream: TmnBufferStream; virtual; abstract;
  public
    constructor Create(ACommand: TmnCustomCommand);
    destructor Destroy; override;
    procedure SetTrigger(TriggerHeader: Boolean); virtual;
    property Stream: TmnBufferStream read GetStream;
    property Header: TmodHeader read FHeader;
    property Cookies: TmnwCookies read FCookies;
    procedure SetCookie(const vNameSpace, vName, Value: string); overload;
    procedure SetCookie(const vName, Value: string); overload;
    function GetCookie(const vNameSpace, vName: string): string;

    procedure Reset;
    procedure ReceiveHeader(WithHead: Boolean); virtual;
    procedure SendHeader(WithHead: Boolean = True); virtual;

    procedure  Clear; virtual;

    function IsHeaderSent: Boolean;

    //Add new header, can dublicate
    procedure AddHeader(const AName: string; AValue: TDateTime); overload;
    procedure AddHeader(const AName, AValue: String); overload; virtual;
    procedure AddHeader(const AName: string; Values: TStringDynArray); overload;
    //Update header by name but adding new value to old value
    procedure PutHeader(AName, AValue: String);

    property Head: string read FHead write SetHead;
    property Parent: TmnCustomCommand read FParent;
    property ContentLength: Int64 read FContentLength write FContentLength;
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;
    //property Latch: Boolean read GetLatch write SetLatch;
    //need disccuss
    property Stamp: string read FStamp write FStamp;
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
    IsSSL: Boolean;
  end;

  TmnRoute = class(TStringList)
  private
    function GetRoute(vIndex: Integer): string;
  public
    property Route[vIndex: Integer]: string read GetRoute; default;
  end;


  TmodOptionValue = (ovUndefined, ovNo, ovYes);

  //  class operator = (const Source: Boolean): TmodOptionValue;

  { TmodOptionValueHelper }

  TmodOptionValueHelper = record helper for TmodOptionValue
  private
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const Value: Boolean);
    function GetAsString: String;
  public
    {class operator Explicit(const Source: Boolean): TmodOptionValue;
    class operator Implicit(Source : Boolean) : TmodOptionValue;
    class operator Implicit(Source : TmodOptionValue): Boolean;}
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsString: string read GetAsString;
  end;

  { TmodRequest }

  TmodCommunicateUsing = record
    KeepAliveTimeOut: Integer;
    KeepAlive: TmodOptionValue;
    AcceptCompressing: TmodOptionValue; //asking server to send compressing
    Compressing: TmodOptionValue; //our data will sent compressed
    WebSocket: Boolean;
  end;

  TStreamMode = set of (
    smRequestCompress,
    smAllowCompress,
    smRespondCompressing // using proxies
  );
  TStreamModeHelper = record helper for TStreamMode
    function RequestCompress: Boolean;
    function RespondCompressing: Boolean;
    function AllowCompress: Boolean;
  end;

  TConnectionType = (
	  ctNormal,
		ctFormData,
		ctWebSocket
	);

  TmodParams = class(TmnFields)
  public
//    property Field; default;
  end;

  TmodRequest = class(TmodCommunicate)
  private
    FParams: TmodParams;
    FRoute: TmnRoute;
    FPath: String;
    FConnectionType: TConnectionType;
    //FChunked: Boolean;
    FProtcolClass: TmnProtcolStreamProxyClass;
    FCompressProxy: TmnCompressStreamProxy;
    FProtcolProxy: TmnProtcolStreamProxy;
    FChunkedProxy: TmnChunkStreamProxy;
    FStream: TmnBufferStream;
    FMode: TStreamMode;
    FDirectory: String;
    procedure SetChunkedProxy(const Value: TmnChunkStreamProxy);
    procedure SetCompressProxy(const Value: TmnCompressStreamProxy);
    procedure SetProtcolClass(const Value: TmnProtcolStreamProxyClass);
    function GetConnected: Boolean;
  protected
    Info: TmodRequestInfo;
    procedure Created; override;
    procedure DoHeaderReceived; override;
    procedure InitProtocol; override;

    function GetStream: TmnBufferStream; override;
    procedure InitProxies(vChunked: Boolean; vCompressClass: TmnCompressStreamProxyClass);
    procedure ResetProxies; virtual;
  public
    Use: TmodCommunicateUsing;
    constructor Create(ACommand: TmnCustomCommand; AStream: TmnBufferStream); //need trigger event
    procedure SetStream(AStream: TmnBufferStream; TriggerHeader: Boolean); //for http client
    destructor Destroy; override;
    procedure  Clear; override;

    function ReadString(out s: string; Count: Integer): Boolean;
    function ReadLine(out s: string): Boolean;

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
    property IsSSL: Boolean read Info.IsSSL write Info.IsSSL;
    property Path: String read FPath write FPath;

    property Directory: String read FDirectory write FDirectory;
    property Route: TmnRoute read FRoute write FRoute;
    property Params: TmodParams read FParams;

    function CollectURI: string;

    //
    property ChunkedProxy: TmnChunkStreamProxy read FChunkedProxy write SetChunkedProxy;

    //Compress on the fly, now we use deflate
    property Mode: TStreamMode read FMode;// write FMode;
    property CompressProxy: TmnCompressStreamProxy read FCompressProxy write SetCompressProxy;

    property ConnectionType: TConnectionType read FConnectionType write FConnectionType;
    //WebSocket
    property ProtcolClass: TmnProtcolStreamProxyClass read FProtcolClass write SetProtcolClass;
    property ProtcolProxy: TmnProtcolStreamProxy read FProtcolProxy write FProtcolProxy;

    property Connected: Boolean read GetConnected;
  end;

  TInterfacedStreamtWrapper = class(TInterfacedPersistent, ImnStreamPersist)
  protected
    FStream: TStream;
    procedure SaveToStream(Stream: TStream; Count: Int64); overload;
    procedure LoadFromStream(Stream: TStream; Count: Int64); overload;
  public
    constructor Create(vStream: TStream);
  end;

  TmodFileDisposition = (
    fdResend, //Force send it even match stamp
    fdAttachment,
    fdInline //Over Attachment
  );
  TmodFileDispositions = set of TmodFileDisposition;

  { TmodRespond }

  TmodRespond = class(TmodCommunicate)
  private
    FAnswer: TmodAnswer;
    FContentType: String;
    procedure SetAnswer(const Value: TmodAnswer);
  protected
    FRequest: TmodRequest;
    function GetStream: TmnBufferStream; override;
    procedure InitProtocol; override;
  public
    constructor Create(ARequest: TmodRequest); //need trigger event
    function WriteString(const s: string): Boolean;
    function WriteLine(const s: string): Boolean;

    function SendUTF8String(const s: UTF8String): Boolean; overload;
    function SendString(const s: string): Boolean; overload;
    function SendStream(s: TStream; ASize: Int64; AAlias: string; AFileDate: TDateTime; const AFileStamp: string = ''; FileDispositions: TmodFileDispositions = []): Boolean; overload;
    function SendStream(s: TStream; ASize: Int64): Boolean; overload;
    function SendStream(s: ImnStreamPersist; Count: Int64): Boolean; overload;

    function SendFile(AFileName: string; const AFileStamp: string = ''; Alias: string = ''; FileDispositions: TmodFileDispositions = []): Boolean;

    function ReceiveStream(s: TStream): Int64; overload;
    function ReceiveStream(s: ImnStreamPersist; Count: Int64): Int64; overload;

    property Request: TmodRequest read FRequest;
    property ContentType: string read FContentType write FContentType;
    property Answer: TmodAnswer read FAnswer write SetAnswer;
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

  {
    Params: (info like remoteip)
    InHeader:
    OutHeader:

    Result: Success or error and message of error
  }

  { TmnCustomCommand }

  TmnCustomCommand = class(TmnNamedObject)
  private
    FRaiseExceptions: Boolean;
  protected
    FRespond: TmodRespond; //need discuss like http client
    FRequest: TmodRequest;

    procedure DoPrepareHeader(Sender: TmodCommunicate); virtual;
    procedure DoSendHeader(Sender: TmodCommunicate); virtual;
    procedure DoHeaderSent(Sender: TmodCommunicate); virtual;

    function CreateRequest(AStream: TmnConnectionStream): TmodRequest; virtual;
    function CreateRespond: TmodRespond; virtual;
    procedure Created; override;
  public
    constructor Create;
    destructor Destroy; override;

    //Prepare called after created in lucking mode
    property RaiseExceptions: Boolean read FRaiseExceptions write FRaiseExceptions default False;
    property Request: TmodRequest read FRequest;
    property Respond: TmodRespond read FRespond;
  end;

  TmnCustomCommandClass = class of TmnCustomCommand;

  TmnCustomServerCommand = class(TmnCustomCommand)
  public
    constructor Create(ARequest: TmodRequest);
  end;

  TmnCustomClientCommand = class(TmnCustomCommand)
  //move create request here
  public
    constructor Create;
  end;

  // Web

  TwebRequest = class(TmodRequest)
  private
    FAccept: String;
    FHost: string;
    FUserAgent: UTF8String;
  protected
    procedure DoPrepareHeader; override; //Called by Client
    procedure DoSendHeader; override;
    procedure DoHeaderSent; override;

    procedure DoHeaderReceived; override; //Called by Server
    procedure Created; override;
  public
    property Host: string read FHost write FHost;
    property Accept: String read FAccept write FAccept;
    property UserAgent: UTF8String read FUserAgent write FUserAgent;
  end;

  { TwebRespond }

  TwebRespond = class(TmodRespond)
  private
    FLocation: string;
    FHomePath: string; //Document root folder
    function GetRequest: TwebRequest;
  protected
    procedure DoPrepareHeader; override; //Called by Server
    procedure DoSendHeader; override;
    procedure DoHeaderSent; override;
    procedure DoHeaderReceived; override; //Called by Client
  public

    function StatusCode: Integer;
    function StatusResult: string;
    function StatusVersion: string;
    //Document root folder
    property HomePath: string read FHomePath write FHomePath;

    property Request: TwebRequest read GetRequest;
    property Location: string read FLocation write FLocation; //Relocation it to another url
  end;

  TwebCommand = class(TmnCustomServerCommand)
  private
  protected
    function CreateRespond: TmodRespond; override;

    procedure DoPrepareHeader(Sender: TmodCommunicate); override;

    procedure Prepare(var Result: TmodRespondResult); virtual;
    procedure RespondResult(var Result: TmodRespondResult); virtual;
    function Execute: TmodRespondResult;
    procedure Unprepare(var Result: TmodRespondResult); virtual;
    procedure Created; override;
  public
  end;

  //*

  TmodModule = class;

  TmodCommand = class(TwebCommand)
  private
    FModule: TmodModule;
    function GetActive: Boolean;
  protected
    procedure SetModule(const Value: TmodModule); virtual;
    procedure Log(S: String); virtual;

    function CreateRequest(AStream: TmnConnectionStream): TmodRequest; override;
    function CreateRespond: TmodRespond; override;
  public
    constructor Create(AModule: TmodModule; ARequest: TmodRequest); virtual;
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
    FUse: TmodCommunicateUsing;
    procedure SetAliasName(AValue: String);
  protected
    FFallbackCommand: TmodCommandClass;
    //Name here will corrected with registered item name for example Get -> GET
    function GetActive: Boolean; virtual;
    function GetCommandClass(var CommandName: String): TmodCommandClass; virtual;
    procedure Created; override;
    procedure DoRegisterCommands; virtual; //deprecated 'use RegisterItems';
    procedure RegisterCommands;
    procedure CreateItems; virtual;
    procedure DoMatch(const ARequest: TmodRequest; var vMatch: Boolean); virtual;
    procedure DoPrepareRequest(ARequest: TmodRequest); virtual;

    function Match(const ARequest: TmodRequest): Boolean; virtual;
    procedure PrepareRequest(ARequest: TmodRequest);
    function CreateCommand(CommandName: String; ARequest: TmodRequest): TmodCommand; overload;

    procedure DoReceiveHeader(ARequest: TmodRequest); virtual;
    procedure ReceiveHeader(ARequest: TmodRequest);

    function RequestCommand(ARequest: TmodRequest): TmodCommand; virtual;
    procedure Log(S: String); virtual;
    procedure Start; virtual;
    procedure Started; virtual;
    procedure Stop; virtual;
    procedure Reload; virtual;
    procedure Init; virtual;
    procedure Idle; virtual;
    procedure InternalError(ARequest: TmodRequest; var Handled: Boolean); virtual;
  public
    //Default fallback module should have no alias name
    //Protocols all should lowercase
    constructor Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules = nil); virtual;
    destructor Destroy; override;
    function RegisterCommand(vName: String; CommandClass: TmodCommandClass; AFallback: Boolean = False): Integer; overload;

    //* Run in Connection Thread
    function Execute(ARequest: TmodRequest): TmodRespondResult;

    property Commands: TmodCommandClasses read FCommands;
    property Active: Boolean read GetActive;
    property Modules: TmodModules read FModules;
    //* use lower case in Protocols
    property Protocols: TArray<String> read FProtocols;
    property AliasName: String read FAliasName write SetAliasName;
    //All modules before used sorted by Level
    property Level: Integer read FLevel write FLevel;

    property Use: TmodCommunicateUsing read FUse write FUse;

    property KeepAliveTimeOut: Integer read FUse.KeepAliveTimeOut write FUse.KeepAliveTimeOut;
    property UseKeepAlive: TmodOptionValue read FUse.KeepAlive write FUse.KeepAlive default ovUndefined;
    property UseCompressing: TmodOptionValue read FUse.Compressing write FUse.Compressing;
    property UseWebSocket: Boolean read FUse.WebSocket write FUse.WebSocket;
  end;

  TmodModuleClass = class of TmodModule;

  TmodModuleServer = class;

  { TmodModules }

  TmodModules = class(TmnNamedObjectList<TmodModule>)
  private
    FActive: Boolean;
    FEndOfLine: String;
    FDefaultProtocol: String;
    FInit: Boolean;
    FServer: TmodModuleServer;
    procedure SetEndOfLine(AValue: String);
  protected
    function CreateRequest(Astream: TmnBufferStream): TmodRequest; virtual;
    function CheckRequest(const ARequest: string): Boolean; virtual;
    function GetActive: Boolean; virtual;
    procedure Created; override;
    procedure Start; //When server start, init values here
    procedure Started; //after all modules started
    procedure Stop;
    procedure Init; //Init called from the first connection
    procedure Idle;
    function Compare(Left: TmodModule; Right: TmodModule): Integer; override;
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
    function Find(const AName: string): TmodModule; overload;
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
    procedure HandleException(E: Exception); override;
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
    FEnabled: Boolean;
    FModules: TmodModules;
    FName: string;
    procedure SetEnabled(AValue: Boolean);
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
    procedure Created; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Modules: TmodModules read FModules;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: string read FName write FName;
  end;

{ Pool }

  TmnPool = class;
  TmnPoolObject = class;
  TPoolObjectClass = class of TmnPoolObject;

  TmnPoolObject = class(TmnObject)
  private
    FPool: TmnPool;
    FSkip: Boolean;
    FName: string;
  protected
    FTerminated: Boolean;
    procedure DoPrepare; virtual; //execute in main thread
    procedure DoProcess; virtual; //execute in poolServices thread
    procedure DoUnprepare; virtual; //execute in poolServices thread

  public
    constructor Create(APool: TmnPool; const vName: string); overload;
    destructor Destroy; override;

    procedure Execute;
    procedure Terminate; virtual;
    procedure AfterConstruction; override;
    property Terminated: Boolean read FTerminated;
    procedure Enter;
    procedure Leave;

    property Name: string read FName;
  end;

  TPoolList = class(TmnObjectList<TmnPoolObject>)
  end;

  TmnPoolThread = class(TThread)
  protected
    FPool: TmnPool;
    procedure Execute; override;
    procedure TerminatedSet; override;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TmnPool = class(TObject)
  private
    FCurrent: TmnPoolObject;

    FLock: TCriticalSection;
    FEvent: TEvent;
    FWaitEvent: TEvent;

    FPoolList: TPoolList;
    FTaskList: TPoolList;
    FStarted: Boolean;
    FTerminated: Boolean;
  protected
    procedure Start;
    procedure Add(vPoolObject: TmnPoolObject);
    procedure Execute; virtual; //run it from the thread
  public
    constructor Create;
    destructor Destroy; override;
    property PoolList: TPoolList read FPoolList;
    property Lock: TCriticalSection read FLock;
    function FindName(vClass: TPoolObjectClass; const vName: string): Boolean;
    procedure SkipClass(vClass: TPoolObjectClass);
    procedure TerminateSet; virtual;
    property Terminated: Boolean read FTerminated;
  end;

function URIDecode(const S: UTF8String): utf8string;
function ParseRaw(const Raw: String; out Method, Protocol, URI: string): Boolean;
function ParseURI(const URI: String; out Address, Params: string): Boolean;
procedure ParseQuery(const Query: String; mnParams: TmnFields);
procedure ParseParamsEx(const Params: String; mnParams: TmnParams);

function ParseAddress(const Request: string; out URIPath: string; out URIQuery: string): Boolean; overload;
function ParseAddress(const Request: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams): Boolean; overload;
procedure ParsePath(const aRequest: string; out Name: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams);
function FormatHTTPDate(vDate: TDateTime): string;
function ExtractDomain(const URI: string): string;

function GetSubPath(const Path: string): string;
function DeleteSubPath(const SubKey, Path: string): string;
function StartsSubPath(const SubKey, Path: string): Boolean;

function ComposeHttpURL(UseSSL: Boolean; const DomainName: string; const Port: string = ''; const Directory: string = ''): string; overload;
function ComposeHttpURL(const Protocol, DomainName: string; const Port: string = ''; const Directory: string = ''): string; overload;

const
  ProtocolVersion = 'HTTP/1.1'; //* Capital letter
  sUserAgent = 'miniWebModule/1.1';

var
  DevelopperMode:Boolean = False;

implementation

uses
  mnUtils;

function ComposeHttpURL(UseSSL: Boolean; const DomainName: string; const Port: string = ''; const Directory: string = ''): string; overload;
begin
  if UseSSL then
    Result := ComposeHttpURL('https', DomainName, Port, Directory)
  else
    Result := ComposeHttpURL('http', DomainName, Port, Directory);
end;

function ComposeHttpURL(const Protocol, DomainName: string; const Port: string = ''; const Directory: string = ''): string; overload;
begin
  Result := Protocol + '://' + DomainName;

  if (Port<>'') and ((Protocol='https') and (Port<>'443'))or((Protocol='http') and (Port<>'80')) then
    Result := Result + ':' + Port;

  if Directory <> '' then
    Result := Result + '/' + Directory;
end;

function URIDecode(const S: UTF8String): utf8string;
var
  c: AnsiChar;
  D: Ansistring;
  i: Integer;
  R: RawByteString;
begin
  Result := '';
  i := Low(S);
  R := '';
  while i <= High(S) do
  begin
    C := S[i];
    {if C = '+' then
    begin
      R := R + ' ';
    end
    else}
    if C = '%' then
    begin
      D := copy(S, i + 1, 2);
      R := R + AnsiChar(StrToInt('$'+D));
      inc(i, 2);
    end
    else
      R := R + c;
    Inc(i);
  end;
  //SetCodePage(R, CP_UTF8, False);
  Result := R;
end;

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
      URI := URIDecode(aRequests[1]);
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

function ParseAddress(const Request: string; out URIPath: string; out URIQuery: string): Boolean;
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

function ParseAddress(const Request: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams): Boolean;
begin
  Result := ParseAddress(Request, URIPath, URIParams);
  if Result then
    if URIQuery <> nil then
      //ParseParams(aParams, False);
      StrToStringsCallback(URIParams, URIQuery, @FieldsCallBack, ['&'], [' ']);
end;

procedure ParsePath(const aRequest: string; out Name: string; out URIPath: string; out URIParams: string; URIQuery: TmnParams);
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

function GetSubPath(const Path: string): string;
begin
  if StartsText(URLPathDelim, Path) then
    Result := SubStr(Path, URLPathDelim, 1)
  else
    Result := SubStr(Path, URLPathDelim, 0)
end;


function DeleteSubPath(const SubKey, Path: string): string;
begin
  if StartsText(URLPathDelim, Path) then
    Result := Copy(Path, Length(URLPathDelim) + Length(SubKey) + 1, MaxInt)
  else
    Result := Copy(Path, Length(SubKey) + 1, MaxInt);
end;

function StartsSubPath(const SubKey, Path: string): Boolean;
begin
  if StartsText(URLPathDelim, Path) then
    Result := StartsStr(Path, URLPathDelim + SubKey)
  else
    Result := StartsStr(Path, SubKey);
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

constructor TmodRespond.Create(ARequest: TmodRequest);
begin
  inherited Create(ARequest.Parent);
  FRequest := ARequest;
end;

procedure TmodRespond.InitProtocol;
begin
  inherited;
end;

function TmodRespond.ReceiveStream(s: ImnStreamPersist; Count: Int64): Int64;
var
  aDecompress: Boolean;
  mStream: TMemoryStream;
//  zStream: {$ifdef FPC}TGZipDecompressionStream;{$else}TDecompressionStream{$endif}
begin
  aDecompress := (Request.Use.AcceptCompressing in [ovUndefined]) and (Header.Field['Content-Encoding'].Have('gzip', [',']));
  if aDecompress then
  begin
    mStream := TMemoryStream.Create;//duplicate memory avoid this :(
    try
      Result := gzipDecompressStream(Stream, mStream, Count);
      s.LoadFromStream(mStream, Result);
    finally
      mStream.Free;
    end;
  end
  else
  begin
    s.LoadFromStream(Stream, Count);
    Result := Count;
  end;
end;

function TmodRespond.ReceiveStream(s: TStream): Int64;
var
  aDecompress: Boolean;
begin
  if (Request.ChunkedProxy<>nil) and (ContentLength = 0) then
    Result := Stream.ReadStream(s, -1)
  else if (ContentLength > 0) and KeepAlive then //Respond.KeepAlive because we cant use compressed with keeplive or contentlength >0
  begin
    if (Request.CompressProxy<>nil) and (Request.CompressProxy.Limit <> 0) then
      Result := Stream.ReadStream(s, -1)
    else
    begin
      aDecompress := (Request.Use.AcceptCompressing in [ovUndefined]) and (Header.Field['Content-Encoding'].Have('gzip', [',']));
      if aDecompress then
        Result := gzipDecompressStream(Stream, s, ContentLength)
      else
        Result := Stream.ReadStream(s, ContentLength);
    end;
  end
  else
    Result := Stream.ReadStream(s, -1); //read complete stream
end;

function TmodRespond.SendStream(s: TStream; ASize: Int64): Boolean;
var
  stream: TInterfacedStreamtWrapper;
begin
  stream := TInterfacedStreamtWrapper.Create(s);
  try
    Result := SendStream(stream, ASize);
  finally
    FreeAndNil(stream);
  end;
end;

function TmodRespond.SendStream(s: ImnStreamPersist; Count: Int64): Boolean;
var
  aCompress: Boolean;

  procedure _SendHeader(ACount: Int64; ACompress: Boolean);
  begin
    if not (resHeaderSent in  Header.States) then
    begin
      ContentLength := ACount;
      if ACompress then
        PutHeader('Content-Encoding', 'gzip');
      SendHeader;
    end;
end;

var
  mStream: TMemoryStream;
  zStream: {$ifdef FPC}TGZipCompressionStream{$else}TZCompressionStream{$endif};
begin
  Result := Count<>0;

  if Count=0 then
    _SendHeader(0, False)
  else
  begin
    aCompress := Request.Mode.AllowCompress;

    if aCompress then
    begin
      mStream := TMemoryStream.Create;
      try
        zStream := {$ifdef FPC}TGZipCompressionStream.Create(clDefault, mStream);{$else}TZCompressionStream.Create(mStream, zcDefault, GzipBits[True]);{$endif}
        try
          s.SaveToStream(zStream, Count);
        finally
          zStream.Free;
        end;

        _SendHeader(mStream.Size, True);

        Stream.Write(mStream.Memory^, mStream.Size);

      finally
        mStream.Free;
      end;
    end
    else
    begin
      _SendHeader(Count, False);
      s.SaveToStream(Self.Stream, Count);
    end;
  end;
end;

function FileStamp(aFileDate: TDateTime; Size: Int64 = 0): string; inline;
begin
  Result := DateTimeToUnix(aFileDate).ToString;
  if Size <> 0 then
    Result := Result + '-' + Size.ToString;
end;

function TmodRespond.SendFile(AFileName: string; const AFileStamp: string; Alias: string; FileDispositions: TmodFileDispositions): Boolean;
var
  aStream: TStream;
  aSize: Int64;
  aFileDate: TDateTime;
begin
  if not FileExists(aFileName) then
  begin
    Answer := hrNotFound;
    exit(False);
  end;

  FileAge(AFileName, aFileDate);
  aSize := GetSizeOfFile(AFileName);

  if not DevelopperMode and not (fdResend in FileDispositions) and (AFileStamp <> '') and (AFileStamp = FileStamp(aFileDate, aSize)) then
  begin
    Answer := hrNotModified;
    exit(False);
  end;

  if Alias='' then
    Alias := ExtractFileName(AFileName);

  Answer := hrOK;

  aStream := TFileStream.Create(AFileName, fmShareDenyNone or fmOpenRead);
  try
    Result := SendStream(aStream, aSize, Alias, aFileDate, '', FileDispositions);
  finally
    aStream.Free;
  end;
end;

function TmodRespond.SendUTF8String(const s: UTF8String): Boolean;
var
  aStream: TmnPointerStream;
begin
  aStream := TmnPointerStream.Create(PByte(s), Length(s));
  try
    Result := SendStream(aStream, Length(s));
  finally
    aStream.Free;
  end;
end;

function TmodRespond.SendString(const s: string): Boolean;
var
  t: UTF8String;
begin
  {$ifdef FPC}
  t := UTF8Encode(s);
  Result := SendString(t);
  {$else}
  Result := SendUTF8String(t);
  {$endif}
end;

function TmodRespond.SendStream(s: TStream; ASize: Int64; AAlias: string; AFileDate: TDateTime; const AFileStamp: string; FileDispositions: TmodFileDispositions): Boolean;
var
  aMIMEItem: TmnMIMEItem;
  aDisposition, aStamp: string;
begin
  aStamp := FileStamp(AFileDate, ASize);

  if not DevelopperMode and not (fdResend in FileDispositions) and (AFileStamp <> '') and (AFileStamp = aStamp) then
  begin
    Answer := hrNotModified;
    exit(False);
  end;

  Stamp := aStamp;

  Header['Cache-Control']  := 'public, max-age=600';
  if AFileDate > 0 then
    Header['Last-Modified']  := FormatHTTPDate(AFileDate);

  if fdInline in FileDispositions then
    aDisposition := 'inline'
  else if fdAttachment in FileDispositions then
    aDisposition := 'attachment'
  else
    aDisposition := '';

  aMIMEItem := DocumentToMIME(AAlias);
  if aMIMEItem <> nil then
  begin
    ContentType := aMIMEItem.ContentType;
    if Binary in aMIMEItem.Features then
    begin
      if aDisposition = '' then
        aDisposition := 'attachment';
      if AAlias <> '' then
        aDisposition := ConcatString(aDisposition, ';', 'filename="' + AAlias + '"')
    end;
  end
  else
  begin
    ContentType := 'application/octet-stream';
    if aDisposition = '' then
      aDisposition := 'attachment';
    if (AAlias <> '') then
        aDisposition := ConcatString(aDisposition, ';', 'filename="' + AAlias + '"')
  end;

  if aDisposition <> '' then
    Header['Content-Disposition'] := aDisposition;

  Result := SendStream(s, ASize);
end;

function TmodRespond.GetStream: TmnBufferStream;
begin
  Result := FRequest.Stream;
end;

function TmodRespond.WriteLine(const s: string): Boolean;
begin
  Result := Stream.WriteUTF8Line(S) > 0;
end;

function TmodRespond.WriteString(const s: string): Boolean;
begin
  Result := Stream.WriteUTF8String(UTF8Encode(S)) > 0;
end;

{ TmodRequest }

function TmodRequest.CollectURI: string;
begin
  Result := URLPathDelim + Address;
  if Query<>'' then
    Result := Result+'?'+Query
end;

constructor TmodRequest.Create(ACommand: TmnCustomCommand; AStream: TmnBufferStream);
begin
  inherited Create(ACommand);
  FStream := AStream;
  Use.KeepAlive   := ovUndefined;
  Use.Compressing := ovUndefined;
  Use.WebSocket   := False;
end;

procedure TmodRequest.Created;
begin
  inherited;
  FRoute := TmnRoute.Create;
  FParams := TmodParams.Create;
end;

destructor TmodRequest.Destroy;
begin
  FreeAndNil(FRoute);
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TmodRequest.ReadLine(out s: string): Boolean;
begin
  Result := Stream.ReadUTF8Line(s);
end;

function TmodRequest.ReadString(out s: string; Count: Integer): Boolean;
begin
  Result := Stream.ReadUTF8String(s, TFileSize(Count));
end;

procedure TmodRequest.ResetProxies;
begin
  CompressProxy.Disable;
  ChunkedProxy.Disable;
  ProtcolProxy.Disable;
end;

procedure TmodRequest.SetChunkedProxy(const Value: TmnChunkStreamProxy);
begin
  if (Value <> nil) and (FChunkedProxy <> nil) then
    raise TmodModuleException.Create('Chunked class is already set!');
  FChunkedProxy := Value;
end;

procedure TmodRequest.SetCompressProxy(const Value: TmnCompressStreamProxy);
begin
  if (Value <> nil) and (FCompressProxy <> nil) then
    raise TmodModuleException.Create('Compress proxy is already set!');
  FCompressProxy := Value;
end;

procedure TmodRequest.SetProtcolClass(const Value: TmnProtcolStreamProxyClass);
begin
  FProtcolClass := Value;
end;

procedure TmodRequest.SetStream(AStream: TmnBufferStream; TriggerHeader: Boolean);
begin
  FStream := AStream;
  SetTrigger(TriggerHeader);
end;

procedure TmodRequest.Clear;
begin
  Initialize(Info);
end;

procedure TmodRequest.DoHeaderReceived;
begin
  //????
end;

procedure TmodRequest.InitProtocol;
begin
  ResetProxies;
  inherited;
end;

function TmodRequest.GetConnected: Boolean;
begin
  Result := (Stream <> nil) and Stream.Connected;
end;

function TmodRequest.GetStream: TmnBufferStream;
begin
  Result := FStream;
end;

procedure TmodRequest.InitProxies(vChunked: Boolean; vCompressClass: TmnCompressStreamProxyClass);
begin
  if vChunked then
  begin
    if ChunkedProxy <> nil then
      ChunkedProxy.Enable
    else
    begin
      ChunkedProxy := TmnChunkStreamProxy.Create;
      Stream.AddProxy(ChunkedProxy);
    end;
  end
  else
    ChunkedProxy.Disable;

  if vCompressClass <> nil then
  begin
    if CompressProxy <> nil then
      CompressProxy.Enable
    else
    begin
      CompressProxy := vCompressClass.Create([cprsRead, cprsWrite], 9);
      Stream.AddProxy(CompressProxy);
    end;
  end
  else
    CompressProxy.Disable;
end;

{ TmodModuleListener }


procedure TmodModuleServer.Created;
begin
  inherited;
  FModules := CreateModules;
end;

constructor TmodModuleServer.Create;
begin
  inherited;
  FEnabled := True;
  Port := '80';
end;

function TmodModuleServer.CreateModules: TmodModules;
begin
  Result := TmodModules.Create(Self);
end;

destructor TmodModuleServer.Destroy;
begin
  inherited;
  FreeAndNil(FModules); //After Inherited
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

procedure TmodModuleConnection.HandleException(E: Exception);
begin
  inherited;
  //ModuleServer.Log(E.Message);
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

    aRequest := ModuleServer.Modules.CreateRequest(Stream);
    try
      ModuleServer.Modules.ParseHead(aRequest, aRequestLine);
      aModule := ModuleServer.Modules.Match(aRequest);

      if (aModule = nil) then
      begin
        Stream.Disconnect; //if failed
      end
      else
        try
          aRequest.Client := RemoteIP;
          aRequest.IsSSL := IsSSL;
          Result := aModule.Execute(aRequest);
        finally
          if Stream.Connected then
          begin
            if (mrKeepAlive in Result.Status) then
            begin
              Stream.ReadTimeout := Result.Timout;
              //Stream.Close([cloWrite]); need flush ???
            end
            else
              Stream.Disconnect;
          end;
        end;
    finally
      FreeAndNil(aRequest); //if create command then aRequest change to nil
    end;
  end;
end;

procedure TmodModuleServer.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then
    Exit;
  FEnabled := AValue;
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
  Modules.Started;
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

{ TmnCustomCommandListener }

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

{ TmnCustomCommand }

procedure TmnCustomCommand.DoSendHeader(Sender: TmodCommunicate);
begin
end;


destructor TmnCustomCommand.Destroy;
begin
  FreeAndNil(FRespond);
  //FreeAndNil(FRequest); in server command free request is outside but http client command free it
  inherited;
end;

constructor TmnCustomCommand.Create;
begin
  inherited Create;

  {FRequest := ARequest; //do not free
  if FRequest <> nil then //like webserver
  begin
    FRequest.FParent := Self;
    //FRequest.DoHeaderReceived;
    //DoHeaderReceived(ARequest);
  end
  else //like httpclient
  begin
    FRequest := CreateRequest(AStream);
  end;}

end;

procedure TmnCustomCommand.Created;
begin
  inherited;
end;

function TmnCustomCommand.CreateRequest(AStream: TmnConnectionStream): TmodRequest;
begin
  Result := TmodRequest.Create(Self, AStream);
end;

function TmnCustomCommand.CreateRespond: TmodRespond;
begin
  Result := TmodRespond.Create(Request);
end;

procedure TmnCustomCommand.DoHeaderSent(Sender: TmodCommunicate);
begin
end;

procedure TmnCustomCommand.DoPrepareHeader(Sender: TmodCommunicate);
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

{ TwebCommand }

procedure TwebCommand.Created;
begin
  inherited;
end;

function TwebCommand.CreateRespond: TmodRespond;
begin
  Result := TwebRespond.Create(Request);
end;

procedure TwebCommand.DoPrepareHeader(Sender: TmodCommunicate);
begin
  inherited;
end;

function TwebCommand.Execute: TmodRespondResult;
begin
  Result.Status := []; //default to be not keep alive, not sure, TODO
  Result.Timout := Request.Use.KeepAliveTimeOut; //not sure, TODO
  Prepare(Result);
  try
    RespondResult(Result);
  finally
    Unprepare(Result);
  end;
end;

procedure TwebCommand.Prepare(var Result: TmodRespondResult);
begin
end;

procedure TwebCommand.RespondResult(var Result: TmodRespondResult);
begin
end;

procedure TwebCommand.Unprepare(var Result: TmodRespondResult);
begin
end;

{ TmodCommand }

procedure TmodCommand.Log(S: String);
begin
  Module.Log(S);
end;

function TmodCommand.CreateRequest(AStream: TmnConnectionStream): TmodRequest;
begin
  Result := TwebRequest.Create(Self, AStream);
end;

function TmodCommand.CreateRespond: TmodRespond;
begin
  Result := TwebRespond.Create(Request);
end;

function TmodCommand.GetActive: Boolean;
begin
  Result := (Module <> nil) and (Module.Active);
end;

procedure TmodCommand.SetModule(const Value: TmodModule);
begin
  FModule := Value;
end;

constructor TmodCommand.Create(AModule: TmodModule; ARequest: TmodRequest);
begin
  inherited Create(ARequest);
  FModule := AModule;
end;

{ TmodModule }

function TmodModule.CreateCommand(CommandName: String; ARequest: TmodRequest): TmodCommand;
var
  //  aName: string;
  aClass: TmodCommandClass;
begin
  //aName := GetCommandName(ARequest, ARequestStream);

  aClass := GetCommandClass(CommandName);
  if aClass <> nil then
  begin
    Result := aClass.Create(Self, ARequest);
    Result.Name := CommandName;
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

procedure TmodModule.InternalError(ARequest: TmodRequest; var Handled: Boolean);
begin

end;

procedure TmodModule.ReceiveHeader(ARequest: TmodRequest);
begin
  ARequest.ReceiveHeader(False); //* Head is already recieved elsewhere
  DoReceiveHeader(ARequest);
end;

procedure TmodModule.Created;
begin
  inherited;
end;

procedure TmodModule.RegisterCommands;
begin
  DoRegisterCommands;
end;

procedure TmodModule.CreateItems;
begin
end;

procedure TmodModule.Start;
begin

end;

procedure TmodModule.Started;
begin
end;

procedure TmodModule.Stop;
begin
end;

function TmodModule.RequestCommand(ARequest: TmodRequest): TmodCommand;
begin
  Result := CreateCommand(ARequest.Command, ARequest);
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
  FUse.KeepAliveTimeOut := cDefaultKeepAliveTimeOut; //TODO move module
  CreateItems;
  RegisterCommands;
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

function TmodModule.Execute(ARequest: TmodRequest): TmodRespondResult;
var
  aCommand: TmodCommand;
  aHandled: Boolean;
begin
  //Result.Status := [mrSuccess];

  ARequest.Use.KeepAliveTimeOut := KeepAliveTimeOut;
  ARequest.Use.KeepAlive        := UseKeepAlive;
  ARequest.Use.Compressing      := UseCompressing;
  ARequest.Use.WebSocket        := UseWebSocket;

  ReceiveHeader(ARequest);

  aCommand := RequestCommand(ARequest);

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
    InternalError(ARequest, aHandled);

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

function TmodModules.Find(const AName: string): TmodModule;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, AName) then
    begin
      Result := Items[i];
      break;
    end;
  end;
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
  QuickSort;
  for aModule in Self do
    aModule.Start;
  FActive := True;
end;

procedure TmodModules.Started;
var
  aModule: TmodModule;
begin
  for aModule in Self do
    aModule.Started;
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

function TmodModules.Compare(Left: TmodModule; Right: TmodModule): Integer;
begin
  Result := Left.Level - Right.Level;
end;

constructor TmodModules.Create(AServer: TmodModuleServer);
begin
  inherited Create;
  FServer := AServer;
end;

procedure TmodModules.Created;
begin
  inherited;
  FEndOfLine := sWinEndOfLine; //for http protocol
end;

function TmodModules.CreateRequest(Astream: TmnBufferStream): TmodRequest;
begin
  Result := TmodRequest.Create(nil, Astream);
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

{ TmnwCookie }

procedure TmnwCookie.SetChanged;
begin
  FChanged := True;
end;

procedure TmnwCookie.Created;
begin
  inherited;
  FAge := -1; //Forever
  Secured := False; //over HTTP and HTTPS
end;

procedure TmnwCookie.Delete;
begin
  FDeleting := True;
  SetChanged;
end;

function TmnwCookie.GenerateValue: string;
var
  aDate: TDateTime;
begin
  aDate := IncSecond(Now, Age);
  Result := Value;
  if FDeleting or (Result = '') then //Delete it
  begin
    Result := Result + '; max-age=0';
  end
  else
  begin
    if Age >= 0 then
      Result := Result + '; max-age=' + Age.ToString;
      //Result := Result + '; Expires=' + FormatHTTPDate(aDate);
  end;

  if Secured then
    Result := Result + '; Secure';

  if Stricted then
    Result := Result + '; SameSite=Strict'
  else
    Result := Result + '; SameSite=None';

  if Domain <> '' then
    Result := Result + '; Domain=' + Domain.ToLower;

  if Path <> '' then
    Result := Result + '; Path=' + Path.ToLower
  else
    Result := Result + '; Path=/';

end;

procedure TmnwCookie.ResetChanged;
begin
  FChanged := False;
end;

function TmnwCookie.GetText: string;
begin
  Result := Name + '=' + GenerateValue;;
end;

procedure TmnwCookie.SetAge(const AValue: Integer);
begin
  if FAge <> AValue then
  begin
    FAge := AValue;
    SetChanged;
  end;
end;

procedure TmnwCookie.SetDomain(const AValue: string);
begin
  if not SameText(FDomain, AValue) then
  begin
    FDomain := AValue;
    SetChanged;
  end;
end;

procedure TmnwCookie.SetPath(const AValue: string);
begin
  if not SameText(FPath, AValue) then
  begin
    FPath := AValue;
    SetChanged;
  end;
end;

procedure TmnwCookie.SetValue(const AValue: string);
begin
  if not SameText(Value, AValue) then
  begin
    inherited SetValue(AValue);
    SetChanged;
  end;
end;

{ TmnwCookies }

procedure CookiesStrToStringsDeqouteCallbackProc(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
var
  Name, Value: string;
  p: Integer;
  Cookie: TmnwCookie;
begin
  if s <> '' then
  begin
    s := Trim(s);
    if s <> '' then
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
      Cookie := (TObject(Sender) as TmnwCookies).Add(Name, Value);
    end;
  end;
end;

procedure TmnwCookies.SetRequestText(S: string);
var
  MatchCount: Integer;
begin
  Clear;
  StrToStringsExCallback(S, 0, Self, [';'], MatchCount, CookiesStrToStringsDeqouteCallbackProc, [' ', #0, #13, #10]);
end;

function TmnwCookies.GetRequestText: string;
var
  Cookie: TmnwCookie;
begin
  Result := '';
  for Cookie in Self do
  begin
    if Result <> '' then
      Result := Result + '; ';
    Result := cookie.Name + '=' + Cookie.Value;
  end;
end;

{ TmodCommunicate }

procedure TmodCommunicate.Clear;
begin
  FHeader.Clear;
end;

function TmodCommunicate.IsHeaderSent: Boolean;
begin
  Result := (resHeaderSent in Header.States);
end;

constructor TmodCommunicate.Create(ACommand: TmnCustomCommand);
begin
  inherited Create;
  FParent := ACommand;
  FHeader := TmodHeader.Create;
  FCookies := TmnwCookies.Create;
  //FCookies.Delimiter := ';';
  FStreamControl := TmodCommunicateStreamControl.Create;
  FStreamControl.FCommunicate := Self;
end;

destructor TmodCommunicate.Destroy;
begin
  SetTrigger(False);
  FreeAndNil(FHeader);
  FreeAndNil(FCookies);
  FreeAndNil(FStreamControl);
  inherited;
end;

function TmodCommunicate.GetCookie(const vNameSpace, vName: string): string;
begin
  if vNameSpace<>'' then
    Result := Cookies.Values[vNameSpace+'.'+vName]
  else
    Result := Cookies.Values[vName];
end;

procedure TmodCommunicate.ReceiveHead;
begin
  Stream.ReadUTF8Line(FHead);
end;

procedure TmodCommunicate.ReceiveHeader(WithHead: Boolean);
begin
  Reset;
  InitProtocol;
  if WithHead then
	  ReceiveHead;
  Header.Clear;
  Header.ReadHeader(Stream);
  DoHeaderReceived;
end;

procedure TmodCommunicate.SendHead;
begin
  InitProtocol;

  if resHeadSent in Header.States then
    raise TmodModuleException.Create('Head is sent');

  if Head = '' then
    raise TmodModuleException.Create('Head is not set');

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

procedure TmodCommunicate.InitProtocol;
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

procedure TmodCommunicate.SetCookie(const vName, Value: string);
begin
  SetCookie('', vName, Value);
end;

procedure TmodCommunicate.SetHead(const Value: string);
begin
  FHead := Value;
end;

{function TmodCommunicate.GetLatch: Boolean;
begin
  Result := resLatch in Header.FStates;
end;

procedure TmodCommunicate.SetLatch(const AValue: Boolean);
begin
  if AValue then
    Header.FStates := Header.FStates + [resLatch]
  else
    Header.FStates := Header.FStates - [resLatch];
end;}

procedure TmodCommunicate.Reset;
begin
  FWritingStarted := False;
  //FHeader.Clear;
  FHeader.FStates := [];
end;

procedure TmodCommunicate.SendHeader(WithHead: Boolean);
var
  item: TmnField;
  s: String;
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is sent');

  Header.FStates := Header.FStates + [resHeaderSending];

  DoPrepareHeader;
  if Parent<> nil then
    Parent.DoPrepareHeader(Self);

  SendHead;

  for item in Header do
  begin
    s := item.GetNameValue(': ');
    Stream.WriteUTF8Line(s);
  end;

  {s := '';
  for item in Header do
  begin
    if s<>'' then s := s + Stream.EndOfLine;
    s := s + item.GetNameValue(': ');
  end;
  Stream.WriteUTF8Line(s);}

  DoWriteCookies;

  DoSendHeader; //enter after

  if Parent<> nil then
    Parent.DoSendHeader(Self);

  Stream.WriteUTF8Line(Utf8string(''));
  Header.FStates := Header.FStates + [resHeaderSent] - [resHeaderSending];

  DoHeaderSent;
  if Parent<> nil then
    Parent.DoHeaderSent(Self);
end;

procedure TmodCommunicate.AddHeader(const AName: string; AValue: TDateTime);
begin
  AddHeader(AName, FormatHTTPDate(AValue));
end;

procedure TmodCommunicate.AddHeader(const AName, AValue: String);
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is already sent: '+ Head);

  Header.Add(AName, AValue);
end;

procedure TmodCommunicate.PutHeader(AName, AValue: String);
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is sent');

  Header.Put(AName, AValue);
end;

procedure TmodCommunicate.DoWriting(vCount: Longint);
begin
  if not FWritingStarted then
  begin
    FWritingStarted := True;
    try
      {if resLatch in Header.States then
        raise TmodModuleException.Create('You can''t send data at this phase'); //maybe before sending header}
      if not (resHeaderSending in Header.States) and not (resHeaderSent in Header.States) then
        SendHeader(True);
    finally
      FWritingStarted := False;
    end;
  end;
end;

procedure TmodCommunicate.DoReading(vCount: Longint);
begin
end;

procedure TmodCommunicate.SetTrigger(TriggerHeader: Boolean);
begin
  if (Stream <> nil) then
  begin
    if TriggerHeader then
      Stream.Control := FStreamControl
    else
      Stream.Control := nil;
  end;
end;

{ TmnRoute }

function TmnRoute.GetRoute(vIndex: Integer): string;
begin
  if vIndex<Count then
    Result := Strings[vIndex]
  else
    Result := '';
end;

{ THttpResultHelper }

function TmodAnswerHelper.ToString: string;
begin
  Result := 'HTTP/1.1 ';
  case Self of
    hrNone: Result := '';
    hrOK: Result := Result + '200 OK';
    hrNoContent: Result := Result + '204 No Content';
    hrError: Result := Result + '500 Internal Server Error';
    hrUnauthorized: Result := Result + '401 Unauthorized';
    hrForbidden : Result := Result + '403 Forbidden';
    hrNotFound: Result := Result + '404 NotFound';
    hrMovedTemporarily: Result := Result + '307 Temporary Redirect';
    hrMovedPermanently: Result := '301 Moved Permanently';
    hrRedirect: Result := Result + '302 Found';
    hrNotModified: Result := Result + '304 Not Modified';
    hrSwitchingProtocols: Result := Result + '101 Switching Protocols';
    hrServiceUnavailable: Result := Result + '503 Service Unavailable';
  end;
end;

{ TmodHeader }

procedure TmodHeader.Clear;
begin
  inherited;
  FStates := [];
end;

function TmodHeader.Domain: string;
var
  s: string;
begin
  s := Origin;
  if s <> '*' then
    Result := ExtractDomain(s)
  else
    Result := '';
end;

function TmodHeader.Host: string;
begin
  Result := Self['Host'];
  if Result = '' then
    Result := 'localhost';
end;

function TmodHeader.Origin: string;
begin
  Result := Self['Origin'];
  if Result = '' then
    Result := '*';
end;

{ TmodCommunicateStreamControl }

procedure TmodCommunicateStreamControl.Writing(Count: Longint);
begin
  inherited;
  if FCommunicate <> nil then
    FCommunicate.DoWriting(Count);
end;

procedure TmodCommunicateStreamControl.Reading(Count: Longint);
begin
  inherited;
  if FCommunicate <> nil then
    FCommunicate.DoReading(Count);
end;

{ TwebRequest }

procedure TwebRequest.Created;
begin
  inherited;
  Accept := '*/*';
  UserAgent := sUserAgent;
end;

procedure TwebRequest.DoHeaderReceived;
var
  aChunked: Boolean;
  //aCompressClass: TmnCompressStreamProxyClass;
begin
  inherited;

  FHost  := Header.Field['Host'].AsString;

  if (Header.Field['Content-Length'].IsExists) then
    ContentLength := Header.Field['Content-Length'].AsInt64;

  Cookies.SetRequestText(Header['Cookie']);

  FAccept := Header.ReadString('Connection');
  KeepAlive := (Use.KeepAlive = ovUndefined) and SameText(Header.ReadString('Connection'), 'Keep-Alive');
  KeepAlive := KeepAlive or ((Use.KeepAlive = ovYes) and not SameText(Header.ReadString('Connection'), 'close'));

  aChunked := Header.Field['Transfer-Encoding'].Have('chunked', [',']);

  Stamp  := Header.Field['If-None-Match'].AsString;

  {aCompressClass := nil;
  if (Header.Field['Content-Encoding'].IsExists) then
  begin
    if Header.Field['Content-Encoding'].Have('gzip', [',']) then
      aCompressClass := TmnGzipStreamProxy
    else if Header.Field['Content-Encoding'].Have('deflate', [',']) then
      aCompressClass := TmnDeflateStreamProxy;
  end;
  InitProxies(aChunked, aCompressClass);}

  FMode := [];

  if Header.Field['Content-Encoding'].Have('gzip', [',']) then
    FMode := FMode + [smRequestCompress];

  if (Header.Field['Accept-Encoding'].Have('gzip', [','])) then
  begin
    case Use.Compressing of
      ovYes:
      begin
        if KeepAlive then  //when keep alive we need content length
          FMode := FMode + [smAllowCompress]
        else
          FMode := FMode + [smRespondCompressing];
      end;
      ovUndefined: FMode := FMode + [smAllowCompress];
      else
      begin
        //nothing
      end;
    end;
  end;

  if (smRequestCompress in Mode) or (smRespondCompressing in Mode) then
  begin
    InitProxies(aChunked, TmnGzipStreamProxy);
    if not (smRequestCompress in Mode) then
      CompressProxy.Disable;
  end
  else
    InitProxies(aChunked, nil);
end;

procedure TwebRequest.DoHeaderSent;
begin
  inherited;
  //We are here the client
  if (Use.Compressing = ovYes) then
    InitProxies(False, TmnGzipStreamProxy);
end;

procedure TwebRequest.DoPrepareHeader;
begin
  inherited;
  PutHeader('Host', Host);
  if (ContentLength > 0) then
    PutHeader('Content-Length', IntToStr(ContentLength));
  if Accept <> '' then
    PutHeader('Accept', Accept);

  case Use.KeepAlive of
    ovUndefined: ; //TODO
    ovYes:
    begin
      PutHeader('Connection', 'Keep-Alive');
      //Keep-Alive: timeout=1200
    end;
    ovNo:
      PutHeader('Connection', 'close');
  end;

  if Use.AcceptCompressing in [ovUndefined, ovYes] then
    PutHeader('Accept-Encoding', 'gzip, deflate');

  if (Use.Compressing = ovYes) then //to send data by request (post)
    PutHeader('Content-Encoding', 'gzip');

  PutHeader('User-Agent', UserAgent);
end;

procedure TwebRequest.DoSendHeader;
var
  s: UTF8String;
begin
  inherited;
  s := UTF8Encode(Cookies.GetRequestText);
  if s <> '' then
    Stream.WriteUTF8Line('Cookie: ' + s);
end;

{ TwebRespond }

procedure TwebRespond.DoHeaderSent;
begin
  inherited;

  if Request.CompressProxy<>nil then
  begin
    Request.CompressProxy.Enabled := smRespondCompressing in Request.Mode;
    Request.CompressProxy.Limit := 0;
  end;
end;

procedure TwebRespond.DoHeaderReceived;
var
  aCompressClass: TmnCompressStreamProxyClass;
  aChunked: Boolean;
begin
  inherited;
  if (Header.Field['Content-Length'].IsExists) then
    ContentLength := Header.Field['Content-Length'].AsInt64;

  if (Header.Field['Content-Type'].IsExists) then
    ContentType  := Header.Field['Content-Type'].AsString;

  if (Header.Field['ETag'].IsExists) then
    Stamp  := Header.Field['ETag'].AsString; //* or maybe 'If-None-Match'

  if (Header.Field['Connection'].IsExists) then
    KeepAlive := SameText(Header['Connection'], 'Keep-Alive');

  aChunked := Header.Field['Transfer-Encoding'].Have('chunked', [',']);

  aCompressClass := nil;
  if Request.Use.AcceptCompressing = ovYes then
  begin
    if Header.Field['Content-Encoding'].Have('gzip', [',']) then
      aCompressClass := TmnGzipStreamProxy
    else if Header.Field['Content-Encoding'].Have('deflate', [',']) then
      aCompressClass := TmnDeflateStreamProxy;
  end;

  Request.InitProxies(aChunked, aCompressClass);

  if (aCompressClass<>nil)and KeepAlive then
    Request.CompressProxy.Limit := ContentLength;
end;

procedure TwebRespond.DoPrepareHeader;
begin
  inherited;
  if (ContentLength > 0) {and (smRespondCompressing in Mode)} then //if we use proxies we cant send content length
    PutHeader('Content-Length', IntToStr(ContentLength));

  if (ContentType <> '') then
    PutHeader('Content-Type', ContentType);

  if (Stamp <> '') then
    PutHeader('ETag', Stamp);

  if smRespondCompressing in Request.Mode then
      PutHeader('Content-Encoding', Request.CompressProxy.GetCompressName);

  if Location <> '' then
    PutHeader('Location', Location)
end;

procedure TwebRespond.DoSendHeader;
var
  Cookie: TmnwCookie;
begin
  inherited;
  if Cookies.Count > 0 then
  begin
    for Cookie in Cookies do
      Stream.WriteUTF8Line('Set-Cookie: ' + Cookie.GetText);
  end;
end;

procedure TmodRespond.SetAnswer(const Value: TmodAnswer);
begin
  if resHeaderSent in Header.States then
    raise TmodModuleException.Create('Header is already sent');
  FAnswer := Value;
  Head := Answer.ToString;
end;

function TwebRespond.StatusCode: Integer;
var
  s: string;
begin
  s := SubStr(Head, ' ', 1);
  Result := StrToIntDef(s, 0);
end;

function TwebRespond.StatusResult: string;
begin
  Result := SubStr(Head, ' ', 2); { TODO : to correct use remain text :) }
end;

function TwebRespond.StatusVersion: string;
begin
  Result := SubStr(Head, ' ', 0);
end;

function TwebRespond.GetRequest: TwebRequest;
begin
  Result := inherited Request as TwebRequest;
end;

{ TmodOptionValueHelper }

function TmodOptionValueHelper.GetAsBoolean: Boolean;
begin
  Result := Self <> ovNo;
end;

function TmodOptionValueHelper.GetAsString: String;
begin
  case Self of
    ovYes: Result := 'Yes';
    ovNo: Result := 'No';
    ovUndefined: Result := 'Undefined';
  end;
end;

{class operator TmodOptionValueHelper.Explicit(const Source: Boolean): TmodOptionValue;
begin
  Result.AsBoolean := Source;
end;

class operator TmodOptionValueHelper.Implicit(Source: Boolean): TmodOptionValue;
begin
  Result.AsBoolean := Source;
end;

class operator TmodOptionValueHelper.Implicit(Source: TmodOptionValue): Boolean;
begin
  Result := Source.AsBoolean;
end;}

procedure TmodOptionValueHelper.SetAsBoolean(const Value: Boolean);
begin
  if Value then
    Self := ovYes
  else
    Self := ovNo;
end;

{ TmnCustomServerCommand }

constructor TmnCustomServerCommand.Create(ARequest: TmodRequest);
begin
  inherited Create;
  FRequest := ARequest; //do not free
  FRequest.FParent := Self;

  FRespond := CreateRespond;
  FRespond.SetTrigger(True);
end;

{ TmnCustomClientCommand }

constructor TmnCustomClientCommand.Create;
begin
  inherited Create;
end;

{ TStreamModeHelper }

function TStreamModeHelper.AllowCompress: Boolean;
begin
  Result := smAllowCompress in Self;
end;

function TStreamModeHelper.RequestCompress: Boolean;
begin
  Result := smRequestCompress in Self;
end;

function TStreamModeHelper.RespondCompressing: Boolean;
begin
  Result := smRespondCompressing in Self;
end;

{ Pool }

{ TmnPoolObject }

constructor TmnPoolObject.Create(APool: TmnPool; const vName: string);
begin
  inherited Create;
  FPool := APool;
  FName := vName;
  FSkip := False;
  FTerminated := False;
//  FPool.Add(Self);
  DoPrepare;
end;

procedure TmnPoolObject.AfterConstruction;
begin
  inherited;
  FPool.Add(Self);
end;

destructor TmnPoolObject.Destroy;
begin
  DoUnprepare;
  inherited;
end;

procedure TmnPoolObject.DoPrepare;
begin
end;

procedure TmnPoolObject.DoProcess;
begin
end;

procedure TmnPoolObject.DoUnprepare;
begin
end;

procedure TmnPoolObject.Enter;
begin
  FPool.Lock.Enter;
end;

procedure TmnPoolObject.Execute;
begin
  DoProcess;
  FPool.Lock.Enter;
  try
    FPool.FTaskList.Extract(Self);
  finally
    FPool.Lock.Leave;
  end;

  Free; //<------ Wrong
end;

procedure TmnPoolObject.Leave;
begin
  FPool.Lock.Leave;
end;

procedure TmnPoolObject.Terminate;
begin
  FTerminated := True;
end;

{ TmnPoolThread }

constructor TmnPoolThread.Create;
begin
  inherited Create(True);

end;

destructor TmnPoolThread.Destroy;
begin

  inherited;
end;

procedure TmnPoolThread.Execute;
begin
  FPool.TerminateSet;
end;

procedure TmnPool.Execute;
var
  aCurrent: TmnPoolObject;
begin
  inherited;
  while not Terminated do
  begin
    Lock.Enter;
    try
      if PoolList.Count <> 0 then
        FCurrent := PoolList.Extract(PoolList.First) { TODO : try use PoolList as queue }
      else
        FCurrent := nil;
    finally
      Lock.Leave;
    end;

    if FCurrent <> nil then
    begin
      FWaitEvent.ResetEvent;
      try
        if not FCurrent.FSkip then
        begin
          //Sleep(2000);
          try
            FCurrent.DoProcess;//
          except
            raise;
{            on E: Exception do
              Log('Error Pool Object [%s]: %s', [FCurrent.ClassName, E.Message]);}
          end;
        end;
      finally
        aCurrent := FCurrent;
        Lock.Enter;
        try
          FCurrent := nil;
        finally
          Lock.Leave;
        end;
        FreeAndNil(aCurrent);
        FWaitEvent.SetEvent;
      end;
    end
    else
    begin
      FEvent.WaitFor;
    end;
  end;
end;

procedure TmnPoolThread.TerminatedSet;
begin
  inherited;
  FPool.TerminateSet;
end;

{ TmnPool }

procedure TmnPool.Add(vPoolObject: TmnPoolObject);
begin
  Lock.Enter;
  try
    Start;

    PoolList.Add(vPoolObject);
    if PoolList.Count=1 then
      FEvent.SetEvent;
  finally
    Lock.Leave;
  end;
end;

constructor TmnPool.Create;
begin
  inherited Create;
  FStarted := False;
  FPoolList := TPoolList.Create;
  FTaskList := TPoolList.Create;
  FLock := TCriticalSection.Create;
  FEvent := TEvent.Create(nil, False, False, '');
  FWaitEvent := TEvent.Create(nil, True, True, '');
end;

destructor TmnPool.Destroy;
begin
  FreeAndNil(FWaitEvent);

  FreeAndNil(FLock);
  FreeAndNil(FEvent);
  FreeAndNil(FTaskList);
  FreeAndNil(FPoolList);

  inherited;
end;

function TmnPool.FindName(vClass: TPoolObjectClass; const vName: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  Lock.Enter;
  try
    for I := 0 to PoolList.Count-1 do
    begin
      if (PoolList[i].ClassType=vClass) and (PoolList[i].Name=vName) then
      begin
        Result := True;
        Break;
      end;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TmnPool.Start;
begin
  if not FStarted then
  begin
    FStarted := True;
  end;
end;

procedure TmnPool.TerminateSet;
begin
  FEvent.SetEvent;
  //FTickEvent.SetEvent;
  //if FCurrent <> nil then FCurrent.FTerminated := True;
end;

procedure TmnPool.SkipClass(vClass: TPoolObjectClass);
var
  PoolObject: TmnPoolObject;
begin
  Lock.Enter;
  try
    for PoolObject in PoolList do
    begin
      if (PoolObject.ClassType=vClass) then
        PoolObject.FSkip := True;
    end;

		//waitforcurrent;

    if (FCurrent <> nil) and (FCurrent.ClassType = vClass) then
    begin
      FCurrent.Terminate;
    end;
  finally
    Lock.Leave;
  end;

  FWaitEvent.WaitFor;
end;

{ TInterfacedStreamtWrapper }

constructor TInterfacedStreamtWrapper.Create(vStream: TStream);
begin
  inherited Create;
  FStream := vStream;
end;

procedure TInterfacedStreamtWrapper.LoadFromStream(Stream: TStream; Count: Int64);
begin
  FStream.CopyFrom(Stream,Count);
end;

procedure TInterfacedStreamtWrapper.SaveToStream(Stream: TStream; Count: Int64);
begin
  Stream.CopyFrom(FStream, Count);
end;

initialization
  DefFormatSettings := TFormatSettings.Invariant;
end.
