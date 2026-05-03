unit mnWebElements;
{$IFDEF FPC}
{$mode delphi}
{$modeswitch prefixedattributes}
{$modeswitch arrayoperators}
{$modeswitch arraytodynarray}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$H+}{$M+}
{**
 *  This file is part of the "MiniLib"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *
 *}

{
   Protocol UserInfo       Host      Port
    ┌─┴─┐   ┌──┴───┐ ┌──────┴────────┬┴┐
GET https://john.doe@www.example.com:123/forum/username/questions/q/10/?tag=networking&order=newest#top
└┬┘                  └──────┬──────┘    └───────────────┬─────────────┘└────────────┬─────────────┘└─┬─┘
Method                  DomainName                    Path                        Query           Fragment
                                        └──┬──┘└───┬───┘└───┬────┴──┬─┘           └─┬─┘
WebElement:                             Module  Namespace Schema  Directory       Params
    └────────────┬──────────────────────┘ ─ ─ ┘        |         |    |
    |          HostURL (From Request or Config)        |         |    |
    └────────────────────────┬─────────────────────────┘         |    |
    |             HomeURL/URL (WebApp)                           |    |
    └────────────────────────┬───────────────────────────────────┘    |
    |                       URL         |                             |
    └────────────────────────┬────────────────────────────────────────┘
                             ?          |               |        |    |
                                        |               |        |    |
                                        └──────────┬────┘        |    |
                                        |       ModulePath       |    |
                                        └──────────┬─────────────┘    |                                        
                                        |         Path                |  
                                        └──────────┬──────────────────┘
                                                   ?     
{

    Application
    
                    Document
┌──────────────────────┴──────────────────────┐
┌──────┬──────────────────────────────────────┐  ─┐
│>Logo │ Brand NavBar                      c =│   ├─ Header
├──────┴──────────────────────────────────────┤  ─┤
│ MenuBar                                     │   │
├────────────┬────────────────────────────────┤   │
│ Sidebar    │ Main                           │   │
│    ─┬─     │                                │   ├─ Container
│ Accordion  │ ┌─ TabControl ──────┐ ┌──────┐ │   │
│            │ │ Tab │    │        │ │ Card │ │   │
│            │ ├─────┴────┴────────┤ ├──────┤ │   │
│            │ │ ┌─ Control ────┐  │ │      │ │   │
│            │ │ └──────────────┘  │ │      │ │   │
│            │ └───────────────────┘ └──────┘ │   │
│            │ ┌─ Form ────────────┐ ┌──────┐ │   │
│            │ │                   │ │ Panel│ │   │
│            │ │ ┌─ Control ────┐  │ │      │ │   │
│            │ │ └──────────────┘  │ │      │ │   │
│            │ │ ┌─ Control ────┐  │ │      │ │   │
│            │ │ └──────────────┘  │ │      │ │   │
│            │ └───────────────────┘ └──────┘ │   │
│            │                                │   │
├────────────┴── Footer ──────────────────────┤  ─┤
│                                             │   ├─ Footer
└─────────────────────────────────────────────┘  ─┘

  https://bootstrap.build/app
  https://www.layoutit.com
  https://coreui.io/bootstrap/docs/components/card/
  https://leafletjs.com/examples.html
  https://github.com/mdbootstrap/mdb-ui-kit

  Testing

    https://pagespeed.web.dev/analysis/http-dirkey-ddns-net-home-demo/zourq3i3sg?form_factor=mobile

  Good example:

    https://bootstrapmade.com/demo/templates/NiceAdmin/index.html
}

{.$define LOG}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Contnrs, Variants, Types, RTTI,
  {$ifdef FPC}
  resource, //* for RT_RCDATA
  {$endif}
  syncobjs, mnDON, mnJSON,
  mnUtils, mnClasses, mnStreams, mnLogs, mnMIME, mnParams, mnTypes,
  mnMultipartData, mnModules, mnWebModules;

{.$define rtti_objects}

type
{$ifdef FPC}
  THandle = Cardinal;
{$else}
  THandle = Int64;
{$endif}

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwElementRenderer = class;
  TmnwElementRenderers = class;
  TmnwRendererClass = class of TmnwRenderer;
  TmnwElementRendererRegister = class;

  TmnwElementClass = class of TmnwElement;
  TElementExtension = class;
  TElementExtensionClass = class of TElementExtension;

  TmnwResponse = class;

  { TElementExtension }

  TElementExtension = class(TCustomAttribute)
  public
    class procedure Update(Element: TmnwElement); virtual; abstract;
    constructor Create; //* Leave it
  end;

  { TID_Extension }

  TID_Extension = class(TElementExtension)
  private
  public
    class procedure Update(Element: TmnwElement); override;
  end;

  { TName_Extension }

  TName_Extension = class(TElementExtension)
  private
  public
    class procedure Update(Element: TmnwElement); override;
  end;

  { TRoute_Extension }

  TRoute_Extension = class(TElementExtension)
  private
  public
    class procedure Update(Element: TmnwElement); override;
  end;

  { TmnwAttribute }

  TmnwAttribute = class(TmnNameValueObject)
  public
    function CreateSubValues(vSeparators: TSysCharSet = [' ']): TStringList;
  end;

  { TmnwAttributes }

  TmnwAttributes = class(TmnNameValueObjectList<TmnwAttribute>)
  protected
    procedure Created; override;
  public
    function ToString: string; override;
    function GetText: string;
    procedure Delete(Name: string); overload;
    function HaveSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function SetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function UnsetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    procedure Append(AAttributes: TmnwAttributes);
  end;

  TDirection = (dirUndefined, dirLeftToRight, dirRightToLeft);

  TLocationRelative = (
    toNone,
    toElement,
    toSchema,
    toHome,
    toCustom
  );

  //Decorate
  TItemStyle = (
    styleUndefined,
    stylePrimary,
    styleSecondary,
    styleSuccess,
    styleDanger,
    styleWarning,
    styleInfo,
    styleLight,
    styleDark,
    styleLink,
    styleNone
  );

  { TLocation }

  TLocation = record
    Where: TLocationRelative;
    Custom: string;
    class operator Explicit(const Source: string): TLocation;
    class operator Implicit(Source : string) : TLocation;
    class operator Implicit(Source : TLocation): string;
    class operator Implicit(Source : TLocationRelative) : TLocation;
    function IsDefeined: Boolean;
  end;

  TImageLocationType = (imgIconClass, imgPath);

  { TImageLocation }

  TImageLocation = record
  private
    FLocationType: TImageLocationType;
    FValue: string;
    function GetIconClass: string;
    function GetPath: string;
    procedure SetIconClass(const AValue: string);
    procedure SetPath(const AValue: string);
  public
    property Path: string read GetPath write SetPath;
    property IconClass: string read GetIconClass write SetIconClass;
  end;

  { TmnwBounding }

  TmnwBounding = record
    Top, Bottom, Right, Left: Double;
    class operator Explicit(const Source: Integer): TmnwBounding;
    class operator Implicit(Source : Integer) : TmnwBounding;
    class operator Implicit(Source : TmnwBounding): Integer;

    class operator Explicit(const Source: Double): TmnwBounding;
    class operator Implicit(Source : Double) : TmnwBounding;
    class operator Implicit(Source : TmnwBounding): Double;
    function ToString: string; inline;
    class operator Initialize({$ifdef FPC}var{$else}out{$endif}Dest: TmnwBounding);
    procedure SetTopBottom(Value: Double);
    procedure SetLeftRight(Value: Double);
	end;

  { TElementClasses }

  TElementClasses = record
    Items: TArray<String>;
    function Find(const Name: string): Integer;
    function Add(const Name: string): Integer;
    function Remove(const Name: string): Boolean;
    procedure AddClasses(const S: string); overload;
    procedure AddClasses(A: TElementClasses); overload;
    function ToString: string;
    class operator Add(A: TElementClasses; B: string): TElementClasses;
    class operator Subtract(A: TElementClasses; B: string): TElementClasses;
    class operator Explicit(const Source: string): TElementClasses;
    class operator Implicit(Source : string) : TElementClasses;
    class operator Implicit(Source : TElementClasses): string;
    procedure Init(classes: string = '');
  end;

  TmnwWeb = class;
  
  { TmnwScope }

  TmnwScope = record
    Element: TmnwElement;
    Attributes: TmnwAttributes;
    Classes: TElementClasses;
    WrapClasses: TElementClasses; //WrapClass is a class used of what parent wrapped it
    function ToString: string;
    function GetText: string;
  end;

  TmnwContext = record
    Sender: TObject;
    Web: TmnwWeb;
    Request: TmodRequest;

    Schema: TmnwSchema;    
    Element: TmnwElement;
    
    Renderer: TmnwRenderer;
    ParentRenderer: TmnwElementRenderer;

    Writer: TmnTidyWriter;
    //
    Data: TmnMultipartData;
    // For
    Stamp: string; //IfNone-Match
    Route: string;   
    SessionID: String;
    Session: TObject;

    // http://host:80/
    function GetHostURL: string; overload;
    // /module/namespace
    function GetHomePath: string; overload;
    // http://host:80/module/namespace/
    function GetHomeURL: string; overload;

    // With Schema
    // /module/namespace/schema
    function GetPath: string; overload;
    // /module/namespace/schema/element    
    function GetPath(e: TmnwElement): string; overload;    

    //this get absolute path http://host:80/module/namespace/schema/element
    function GetURL(e: TmnwElement): string; overload;
    //this get absolute path http://host:80/module/namespace/schema
    function GetURL: string; overload;

    //this get path relative requested path /element1/element2
    function GetRelativePath(e: TmnwElement): string; overload;    

    //Schema URL with http://host:80/assets/namespace/schema
    function GetAssetsPath: string;
    function GetAssetsURL: string;
    //Folder of HomeFolder of assets
    function GetAssetFolder: string;
    function GetLocationPath(Location: TLocation): string;
  end;

  TmnwObject = class(TmnNamedObject);

  TLibraryOption = (libDefer, libCross);
  
  TLibraryOptions = set of TLibraryOption;

  TLibrarySourceType = (
    stStyle, 
    stScript
  );

  TLibrarySourceWhere = (
    stOnline, 
    stEmbed,
    stResource
  );
  
  TLibrarySource = class(TmnNamedObject)
  public    
    SourceType: TLibrarySourceType;
    Where: TLibrarySourceWhere;
    Value: string;
    LocalFileName: string;    
    Integrity: string;
    Direction: TDirection;
    Options: TLibraryOptions;    
    constructor Create; virtual;
  end;

  TLibrarySources = class(TmnNamedObjectList<TLibrarySource>)
  private
  public
    //LocalFile: from assets, only file name, not with path
    //OnlineFile: if OnlineFile ended with / LocalFile will added
    function Add(SourceType: TLibrarySourceType; Where: TLibrarySourceWhere; const OnlineFile, LocalFileName: string; Direction: TDirection; Integrity: string = ''; Options: TLibraryOptions = [libDefer, libCross]): TLibrarySource; overload;

    function Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFileName: string): TLibrarySource; overload;
    function Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFileName: string; Integrity: string; Options: TLibraryOptions = [libDefer, libCross]): TLibrarySource; overload;

    function AddStyle(const EmbedText: string; Direction: TDirection = dirUndefined): TLibrarySource; overload;
  end;
  
  TmnwLibrary = class abstract(TmnNamedObject)
  private
    FUsage: Integer;
    FPriority: Integer;
    FEndOfBody: Boolean;
    FDependsOn: TmnwLibrary;
    FSources: TLibrarySources;
  protected
    function CheckOffline(const Context: TmnwContext; const FileName: string): Boolean;    
    procedure Created; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
     
    procedure AddHead(const Context: TmnwContext); virtual; 
    
    procedure IncUsage;
    procedure DecUsage;
    property Usage: Integer read FUsage;
    property EndOfBody: Boolean read FEndOfBody write FEndOfBody;
    property Priority: Integer read FPriority write FPriority;
    property DependsOn: TmnwLibrary read FDependsOn write FDependsOn;
    property Sources: TLibrarySources read FSources;
  end;

  TmnwLibraryClass = class of TmnwLibrary;

  { TmnwLibraries }

  TmnwLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  protected
    function Compare(Item1, Item2: TmnwLibrary): Integer; override;
  public
    procedure Use(ALibraryClass: TmnwLibraryClass); overload;
    procedure Use(ALibrary: TmnwLibrary); overload;
    procedure Use(ALibraryName: string); overload;
    function Find(ALibrary: string): TmnwLibrary; overload;
    function Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary; overload;
    function RegisterLibrary(ALibraryClass: TmnwLibraryClass; UseIt: Boolean = False): TmnwLibrary; overload;
    function RegisterLibrary(ALibraryClass: TmnwLibraryClass; Priority: Integer; UseIt: Boolean = False): TmnwLibrary; overload;
  end;

  TmnwUsedLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  public
    procedure Use(ALibraryClass: TmnwLibraryClass); overload;
    procedure Use(ALibrary: TmnwLibrary); overload;
    procedure Use(ALibraryName: string); overload;
  end;

  TJQuery_Library = class(TmnwLibrary)
  protected
    procedure Created; override;     
  public
  end;

  { TWebElements_Library }

  TWebElements_Library = class(TmnwLibrary)
  protected
    procedure Created; override;     
  public
  end;

  TElementExecute = reference to procedure;

  TmnwRequestState = (rsBeforeRequest, rsAfterRequest);

  TmnwElementState = set of (
    estComposing,
    estComposed
  );

  TmnwElementKind = (
    elNoRender,
    elNoRespond,
//    elFallback, //* if no child have the route name, it take the respond if have a name
    elEmbed, //* created by parent
    elInternal //* we will render it manually
  );
  TmnwElementKinds = set of TmnwElementKind;


  TmnwPriority = (priorityNormal, priorityStart, priorityEnd);

  TTheme = (themeUndefined, themeLight, themeDark);
  TmnwShadow = (shadowUndefined, shadowLight, shadowHeavy);

  TmnwAlign = (alignDefault, alignStart, alignCenter, alignStreach, alignBaseline, alignEnd);
  TmnwFixed= (fixedDefault, fixedTop, fixedBottom, fixedStart, fixedEnd, stickyTop, stickyBottom, stickyStart, stickyEnd);

  TRespondProc = reference to procedure (const AContext: TmnwContext; AResponse: TmnwResponse);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FHandle: THandle;
    FStyle: String;
    FVisible: Boolean;
    FSchema: TmnwSchema;
    FParent: TmnwElement;

    FRoute: String;
    FComment: String;
    FID: String;
    FName: String;
    FElementClass: String;
    FAttributes: TmnwAttributes;
    FKind: TmnwElementKinds;
    FPriority: TmnwPriority;
    FState: TmnwElementState;
    FOnExecute: TElementExecute;
    FOnRespond: TRespondProc;
    FPrepared: Boolean;
    FIsRoot: Boolean;
    FTimeStamp: Int64;
    procedure SetState(const AValue: TmnwElementState);
    function GetRespondIt: Boolean;
    function GetRenderIt: Boolean;
    procedure SetRenderIt(const Value: Boolean);
  protected    
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;

    procedure ServeFolder(APath: string; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse);
    function ServeFile(HomeFolder: string; DefaultDocuments: TStringList; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse): Boolean; overload;
    function ServeFile(HomeFolder: string; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse): Boolean; overload;

    procedure DoPrepareRenderer(const AContext: TmnwContext); virtual;   
    procedure DoPrepare; virtual;
    
    procedure DoCompose(const AContext: TmnwContext); virtual;
    procedure DoComposed; virtual;

    procedure DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;

    procedure PrepareRenderer(const AContext: TmnwContext); 
    procedure Prepare; 

    procedure DoExecute; virtual;
    procedure Execute;
    procedure DoChanged; virtual;
    procedure Changed;

    procedure SendMessage(AttachmentName:string; AMessage: string); overload;
    procedure SendInteractive(AMessage: string); overload;

    procedure SendMessage(JSON: TDON_Pair); overload; virtual;
    procedure ReceiveMessage(JSON: TDON_Pair); virtual;
    
    function GenHandle: Integer;
    function GenID: string;
    function GenRoute: string;
    function GenName: string;
  public
    constructor Create(AParent: TmnwElement; AKind: TmnwElementKinds = []); virtual;
    destructor Destroy; override;    

    function Add(O: TmnwElementClass): TmnwElement; overload;
    procedure Add(O: TmnwElement); overload;
    function Add<O: TmnwElement>(const AID: String = ''; const AName: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function FindByRoute(const ARoute: string; Level: Integer = 0): TmnwElement;
    function FindByID(const aID: string): TmnwElement;
    function FindByName(const aName: string): TmnwElement;
    function FindParentName(const aName: string): TmnwElement;
    function FindParentID(const aID: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; virtual; //I wish i have templates/meta programming in pascal
    property Schema: TmnwSchema read FSchema;
    property Parent: TmnwElement read FParent;

    //GetPath get path to the schema, not to domain/host
    //Use Contex.GetPath(e) to get path to the module name
    //this get path with schema/element/element/element
    function GetPath: string; 
    //this get path without schema and parent element name, element/element
    function GetPathTo(ToElement: TmnwElement): string; overload;
    //Include Host
    
    function CreateRenderer(const Context: TmnwContext): TmnwElementRenderer;
    procedure Compose(const AContext: TmnwContext); virtual;
    procedure AddState(AState: TmnwElementState);
    procedure RemoveState(AState: TmnwElementState);

    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif} //* see TmnObjectList

    function GetContentType(Route: string = ''): string; virtual;

    procedure RespondInit(const AContext: TmnwContext; AResponse: TmnwResponse);
    procedure Respond(const AContext: TmnwContext; AResponse: TmnwResponse);

    //* Original Render
    procedure Render(const Context: TmnwContext; AResponse: TmnwResponse); overload;

    function CanRender: Boolean; virtual;

    property IsRoot: Boolean read FIsRoot write FIsRoot;

    property ID: String read FID write FID;
    property Name: String read FName write FName;
    property Route: String read FRoute write FRoute; 
    property Style: String read FStyle write FStyle; //* no, it is not css style
    property ElementClass: String read FElementClass write FElementClass;
    property Comment: String read FComment write FComment;

    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;


    property RespondIt: Boolean read GetRespondIt; // false: do not use respond
    property RenderIt: Boolean read GetRenderIt write SetRenderIt;


    property Attributes: TmnwAttributes read FAttributes;
    property Kind: TmnwElementKinds read FKind write FKind;
    property Priority: TmnwPriority read FPriority write FPriority;
    property State: TmnwElementState read FState write SetState;

    property OnExecute: TElementExecute read FOnExecute write FOnExecute;
    property OnRespond: TRespondProc read FOnRespond write FOnRespond;
    property Handle: THandle read FHandle;

    property TimeStamp: Int64 read FTimeStamp;
  end;

  TmnwMessage = class(TObject)
  public
    Content: string;
  end;

  { TmnwMessages }

  TmnwMessages = class(TmnObjectList<TmnwMessage>)
  public
    procedure Add(s: string); overload;
  end;

  { TmnwAttachment }

  TmnwAttachment = class(TmnNamedObject)
  private
    FInteractive: Boolean;
    FTerminated: Boolean;
    procedure SendMessage(const Message: string);
  protected
    procedure Loop; virtual;
    procedure Terminate; virtual;
  public
    Schema: TmnwSchema;
    Stream: TmnBufferStream;
    destructor Destroy; override;
    property Terminated: Boolean read FTerminated;
    property Interactive: Boolean read FInteractive;
  end;

  { TmnwAttachments }

  TmnwAttachments = class(TmnNamedObjectList<TmnwAttachment>)
  private
    FLock: TCriticalSection;
  protected
    procedure Created; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    procedure SendMessage(const Message: string); overload;
    procedure SendMessage(const AttachmentName: string; const Message: string); overload;
    procedure Add(AAttachment: TmnwAttachment);
    procedure Remove(AAttachment: TmnwAttachment);
    property Lock: TCriticalSection read FLock;
  end;

  TmnwSchamaCapability = (
    schemaStartup, //* Create it when registered
    schemaSession,
    schemaAttach, //Allow websocket connections, Interactive also allow websocket
    schemaPermanent, //* Not deleted when restart server
    schemaDynamic  //* dynamic, do not add it to the list, not cached, becareful
  );

  TmnwSchemaCapabilities = set of TmnwSchamaCapability;

  TmnwWebModule = class;

  TmnwSchemaPhase = (
    scmpNew,
    scmpNormal,
    scmpReleased
	);

  { TmnwSchema }

  TmnwSchema = class(TmnwElement)
  private
    FAttached: Boolean;
    FAttachments: TmnwAttachments;
    FDefaultDocuments: TStringList;
    FLock: TCriticalSection;
    FWeb: TmnwWeb;
    FPhase: TmnwSchemaPhase;
    FNamingLastNumber: THandle;
    function GetReleased: Boolean;
    procedure SetDefaultDocuments(AValue: TStringList);
  protected
    Usage: Integer;
    procedure UpdateAttached;
    class procedure Registered; virtual;
    procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoAccept(var AContext: TmnwContext; var Resume: Boolean); virtual;
    procedure DoPrepare; override;
    procedure DoChildRespond(AElement: TmnwElement; const AContext: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure AttachedMessage(const s: string); virtual; //from websocket
    procedure InteractiveMessage(const s: string);
    property DefaultDocuments: TStringList read FDefaultDocuments write SetDefaultDocuments;
  public
    LastAccess: TDateTime;
    IsManual: Boolean;
    Direction: TDirection;
    RefreshInterval: Integer; //* in seconds, for refresh elements that need auto refresh
    HomeFolder: string;
    ServeFiles: TmodServeFiles;
    SessionID: string;
    Interactive: Boolean;
    constructor Create(AWeb: TmnwWeb; AName:string; ARoute: string = ''); reintroduce;
    destructor Destroy; override;

    class function GetCapabilities: TmnwSchemaCapabilities; virtual;
    function NewHandle: THandle;

    function GetHomeFolder: string;
    //* Attaching cap
    //function Interactive: Boolean;

    procedure Start; virtual;
    function Accept(var AContext: TmnwContext): Boolean;
    procedure Compose(const AContext: TmnwContext); override;

    // Executed from a thread of connection of WebSocket, it stay inside until the disconnect or terminate
    procedure Attach(Route: string; Sender: TObject; AStream: TmnBufferStream); // in connection thread

    property Attachments: TmnwAttachments read FAttachments;
    property Attached: Boolean read FAttached;
    property Released: Boolean read GetReleased;
    property Phase: TmnwSchemaPhase read FPhase;
    property Lock: TCriticalSection read FLock;
    property Web: TmnwWeb read FWeb;
  public
    type

    TFileOptions = set of (
      ftEmbed, 
      ftResource
    );

    { TFile }

    //* For resource Use FileName := 'myfile.js' but the resource name will took as myfile only, extention will be for mime

    { TElement }

    TElement = class(TmnwElement) //nothing
    public
      constructor Create(AParent: TmnwElement; ARoute: string); reintroduce;
    end;

    [TRoute_Extension]
    TRoute = class(TmnwElement)          
    protected
      procedure Created; override;
    public
      constructor Create(AParent: TmnwElement; ARoute: string; AKind: TmnwElementKinds = [elNoRespond]); reintroduce; virtual; 
    end;
    
    [TID_Extension]
    TFile = class(TmnwElement)
    protected
      procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    public
      FileName: string;
      Options: TFileOptions;
      constructor Create(AParent: TmnwElement; AOptions: TFileOptions = []; AFileName: string = ''; ARoute: string = ''); reintroduce;
      function GetContentType(Route: string): string; override;
    end;

    { TMemoryImage }

    [TID_Extension]
    TMemory = class(TmnwElement)
    private
      ContentType: string;
      FData: TMemoryStream;
    protected
      procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    protected
      procedure Created; override;
    public
      FileDate: TDateTime;
      FileName: string;
      FilePath: string;
      destructor Destroy; override;
      function GetContentType(Route: string): string; override;
      procedure LoadFromFile(const AFileName: string);
      procedure LoadFromStream(AStream: TStream; AContentType: string);
      property Data: TMemoryStream read FData;
    end;

  end;

  TmnwSchemaClass = class of TmnwSchema;

  { TmnwElementRenderer }

  TmnwElementRenderer = class(TObject)
  private
    FRenderer: TmnwRenderer;
    FRendererRegister: TmnwElementRendererRegister;
  protected
    //* Keep `var`
    procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); virtual;

    procedure RenderChilds(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);

    //This function called one time
    procedure AddHead(const Scope: TmnwScope; const Context: TmnwContext); virtual;
    //* Called to parent to wrap the child rendering, each chiled will wrap it with this render
    //* This method exists in parent render
    //* Keep `var`
    procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Called only if have parent but exists in a child
    procedure DoEnterOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Content render
    procedure DoEnterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoLeaveRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    property Renderer: TmnwRenderer read FRenderer;
    property RendererRegister: TmnwElementRendererRegister read FRendererRegister;
  public
    procedure Render(AElement: TmnwElement; const Context: TmnwContext; AResponse: TmnwResponse);
    constructor Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwElementRendererRegister); virtual; //useful for creating it by RendererClass.Create
    procedure CollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
  end;

  TmnwElementRendererClass = class of TmnwElementRenderer;

  TmnwElementLibraryItem = class(TmnObject)
  public
    Usage: Integer;
    RendererClass: TmnwElementRendererClass;
  end;
  
  TmnwElementLibrary = class(TmnObjectList<TmnwElementLibraryItem>)
  public  
    function Add(ARendererClass: TmnwElementRendererClass): TmnwElementLibraryItem;
  end;

  { TmnwRenderer }

  TmnwRenderer = class abstract(TmnwObject)
  private
    FModule: TmodWebModule;
    FLibraries: TmnwLibraries;
    FParams: TmnwAttributes;
  protected
    {$ifdef rtti_objects}
    procedure RegisterClasses(ASchemaClass: TmnwSchemaClass);
    {$endif}
    procedure DoBeginRender; virtual;
    procedure DoEndRender; virtual;    

  public
    constructor Create(AModule: TmodWebModule); virtual;
    destructor Destroy; override;

    class function ElementRenderers: TmnwElementRenderers; virtual; abstract; 
    class function RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = False): TmnwElementRendererRegister; 
    
    procedure BeginRender;
    procedure EndRender;
    
    function CreateRenderer(AElementClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property Params: TmnwAttributes read FParams;
    property Libraries: TmnwLibraries read FLibraries;
    property Module: TmodWebModule read FModule;

    procedure AddHead(const Context: TmnwContext); virtual; 
  public
    RendererID: Integer;
  end;

  TmnwPlaneRenderer = class(TmnwRenderer)
  protected
    class var Plane_ElementRenderers: TmnwElementRenderers;
    procedure Created; override;
  public
    class function ElementRenderers: TmnwElementRenderers; override;
    class destructor Destroy;      
  end;
  
  { TmnwSchemaItem }

  TmnwSchemaItem = class(TmnNamedObject)
  public
    SchemaClass: TmnwSchemaClass;
    destructor Destroy; override;
  end;

  TRegisteredSchemas = class(TmnNamedObjectList<TmnwSchemaItem>)
  end;

  { TmnwSchemaObject }

  TmnwSchemaObject = class(TmnwSchemaItem)
  private
    FLock: TCriticalSection;
  public
    Schema: TmnwSchema;
    ManualSchema: Boolean; //Schema set from outside not by request
    constructor Create;
    destructor Destroy; override;
    property Lock: TCriticalSection read FLock;
  end;

  TAssetsSchema = class;

  TmnwAppOptions = set of (
    apoHeader,
    apoSideBar,
    apoFooter
  );

  TOnlineFiles = (
    olfSmart,
    olfOnline,
    olfOffline
  );

  { TmnwWeb }

  TmnwWeb = class(TmnObjectList<TmnwSchema>)
  private
    FOptions: TmnwAppOptions;
    FHomeFolder: string;
    FAppFolder: string;
    FWorkFolder: string;
    FAssets: TAssetsSchema;
    FDefaultSchema: TmnwSchemaItem;
    FShutdown: Boolean;
    FLock: TCriticalSection;
    FRegistered: TRegisteredSchemas;
    FTimeStamp: Int64;
    FOnlineFiles: TOnlineFiles;
  protected
    procedure SchemaCreated(Schema: TmnwSchema); virtual;
    procedure Created; override;
    procedure ClearSchemas;
  public
    Started: Boolean;

    IsSecure: Boolean;
    Domain: string; //localhost
    Port: string;
    
    ModuleName: string; //Module Name

    CompactMode: Boolean;
    DefaultAge: Integer;

    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;

    function RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass; AsDefaultSchema: Boolean = False): TmnwSchema;
    property Registered: TRegisteredSchemas read FRegistered;

    function FindBy(const aSchemaName: string; const aSessionID: string): TmnwSchema;
    function CreateSchema(const aSchemaName: string): TmnwSchema; overload;
    function CreateSchema(const SchemaClass: TmnwSchemaClass; AName: string): TmnwSchema; overload;
    function CreateSchema(SchemaItem: TmnwSchemaItem): TmnwSchema; overload;
    function ReleaseSchema(const aSchemaName: string; aSessionID: string): TmnwSchema;
    function GetElement(var AContext: TmnwContext; out Element: TmnwElement): Boolean;

    //for HTML
    procedure Respond(var AContext: TmnwContext; AResponse: TmnwResponse);
    //for WebSocket
    function Attach(const AContext: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;

    function GetHostURL: string; virtual;    

    property Lock: TCriticalSection read FLock;
    property Assets: TAssetsSchema read FAssets;
    property DefaultSchema: TmnwSchemaItem read FDefaultSchema;
    //Public Web Files
    property HomeFolder: string read FHomeFolder write FHomeFolder;
    //Private Files
    property WorkFolder: string read FWorkFolder write FWorkFolder;
    //Exe path
    property AppFolder: string read FAppFolder write FAppFolder;
    property Shutdown: Boolean read FShutdown;
    property Options: TmnwAppOptions read FOptions write FOptions;
    property OnlineFiles: TOnlineFiles read FOnlineFiles write FOnlineFiles;
    property TimeStamp: Int64 read FTimeStamp;
  end;

  TSize = (
		szUndefined,
	 	szVerySmall,
		szSmall,
		szNormal,
		szLarge,
		szVeryLarge,

    szParent,
    szContent
	);

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  { THTML }

  THTML = class(TmnwSchema)
  public
    type
      TNavBar = class;
      TMenuBar = class;
      THeader = class;
      TContent = class;
      TSideBar = class;
      TFooter = class;
      TToast = class;
      TMain = class;
      TImage = class;
      TImageFile = class;
      TBody = class;
      TDocument = class;

      { THTMLElement }

      THTMLElement = class(TmnwElement)
      protected
      public
      end;

      { TComment }

      TComment = class(THTMLElement)
      public
        Comment: string;
      end;

      THTMLLayout = class abstract(THTMLElement)
      public
        Fixed: TmnwFixed;
        Solitary: Boolean; //* Single in Row
        Align: TmnwAlign;
        AlignItems: TmnwAlign;
        JustifyItems: TmnwAlign;

        Margin: TmnwBounding;
        Padding: TmnwBounding;
        Medium: Boolean; //Medium or above
      end;

      { THTMLComponent }

      THTMLComponent = class abstract(THTMLLayout)
      protected
        procedure Created; override;
      public
      end;

      { THTMLControl }

      THTMLControl = class abstract(THTMLComponent)
      protected
        procedure Created; override;
      public
        Size: TSize; //Max Width
        Shadow: TmnwShadow;
        Hint: string;
        ControlStyle: TItemStyle;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
      public
        //A script that will be downloaded in parallel to parsing the page, and executed after the page has finished parsing:
        Defer: Boolean;
//        Async: Boolean;
      end;

      TCSSFile = class(TFile)
      protected
      end;

      { TAssets }

      TAssets = class(THTMLElement)
      protected
        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      public
        HomeFolder: string;
        ServeFiles: TmodServeFiles;
        function GetContentType(Route: string): string; override;
      end;

      TFolder = class(THTMLElement)
      protected
        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      public
        HomeFolder: string;
        ServeFiles: TmodServeFiles;
        function GetContentType(Route: string): string; override;
      end;

      TComposeProc = reference to procedure(Inner: TmnwElement; AResponse: TmnwResponse);

      { TDynamicCompose }

      TDynamicCompose = class(THTMLElement)
      protected
        type

          { TInner}

          TInner = class(TmnwElement)
          public
          end;

        procedure InnerCompose(Inner: TmnwElement; AResponse: TmnwResponse); virtual;

        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      public
        OnCompose: TComposeProc;
        constructor Create(AParent: TmnwElement; AOnCompose: TComposeProc = nil); reintroduce;
      end;

      [TID_Extension]
      [TRoute_Extension]
      TIntervalCompose = class(TDynamicCompose)
      public
        Code: string;
      end;

      { TDocument }

      TDocument = class(TAssets)
      private
        FTitle: string;
        FVersion: integer;
        FBody: TBody;
      protected
        procedure Created; override;
      public
        property Version: integer read FVersion write FVersion;
        property Title: string read FTitle write FTitle;
        destructor Destroy; override;
        property Body: TBody read FBody;
      end;

      { TBody }

      TBody = class(THTMLElement)
      private
        function GetWide: Boolean;
        procedure SetWide(const Value: Boolean);
      protected
        FHeader: THeader;

        FContent: TContent;
        FSideBar: TSideBar;
        FMain: TMain;

        FFooter: TFooter;
        FToast: TToast;
      protected
      public
        Theme: TTheme;
        FontName: string;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKinds =[]); override;
        destructor Destroy; override;
        property Header: THeader read FHeader;
        property SideBar: TSideBar read FSideBar;
        property Main: TMain read FMain;
        property Footer: TFooter read FFooter;
        property Toast: TToast read FToast;
        property Wide: Boolean read GetWide write SetWide;
      end;

      TNavTools = class(THTMLComponent)
      end;

      TDropdownOptions = set of (dropArraw, dropSplit, dropEnd);

      TNavDropdown = class(THTMLComponent)
      protected
      public
        Options: TDropdownOptions;
        Caption: string;
        Image: TImageLocation;
      end;

      { TNavBar }

      [TID_Extension]
      TNavBar = class(THTMLComponent)
      private
        FTools: TNavTools;
        FImage: TImageFile;
      public
        Title: string;
//        LogoImage: string;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKinds =[]); override;
        destructor Destroy; override;
        property Image: TImageFile read FImage;
        property Tools: TNavTools read FTools;
      end;

      THeader = class(THTMLControl)
      private
        function GetMenuBar: TMenuBar;
        function GetNavBar: TNavBar;
      protected
        FNavBar: TNavBar;
        FMenuBar: TMenuBar;
        procedure Created; override;
      public
        property MenuBar: TMenuBar read GetMenuBar;
        property NavBar: TNavBar read GetNavBar;
      end;

      TContent = class(THTMLComponent)
      protected
      public
        Wide: Boolean;
      end;

      TMenuBar = class(TNavBar)
      public
      end;

      TFooter = class(THTMLComponent)
      public
      end;

      TToast = class(THTMLComponent)
      public
      end;

      { TSideBar }

      [TID_Extension]
      TSideBar = class(THTMLControl)
      protected
        procedure Created; override;
      public
        Theme: TTheme;
        function CanRender: Boolean; override;
      end;

      TMain = class(THTMLLayout)
      protected
        procedure Created; override;
      public
        Gap: Integer;
      end;

      TRow = class(THTMLLayout)
      public
        ContentAlign: TmnwAlign;
      end;

      TColumn = class(THTMLLayout)
      public
        Size: Integer;
      end;

      { TBar }

      TBar = class(THTMLLayout)
      protected
        procedure Created; override;
      public
      end;

      THTMLItem = class abstract(THTMLControl)
      private
        FCaption: string;
        procedure SetCaption(const AValue: string);
      public
        Image: TImageLocation;
        AutoHideText: Boolean;
        property Caption: string read FCaption write SetCaption;
      end;

      TClickType = (clickNavigate, clickNewWindow, clickAction, clickNone);

      { TClickable }

      TClickable = class abstract(THTMLItem)
      private
      protected
        procedure ReceiveMessage(JSON: TDON_Pair); override;
      public
        ClickType: TClickType;
      end;

      [TID_Extension]

      { TAccordion }

      TAccordion = class(THTMLLayout)
      protected
        procedure Created; override;
      public
        AlwaysOpen: Boolean;
      end;

      [TID_Extension]
      TAccordionSection = class(THTMLLayout)
      public
        Image: TImageLocation;
        Caption: string;
        Expanded: Boolean;
        SaveState: Boolean;
      end;

      TAccordionItem = class(TClickable)
      public
      end;

      { TCard }

      [TID_Extension]
      TCard = class(THTMLItem)
      protected
        procedure Created; override;
      public
        Collapse: Boolean;
      end;

      { TPanel }

      TPanel = class(THTMLItem)
      public
      end;

      { TLink }

			TLink = class(TClickable)
      public
        Location: string;
        NoDecoration: Boolean;
        constructor Create(AParent: TmnwElement; const ALocation: string = ''; ACaption: string = ''); reintroduce;
      end;

      [TID_Extension]
      TCollapseCaption = class(THTMLItem)
      protected
        procedure DoCompose(const AContext: TmnwContext); override;
      public
      end;

      TThemeModeButton = class(THTMLItem)
      public
      end;

      { TDropdown }

      [TID_Extension]
      TDropdown = class(THTMLControl)
      protected
        procedure Created; override;
      public
        Options: TDropdownOptions;
        Caption: string;
        Image: TImageLocation;
      end;

      TDropdownItem = class(TLink)
      private
      protected
      public
      end;

      { THTMLGroup }

      THTMLGroup = class(THTMLElement)
      protected
      public
        function CanRender: Boolean; override;
      end;

      { TGroupButtons }

      [TID_Extension]
      TGroupButtons = class(THTMLGroup)
      protected
      public
      end;

      [TID_Extension]
      TToolbar = class(THTMLGroup)
      protected
      public
      end;

      { TForm }

      TFormButton = record
        Caption: string;
      end;

      [TID_Extension]
      [TName_Extension]
      TForm = class(THTMLElement)
      private
      protected
        procedure DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse); override;
        procedure Created; override;
        procedure DoComposed; override;
      public
        PostTo: TLocation;

        //RedirectTo: TLocation;
        RedirectTo: string;

        Submit: TFormButton;
        Cancel: TFormButton;
        Reset: TFormButton;
      end;

      TParagraph = class(THTMLElement)
      public
        Text: string;
        constructor Create(AParent: TmnwElement; AText: string = ''); reintroduce;
      end;

      TCode = class(THTMLComponent)
      public
        Language: string;
        Text: string;
        constructor Create(AParent: TmnwElement; AText: string = ''; ALanguage: string = ''); reintroduce;
      end;

      TMultilineCode = class(TCode)
      public
      end;

      { TAction }

      {
        Not rendered, but can have a route and contain childs
      }

      [TRoute_Extension]
      TAction = class(THTMLElement)
      protected
        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      public        
        procedure Loop; virtual;
        constructor Create(AParent: TmnwElement; AName: string; ARoute: string = ''; ActionProc: TRespondProc = nil); reintroduce; overload;
      end;

			TSpan = class(THTMLElement)
      public
        Text: string;
        constructor Create(AParent: TmnwElement; const AText: string); reintroduce;
      end;

      { TButton }

      TButton = class(TClickable)
      private
      protected
        procedure Created; override;
      public
        JSFunction: string;
      end;

      { TZoomButtons }

      TZoomButtons = class(TGroupButtons)
      protected
        FButtonSmall: TButton;
        FButtonNormal: TButton;
        FButtonLarge: TButton;
        procedure Created; override;
      public
      end;

      TNavItem = class(TClickable)
      private
      protected
      public
        LinkTo: string;
      end;

      TMenuItem = class(TClickable)
      private
      protected
      public
      end;

      TSubMenu = class(TClickable)
      private
      protected
      public
      end;

      { TInput }

      [TID_Extension]
      TInput = class(THTMLComponent)
      private
        FCaption: string;
        FValue: string;
        procedure SetCaption(const AValue: string);
        procedure SetValue(const AValue: string);
      protected
        procedure Created; override;
        procedure ReceiveMessage(JSON: TDON_Pair); override;
      public
        PlaceHolder: string;
        HelpText: string;
        EditType: string;
        Required: Boolean;
      public
        property Value: string read FValue write SetValue;
        property Caption: string read FCaption write SetCaption;
      end;

      { TInputPassword }

      [TID_Extension]
      TInputPassword = class(TInput)
      protected
        procedure Created; override;
      end;

      TCustomImage = class(THTMLComponent)
      public
        AltText: string;
        //Width, Height: double;
      end;

      [TID_Extension]
      TImage = class(TCustomImage)
      protected
        procedure DoCompose(const AContext: TmnwContext); override;
      public
        Source: TLocation;
      end;

      { TMemoryImage }

      [TRoute_Extension]
      TImageFile = class(TCustomImage)
      private
      protected
        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      public
        FileName: string;
        function CanRender: Boolean; override;
        function GetContentType(Route: string): string; override;
      end;

      { TImageMemory }

      [TRoute_Extension]
      TImageMemory = class(TCustomImage)
      private
        FData: TMemoryStream;
      protected
        procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
      protected
        procedure Created; override;
      public
        FileName: string;
        FilePath: string;
        destructor Destroy; override;
        function GetContentType(Route: string): string; override;
        procedure LoadFromFile(const AFileName: string);
        procedure LoadFromStream(AStream: TStream);
        property Data: TMemoryStream read FData;
      end;

      { Break }

      TBreak = class(THTMLElement)
      private
      public
      end;

      //* Custom Tag
      TTag = class(THTMLElement) //TODO
      public
      end;

  private
    FDocument: TDocument;
  protected
    procedure Created; override;
  public
    //function GetContentType(Route: string): string; override;
    property Document: TDocument read FDocument;
  end;

  { TmnwElementRendererRegister }

  TmnwElementRendererRegister = class(TObject)
  public
    Index: Integer;
    ElementClass: TmnwElementClass;
    RendererClass: TmnwElementRendererClass;
    Extensions: TClassList;
    constructor Create;
    destructor Destroy; override;
  end;
  TmnwElementRendererRegisterClass = class of TmnwElementRendererRegister;

  { TmnwElementRenderers }

  //TODO use hash table TDicionary 
  TmnwElementRenderers = class(TmnObjectList<TmnwElementRendererRegister>)
  protected
  public
    constructor Create;
    function Find(AElementClass: TmnwElementClass): TmnwElementRendererRegister;
    function FindByParents(AElementClass: TmnwElementClass): TmnwElementRendererRegister;
    function RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = False): TmnwElementRendererRegister; overload;
  end;

  TmnwRendererRegister = class(TmnNamedObject)  
  public    
    RendererClass: TmnwRendererClass;
  end;  
  
  TmnwRenderers = class(TmnNamedObjectList<TmnwRendererRegister>)
  private
    FCurrent: TmnwRendererRegister;
  public
    function RegisterRenderer(AName: string; ARendererClass: TmnwRendererClass): TmnwRendererRegister; overload;
    function FindBy(ARendererClass: TmnwRendererClass): TmnwRendererRegister; overload;
    procedure Switch(AName: string); overload;
    procedure Switch(ARendererClass: TmnwRendererClass); overload;
    property Current: TmnwRendererRegister read FCurrent;
  end;
  
  TmnwResponse = class(TwebResponse)
  private
    FSession: TmnwCookie;
    function GetSessionID: string;
    procedure SetSessionID(const Value: string);    
  protected
    procedure SetAnswer(const Value: TmodAnswer); override;
    procedure DoWriteCookies; override;
    procedure Created; override;
  public
    destructor Destroy; override;
    property Session: TmnwCookie read FSession;
    property SessionID: string read GetSessionID write SetSessionID;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TmnwSchema)
  private
    FLogoFile: string;
  protected
    //FLogo: THTML.TMemory;  
    procedure Created; override;
  public    
    class function GetCapabilities: TmnwSchemaCapabilities; override;
    procedure Start; override;
    //property Logo: THTML.TMemory read FLogo;
    property LogoFile: string read FLogoFile write FLogoFile;
  end;

  //Return error as json if fail with message of error, so we need JS to post
  TLoginElement = class(THTML.THTMLItem)
  protected
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  TLoginSchema = class(THTML)
  private
  public
  protected
    procedure DoLogin(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoLogout(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoChildRespond(AElement: TmnwElement; const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoCompose(const AContext: TmnwContext); override;
  public
  end;

  { TmnwWebCommand }

  TmnwWebCommand = class(TwebCommand)
  private
    function GetModule: TmnwWebModule;
    function GetResponse: TmnwResponse;
  protected
    function CreateResponse: TmodResponse; override;
  public
    RendererID: Integer;
    procedure RespondResult(var Result: TmodRespondResult); override;
    property Module: TmnwWebModule read GetModule;
    property Response: TmnwResponse read GetResponse;
  end;

  { TmnwWebModule }

  TmnwWebModule = class(TmodWebModule)
  private
    FWeb: TmnwWeb;
  protected
    function CreateRenderer: TmnwRenderer; virtual;
    procedure InitItems; override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;
    procedure Start; override;
    procedure Stop; override;
  public
    destructor Destroy; override;
    constructor Create(AModules: TmodModules; const AName: string; const AAliasName: String); override;
    property Web: TmnwWeb read FWeb;
  end;

//* You need to compile it by brcc32 mnWebElements.rc or wait another 100 years till Delphi/FPC auto compile it
{$R 'mnWebElements.res' 'mnWebElements.rc'}

const
  woFullTag = [woOpenIndent, woCloseIndent];

function DirectionToStr(Direction: TDirection): string;
function GetTimeStamp: Int64;

//Short functions
//Single Quote
function SQ(s: string): string; inline;
//Double Quote
function DQ(s: string): string; inline;

//Name Value with Quote 
function NV(const Name, Value: string): string; overload; inline;
function NV(const Name, Value, Default: string): string; overload; inline;

function AddIf(const Value: string; Add: string): string; overload; inline;
function When(const Value: string; const Default: string = ''): string; overload; inline;
function When(Condition: Boolean; const Value: string; const Default: string = ''): string; overload; inline;
function When(Value: Boolean; Kind: TmnwElementKind): TmnwElementKinds; overload;
function StartURL(const Path: string): string; inline;
function EndURL(const Path: string): string; inline;

function NewUUID: string;

function Renderers: TmnwRenderers;
function Libraries: TmnwLibraries; //TODO
var
  GlobalTimeStamp: Int64;

implementation

uses  
  Generics.Collections;

function GetTimeStamp: Int64;
var
  t: Double absolute Result;
begin
  t := Now;
end;

function NewUUID: string;
begin
  Result := UUIDToString(TGUID.NewGuid);
end;

function DirectionToStr(Direction: TDirection): string;
begin
  if Direction = dirRightToLeft then
    Result := 'rtl'
  else if Direction = dirLeftToRight then
    Result := 'ltr';
end;

function Space(const s: string): string; overload; inline;
begin
  if s <> '' then
    Result := ' ' + s
  else
    Result := s;
end;

function Space(const s1, s2: string): string; overload; inline;
begin
  if (s1 <> '') and (s2 <> '') then
    Result := s1 + ' ' + s2
  else
    Result := s1 + s2;
end;

function SQ(s: string): string; inline;
begin
  Result := QuoteStr(s, '''');
end;

function DQ(s: string): string; inline;
begin
  Result := QuoteStr(s, '"');
end;

//return "Name" "Value" if Value not empty
function NV(const Name, Value: string): string; overload; inline;
begin
  if Value <> '' then
    Result := ' ' + Name + '=' + DQ(Value)
  else
    Result := '';
end;

function NV(const Name, Value, Default: string): string; overload; inline;
begin
  if Value <> '' then
    Result := ' ' + DQ(Name) + '=' + DQ(Value)
  else if Default <> '' then
    Result := ' ' + DQ(Name) + '=' + DQ(Default)
  else
    Result := '';
end;

function AddIf(const Value: string; Add: string): string; overload; inline;
begin
  if Value <> '' then
    Result := Value + Add
  else
    Result := '';
end;

function When(const Value: string; const Default: string = ''): string; overload; inline;
begin
  if Value = '' then
    Result := Default
  else
    Result := Value;
end;

function When(Condition: Boolean; const Value: string; const Default: string = ''): string; overload; inline;
begin
  if Condition then
    Result := Value
  else
    Result := Default;
end;

function When(Value: Boolean; Kind: TmnwElementKind): TmnwElementKinds;
begin
 if Value then
  Result := [Kind]
 else
  Result := [];
end;

function StartURL(const Path: string): string;
begin
  Result := AddStartURLDelimiter(Path);
end;

function EndURL(const Path: string): string;
begin
  Result := AddEndURLDelimiter(Path);
end;

function ExtractClassName(const ClassName: string; ToLower: Boolean = False): string;
var
  p: Integer;
begin
  p := ReversePos('.', ClassName);
  if p > 0 then
    Result := Copy(ClassName, p + 2, MaxInt) //* skip T
  else
    Result := Copy(ClassName, 2, MaxInt); //* skip T
  if ToLower then
    Result := LowerCase(Result);
end;

procedure NewID(Element: TmnwElement);
begin
  if Element.ID = '' then
    Element.ID := LowerCase(ExtractClassName(Element.ClassName) + '-' + Element.GenHandle.ToString);
end;

procedure NewName(Element: TmnwElement; AddNumber: Boolean = True);
var
  s: string;
begin
  if Element.Name = '' then
  begin
    s := ExtractClassName(Element.ClassName, True);
    if AddNumber then
      Element.Name := s + '-' + Element.GenHandle.ToString
    else
      Element.Name := s;
  end;
end;

procedure NewRoute(Element: TmnwElement); inline;
begin
  if Element.Route = '' then
    Element.Route := LowerCase(ExtractClassName(Element.ClassName) + '-' + Element.GenHandle.ToString);
end;

var
  //*Should be by base class categoried
  FRenderers: TmnwRenderers = nil;
  FLibraries: TmnwLibraries = nil;
  
function Renderers: TmnwRenderers;
begin
  if FRenderers = nil then
    FRenderers := TmnwRenderers.Create;
  Result := FRenderers;  
end;

function Libraries: TmnwLibraries;
begin
  if FLibraries = nil then
    FLibraries := TmnwLibraries.Create;
  Result := FLibraries;  
end;
  
{$ifdef rtti_objects}
procedure CacheClasses;
var
  Context: TRTTIContext;
  rTypes: TArray<TRttiType>;
  rType: TRttiType;
begin
  if CacheClassObjects <> nil then
    exit;
  CacheClassObjects := TCacheClassObjects.Create;
  Context := TRTTIContext.Create;
  try
    rTypes := Context.GetTypes;
    for rType in rTypes do
      if (rType.TypeKind = tkClass) and rType.IsInstance
        and (rType.AsInstance.MetaclassType.InheritsFrom(TmnwElement)
            or
            rType.AsInstance.MetaclassType.InheritsFrom(TmnwRenderer)
            )
        then
      begin
        CacheClassObjects.AddClass(TmnwElementClass(rType.AsInstance.MetaclassType));
//        log.WriteLn(rType.ToString);
      end;
  finally
    Context.Free;
  end;
end;
{$endif}

procedure rttiCollectExtensions(rttiContext: TRttiContext; ElementClass: TClass; List: TClassList); overload;
var
  rttiType: TRttiType;
  attribute: TCustomAttribute;
begin
  rttiType := rttiContext.GetType(ElementClass);
  for attribute in rttiType.GetAttributes do
    if List.IndexOf(attribute.ClassType)<0 then
      List.Add(attribute.ClassType);
  if ElementClass.ClassParent <> nil then
    rttiCollectExtensions(rttiContext, ElementClass.ClassParent, List);
end;

procedure rttiCollectExtensions(ElementClass: TmnwElementClass; ToList: TClassList); overload;
var
  rttiContext: TRttiContext;
  attribute: TCustomAttributeClass;
  list: TClassList;
begin
  if ElementClass = nil then
    raise Exception.Create('Element is nil');
  if ElementClass <> nil then
  begin
    list := TClassList.Create;
    rttiContext := TRttiContext.Create;
    try
      rttiCollectExtensions(rttiContext, ElementClass, list);
      for attribute in list do
        if attribute.InheritsFrom(TElementExtension) then
          ToList.Add(attribute);
    finally
      rttiContext.Free;
      list.Free;
    end;
  end;
end;

{ TmnwMessages }

procedure TmnwMessages.Add(s: string);
var
  aMessage: TmnwMessage;
begin
  aMessage := TmnwMessage.Create;
  aMessage.Content := s;
  Add(aMessage);
end;

{ TmnwAttachment }

procedure TmnwAttachment.SendMessage(const Message: string);
begin
  Stream.WriteUTF8Line(Message);
//  Stream.Close([cloData]);
end;

procedure TmnwAttachment.Loop;
var
  s: string;
  lCmd, lValue, eol: string;
  procedure DetectEOL;
  begin
    if RightStr(s, 2) = sWinEndOfLine then
      eol := sWinEndOfLine
    else if RightStr(s, 1) = sUnixEndOfLine then
      eol := sUnixEndOfLine
    else if RightStr(s, 1) = sMacEndOfLine then
      eol := sMacEndOfLine
    else
      eol := '';
    lCmd := SubStr(s, 1, -eol.Length);
    SpliteStr(lCmd, ' ', lCmd, lValue);
  end;

  procedure MessageIt;
  begin
    if Interactive then
      Schema.InteractiveMessage(s)
    else
      Schema.AttachedMessage(s)
  end;
begin
  while not Terminated and Stream.Connected and not (cloTransmission in Stream.State) do
  begin
    if Stream.ReadUTF8String(s) then
    begin
{      Schema.Attachments.Lock.Enter;
      try
        Schema.Attachments.Messages.Add(s);
      finally
        Schema.Attachments.Lock.Leave;
      end;}
      if Interactive then // It is json
        Schema.InteractiveMessage(s)
      else
      begin
        if s.StartsWith('{') then
          Schema.AttachedMessage(s)
        else
        begin
          if CompareLeftStr(s,'echo') then
          begin
            DetectEOL;
            Stream.WriteUTF8String(lValue + eol); //testing propuse
          end
          else if Schema.Interactive then
          begin
            DetectEOL;
            if (lCmd = 'attach') then
            begin
              Stream.WriteUTF8String('attached'+eol);
              Name := lValue;
              FInteractive := True;
            end
            else if (lCmd = 'interactive') then
            begin
              Stream.WriteUTF8String('attached'+eol);
              Name := ''; // no name
              FInteractive := True;
            end
            else
              Schema.AttachedMessage(s);
          end
          else
            Schema.AttachedMessage(s);
        end;
      end;
    end;
  end;
end;

procedure TmnwAttachment.Terminate;
begin
  FTerminated := True;
  Stream.Close;
end;

destructor TmnwAttachment.Destroy;
begin
  inherited;
end;

{ TmnwAttachments }

procedure TmnwAttachments.Created;
begin
  inherited;
end;

constructor TmnwAttachments.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
//  FMessages := TmnwMessages.Create;
end;

destructor TmnwAttachments.Destroy;
begin
//  FreeAndNil(FMessages);
  FreeAndNil(FLock);
  inherited;
end;

procedure TmnwAttachments.Terminate;
var
  Attachment: TmnwAttachment;
begin
  Lock.Enter;
  try
    for Attachment in Self do
    begin
      Attachment.Terminate;
    end;
  finally
    Lock.Leave;
  end;
end;

procedure TmnwAttachments.SendMessage(const Message: string);
begin
  SendMessage('', Message);
end;

procedure TmnwAttachments.Add(AAttachment: TmnwAttachment);
begin
  Lock.Enter;
  try
    inherited Add(AAttachment);
  finally
    Lock.Leave;
  end;
end;

procedure TmnwAttachments.Remove(AAttachment: TmnwAttachment);
begin
  Lock.Enter;
  try
    inherited Remove(AAttachment);
  finally
    Lock.Leave;
  end;
end;

procedure TmnwAttachments.SendMessage(const AttachmentName, Message: string);
var
  Attachment: TmnwAttachment;
begin
  Lock.Enter;
  try
    for Attachment in Self do
    begin
      if (Attachment.Name = '') or SameText(AttachmentName, Attachment.Name) then
        Attachment.SendMessage(Message);
    end;
  finally
    Lock.Leave;
  end;
end;

{ TmnwAttributes }

function TmnwAttributes.GetText: string;
begin
  Result := ToString;
  if Result <> '' then
    Result := ' ' + Result;
end;

function TmnwAttributes.HaveSubValue(const AName, AValue: String; vSeparators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
  aAttribute: TmnwAttribute;
begin
  aAttribute := Find(AName);
  Result := aAttribute <> nil;
  if Result then
  begin
    SubValues := aAttribute.CreateSubValues(vSeparators);
    try
      Result := SubValues.IndexOf(AValue) >= 0;
    finally
      SubValues.Free;
    end;
  end;
end;

function TmnwAttributes.SetSubValue(const AName, AValue: String; vSeparators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
  aAttribute: TmnwAttribute;
begin
  aAttribute := Find(AName);
  Result := aAttribute <> nil;
  if not Result then
  begin
    aAttribute := TmnwAttribute.Create(AName, '');
    Add(aAttribute);
  end;

  SubValues := aAttribute.CreateSubValues(vSeparators);
  try
    Result := SubValues.IndexOf(AValue)<0;
    if Result then
    begin
      SubValues.Add(AValue);
      SubValues.Delimiter := ' ';
      aAttribute.Value := SubValues.DelimitedText;
    end;
  finally
    SubValues.Free;
  end;
end;

function TmnwAttributes.ToString: string;
var
  a: TmnwAttribute;
  idItem: Integer;
  sb: TStringBuilder;
begin
  idItem := IndexOfName('id');
  if (idItem > 0) then
    Move(idItem, 0);

  sb := TStringBuilder.Create;
  try
    for a in Self do
    begin
      if sb.Length > 0 then
        sb.Append(' ');
      sb.Append(a.Name).Append('=').Append(QuoteStr(a.Value, '"'));
    end;
    Result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TmnwAttributes.UnsetSubValue(const AName, AValue: String; vSeparators: TSysCharSet): Boolean;
var
  SubValues: TStringList;
  i: Integer;
  aAttribute: TmnwAttribute;
begin
  aAttribute := Find(AName);
  Result := aAttribute = nil;
  if not Result then
  begin
    SubValues := aAttribute.CreateSubValues(vSeparators);
    try
      i := SubValues.IndexOf(AValue);
      Result := i>=0;
      if Result then
      begin
        SubValues.Delete(i);
        SubValues.Delimiter := ' ';
        aAttribute.Value := SubValues.DelimitedText;
        if AutoRemove and (aAttribute.Value = '') then
          Remove(aAttribute);
      end;
    finally
      SubValues.Free;
    end;
  end;
end;

procedure TmnwAttributes.Append(AAttributes: TmnwAttributes);
var
  fromAttibute: TmnwAttribute;
begin
  for fromAttibute in AAttributes do
  begin
    Add(TmnwAttribute.CreateFrom(fromAttibute));
  end;
end;

procedure TmnwAttributes.Created;
begin
  inherited;
  //AutoRemove := True; //no AltTxt in image should writen even if it empty
end;

procedure TmnwAttributes.Delete(Name: string);
var
  i: Integer;
begin
  i:= IndexOfName(Name);
  if i>=0 then
    Delete(i);
end;

{ TmnwElementRenderer }

procedure TmnwElementRenderer.RenderChilds(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  o: TmnwElement;
  ParentRenderer: TmnwElementRenderer;
  StartElements, NormalElements, EndElements: TList<TmnwElement>;
begin
  ParentRenderer := Context.ParentRenderer;
  Context.ParentRenderer := Self;
  
  // Single pass to categorize elements
  StartElements := TList<TmnwElement>.Create;
  NormalElements := TList<TmnwElement>.Create;
  EndElements := TList<TmnwElement>.Create;
  try
    for o in Scope.Element do
      if not (elInternal in o.Kind) then
        case o.Priority of
          priorityStart: StartElements.Add(o);
          priorityEnd: EndElements.Add(o);
        else
          NormalElements.Add(o);
        end;

    // Render in priority order
    for o in StartElements do
      o.Render(Context, AResponse);
    for o in NormalElements do
      o.Render(Context, AResponse);
    for o in EndElements do
      o.Render(Context, AResponse);
  finally
    EndElements.Free;
    NormalElements.Free;
    StartElements.Free;
    Context.ParentRenderer := ParentRenderer;
  end;
end;

procedure TmnwElementRenderer.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  RenderChilds(Scope, Context, AResponse);
end;

procedure TmnwElementRenderer.DoLeaveRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterOuterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoLeaveOuterRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; const Context: TmnwContext; AResponse: TmnwResponse);
var
  aScope: TmnwScope;
begin
  aScope.Attributes := TmnwAttributes.Create;
  aScope.Element := AElement;
  try
    CollectAttributes(aScope, Context);

    if Context.ParentRenderer <> nil then
      Context.ParentRenderer.DoEnterChildRender(aScope, Context);

    DoEnterRender(aScope, Context);
    DoInnerRender(aScope, Context, AResponse);
    DoLeaveRender(aScope, Context);

    if Context.ParentRenderer <> nil then
      Context.ParentRenderer.DoLeaveChildRender(aScope, Context);

  finally
    FreeAndNil(aScope.Attributes);
  end;
end;

constructor TmnwElementRenderer.Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwElementRendererRegister);
begin
  inherited Create;
  FRenderer := ARenderer;
  FRendererRegister:= ARendererRegister;
end;

procedure TmnwElementRenderer.AddHead(const Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.CollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Attributes.Append(Scope.Element.Attributes);
  Scope.Classes := Scope.Element.ElementClass;

  if Scope.Element.ID <> '' then
    Scope.Attributes['id'] := Scope.Element.ID;
  if Scope.Element.Name <> '' then
    Scope.Attributes['name'] := Scope.Element.Name;

  DoCollectAttributes(Scope, Context);
end;

{ TElementExtension }

constructor TElementExtension.Create;
begin
  inherited Create;
end;

function TmnwAttribute.CreateSubValues(vSeparators: TSysCharSet): TStringList;
begin
  Result := TStringList.Create;
  if Self <> nil then
    StrToStrings(Value, Result, vSeparators, []);
end;

procedure TmnwElement.Render(const Context: TmnwContext; AResponse: TmnwResponse);
var
  er: TmnwElementRenderer;
begin
  if CanRender then
  begin    
    er := CreateRenderer(Context);
    if er <> nil then
    try
      try
        er.Render(Self, Context, AResponse);
      except
        on E: Exception do
          raise Exception.Create('Error in '+ ClassName +': ' + E.Message);
      end;
    finally
      er.Free;
    end;
  end;
end;

procedure TmnwElement.PrepareRenderer(const AContext: TmnwContext);
var
  o: TmnwElement;
begin
  DoPrepareRenderer(AContext);
  for o in Self do
  begin
    o.PrepareRenderer(AContext); 
  end;
end;

function TmnwElement.CanRender: Boolean;
begin
  Result := RenderIt;
end;

function TmnwElement.CreateRenderer(const Context: TmnwContext): TmnwElementRenderer;
begin
  if (Context.Renderer <> nil) then
  begin
    Result := Context.Renderer.CreateRenderer(Self);
    //PrepareRenderer(Context);
  end
  else
    Result := nil;
end;

{ TmnwSchema.TmnwElementRenderers }

function TmnwElementRenderers.RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean): TmnwElementRendererRegister;
begin
  if not AElementClass.InheritsFrom(TmnwElement) then
    raise Exception.Create('Element should inherited from THTML');
      
  if not ARendererClass.InheritsFrom(TmnwElementRenderer) then
    raise Exception.Create('Renderer should inherited from TmnwElementRenderer');
      
  Result := Find(AElementClass);
  if Result <> nil then
  begin
    if (Replace) and (AElementClass.InheritsFrom(Result.ElementClass)) then
      Result.RendererClass := ARendererClass
    else
      raise Exception.Create('You can''t re-register same class: ' + AElementClass.ClassName);
  end
  else
  begin
    Result := TmnwElementRendererRegister.Create;
    Result.ElementClass := AElementClass;
    Result.RendererClass := ARendererClass;
    rttiCollectExtensions(Result.ElementClass, Result.Extensions);
    Result.Index := Add(Result);
  end;
end;

constructor TmnwElementRenderers.Create;
begin
  inherited Create;
  RegisterRenderer(TmnwElement, TmnwElementRenderer);  
end;

function TmnwElementRenderers.Find(AElementClass: TmnwElementClass): TmnwElementRendererRegister;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AElementClass = Items[i].ElementClass then
      Exit(Items[i]);
  Result := nil;
{  if (Result <> nil) and (AElementClass <> Result.ElementClass) then
    Log.WriteLn(lglError, '> ' + AElementClass.ClassName + ' with ' + Result.ElementClass.ClassName);}
end;

function TmnwElementRenderers.FindByParents(AElementClass: TmnwElementClass): TmnwElementRendererRegister;
var
  aClass: TmnwElementClass;
begin
  aClass := AElementClass;
  while aClass <> nil do
  begin
    Result := Find(aClass);
    if Result <> nil then
    begin
      if aClass <> AElementClass then
        Result := RegisterRenderer(AElementClass, Result.RendererClass);
      Exit;
    end;
    if aClass.ClassParent.InheritsFrom(TmnwElement) then
      aClass := TmnwElementClass(aClass.ClassParent)
    else
      aClass := nil;
  end;
  Result := nil;
end;

{function TmnwElementRenderers.FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
var
  o: TmnwElementRendererRegister;
begin
  o := Find(AObjectClass, True);
  if o <> nil then
    Result := o.RendererClass
  else
    Result := TmnwElementRenderer;
end;}

{ TmnwSchemaObject }

constructor TmnwSchemaObject.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TmnwSchemaObject.Destroy;
begin
  FreeAndNil(Schema);
  FreeAndNil(FLock);
  inherited;
end;

{ TmnwWeb }

destructor TmnwWeb.Destroy;
begin
  FreeAndNil(FRegistered);
  FreeAndNil(FLock);
  inherited;
end;

procedure TmnwWeb.Start;
var
  item: TmnwSchema;
begin
  FShutdown := False;
  for item in Self do
  begin
    item.Start;
  end;
  Started := True;
end;

procedure TmnwWeb.Stop;
begin
  FShutdown := True;
  ClearSchemas;
  Started := False;
end;

function TmnwWeb.RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass; AsDefaultSchema: Boolean = False): TmnwSchema;
var
  aSchemaItem: TmnwSchemaItem;
begin
  aSchemaItem := TmnwSchemaItem.Create;
  aSchemaItem.Name := AName;
  aSchemaItem.SchemaClass := SchemaClass;
  Registered.Add(aSchemaItem);
  if AsDefaultSchema then
    FDefaultSchema := aSchemaItem;

  aSchemaItem.SchemaClass.Registered;
  if schemaStartup in aSchemaItem.SchemaClass.GetCapabilities then
  begin
    Result := CreateSchema(SchemaClass, AName);
    Result.FPhase := scmpNormal;
    Add(Result);
  end
  else
    Result := nil;
end;

function TmnwWeb.FindBy(const aSchemaName: string; const aSessionID: string): TmnwSchema;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, aSchemaName) and (not (schemaSession in Items[i].GetCapabilities) or (aSessionID = Items[i].SessionID)) then
      Result := Items[i];
    if Result <> nil then
      break;
  end;
end;

function TmnwWeb.CreateSchema(const aSchemaName: string): TmnwSchema;
var
  SchemaItem: TmnwSchemaItem;
begin
	SchemaItem := Registered.Find(aSchemaName);
  if SchemaItem <> nil then
  begin
    Result := CreateSchema(SchemaItem);
    SchemaCreated(Result);
    if Started then
      Result.Start;
    //Add(SchemaObject); no, when compose it we add it
  end
  else
    Result := nil;
end;

function TmnwWeb.ReleaseSchema(const aSchemaName: string; aSessionID: string): TmnwSchema;
begin
  Lock.Enter;
  try
    Result := FindBy(aSchemaName, aSessionID);
    if Result <> nil then
    begin
      Extract(Result);
      Result.FPhase := scmpReleased;
    end;
  finally
    Lock.Leave
  end;
end;

//Main
function TmnwWeb.GetElement(var AContext: TmnwContext; out Element: TmnwElement): Boolean;
var
  aElement: TmnwElement;
  Routes: TStringList;
  i: Integer;
  aSchemaName, aRoute: string;
  aSchema: TmnwSchema; 
begin
  Element := nil;
  aSchema := nil;
  Result := False;
  Routes := TStringList.Create;
  try
    StrToStrings(AContext.Route, Routes, [URLDelimiter]);
    if (Routes.Count > 0) then
      aSchemaName := Routes[0]
    else
      aSchemaName := '';

    //Find already exists Schema
    Lock.Enter;
    try
      aSchema := FindBy(aSchemaName, AContext.SessionID);
    finally
      Lock.Leave;
    end;

    if aSchema = nil then // Not cached, create it.
    begin
      aSchema := CreateSchema(aSchemaName);
      if aSchema = nil then  //* Fallback
      begin
        Lock.Enter;
        try
          aSchema := FindBy('', AContext.SessionID);
        finally
          Lock.Leave;
        end;
        if aSchema = nil then
          aSchema := CreateSchema('');
{        if Schema = nil then
          Schema := CreateSchema(DefaultSchema);}
        if aSchema <> nil then
          aSchemaName := '';
      end;

      if (aSchema <> nil) and (schemaSession in aSchema.GetCapabilities) then
        aSchema.SessionID := AContext.SessionID;
    end;

{
    if Schema = nil then
      Schema := First; //* Fallback //taskeej
}
    if aSchemaName <> '' then
    begin
      if (Routes.Count > 0) then
      begin
        Routes.Delete(0);
        AContext.Route := DeleteSubPath(aSchemaName, AContext.Route);
      end;
    end;

    Lock.Enter;
    try
      if aSchema <> nil then
        Inc(aSchema.Usage);
    finally
      Lock.Leave;
    end;

    if (aSchema <> nil) then
    begin
      AContext.Schema := aSchema;      

      AContext.SessionID := AContext.Request.Params.Values['session'];
      if AContext.SessionID = '' then
        AContext.SessionID := AContext.Request.Cookies.Values['session'];
      
      if aSchema.Accept(AContext) then
      begin
        if not (estComposed in aSchema.State) then
        begin
          aSchema.Lock.Enter;
          try
            try
              aSchema.Compose(AContext); //Compose
        except
          aSchema.Lock.Leave;
          AContext.Schema := nil;
          FreeAndNil(aSchema);
          raise;
        end;
          finally
            if aSchema <> nil then
              aSchema.Lock.Leave;
          end;
        end;

        if (estComposed in aSchema.State) then
        begin                 
          Element := aSchema;
          Result := True;

          //Finding nested element inside Schema
          aElement := aSchema;
          i := 0;
          while i < Routes.Count do
          begin
            aRoute := Routes[i];
            if aRoute = '' then
            begin
              Result := True;
              break;
            end
            else
            begin
              aElement := aElement.FindByRoute(aRoute);
              if (aElement = nil) then
              begin
                Result := False;
                break;
              end
              else
              begin
//                if not (elNoRespond in aElement.Kind) then                
                begin
                  Element := aElement;
                  Result := True;
                end;
                AContext.Route := DeleteSubPath(aRoute, AContext.Route);
              end;
            end;
            inc(i);
          end;
        end;
      end;
    end;
  finally
    Routes.Free;
  end;
end;

procedure TmnwWeb.Respond(var AContext: TmnwContext; AResponse: TmnwResponse);
var
  aElement: TmnwElement;
begin
  if Shutdown then
    exit;

  try
    GetElement(AContext, aElement);
    if aElement <> nil then
    begin
    //aContext.Schema := aSchema;
      AContext.Element := aElement;      
      
      AResponse.Answer := hrOK;
      AResponse.Redirect := '';
      AResponse.SessionID := AResponse.Request.GetCookie('', 'session');
      AResponse.Session.Age := DefaultAge;
      AResponse.Session.Domain := Domain;
      //AResponse.Session.Path := StartURL(Alias, True);
      AResponse.Session.Path := AContext.GetHomePath;
      AResponse.Session.ResetChanged;

      AResponse.Header['access-control-allow-origin'] := '*';
      //AResponse.Header['Access-Control-Allow-Origin'] := '*';
      //AResponse.Header['Access-Control-Allow-Headers'] := ' X-PINGOTHER, Content-Type';
      //AResponse.Header['Access-Control-Allow-Methods'] := 'HEAD,POST,GET,OPTIONS,PUT,DELETE,CONNECT,TRACE,PATCH';
      //AResponse.Header['Access-Control-Expose-Headers'] := ' Content-Encoding, Kuma-Revision';     

      if not AResponse.IsResponded then
        aElement.RespondInit(AContext, AResponse); //For check Login in header before redirecting if needed

      //* If you call schema name without ending by /
      if not AResponse.IsResponded then
      begin
        if (aElement = AContext.Schema) and (AContext.Schema.Name <> '') and (AContext.Route = '') then
          AResponse.RespondRedirectTo(IncludeURLDelimiter(AContext.GetPath(AContext.Schema)))
        else
          AResponse.ContentType := aElement.GetContentType(AContext.Route);
      end;

      //* Resume maybe come false in action
      //* We will render it now
      if not AResponse.IsResponded then
      begin
        aElement.PrepareRenderer(AContext); 
        if not AResponse.IsResponded then
          aElement.Respond(AContext, AResponse);
      end;

      if not (AResponse.IsHeaderSent) then
      begin
        if (AResponse.Answer =hrOK) and (not AResponse.IsResponded) then
          AResponse.RespondNoContent
        else if AResponse.Answer = hrNotFound then
          AResponse.RespondNotFound;
      end;
    end
    else
    begin
      if not AResponse.IsHeaderSent then
        AResponse.RespondNotFound;
    end;

    if AContext.Schema <> nil then
    begin
      Lock.Enter;
      try
        AContext.Schema.LastAccess := Now;
        Dec(AContext.Schema.Usage);
        if (AContext.Schema.Usage = 0) and (AContext.Schema.Released) then
          FreeAndNil(AContext.Schema)
        else
        begin
          if AContext.Schema.Phase = scmpNew then
          begin
            AContext.Schema.FPhase := scmpNormal;
            Add(AContext.Schema);
          end;
        end;
      finally
        Lock.Leave;
      end;
    end;
  except
    {$ifdef DEBUG}
    on E: Exception do
    begin
      if not (AResponse.IsHeaderSent) then
      begin
        AResponse.Answer := hrError;
        AResponse.ContentType := 'text/html';
      end;
      AResponse.SendUTF8String('Server Error: ' + E.Message);
    end;
    {$else}
      raise;
    {$endif}
  end;
end;

function TmnwWeb.Attach(const AContext: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;
var
  Routes: TStringList;
  i: Integer;
  aRoute: string;
  aSchema: TmnwSchema;
begin
  Result := nil;
  if Shutdown then
    exit(nil);

  aSchema := nil;
  Routes := TStringList.Create;
  try
    i := 0;
    StrToStrings(AContext.Route, Routes, [URLDelimiter]);
    if (i < Routes.Count) then
    begin
      aRoute := Routes[i];
      aSchema := FindBy(aRoute, '');
    end;

    if aSchema = nil then
      aSchema := First; //* fallback

    if (aSchema <> nil) then
    begin
      if aSchema.Interactive or (schemaAttach in aSchema.GetCapabilities) then
        DeleteSubPath(aRoute, AContext.Route)
      else
        exit(nil);
    end;
  finally
    Routes.Free;
  end;

  if aSchema <> nil then
  begin
    aSchema.Attach(aRoute, Sender, AStream);
  end
  else
    Result := nil;
end;

procedure TmnwWeb.SchemaCreated(Schema: TmnwSchema);
begin
end;

procedure TmnwWeb.Created;
begin
  inherited;
  FAssets := RegisterSchema('assets', TAssetsSchema) as TAssetsSchema;
end;

function TmnwWeb.CreateSchema(const SchemaClass: TmnwSchemaClass; AName: string): TmnwSchema;
begin
  Result := SchemaClass.Create(Self, AName, AName);
end;

procedure TmnwWeb.ClearSchemas;
var
  i: Integer;
begin
  i := Count-1;
  while i>=0 do
  begin
    if not (schemaPermanent in Items[i].GetCapabilities) then
      Delete(i);
    Dec(i);
  end;
end;

constructor TmnwWeb.Create;
begin
  FTimeStamp := GetTimeStamp;
  FLock := TCriticalSection.Create;
  FRegistered := TRegisteredSchemas.Create;
  DefaultAge := -1; //Forever
  inherited;
end;

function TmnwWeb.GetHostURL: string;
begin
  Result := ComposeHttpURL(IsSecure, Domain, Port);
end;

function TmnwWeb.CreateSchema(SchemaItem: TmnwSchemaItem): TmnwSchema;
begin
  Result := CreateSchema(SchemaItem.SchemaClass, SchemaItem.Name);
  if Result <> nil then
  begin
    SchemaCreated(Result);
    if Started then
      Result.Start;
  end;
end;

{ TLocation }

class operator TLocation.Implicit(Source: string): TLocation;
begin
  Result.Custom := Source;
end;

class operator TLocation.Implicit(Source: TLocationRelative): TLocation;
begin
  Result.Where := Source;
end;

class operator TLocation.Explicit(const Source: string): TLocation;
begin
  Result.Custom := Source;
end;

function TLocation.IsDefeined: Boolean;
begin
  Result := (Custom <> '') or (Where <> toNone);
end;

class operator TLocation.Implicit(Source: TLocation): string;
begin
  Result := Source.Custom;
end;

{ TImageLocation }

procedure TImageLocation.SetPath(const AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
  FLocationType := imgPath;
end;

procedure TImageLocation.SetIconClass(const AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
  FLocationType := imgIconClass;
end;

function TImageLocation.GetIconClass: string;
begin
  if FLocationType = imgIconClass then
    Result := FValue
  else
    Result := '';
end;

function TImageLocation.GetPath: string;
begin
  if FLocationType = imgPath then
    Result := FValue
  else
    Result := '';
end;

{ TmnwBounding }

class operator TmnwBounding.Explicit(const Source: Integer): TmnwBounding;
begin
  Result.Left := Source;
  Result.Top := Source;
  Result.Right := Source;
  Result.Bottom := Source;
end;

class operator TmnwBounding.Implicit(Source: Integer): TmnwBounding;
begin
  Result.Left := Source;
  Result.Top := Source;
  Result.Right := Source;
  Result.Bottom := Source;
end;

class operator TmnwBounding.Implicit(Source: TmnwBounding): Integer;
begin
  Result := Round(Source.Top);
  //maybe exception if not equal
end;

class operator TmnwBounding.Explicit(const Source: Double): TmnwBounding;
begin
  Result.Left := Source;
  Result.Top := Source;
  Result.Right := Source;
  Result.Bottom := Source;
end;

class operator TmnwBounding.Implicit(Source: Double): TmnwBounding;
begin
  Result.Left := Source;
  Result.Top := Source;
  Result.Right := Source;
  Result.Bottom := Source;
end;

class operator TmnwBounding.Implicit(Source: TmnwBounding): Double;
begin
  Result := Source.Left;
end;

function TmnwBounding.ToString: string;
begin
  if (Top = Left) and (Top = Bottom) and ((Top = Right)) then
    Result := Top.ToString
  else if (Top = Bottom) and (Left = Right) then
    Result := Top.ToString + ' ' + Left.ToString
  else
    Result := Top.ToString + ' ' + Bottom.ToString + ' ' + Right.ToString + ' ' + Left.ToString
end;

class operator TmnwBounding.Initialize({$ifdef FPC}var{$else}out{$endif}Dest: TmnwBounding);
begin
  Dest.Top := -1;
  Dest.Bottom := -1;
  Dest.Right := -1;
  Dest.Left := -1;
end;

procedure TmnwBounding.SetTopBottom(Value: Double);
begin
  Top := Value;
  Bottom := Value;
end;

procedure TmnwBounding.SetLeftRight(Value: Double);
begin
  Left := Value;
  Right := Value;
end;

{ THTML }

procedure THTML.Created;
begin
  inherited;
  FDocument := TDocument.Create(Self, [elEmbed]);
end;

{function THTML.GetContentType(Route: string): string;
begin
  if (Route = '') or (Route = URLDelimiter) then
    Result := 'text/html'
  else
    Result := DocumentToContentType(Route);
end;}

{ THTML.THTMLComponent }

procedure THTML.THTMLComponent.Created;
begin
  inherited;
end;

{ THTML.THTMLControl }

procedure THTML.THTMLControl.Created;
begin
  inherited;
  Size := szUndefined;
end;

{ TmnwElementRendererRegister }

constructor TmnwElementRendererRegister.Create;
begin
  inherited Create;
  Extensions := TClassList.Create;
end;

destructor TmnwElementRendererRegister.Destroy;
begin
  FreeAndNil(Extensions);
  inherited;
end;

{ TmnwSchema }

constructor TmnwSchema.Create(AWeb: TmnwWeb; AName: string; ARoute: string);
begin
  inherited Create(nil);
  FWeb := AWeb;
  FDefaultDocuments := TStringList.Create;
  FDefaultDocuments.Add('index.html');
  FDefaultDocuments.Add('index.htm');
  FDefaultDocuments.Add('default.html');
  FDefaultDocuments.Add('default.htm');
  FName := AName;
  if ARoute = '' then
    FRoute := FName
  else
    FRoute := ARoute;
  FSchema := Self;
  FIsRoot := True;
  FAttachments := TmnwAttachments.Create;
  FLock := TCriticalSection.Create;
  RefreshInterval := 1;
  {$ifdef rtti_objects}
  CacheClasses;
  {$endif}
end;

destructor TmnwSchema.Destroy;
begin
  FAttachments.Terminate;
  FAttachments.Clear;
  FreeAndNil(FAttachments);
  FreeAndNil(FLock);
  FreeAndNil(FDefaultDocuments);
  inherited;
end;

procedure TmnwSchema.Attach(Route: string; Sender: TObject; AStream: TmnBufferStream);
var
  Attachment: TmnwAttachment;
begin
  Attachment := TmnwAttachment.Create;
  Attachment.Schema := Self;
  Attachment.Stream := AStream;
  Attachments.Add(Attachment);
  UpdateAttached;
  try
    Attachment.Loop;
    if not Attachment.Terminated then
      Attachment.Terminate;
  finally
    Attachments.Remove(Attachment);
    UpdateAttached;
  end;
end;

procedure TmnwSchema.AttachedMessage(const s: string);
begin
end;

procedure TmnwElement.SendMessage(AttachmentName: string; AMessage: string);
begin
  if Schema <> nil then
    Schema.Attachments.SendMessage(AttachmentName, AMessage);
end;

function TmnwElement.ServeFile(HomeFolder: string; DefaultDocuments: TStringList; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse): Boolean;
var
  aDocument, aRequestDocument, aFile: string;
  IsDocument, IsDirectory, Expanded: Boolean;
begin
  Result := True;
  if HomeFolder = '' then
  begin
    Result := False;
    Exit;
  end;

  WebExpandFile(HomeFolder, AContext.Route, aRequestDocument, False);
  Expanded := WebExpandFile(HomeFolder, AContext.Route, aDocument, serveSmart in Options);

  if not Expanded then
  begin
    if (AContext.Route = '') or IsStrInArray(AContext.Route, ['\', '/']) then
    begin
      if (serveIndexRoot in Options) and EndsDelimiter(aDocument) and DirectoryExists(aDocument) then
      begin
        if StartsStr(HomeFolder, aDocument) then
          ServeFolder(aDocument, Options, AContext, AResponse)
        else
          AResponse.RespondUnauthorized;
      end
      else
        Result := False;
    end
    else
      Result := False;
    Exit;
  end;

  IsDocument := FileExists(aDocument);
  IsDirectory := DirectoryExists(aDocument);

  if ((AContext.Route = '') and not IsDocument) or
     (not EndsDelimiter(aRequestDocument) and IsDirectory) then
  begin
    AResponse.RespondRedirectTo(AResponse.Request.Address);
    Exit;
  end;

  if EndsDelimiter(aDocument) then
  begin
    if serveDefault in Options then
    begin
      aFile := FindDefaultDocument(aDocument, DefaultDocuments);
      IsDocument := FileExists(aFile);
      if IsDocument then
      begin
        aDocument := aFile;
        IsDirectory := False;
      end;
    end;

    if IsDirectory and (serveIndex in Options) then
    begin
      if StartsStr(HomeFolder, aDocument) then
        ServeFolder(aDocument, Options, AContext, AResponse)
      else
        AResponse.RespondUnauthorized;
      Exit;
    end;
  end;

  if StartsText('.', ExtractFileName(aDocument)) then
    AResponse.RespondForbidden
  else if IsDocument then
  begin
    if StartsText(HomeFolder, aDocument) then
      AResponse.SendFile(aDocument)
    else
      AResponse.RespondUnauthorized;
  end
  else
    Result := False;
end;

function TmnwElement.ServeFile(HomeFolder: string; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse): Boolean;
begin
  Result := ServeFile(HomeFolder, nil, Options, AContext, AResponse);
end;

procedure TmnwElement.ServeFolder(APath: string; Options: TmodServeFiles; const AContext: TmnwContext; AResponse: TmnwResponse);
var
  Files: TStringList;

  procedure AddLink(const s: string);
  begin
    AContext.Writer.OpenInlineTag('li');
    AContext.Writer.AddInlineTag('a', 'href="' + s + '"', s);
    AContext.Writer.CloseTag('li');
  end;

  procedure WriteSection(const ACaption: string; AFilter: TEnumFilesOptions; const AExtra: string = '');
  var
    s: string;
  begin
    Files.Clear;
    EnumFiles(Files, APath, '*.*', AFilter);
    AContext.Writer.AddTag('h2', '', ACaption);
    AContext.Writer.OpenTag('ul');
    if AExtra <> '' then
      AddLink(AExtra);
    for s in Files do
      if not StartsText('.', s) then
        AddLink(s);
    AContext.Writer.CloseTag('ul');
  end;

begin
  AResponse.ContentType := DocumentToContentType('html');
  Files := TStringList.Create;
  try
    AContext.Writer.WriteLn('<!DOCTYPE html>');
    AContext.Writer.OpenTag('html');
    AContext.Writer.OpenTag('head');
    AContext.Writer.AddTag('title', '', 'Index of ' + APath);
    AContext.Writer.AddShortTag('link', 'rel="icon" href="data:,"'); //disable call favicon.ico
    AContext.Writer.AddShortTag('meta', 'charset="UTF-8"');
    AContext.Writer.AddShortTag('meta', 'name="viewport" content="width=device-width, initial-scale=1"');
    AContext.Writer.AddTag('style', '', 'body { font-family: monospace; }');
    AContext.Writer.CloseTag('head');
    AContext.Writer.OpenTag('body');
    AContext.Writer.AddTag('h1', '', 'Index of ' + AContext.Route);
    WriteSection('Folders', [efDirectory], '..');
    WriteSection('Files', [efFile]);
    AContext.Writer.CloseTag('body');
    AContext.Writer.CloseTag('html');
  finally
    Files.Free;
  end;
end;

procedure TmnwSchema.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  if not (serveEnabled in ServeFiles) or not ServeFile(GetHomeFolder, DefaultDocuments, ServeFiles, AContext, AResponse) then    
    Render(AContext, AResponse);
end;

procedure TmnwSchema.DoAccept(var AContext: TmnwContext; var Resume: Boolean);
begin
end;

procedure TmnwSchema.DoChildRespond(AElement: TmnwElement; const AContext: TmnwContext; AResponse: TmnwResponse);
begin
end;

procedure TmnwSchema.InteractiveMessage(const s: string);
var
  Json: TDON_Pair;
  element: TmnwElement;
  elementID: string;
  Error: string;
begin
  if s.StartsWith('{') then
  begin
    Json := JsonParseStringPair(s, Error, [jsoSafe]);
    try
      elementID := Json['element'].AsString;
      element := FindByID(elementID);
      if element <> nil then
      begin
        Lock.Enter;
        try
          element.ReceiveMessage(Json);
        finally
          Lock.Leave;
        end;
      end;
    finally
      Json.Free;
    end;
  end
end;

class procedure TmnwSchema.Registered;
begin
end;

procedure UpdateElement(Element: TmnwElement);
var
  rttiContext: TRttiContext;
  attribute: TCustomAttributeClass;
  list: TClassList;
begin
  if Element = nil then
    raise Exception.Create('Element is nil');
  if Element.ClassType <> nil then
  begin
    list := TClassList.Create;
    rttiContext := TRttiContext.Create;
    try
      rttiCollectExtensions(rttiContext, Element.ClassType, list);
      for attribute in list do
        if attribute.InheritsFrom(TElementExtension) then
          TElementExtensionClass(attribute).Update(Element);
    finally
      rttiContext.Free;
      list.Free;
    end;
  end;
end;

class function TmnwSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  Result := [];
end;

function TmnwSchema.Accept(var AContext: TmnwContext): Boolean;
begin
  Result := True;
  DoAccept(AContext, Result);
end;

{function TmnwSchema.Interactive: Boolean;
begin
  Result := schemaInteractive in GetCapabilities;
end;}

procedure TmnwSchema.Compose(const AContext: TmnwContext);
begin
  AddState([estComposing]);
  inherited;
  RemoveState([estComposing]);
  AddState([estComposed]);
end;

procedure TmnwSchema.DoPrepare;
begin
  inherited;
end;

function TmnwSchema.GetReleased: Boolean;
begin
  Result := (FPhase = scmpReleased) or (schemaDynamic in GetCapabilities);
end;

procedure TmnwSchema.SetDefaultDocuments(AValue: TStringList);
begin
  FDefaultDocuments.Assign(AValue);
end;

procedure TmnwSchema.Start;
begin
  if (HomeFolder = '') then
    HomeFolder := Web.HomeFolder;
end;

function TmnwSchema.NewHandle: THandle;
begin
  AtomicIncrement(FNamingLastNumber);
  Result := FNamingLastNumber;
end;

function TmnwSchema.GetHomeFolder: string;
begin
  if HomeFolder = '' then
    Result := Web.HomeFolder
  else
    Result := HomeFolder;
end;

procedure TmnwSchema.UpdateAttached;
begin
  Attachments.Lock.Enter;
  try
    FAttached := Attachments.Count > 0;
  finally
    Attachments.Lock.Leave;
  end;
end;

{function TmnwSchema.GetDefaultDocument(vRoot: string): string;
var
  i: Integer;
  aFile: string;
begin
  //TODO baaad you need to lock before access
  vRoot := IncludePathDelimiter(vRoot);
  for i := 0 to DefaultDocument.Count - 1 do
  begin
    aFile := vRoot + DefaultDocument[i];
    if FileExists(aFile) then
    begin
      Result := aFile;
      Exit;
    end;
  end;

  if DefaultDocument.Count<>0 then
    Result := vRoot + DefaultDocument[0]
  else
    Result := vRoot;
end;}

{ TmnwSchema.TElement }

constructor TmnwSchema.TElement.Create(AParent: TmnwElement; ARoute: string);
begin
  inherited Create(AParent);
  Route := ARoute;
end;

function TmnwRenderer.CreateRenderer(AElementClass: TmnwElementClass): TmnwElementRenderer;
var
  aRendererRegister: TmnwElementRendererRegister;
begin
  aRendererRegister := ElementRenderers.FindByParents(AElementClass);
  if aRendererRegister <> nil then
  begin
    Result := aRendererRegister.RendererClass.Create(Self, aRendererRegister);
  end
  else
    Result := nil;
end;

function TmnwRenderer.CreateRenderer(AObject: TmnwElement): TmnwElementRenderer;
begin
  Result := CreateRenderer(TmnwElementClass(AObject.ClassType));
end;

{ THTML.TDocument }

procedure THTML.TDocument.Created;
begin
  inherited;
  FBody := TBody.Create(Self, [elEmbed, elInternal]);
end;

destructor THTML.TDocument.Destroy;
begin
{  FreeAndNil(FBody); }
  inherited;
end;

{ THTML.TInput }

procedure THTML.TInput.SetValue(const AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
  if (estComposed in State) and (Schema <> nil) and Schema.Attached then
    SendInteractive('"command": "change", "content": ' + DQ(Value));
end;

procedure THTML.TInput.SetCaption(const AValue: string);
begin
  if FCaption =AValue then Exit;
  FCaption :=AValue;
end;

procedure THTML.TInput.Created;
begin
  inherited;
  EditType := 'text';
end;

procedure THTML.TInput.ReceiveMessage(JSON: TDON_Pair);
begin
  if JSON['command'].AsString = 'change' then
  begin
    if JSON['content'].IsExists then
      FValue := JSON['content'].AsString;
    if JSON['caption'].IsExists then
      FCaption := JSON['caption'].AsString;
  end;
end;

{ THTML.TInputPassword }

procedure THTML.TInputPassword.Created;
begin
  inherited;
  EditType := 'password';
end;

{ THTML.TImageMemory }

procedure THTML.TImageMemory.Created;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor THTML.TImageMemory.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function THTML.TImageMemory.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

procedure THTML.TImageMemory.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  Data.Seek(0, soBeginning);
  AResponse.SendStream(Data, Data.Size, FileName, InstanceDate);
end;

procedure THTML.TImageMemory.LoadFromFile(const AFileName: string);
begin
  Data.LoadFromFile(AFileName);
  FileName := ExtractFileName(AFileName);
end;

procedure THTML.TImageMemory.LoadFromStream(AStream: TStream);
begin
  Data.LoadFromStream(AStream);
  FileName := '';
  FilePath := '';
end;

{ TmnwElement }

function TmnwElement.This: TmnwElement;
begin
  Result := Self;
end;

function TmnwElement.GetPath: string;
begin
  if Self = nil then
    exit('');

  if (Parent <> nil) then
  begin
    if Route <> '' then
      Result := AddEndURLDelimiter(Parent.GetPath) + Route
    else
      Result := Parent.GetPath;
  end
  else
    Result := Route;
end;

function TmnwElement.GetPathTo(ToElement: TmnwElement): string;
begin
  if (Self = nil) or (Self = ToElement) then
    exit('');

  if (Parent <> nil) then
  begin
    if Route <> '' then
      Result := ConcatString(Parent.GetPathTo(ToElement), URLDelimiter, Route)
    else
      Result := Parent.GetPathTo(ToElement);
  end
  else
    Result := '';
end;

function TmnwElement.GetRenderIt: Boolean;
begin
  Result := not (elNoRender in Kind);
end;

function TmnwElement.GetRespondIt: Boolean;
begin
  Result := not (elNoRespond in Kind);
end;

procedure TmnwElement.SetRenderIt(const Value: Boolean);
begin
  if Value then
    Kind := Kind - [elNoRender]
  else
    Kind := Kind + [elNoRender];
end;

procedure TmnwElement.SetState(const AValue: TmnwElementState);
begin
  if FState =AValue then Exit;
  FState :=AValue;
end;

procedure TmnwElement.Update;
begin

end;

procedure TmnwElement.Added(Item: TmnwElement);
begin
  inherited;
  Item.Update;
end;

procedure TmnwElement.Check;
var
  o: TmnwElement;
begin
  for o in Self do
    o.Check;
end;

function TmnwElement.Find(const Name: string): TmnwElement;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, Name) then
      Exit(Items[i]);
  Result := nil;
end;

function TmnwElement.FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean): TmnwElement;
var
  o: TmnwElement;
begin
  for o in Self do
    if o.InheritsFrom(ObjectClass) and SameText(o.Name, AName) then
      Exit(o);
  for o in Self do
  begin
    Result := o.FindObject(ObjectClass, AName);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
  if RaiseException then
    raise Exception.Create(ObjectClass.ClassName + ': ' + AName + ' not exists in ' + Name);
end;

function TmnwElement.FindParentID(const aID: string): TmnwElement;
var
  e: TmnwElement;
begin
  Result := nil;
  e := Self;
  while e <> nil do
  begin
    if SameText(e.ID, aID) then
      Exit(e);

    e := e.Parent;
  end;
end;

function TmnwElement.FindParentName(const aName: string): TmnwElement;
var
  p: TmnwElement;
begin
  p := Self;
  while p<>nil do
  begin
    if SameText(p.Name, aName) then
      Exit(p);

    p := p.Parent;
  end;

  Result := nil;
end;

procedure TmnwElement.DoPrepare;
begin
end;

function TmnwElement.FindByID(const aID: string): TmnwElement;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].ID, aID) then
      Exit(Items[i]);
    Result := Items[i].FindByID(aID);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TmnwElement.FindByName(const aName: string): TmnwElement;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, aName) then
      Exit(Items[i]);
    Result := Items[i].FindByName(aName);
    if Result <> nil then
      Exit;
  end;
  Result := nil;
end;

function TmnwElement.FindByRoute(const ARoute: string; Level: Integer): TmnwElement;
var
  i: Integer;
begin
  // Find route only on first level, but we ignore the level of route = ''
  for i := 0 to Count - 1 do
  begin
    if Items[i].Route = '' then // Ignoreing level of empty route
    begin
      Result := Items[i].FindByRoute(ARoute, Level + 1);
      if Result <> nil then
        Exit;
    end
    else if SameText(Items[i].Route, ARoute) then
      Exit(Items[i]);
  end;
  Result := nil;
end;

procedure TmnwElement.DoCompose(const AContext: TmnwContext);
begin
end;

procedure TmnwElement.DoExecute;
begin
end;

procedure TmnwElement.Execute;
begin
  if Assigned(OnExecute) then
    OnExecute();
  DoExecute;
end;

procedure TmnwElement.DoChanged;
begin
end;

procedure TmnwElement.Changed;
begin
  DoChanged;
end;

procedure TmnwElement.Prepare;
begin
  if not FPrepared then
  begin
    DoPrepare;
    FPrepared := True;
  end;
end;

procedure TmnwElement.SendInteractive(AMessage: string);
begin
  SendMessage('', '{"element": ' + DQ(ID) + ', ' + AMessage + '}');
end;

procedure TmnwElement.SendMessage(JSON: TDON_Pair);
begin
end;

procedure TmnwElement.ReceiveMessage(JSON: TDON_Pair);
begin
end;

function TmnwElement.GenHandle: Integer;
begin
  if Handle = 0 then
  begin
    FHandle := Schema.NewHandle
  end;
  Result := Handle;
end;

function TmnwElement.GenID: string;
begin
  NewID(Self);
  Result := ID
end;

function TmnwElement.GenRoute: string;
begin
  NewRoute(Self);
  Result := Route;
end;

function TmnwElement.GenName: string;
begin
  NewName(Self);
  Result := Name;
end;

procedure TmnwElement.DoPrepareRenderer(const AContext: TmnwContext);
begin
end;

procedure TmnwElement.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
end;

procedure TmnwElement.DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
end;

constructor TmnwElement.Create(AParent: TmnwElement; AKind: TmnwElementKinds);
begin
  inherited Create;
  FTimeStamp := GetTimeStamp;
  FEnabled := True;
  FVisible := True;  
  FName := '';
  FAttributes := TmnwAttributes.Create;
  FKind := AKind;
  FParent := AParent;
  if FParent <> nil then
  begin
    FSchema:= FParent.FSchema;
    FParent.Add(Self);
  end;
end;

destructor TmnwElement.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

procedure TmnwElement.Add(O: TmnwElement);
begin
  O.FParent := Self;
  O.FSchema := FSchema;
  inherited Add(O);
end;

function TmnwElement.Add(O: TmnwElementClass): TmnwElement;
begin
  Result := O.Create(Self);
end;

//in FPC if you got error, change <O: TmnwElement> to <O>
function TmnwElement.Add<O>(const AID: String; const AName: String): O;
begin
  Result := O.Create(Self);
  Result.FID := AID;
  Result.FName := AName;
end;

function TmnwElement.IndexOfName(vName: string): Integer;
var
  i: integer;
begin
  Result := -1;
  if vName = '' then
    Exit;
  for i := 0 to Count - 1 do
    if SameText(Items[i].Name, vName) then
      Exit(i);
end;

procedure TmnwElement.Respond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  if (Schema <> nil) and (Schema <> Self) then
    Schema.DoChildRespond(Self, AContext, AResponse);
  if not AResponse.IsResponded and Assigned(OnRespond) then
    OnRespond(AContext, AResponse);
//  if not AResponse.IsResponded then  
  DoRespond(AContext, AResponse);
end;

procedure TmnwElement.Compose(const AContext: TmnwContext);
var
  o: TmnwElement;
begin
//  Clear; //*Should not clear here
//  Prepare;
  DoCompose(AContext);
  UpdateElement(Self);
  for o in Self do
  begin
    o.Compose(AContext); //Compose
  end;
  DoComposed;
end;

procedure TmnwElement.DoComposed;
begin
end;

procedure TmnwElement.AddState(AState: TmnwElementState);
var
  o: TmnwElement;
begin
  FState := FState + AState;
  for o in Self do
  begin
    o.AddState(AState);
  end;
end;

procedure TmnwElement.RemoveState(AState: TmnwElementState);
var
  o: TmnwElement;
begin
  FState := FState - AState;
  for o in Self do
  begin
    o.RemoveState(AState);
  end;
end;

procedure TmnwElement.Clear;
begin
  inherited;
  RemoveState([estComposed]);
end;

function TmnwElement.GetContentType(Route: string): string;
begin
  Result := 'text/html';
end;

procedure TmnwElement.RespondInit(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
//  AResponse.PutHeader('Content-Type', GetContentType(AContext.Route));
  DoRespondHeader(AContext, AResponse);
end;

{ TmnwRenderer }

procedure TmnwRenderer.AddHead(const Context: TmnwContext);
begin
end;

procedure TmnwRenderer.BeginRender;
begin
  DoBeginRender;
end;

constructor TmnwRenderer.Create(AModule: TmodWebModule);
{var
  o: TmnwRenderer.TmnwElementRendererRegister;}
begin
  FLibraries := TmnwLibraries.Create;
  inherited Create;
  FModule := AModule;
  FParams := TmnwAttributes.Create;
  //Renderers := TmnwElementRenderers.Create();
{  for o in Renderers do
    log.WriteLn(o.ObjectClass.ClassName);}
  {log.WriteLn('---------------------------');
  for o in Renderers do
    log.WriteLn(o.ObjectClass.ClassName);}
end;

destructor TmnwRenderer.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FLibraries);
  inherited;
end;

procedure TmnwRenderer.EndRender;
begin
  DoEndRender;
end;

class function TmnwRenderer.RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean): TmnwElementRendererRegister;
begin
  Result := ElementRenderers.RegisterRenderer(AElementClass, ARendererClass, Replace);
end;

procedure TmnwRenderer.DoBeginRender;
begin
end;

procedure TmnwRenderer.DoEndRender;
begin
end;

{ TmnwSchemaItem }

destructor TmnwSchemaItem.Destroy;
begin
  inherited;
end;

{$ifdef rtti_objects}
{ TCacheClassObjects }

procedure TCacheClassObjects.AddClass(ObjectClass: TClass);
var
  aObject: TCacheClassObject;
begin
  aObject:=TCacheClassObject.Create;
  aObject.ObjectClass := ObjectClass;
  inherited Add(aObject);
end;
{$endif}

{ THTML.TFile }

procedure TmnwSchema.TFile.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  aStream: TResourceStream;
begin
  inherited;
  if ftResource in Options then
    AResponse.SendResource(FileName)
  else
    AResponse.SendFile(FileName);
end;

constructor TmnwSchema.TFile.Create(AParent: TmnwElement; AOptions: TFileOptions; AFileName: string; ARoute: string );
begin
  inherited Create(AParent);
  Options := AOptions;
  FileName := AFileName;
  if not (ftEmbed in Options) then
    if (ARoute = '') then
      Route := ExtractFileName(FileName)
    else
      Route := ARoute;
end;

function TmnwSchema.TFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

procedure TmnwSchema.TMemory.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  Data.Seek(0, soBeginning);
  AResponse.SendStream(Data, Data.Size, FileName, FileDate);
end;

procedure TmnwSchema.TMemory.Created;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor TmnwSchema.TMemory.Destroy;
begin
  inherited;
  FreeAndNil(FData);
end;

function TmnwSchema.TMemory.GetContentType(Route: string): string;
begin
  Result := ContentType;
end;

procedure TmnwSchema.TMemory.LoadFromFile(const AFileName: string);
begin
  Data.LoadFromFile(AFileName);
  FileAge(AFileName, FileDate);
  FileName := ExtractFileName(AFileName);
  ContentType := DocumentToContentType(AFileName);
end;

procedure TmnwSchema.TMemory.LoadFromStream(AStream: TStream; AContentType: string);
begin
  Data.LoadFromStream(AStream);
  ContentType := AContentType;
  FileDate := 0;
  FileName := '';
  FilePath := '';
end;

{ THTML.TAssets }

procedure THTML.TAssets.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  ServeFile(Schema.GetHomeFolder, [serveDefault], AContext, AResponse);
end;

function THTML.TAssets.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TFolder }

procedure THTML.TFolder.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  ServeFile(HomeFolder, ServeFiles, AContext, AResponse);
end;

function THTML.TFolder.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TDynamicCompose }

constructor THTML.TDynamicCompose.Create(AParent: TmnwElement; AOnCompose: TComposeProc);
begin
  inherited Create(AParent);
  OnCompose := AOnCompose;
end;

procedure THTML.TDynamicCompose.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  Inner: TInner;
begin
  inherited;
  Inner:= TInner.Create(nil);
  try
    Inner.FSchema := Schema;
    Inner.FParent := Self; //Fake Parent do not add it to the list;
    Inner.IsRoot := AContext.Element = Self; // if compused from Schema of parents, or just directly composed
    InnerCompose(Inner, AResponse);
    if Assigned(OnCompose) then
      OnCompose(Inner, AResponse);
    Inner.Compose(AContext);

    Inner.Render(AContext, AResponse);
  finally
    Inner.Free;
  end;
end;

procedure THTML.TDynamicCompose.InnerCompose(Inner: TmnwElement; AResponse: TmnwResponse);
begin
end;

{ TmnwLibrary }

procedure TmnwLibrary.AddHead(const Context: TmnwContext);
var
  source: TLibrarySource;
  url: string;  
  aDirection: TDirection;
  local: Boolean;
begin
  for source in Sources do
  begin
    aDirection := Context.Schema.Direction;
    if aDirection = dirUndefined then
      aDirection := dirLeftToRight;      
    
    if (source.Direction = dirUndefined) or (source.Direction = aDirection) then
    begin
      if source.Where in [stOnline, stResource]  then           
      begin
        if (source.Value = '') or (source.Where = stResource) or CheckOffline(Context, source.Name) then
        begin
          url := EndUrl(Context.GetAssetsURL) + source.Name;
          local := True;
        end
        else if source.SourceType in [stStyle, stScript] then           
        begin
          url := source.Value;
          local:= False;
        end;

        if not StartsText('http', url) then
        begin
          if Context.Request.IsSecure then
            url := 'https://' + url
          else
            url := 'http://' + url;
        end;
        
        case source.SourceType of
          stStyle: Context.Writer.AddLinkStyle(url, When(not local, source.Integrity), libDefer in source.Options, libCross in source.Options);
          stScript: Context.Writer.AddLinkScript(url, When(not local, source.Integrity), libDefer in source.Options, libCross in source.Options);
        end;
      end
      else
      begin
        case source.SourceType of
          stStyle: Context.Writer.AddEmbedStyle(source.Value);
          stScript: Context.Writer.AddEmbedScript(source.Value, libDefer in source.Options);
        end;
      end;
    end;    
  end;
end;

function TmnwLibrary.CheckOffline(const Context: TmnwContext; const FileName: string): Boolean;
begin
  with Context.Schema do
    Result := (Web.OnlineFiles = olfOffline) or ((Web.OnlineFiles = olfSmart) and FileExists(Context.GetAssetFolder + FileName));
end;

constructor TmnwLibrary.Create;
var
  i: Integer;
  s: string;
begin
  inherited Create;  
  
  FSources := TLibrarySources.Create;
  s := ClassName;
  i:= Pos('_', s);
  if i > 0 then
    s := MidStr(s, 2, i - 2)
  else
    s := MidStr(s, 2, MaxInt);
  Name := s;
end;

procedure TmnwLibrary.Created;
begin
  inherited;
end;

procedure TmnwLibrary.DecUsage;
begin
  FUsage := FUsage - 1;
end;

destructor TmnwLibrary.Destroy;
begin
  inherited;
  FreeAndNil(FSources);
end;

procedure TmnwLibrary.IncUsage;
begin
  FUsage := FUsage + 1;
end;

{ TmnwLibraries }

function TmnwLibraries.Find(ALibrary: string): TmnwLibrary;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (SameText(Items[i].Name, ALibrary)) then
    begin
      Result := Items[i];
      break;
    end;
end;

function TmnwLibraries.RegisterLibrary(ALibraryClass: TmnwLibraryClass; UseIt: Boolean = False): TmnwLibrary;
begin
  Result := ALibraryClass.Create;
  Add(Result);
  if UseIt then
    Use(Result);
end;

procedure TmnwLibraries.Use(ALibrary: TmnwLibrary);
begin
  if ALibrary <> nil then
  begin
    ALibrary.IncUsage;
//    Move(IndexOf(ALibrary), 0);
  end
  else
    raise Exception.Create('library is nil');
end;

function TmnwLibraries.Compare(Item1, Item2: TmnwLibrary): Integer;
begin
  Result := Item1.Priority - Item2.Priority;
  //TODO use DependsOn
end;

function TmnwLibraries.Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i] is ALibraryClass then
    begin
      Result := Items[i];
      break;
    end;
end;

function TmnwLibraries.RegisterLibrary(ALibraryClass: TmnwLibraryClass; Priority: Integer; UseIt: Boolean): TmnwLibrary;
begin
  Result := ALibraryClass.Create;
  Result.Priority := Priority;
  Add(Result);
  if UseIt then
    Use(Result);
end;

procedure TmnwLibraries.Use(ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryClass);
  if ALibrary = nil then
    RegisterLibrary(ALibraryClass, True)
  else
    Use(ALibrary)
end;

procedure TmnwLibraries.Use(ALibraryName: string);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryName);
  if ALibrary <> nil then
    Use(ALibrary)
  else
    raise Exception.Create('There is no library: ' + ALibraryName);
end;

{ TJQuery_Library }

procedure TJQuery_Library.Created;
begin
  inherited;
  Sources.Add(stScript, 'cdn.jsdelivr.net/npm/jquery@3.7.1/dist/', 'jquery.min.js');
end;

{ THTML }

procedure TWebElements_Library.Created;
begin
  inherited;
  Sources.Add(stScript, '', 'web-elements.js', '?v=' + IntToStr(GlobalTimeStamp));
  Sources.Add(stStyle, '', 'web-elements.css', '?v=' + IntToStr(GlobalTimeStamp));
end;

{ THTML.TImage }

procedure THTML.TImage.DoCompose(const AContext: TmnwContext);
begin
  inherited;
end;

{ THTML.TBody }

constructor THTML.TBody.Create(AParent: TmnwElement; AKind: TmnwElementKinds);
begin
  inherited;
  //This object auto free by parents
  FHeader := THeader.Create(Self, [elEmbed, elNoRespond] + When(not (apoHeader in Schema.Web.Options), elNoRender));
  FHeader.Priority := priorityStart;

  FContent := TContent.Create(Self, [elEmbed, elNoRespond]);
  with FContent do
  begin
    FSideBar := TSideBar.Create(This, [elEmbed, elNoRespond] + When(not (apoSideBar in Schema.Web.Options), elNoRender));
    FSideBar.Priority := priorityStart;
    FMain := TMain.Create(This, [elEmbed]);
  end;

  FFooter := TFooter.Create(Self, [elEmbed, elNoRespond, elNoRender]);
  FFooter.Priority := priorityEnd;
  FToast := TToast.Create(Self, [elEmbed, elNoRespond]);
  FToast.Priority := priorityEnd;
end;

destructor THTML.TBody.Destroy;
begin
  inherited;
end;

function THTML.TBody.GetWide: Boolean;
begin
  Result := FContent.Wide
end;

procedure THTML.TBody.SetWide(const Value: Boolean);
begin
  FContent.Wide := Value;
end;

{ THTML.TNavBar }

constructor THTML.TNavBar.Create(AParent: TmnwElement; AKind: TmnwElementKinds);
begin
  inherited;
  FImage := TImageFile.Create(This, [elInternal, elEmbed]);
  FImage.FileName := Schema.Web.Assets.LogoFile;
  FTools := TNavTools.Create(This, [elInternal, elEmbed]);
end;

destructor THTML.TNavBar.Destroy;
begin
  inherited;
end;

{ THTML.THeader }

function THTML.THeader.GetMenuBar: TMenuBar;
begin
  if FMenuBar = nil then
    FMenuBar := TMenuBar.Create(Self, [elEmbed]);
  Result := FMenuBar;
end;

function THTML.THeader.GetNavBar: TNavBar;
begin
  if FNavBar = nil then
    FNavBar := TNavBar.Create(Self, [elEmbed]);
  Result := FNavBar;
end;

procedure THTML.THeader.Created;
begin
  inherited;
  Shadow := shadowHeavy;
  if ID = '' then
    ID := 'header';
end;

{ THTML.TSideBar }

function THTML.TSideBar.CanRender: Boolean;
begin
  Result :=inherited CanRender and (Count > 0);
end;

procedure THTML.TSideBar.Created;
begin
  inherited;
  Shadow := shadowHeavy;
  Theme := themeUndefined;
end;

{ THTML.TMain }

procedure THTML.TMain.Created;
begin
  inherited;
  Gap := 1;
end;

{ THTML.TBar }

procedure THTML.TBar.Created;
begin
  inherited;
  //Padding := -1;
end;

{ THTML.TCard }

procedure THTML.TCard.Created;
begin
  inherited;
  Shadow := shadowLight;
end;

{ THTML.TForm }

procedure THTML.TForm.DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  if (RedirectTo <> '') and (AResponse.Answer = hrNone) then
  begin
    AResponse.Answer := hrRedirect;
    AResponse.Redirect := RedirectTo;
  end;
end;

procedure THTML.TForm.Created;
begin
  inherited;
  PostTo.Where := toElement;
end;

procedure THTML.TForm.DoComposed;
begin
  inherited;
  if PostTo.Where = toElement then
    NewRoute(Self);
end;

{ THTML.TParagraph }

constructor THTML.TParagraph.Create(AParent: TmnwElement; AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

{ THTML.TAction }

constructor THTML.TAction.Create(AParent: TmnwElement; AName, ARoute: string; ActionProc: TRespondProc);
begin
  inherited Create(AParent);
  Name := AName;
  Route := ARoute;
  OnRespond := ActionProc;
end;

procedure THTML.TAction.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  try
    Execute;
    AContext.Writer.WriteLn('Executed');
  finally
  end;
end;

procedure THTML.TAction.Loop;
begin
end;

{ THTMLItem }

procedure THTML.THTMLItem.SetCaption(const AValue: string);
begin
  if FCaption =AValue then Exit;
  FCaption :=AValue;
  if (estComposed in State) and (Schema <> nil) and Schema.Attached then
    SendInteractive('"command": "change", "content": ' + DQ(Caption));
end;

{ THTML.TClickable }

procedure THTML.TClickable.ReceiveMessage(JSON: TDON_Pair);
begin
  if JSON['command'].AsString = 'change' then
  begin
    if JSON['caption'].IsExists then
      FCaption := JSON['caption'].AsString;
  end
  else if JSON['command'].AsString = 'click' then
    Execute;
end;

{ THTML.TAccordion }

procedure THTML.TAccordion.Created;
begin
  inherited;
end;

{ TNameAttribute }

class procedure TID_Extension.Update(Element: TmnwElement);
begin
  NewID(Element);
end;

{ TName_Extension }

class procedure TName_Extension.Update(Element: TmnwElement);
begin
  NewName(Element);
end;

{ TRoute_Extension }

class procedure TRoute_Extension.Update(Element: TmnwElement);
begin
  NewRoute(Element);
end;

function TmnwWebCommand.CreateResponse: TmodResponse;
begin
  Result := TmnwResponse.Create(Request);
end;

function TmnwWebCommand.GetModule: TmnwWebModule;
begin
  Result := (inherited Module) as TmnwWebModule;
end;

function TmnwWebCommand.GetResponse: TmnwResponse;
begin
  Result := inherited Response as TmnwResponse;
end;

//Main
procedure TmnwWebCommand.RespondResult(var Result: TmodRespondResult);
var
  aContext: TmnwContext;
  aDomain, aPort: string;
begin
  inherited;
  if (Request.Path = '') and (Request.URI <> '') then
  begin
    Response.RespondRedirectTo(IncludeURLDelimiter(Request.URI));
    exit;
  end;
  AtomicIncrement(RendererID);
  InitMemory(aContext, SizeOf(aContext));

  aContext.Route := DeleteSubPath('', Request.Path);
  aContext.Sender := Self;
  aContext.Request := Request;

  if Module.Domain <> '' then
  begin
    aDomain := Module.Domain;
    aPort := Module.Port;
  end
  else
    SpliteStr(Request.Header['Host'], ':', aDomain, aPort);

  if Module.Web.Domain = '' then
  begin
    Module.Web.Lock.Enter; //smart huh, first connection will setup the domain name, i don't like it
    try
      Module.Web.Domain := aDomain;
      Module.Web.Port := aPort;
    finally
      Module.Web.Lock.Leave;
    end;
  end;

  if (aDomain='') and Request.Connected then
    raise Exception.Create('Domain is not defined');

  if Request.ConnectionType = ctWebSocket then
  begin
    //Serve the websocket
    if (Module as TmnwWebModule).Web.Attach(aContext, Self, Response.Stream) = nil then
      Result.Status := []; // Disconnect
  end
  else
  begin
    aContext.Web := Module.Web;
    aContext.Renderer := (Module as TmnwWebModule).CreateRenderer;
    aContext.Renderer.RendererID := RendererID;
    aContext.Renderer.Libraries.QuickSort;
    aContext.Writer := TmnTidyWriter.Create('html', Response.Stream);
    aContext.Writer.Compact := Module.Web.CompactMode;

    aContext.Data := TmnMultipartData.Create; //yes always created, i maybe pass params that come from Query (after ? )
    if Request.ConnectionType = ctFormData then
    begin
      aContext.Data.Boundary := Request.Header.Field['Content-Type'].SubValue('boundary');
      aContext.Data.TempPath := (Module as TmnwWebModule).WorkFolder + 'temp';
      aContext.Data.Read(Request.Stream);
    end;
    
    try
      aContext.Stamp := Request.Header['If-None-Match'];

      Response.Answer := hrOK;
      Response.ContentType := DocumentToContentType('html');
      (Module as TmnwWebModule).Web.Respond(aContext, Response);

      //SessionID
    finally
      FreeAndNil(aContext.Writer);
      FreeAndNil(aContext.Renderer);
      FreeAndNil(aContext.Data);
    end;
  end;
end;

{ TAssetsSchema }

procedure TAssetsSchema.Created;
begin
  inherited;
//  Kind := Kind + [elFallback];
  //FLogo := THTML.TMemory.Create(This);
  //FLogo.Name := 'logo';
  //FLogo.Route := 'logo';
  FPhase := scmpNormal;
  ServeFiles := [serveEnabled, serveSmart, serveDefault];
end;

procedure TAssetsSchema.Start;
var
  minilib: string;
begin
  inherited;
  Name := 'Assets';
  Route := 'assets';
  //TCSSFile.Create(This, [ftResource], 'mnWebElements.css');
  minilib := GetEnvironmentVariable('minilib');
  if minilib = '' then
  begin
    if FileExists(GetHomeFolder + 'mnWebElements.js') then
    begin
      TFile.Create(This, [], GetHomeFolder + 'web-elements.js', 'web-elements.js');
      TFile.Create(This, [], GetHomeFolder + 'web-elements.css', 'web-elements.css');
    end
    else
    begin
      TFile.Create(This, [ftResource], 'WebElements_CSS.css', 'web-elements.css');
      TFile.Create(This, [ftResource], 'WebElements_JS.js', 'web-elements.js');
    end;
  end
  else
  begin
    TFile.Create(This, [], ExpandFileName(IncludePathDelimiter(minilib) + '/socket/source/mnWebElements.js'), 'web-elements.js');
    TFile.Create(This, [], ExpandFileName(IncludePathDelimiter(minilib) + '/socket/source/mnWebElements.css'), 'web-elements.css');
  end;

  with TElement.Create(This, 'resource') do
  begin
    TFile.Create(This, [ftResource], 'WebElements_CSS.css', 'web-elements.css');
    TFile.Create(This, [ftResource], 'WebElements_JS.js', 'web-elements.js');
  end;
end;

class function TAssetsSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  Result := inherited + [schemaStartup, schemaPermanent];
end;

{ TmnwWebModule }

procedure TmnwWebModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  inherited;
  if (ARequest.Route.Count > 0) then
  begin
	//TODO I do not understand this part?
    if StartsStr('.', ARequest.Route[ARequest.Route.Count - 1]) then
      ARequest.Command := ARequest.Route[ARequest.Route.Count - 1]
    else
      ARequest.Command := ARequest.Route[0]; //TODO or 1?
  end
  else
    ARequest.Command := '';
end;

procedure TmnwWebModule.Start;
begin
  inherited;
//  AssetsURL := '/' + AliasName + '/' + Web.Assets.Route;
  if Web.HomeFolder = '' then
    Web.HomeFolder := HomeFolder;
  if Web.Domain = '' then
    Web.Domain := Domain;
  if Web.Port = '' then
    Web.Port := Port;
  if Web.WorkFolder = '' then
    Web.WorkFolder := WorkFolder;
  if Web.ModuleName = '' then
    Web.ModuleName := AliasName;
  //Web.Assets.HomeFolder := Web.HomeFolder;
  Web.IsSecure := Modules.IsSecure;

  Web.Start;
end;

procedure TmnwWebModule.Stop;
begin
  Web.Stop;
  inherited;
end;

procedure TmnwWebModule.InitItems;
begin
  inherited;
  RegisterCommand('', TmnwWebCommand, true);
end;

function TmnwWebModule.CreateRenderer: TmnwRenderer;
begin
  if Renderers.Current = nil then
    Result := TmnwPlaneRenderer.Create(Self)
  else
    Result := Renderers.Current.RendererClass.Create(Self);
end;

destructor TmnwWebModule.Destroy;
begin
  inherited;
  FreeAndNil(FWeb); //keep behind inherited
end;

constructor TmnwWebModule.Create(AModules: TmodModules; const AName: string; const AAliasName: String);
begin
  FWeb := TmnwWeb.Create;
  inherited;
end;

{ TElementClasses }

function TElementClasses.Add(const Name: string): Integer;
begin
  if Name = '' then
    exit(-1);

  Result:= Find(Name);
  if Result < 0 then
  begin
    Items := Items + [Name];
    Result := Length(Items) - 1;
  end;
end;

procedure Classes_StrToStringsExCallbackProc(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
type
  PElementClasses = ^TElementClasses;
begin
  PElementClasses(Sender)^.Add(S);
end;

procedure TElementClasses.AddClasses(const S: string);
var
  MatchCount: Integer;
begin
  StrToStringsExCallback(S, 0, @Self, [' '], MatchCount, @Classes_StrToStringsExCallbackProc, []);
end;

function TElementClasses.Find(const Name: string): Integer;
var
 i: Integer;
begin
  for i := 0 to Length(Items) -1 do
  begin
    if SameText(Name, Items[i]) then
      exit(i)
  end;
  Result := -1
end;

class operator TElementClasses.Add(A: TElementClasses; B: string): TElementClasses;
begin
  A.Add(B);
  Result := A;
end;

procedure TElementClasses.AddClasses(A: TElementClasses);
var
 itm : String;
begin
  for itm in A.Items do
  begin
    Add(itm);
  end;
end;

class operator TElementClasses.Explicit(const Source: string): TElementClasses;
begin
  InitMemory(Result, SizeOf(Result));
  Result.AddClasses(Source)
end;

class operator TElementClasses.Implicit(Source: string): TElementClasses;
begin
  InitMemory(Result, SizeOf(Result));
  Result.AddClasses(Source)
end;

class operator TElementClasses.Implicit(Source: TElementClasses): string;
begin
  Result := Source.ToString
end;

procedure TElementClasses.Init(classes: string);
begin
  InitMemory(Self, SizeOf(Self));
  AddClasses(classes);
end;

function TElementClasses.Remove(const Name: string): Boolean;
var
  index: integer;
begin
  index := Find(Name);
  Result := index >= 0;
  if Result then
    Delete(Items, index, 1);
end;

class operator TElementClasses.Subtract(A: TElementClasses; B: string): TElementClasses;
var
  i: Integer;
begin
  i := A.Find(B);
  if i>=0 then
    Delete(A.Items, i, 1);
  Result := A;
end;

function TElementClasses.ToString: string;
var
 itm : String;
begin
  Result := '';
  for itm in Items do
  begin
    if Result <> '' then
      Result := Result + ' ' + itm
    else
      Result := itm;
  end;

  if Result <> '' then
    Result := 'class="'+Result+'"';
end;

{ TmnwScope }

function TmnwScope.GetText: string;
begin
  Result := ToString;
  if (Result <> '') then
    Result := ' ' + Result;
end;

function TmnwScope.ToString: string;
var
  s: string;
begin
  s := Attributes.ToString;
  Result := Space(s, Classes.ToString);
end;

{ THTML.TLink }

constructor THTML.TLink.Create(AParent: TmnwElement; const ALocation: string; ACaption: string);
begin
  inherited Create(AParent);
  Location := ALocation;
  FCaption := ACaption;
end;

{ THTML.TCollapseCaption }

procedure THTML.TCollapseCaption.DoCompose(const AContext: TmnwContext);
begin
  inherited;
end;

{ THTML.TDropdown }

procedure THTML.TDropdown.Created;
begin
  inherited;
  Options := [dropArraw];
end;

{ THTML.TZoomButtons }

procedure THTML.TZoomButtons.Created;
begin
  inherited;
  FButtonSmall := TButton.Create(Self, [elEmbed]);
  FButtonSmall.ID := 'zoom-small';
  FButtonSmall.ControlStyle := styleUndefined;
  FButtonSmall.Image.IconClass := 'icon mw-font-small';
  FButtonSmall.JSFunction := 'mnw.switch_zoom';

  FButtonNormal := TButton.Create(Self, [elEmbed]);
  FButtonNormal.ID := 'zoom-normal';
  FButtonNormal.ControlStyle := styleUndefined;
  FButtonNormal.Image.IconClass := 'icon mw-font-normal';
  FButtonNormal.JSFunction := 'mnw.switch_zoom';

  FButtonLarge := TButton.Create(Self, [elEmbed]);
  FButtonLarge.ID := 'zoom-large';
  FButtonLarge.ControlStyle := styleUndefined;
  FButtonLarge.Image.IconClass := 'icon mw-font-large';
  FButtonLarge.JSFunction := 'mnw.switch_zoom';
end;

{ THTML.THTMLGroup }

function THTML.THTMLGroup.CanRender: Boolean;
begin
  Result := inherited CanRender and (Count>0);
end;

{ THTML.TSpan }

constructor THTML.TSpan.Create(AParent: TmnwElement; const AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

procedure THTML.TButton.Created;
begin
  inherited;
  ControlStyle := stylePrimary;
end;

{ TmnwContext }

function TmnwContext.GetPath: string;
begin
  Result := GetPath(Schema);
end;

function TmnwContext.GetPath(e: TmnwElement): string;
begin
  Result := GetHomePath + StartURL(e.GetPath);
end;

function TmnwContext.GetRelativePath(e: TmnwElement): string;
begin
  if Element = nil then  
    Result := e.GetPathTo(Schema)
  else
    Result := e.GetPathTo(Element);
end;

function TmnwContext.GetURL: string;
begin
  Result := GetURL(Schema);
end;

function TmnwContext.GetHomePath: string;
begin
  Result := StartURL(Schema.Web.ModuleName + StartURL(Request.NameSpace));
end;

function TmnwContext.GetHomeURL: string;
begin
  Result := Web.GetHostURL + GetHomePath;
end;

function TmnwContext.GetHostURL: string;
begin
  Result := Web.GetHostURL;
end;

function TmnwContext.GetLocationPath(Location: TLocation): string;
begin
  if Location.Custom <> '' then
    Result := Location.Custom
  else if Location.Where = toSchema then
    Result := GetPath(Schema)
  else if Location.Where = toElement then
    Result := GetPath(Element)
  else if Location.Where = toHome then
    Result := URLDelimiter;
end;

function TmnwContext.GetURL(e: TmnwElement): string;
begin
  Result := GetHomeURL + StartURL(e.GetPath);
end;

function TmnwContext.GetAssetFolder: string;
begin
  if Schema.Web.Assets <> nil then
    Result := Schema.Web.Assets.HomeFolder
  else
    Result := Schema.Web.HomeFolder;
end;

function TmnwContext.GetAssetsPath: string;
begin
  Result := GetPath(Schema.Web.Assets);
end;

function TmnwContext.GetAssetsURL: string;
begin
  Result := GetURL(Schema.Web.Assets);
end;

{ TmnwResponse }

procedure TmnwResponse.Created;
begin
  inherited;
  FSession := TmnwCookie.Create('session');
  FSession.Stricted := True;
  FSession.Secured := False;
  FSession.ResetChanged;
end;

destructor TmnwResponse.Destroy;
begin
  FreeAndNil(FSession);
  inherited;
end;

procedure TmnwResponse.DoWriteCookies;
var
  s: string;
begin
  if Session.Changed then //If Value = '' that mean we will delete it
  begin
    s := Session.GetText;
    Stream.WriteUTF8Line('Set-Cookie: ' + s);
  end;
  inherited;
end;

function TmnwResponse.GetSessionID: string;
begin
  Result := Session.Value;
end;

procedure TmnwResponse.SetAnswer(const Value: TmodAnswer);
begin
  inherited;  
end;

procedure TmnwResponse.SetSessionID(const Value: string);
begin
  Session.Value := Value;
end;

{ TLoginSchema }

procedure TLoginSchema.DoRespondHeader(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  if (AContext.Data <> nil) and SameText(AContext.Data.Values['execute'], 'true') then
  begin
    DoLogin(AContext, AResponse);
  end;
  inherited;
end;

procedure TLoginSchema.DoChildRespond(AElement: TmnwElement; const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  if (AElement.Name = 'login-form') and (AContext.Data <> nil) and (SameText(AContext.Data.Values['execute'], 'true') ) then
  begin
    DoLogin(AContext, AResponse);
  end;
end;

procedure TLoginSchema.DoCompose(const AContext: TmnwContext);
begin
  inherited;
  with Document do
  begin
    Title := 'Login';
    Direction := dirLeftToRight;

    TAction.Create(This, 'login', 'login', DoLogin);
    TAction.Create(This, 'logout', 'login', DoLogout);
    
    with Body do
    begin
      with Main do
      begin
        with TCard.Create(this) do
        begin
          Solitary := True;
          Size := szNormal;
          Caption := 'Login';

          Add(TLoginElement);
        end;
      end;
    end;
  end;
end;

procedure TLoginSchema.DoLogin(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

procedure TLoginSchema.DoLogout(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  AResponse.SessionID := '';
  AResponse.RespondRedirectTo(AContext.GetHomePath);
end;

{ THTML.TImageFile }

function THTML.TImageFile.CanRender: Boolean;
begin
  Result := (inherited CanRender) and (FileName <> '');
end;

procedure THTML.TImageFile.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  AResponse.SendFile(FileName);
end;

function THTML.TImageFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

{ TLoginElement }

procedure TLoginElement.DoCompose(const AContext: TmnwContext);
begin
  Solitary := True;
  Size := szNormal;
  Caption := 'Login';

  with THTML, Self do
  begin
    with THTML.TForm.Create(This) do
    begin
      Route := 'login';
      Name := 'login-form';
      PostTo.Where := toElement;

      with TInput.Create(This) do
      begin
        ID := 'username';
        Name := 'username';
        Caption := 'Username';
        PlaceHolder := 'Type user name';
      end;

      with TInputPassword.Create(This) do
      begin
        ID := 'password';
        Name := 'password';
        Caption := 'Password';
        HelpText := 'You need to use numbers';
      end;

      TBreak.Create(This);

      Submit.Caption := 'Submit';
      Reset.Caption := 'Reset';
    end;
  end;
  inherited;
end;

{ TmnwElementLibrary }

function TmnwElementLibrary.Add(ARendererClass: TmnwElementRendererClass): TmnwElementLibraryItem;
begin
  Result := TmnwElementLibraryItem.Create;
  Result.RendererClass := ARendererClass;
  inherited Add(Result);
end;

{ THTML.TCode }

constructor THTML.TCode.Create(AParent: TmnwElement; AText, ALanguage: string);
begin
  inherited Create(AParent);
  Text := AText;
  Language := ALanguage;
end;

{ TmnwRenderers }

function TmnwRenderers.FindBy(ARendererClass: TmnwRendererClass): TmnwRendererRegister;
var
 i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    if Items[i].RendererClass = ARendererClass then
      exit(Items[i]);
  end;
  Result := nil;
end;

function TmnwRenderers.RegisterRenderer(AName: string; ARendererClass: TmnwRendererClass): TmnwRendererRegister;
begin
  if ARendererClass = nil then
    raise Exception.Create('RendererClass is null to register');
  Result := TmnwRendererRegister.Create;
  Result.Name := AName;
  Result.RendererClass := ARendererClass;
  Add(Result);
  if FCurrent = nil then  
    FCurrent := Result;
end;

procedure TmnwRenderers.Switch(AName: string);
var
  itm: TmnwRendererRegister;
begin
  itm := Find(AName);
  if itm = nil then
    raise Exception.Create('Renderer ' + AName + ' not exists');
  FCurrent := itm;
end;

procedure TmnwRenderers.Switch(ARendererClass: TmnwRendererClass);
var
  itm: TmnwRendererRegister;
begin
  itm := FindBy(ARendererClass);
  if itm = nil then
    raise Exception.Create('Renderer ' + ARendererClass.ClassName + ' not registered');
  FCurrent := itm;
end;

{ TmnwPlaneRenderer }

procedure TmnwPlaneRenderer.Created;
begin
  inherited;
  //Libraries.RegisterLibrary(TWebElements_Library, 2000, True);
end;

class destructor TmnwPlaneRenderer.Destroy;
begin
  FreeAndNil(Plane_ElementRenderers);
end;

class function TmnwPlaneRenderer.ElementRenderers: TmnwElementRenderers;
begin
  if Plane_ElementRenderers = nil then
    Plane_ElementRenderers := TmnwElementRenderers.Create;
  Result := Plane_ElementRenderers;
end;

{ TmnwSchema.TRoute }

constructor TmnwSchema.TRoute.Create(AParent: TmnwElement; ARoute: string; AKind: TmnwElementKinds);
begin
  inherited Create(AParent, AKind);
  Name := ARoute;
  Route := ARoute;
end;

procedure TmnwSchema.TRoute.Created;
begin
  inherited;
  Kind := Kind + [elNoRespond];
end;

{ TLibrarySources }

function TLibrarySources.Add(SourceType: TLibrarySourceType; Where: TLibrarySourceWhere; const OnlineFile, LocalFileName: string; Direction: TDirection; Integrity: string = ''; Options: TLibraryOptions = [libDefer, libCross]): TLibrarySource; 
begin
  Result := TLibrarySource.Create;
  Result.Name := LocalFileName;
  Result.SourceType := SourceType;
  Result.Where := Where;
  if EndsDelimiter(OnlineFile) then 
    Result.Value := OnlineFile + LocalFileName
  else  
    Result.Value := OnlineFile;
  Result.Direction := Direction;
  Result.Integrity := Integrity;
  Result.Options := Options;
  inherited Add(Result);
end;

function TLibrarySources.Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFileName: string; Integrity: string; Options: TLibraryOptions): TLibrarySource;
begin
  Result := Add(SourceType, stOnline, OnlineFile, LocalFileName, dirUndefined, Integrity, Options);
end;

function TLibrarySources.Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFileName: string): TLibrarySource;
begin
  Result := Add(SourceType, stOnline, OnlineFile, LocalFileName, dirUndefined, '');
end;

function TLibrarySources.AddStyle(const EmbedText: string; Direction: TDirection): TLibrarySource;
begin
  Result := Add(stStyle, stEmbed, EmbedText, '', Direction);
end;

{ TLibrarySource }

constructor TLibrarySource.Create;
begin
  inherited;
  Options := [libDefer, libCross];
end;

{ TmnwUsedLibraries }

procedure TmnwUsedLibraries.Use(ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Libraries.Find(ALibraryClass);
  if ALibrary = nil then
    Libraries.RegisterLibrary(ALibraryClass, True)
  else
    Use(ALibrary);
end;

procedure TmnwUsedLibraries.Use(ALibraryName: string);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Libraries.Find(ALibraryName);
  if ALibrary <> nil then
    Use(ALibrary)
  else
    raise Exception.Create('There is no library: ' + ALibraryName);
end;

procedure TmnwUsedLibraries.Use(ALibrary: TmnwLibrary);
begin
  if ALibrary <> nil then
  begin
  end
  else
    raise Exception.Create('library is nil');
end;

initialization
  GlobalTimeStamp := GetTimeStamp;
finalization
  FreeAndNil(FRenderers);
  FreeAndNil(FLibraries);
end.
