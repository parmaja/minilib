unit mnWebElements;//* BETA
{$IFDEF FPC}
{$mode delphi}
{$modeswitch prefixedattributes}
{$modeswitch arrayoperators}
{$modeswitch arraytodynarray}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$ENDIF}
{$H+}{$M+}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 * @author    Belal Hamed <belal, belalhamed@gmail.com>
 *
 *}

{
            Userinfo       Host      Port
            ┌──┴───┐ ┌──────┴────────┬┴┐
GET https://john.doe@www.example.com:123/username/forum/questions/qst1/?tag=networking&order=newest#top
                     └──────┬──────┘    └───────────────┬────────┘└───────────┬─────────────┘ └┬─┘
                       DomainName                      Path(Full)           Query             Fragment
                                        └───┬───┘└──┬──┘└──┬─────┘──┬─┘         ┬
                                        Directory Alias   Schema  Path      Params
    └────────────────────────┬─────────┘─ ─ ┘    Module Name
                          HomeURL
}

{

    Application
     Document

┌──────┬──────────────────────────────────────┐  ─┐
│>Logo │ Brand NavBar                        =│   ├─ Header
├──────┴──────────────────────────────────────┤  ─│
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
│            │ │                   │ │ Card │ │   │
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

Good example:
  https://bootstrapmade.com/demo/templates/NiceAdmin/index.html
}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Contnrs, Variants, Types, RTTI,
  {$ifdef FPC}
  resource, //* for RT_RCDATA
  {$endif}
  syncobjs, mnDON, mnJSON,
  mnUtils, mnClasses, mnStreams, mnLogs, mnMIME, mnParams,
  mnMultipartData, mnModules, mnWebModules;

{.$define rtti_objects}

const
  cIndentSpaces = 2;

type
{$ifdef FPC}
  THandle = Cardinal;
{$else}
  THandle = Int64;
{$endif}

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwElementRenderer = class;
  TmnwRendererClass = class of TmnwRenderer;

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
    function HaveSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function SetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function UnsetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    procedure Append(AAttributes: TmnwAttributes);
  end;

  TDirection = (dirUnkown, dirLeftToRight, dirRightToLeft);

  TLocationRelative = (
    toNone,
    toElement,
    toSchema,
    toHome,
    toCustom
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

  { TElementClasses }

  TElementClasses = record
    Items: TArray<String>;
    function Find(const Name: string): Integer;
    function Add(const Name: string): Integer;
    procedure AddClasses(const S: string);
    function ToString: string;
    class operator Add(A: TElementClasses; B: string): TElementClasses;
    class operator Subtract(A: TElementClasses; B: string): TElementClasses;
    class operator Explicit(const Source: string): TElementClasses;
    class operator Implicit(Source : string) : TElementClasses;
    class operator Implicit(Source : TElementClasses): string;
    procedure Init(classes: string = '');
  end;

  { TmnwScope }

  TmnwScope = record
    Element: TmnwElement;
    Attributes: TmnwAttributes;
    Classes: TElementClasses;
    function ToString: string;
    function GetText: string;
  end;

  TmnwContext = record
    Sender: TObject;
    Schema: TmnwSchema;
    Element: TmnwElement;
    Renderer: TmnwRenderer;

    SessionID: string;
    Stamp: string; //IfNone-Match
    Route: string;

    ParentRenderer: TmnwElementRenderer;
    Writer: TmnwWriter;
    Data: TmnMultipartData;
  end;

  TmnwObject = class(TmnNamedObject);

  TmnwLibrary = class abstract(TmnNamedObject)
  private
    FUsage: Integer;
  protected
  public
    IsLocal: Boolean;
    procedure AddHead(const Context: TmnwContext); virtual; abstract;
    procedure IncUsage;
    procedure DecUsage;
    property Usage: Integer read FUsage;
  end;

  TmnwCustomLibrary = class(TmnwLibrary)
  public
    Source: string;
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TmnwLibraryClass = class of TmnwLibrary;

  { TmnwLibraries }

  TmnwLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  public
    Local: Boolean; //* when find lib, find local first
    procedure Use(ALibrary: TmnwLibrary); overload;
    procedure Use(ALibraryName: string); overload;
    function Find(ALibrary: string; OnlyLocal: Boolean = False): TmnwLibrary; overload;
    procedure RegisterLibrary(ALibraryName: string; IsLocal: Boolean; ALibraryClass: TmnwLibraryClass); overload;
    procedure RegisterLibrary(ALibraryName: string; IsLocal: Boolean; Source: string); overload;
  end;

  TJQuery_Library = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  { TJQuery_LocalLibrary }

  TJQuery_LocalLibrary = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  { TWebElements_Library }

  TWebElements_Library = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TElementExecute = reference to procedure;

  TmnwRequestState = (rsBeforeRequest, rsAfterRequest);

  TmnwElementState = set of (
    estComposing,
    estComposed
  );

  TmnwElementKind = set of(
//    elRender,
    elEmbed, //* created by parent
    elInternal, //* do not render we will call it manually
    elFallback //* if no child have the route name, it take the respond if have a name
  );

  TmnwPriority = (priorityNormal, priorityStart, priorityEnd);

  TmnwAlign = (alignDefault, alignStart, alignCenter, alignStreach, alignEnd);
  TmnwFixed= (fixedDefault, fixedTop, fixedBottom);

  TActionProc = reference to procedure (const AContext: TmnwContext; AResponse: TmnwResponse);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FHandle: THandle;
    FStyle: String;
    FVisible: Boolean;
    FRenderIt: Boolean;
    FSchema: TmnwSchema;
    FParent: TmnwElement;

    FRoute: String;
    FComment: String;
    FID: String;
    FName: String;
    FElementClass: String;
    FAttributes: TmnwAttributes;
    FKind: TmnwElementKind;
    FPriority: TmnwPriority;
    FState: TmnwElementState;
    FOnExecute: TElementExecute;
    FOnAction: TActionProc;
    FPrepared: Boolean;
    FIsRoot: Boolean;
    FTimeStamp: Int64;
    procedure SetState(const AValue: TmnwElementState);
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;

    procedure DoPrepare; virtual;
    procedure DoCompose; virtual;
    procedure DoComposed; virtual;
    procedure DoRespondHeader(AContext: TmnwContext); virtual;
    procedure DoAction(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); virtual;

    procedure DoExecute; virtual;
    procedure Execute;
    procedure DoChanged; virtual;
    procedure Changed;
    procedure Prepare;
    procedure SendMessage(AMessage: string); overload;
    procedure SendMessage(JSON: TDON_Pair); overload; virtual;
    procedure ReceiveMessage(JSON: TDON_Pair); virtual;
    function GenHandle: Integer;
    function GenID: string;
    function GenRoute: string;
    function GenName: string;
  public
    constructor Create(AParent: TmnwElement; AKind: TmnwElementKind = []; ARenderIt: Boolean = True); virtual;
    destructor Destroy; override;

    class function ClassLevel: Integer;

    procedure Add(O: TmnwElement); overload;
    function Add<O: TmnwElement>(const AID: String = ''; const AName: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function FindByRoute(const Route: string): TmnwElement;
    function FindByID(const aID: string): TmnwElement;
    function FindByName(const aName: string): TmnwElement;
    function FindParentName(const aName: string): TmnwElement;
    function FindParentID(const aID: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; virtual; //I wish i have templates/meta programming in pascal
    property Schema: TmnwSchema read FSchema;
    property Parent: TmnwElement read FParent;

    function GetPath(Full: Boolean = True): string; virtual;

    function CreateRender(const Context: TmnwContext): TmnwElementRenderer;
    procedure Compose; virtual;
    procedure AddState(AState: TmnwElementState);
    procedure RemoveState(AState: TmnwElementState);

    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif} //* see TmnObjectList

    function GetContentType(Route: string): string; virtual;


    procedure Action(const AContext: TmnwContext; AResponse: TmnwResponse);
    procedure Respond(const AContext: TmnwContext; AResponse: TmnwResponse);

    //* Original Render
    procedure Render(const Context: TmnwContext; AResponse: TmnwResponse); overload;

    function CanRender: Boolean; virtual;

    //* This will just prepare to Render(Context)

    property Route: String read FRoute write FRoute; //TODO change it to Alias
    property Name: String read FName write FName;
    property Style: String read FStyle write FStyle; //* no, it is not css style
    property ElementClass: String read FElementClass write FElementClass;
    property ID: String read FID write FID;
    property Comment: String read FComment write FComment;
    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;

    property RenderIt: Boolean read FRenderIt write FRenderIt;
    property IsRoot: Boolean read FIsRoot write FIsRoot;


    property Attributes: TmnwAttributes read FAttributes;
    property Kind: TmnwElementKind read FKind write FKind;
    property Priority: TmnwPriority read FPriority write FPriority;
    property State: TmnwElementState read FState write SetState;

    property OnExecute: TElementExecute read FOnExecute write FOnExecute;
    property OnAction: TActionProc read FOnAction write FOnAction;
    property Handle: THandle read FHandle;
    property TimeStamp: Int64 read FTimeStamp;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (woEndLine, woOpenIndent, woCloseIndent);

  TmnwWriter = class(TmnNamedObject)
  private
    Level: Integer;
    NewLine: Boolean;
    FStream: TmnBufferStream;
  public
    Compact: Boolean;
    constructor Create(AName: string; AStream: TmnBufferStream);
    procedure Write(S: string; Options: TmnwWriterOptions = []); virtual;
    procedure WriteLn(const S: string = ''; Options: TmnwWriterOptions = []);
    procedure WriteLines(const S: string = ''; Options: TmnwWriterOptions = []);
    function WriteStream(AStream: TStream; Count: TFileSize = 0): TFileSize; overload; inline;
    property Stream: TmnBufferStream read FStream write FStream;
  end;

  { TmnwOutput }

  TmnwOutput = class(TmnNamedObjectList<TmnwWriter>)
  private
  public
    procedure Write(const Target, S: string; Options: TmnwWriterOptions = []); overload;
    procedure WriteLn(const Target, S: string; Options: TmnwWriterOptions = []); overload;
    property Item; default;
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

  TmnwAttachment = class(TObject)
  private
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
  end;

  { TmnwAttachments }

  TmnwAttachments = class(TmnObjectList<TmnwAttachment>)
  private
    FLock: TCriticalSection;
  protected
    procedure Created; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;
    procedure SendMessage(const Message: string);
    procedure Add(AAttachment: TmnwAttachment);
    procedure Remove(AAttachment: TmnwAttachment);
    property Lock: TCriticalSection read FLock;
  end;

  TmnwSchamaCapability = (
    schemaSession,
    schemaDynamic,  //* dynamic, do not add it to the list, not cached, becareful
    schemaSessions
  );

  TmnwSchemaCapabilities = set of TmnwSchamaCapability;

  TmnwApp = class;
  TUIWebModule = class;

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
    FLock: TCriticalSection;
    FApp: TmnwApp;
    FPhase: TmnwSchemaPhase;
    FNamingLastNumber: THandle;
    function GetReleased: Boolean;
  protected
    Usage: Integer;
    procedure UpdateAttached;
    procedure DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse); override;
    procedure DoAccept(var Resume: Boolean); virtual;
    procedure ProcessMessage(const s: string);
  public
    LastAccess: TDateTime;
    IsManual: Boolean;
    Direction: TDirection;
    RefreshInterval: Integer; //* in seconds, for refresh elements that need auto refresh
    HomePath: string;
    ServeFiles: Boolean;
    AllowIndex: Boolean;
    SessionID: string;
    Interactive: Boolean;
    constructor Create(AName:string; ARoute: string = ''); reintroduce;
    destructor Destroy; override;

    class function GetCapabilities: TmnwSchemaCapabilities; virtual;
    function NewHandle: THandle;

    //* Attaching cap
    //function Interactive: Boolean;

    function Accept: Boolean;
    procedure Compose; override;

    // Executed from a thread of connection of WebSocket, it stay inside until the disconnect or terminate
    procedure Attach(Route: string; Sender: TObject; AStream: TmnBufferStream); // in connection thread

    property Attachments: TmnwAttachments read FAttachments;
    property Attached: Boolean read FAttached;
    property Released: Boolean read GetReleased;
    property Phase: TmnwSchemaPhase read FPhase;
    property Lock: TCriticalSection read FLock;
    property App: TmnwApp read FApp;
  public
    type

    TFileOptions = set of (ftEmbed, ftResource);

    { TFile }

    //* For resource Use FileName := 'myfile.js' but the resource name will took as myfile only, extention will be for mime

    { TElement }

    TElement = class(TmnwElement) //nothing
    public
      constructor Create(AParent: TmnwElement; ARoute: string); reintroduce;
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

  TmnwRendererRegister = class;

  { TmnwElementRenderer }

  TmnwElementRenderer = class(TObject)
  private
    FRenderer: TmnwRenderer;
    FRendererRegister: TmnwRendererRegister;
  protected
    procedure DoCollectAttributes(var Scope: TmnwScope); virtual;
    //* This called once from the TmnwRenderer
    procedure DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer); virtual;
    procedure Prepare(AElement: TmnwElement; ARenderer: TmnwRenderer);

    procedure RenderChilds(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);

    //* Called to parent to wrap the child rendering, each chiled will wrap it with this render
    //* This method exists in parent render
    procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Called only if have parent but exists in a child
    procedure DoEnterOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Content render
    procedure DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); virtual;
    procedure DoAfterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    property Renderer: TmnwRenderer read FRenderer;
    property RendererRegister: TmnwRendererRegister read FRendererRegister;
  public
    procedure Render(AElement: TmnwElement; const Context: TmnwContext; AResponse: TmnwResponse);
    constructor Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwRendererRegister); virtual; //useful for creating it by RendererClass.Create
    procedure CollectAttributes(var Scope: TmnwScope);
  end;

  TmnwElementRendererClass = class of TmnwElementRenderer;

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

    class constructor RegisterObjects;
  public
    type
      TmnwRegisterHow = (None, Replace, Extend);
    constructor Create(AModule: TmodWebModule; IsLocal: Boolean); virtual;
    destructor Destroy; override;
    class destructor Destroy;

    procedure BeginRender;
    procedure EndRender;

    class procedure RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; How: TmnwRegisterHow = None);
    function CreateRenderer(AElementClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property Params: TmnwAttributes read FParams;
    property Libraries: TmnwLibraries read FLibraries;
    property Module: TmodWebModule read FModule;

    procedure AddHead(const Context: TmnwContext); virtual; abstract;
  public
    RendererID: Integer;
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

  { TmnwApp }

  TmnwApp = class(TmnObjectList<TmnwSchema>)
  private
    FRegistered: TRegisteredSchemas;
    FHomePath: string;
    FAppPath: string;
    FAssets: TAssetsSchema;
    FLock: TCriticalSection;
    FSessionTimeout: Integer;
    FShutdown: Boolean;
    FWorkPath: string;
  protected
    procedure SchemaCreated(Schema: TmnwSchema); virtual;
    procedure Created; override;
  public
    IsSSL: Boolean;
    Domain: string; //localhost
    Port: string;
    Directory: string;
    Alias: string; //ModuleName
    CompactMode: Boolean;
    IsLocal: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;

    procedure RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass);
    property Registered: TRegisteredSchemas read FRegistered;

    function FindBy(const aSchemaName: string; const aSessionID: string): TmnwSchema;
    function CreateSchema(const aSchemaName: string): TmnwSchema;
    function ReleaseSchema(const aSchemaName: string; aSessionID: string): TmnwSchema;
    function GetElement(var AContext: TmnwContext; out Schema: TmnwSchema; out Element: TmnwElement): Boolean;

    //for HTML
    procedure Respond(var AContext: TmnwContext; AResponse: TmnwResponse);
    //for WebSocket
    function Attach(const AContext: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;

    function GetPath: string; virtual;
    function GetHostURL: string; virtual;
    function GetHomeURL: string; virtual;
    //Relative
    function GetAssetsURL: string; virtual;

    property Lock: TCriticalSection read FLock;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout; //in seconds
    property Assets: TAssetsSchema read FAssets;
    property HomePath: string read FHomePath write FHomePath;
    property WorkPath: string read FWorkPath write FWorkPath;
    property AppPath: string read FAppPath write FAppPath;
    property Shutdown: Boolean read FShutdown;
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

  { TmnwHTMLWriterHelper }

  TmnwHTMLWriterHelper = class helper for TmnwWriter
  public
    procedure OpenTag(const Tag: string); overload;
    procedure OpenTag(const TagName, TagAttributes: string; TagText: string = ''); overload;
    procedure OpenInlineTag(const TagName:string; TagAttributes: string = ''; TagText: string = ''); overload; // keep inline
    procedure CloseTag(const Tag: string);
    procedure AddShortTag(const TagName:string; TagAttributes: string = ''); overload; //* Self closed tag, without </tagname>
    procedure AddInlineShortTag(const TagName:string; TagAttributes: string = ''); overload; //* Self closed tag, without </tagname>
    procedure AddTag(const TagName, TagAttributes: string); overload;
    procedure AddTag(const TagName, TagAttributes, Value: string); overload;
    procedure AddInlineTag(const TagName, TagAttributes, Value: string); overload;
    procedure ReadFromFile(FileName: string);
  end;

  { THTML }

  THTML =class(TmnwSchema)
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
      TButtons = class;
      TBody = class;
      TDocument = class;

      { THTMLElement }

      THTMLElement = class abstract(TmnwElement)
      protected
      public
      end;

      THTMLLayout = class abstract(THTMLElement)
      public
        Anchor: Boolean;
        Align: TmnwAlign;
        Fixed: TmnwFixed;
        Margin: Integer;
        Padding: Integer;
      end;

      { THTMLComponent }

      THTMLComponent = class abstract(THTMLLayout)
      protected
        procedure Created; override;
      public
        Size: TSize; //Max Width
        Shadow: Boolean;
      end;

      THTMLControl = class abstract(THTMLComponent)
      public
        Hint: string;
      end;

      { TComment }

      TComment = class(THTMLElement)
      public
        Comment: string;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
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
        HomePath: string;
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
        Theme: string;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKind =[]; ARenderIt: Boolean =True); override;
        destructor Destroy; override;
        property Header: THeader read FHeader;
        property SideBar: TSideBar read FSideBar;
        property Main: TMain read FMain;
        property Footer: TFooter read FFooter;
        property Toast: TToast read FToast;
        property Wide: Boolean read GetWide write SetWide;
      end;

      { TNavBar }

      [TID_Extension]
      TNavBar = class(THTMLComponent)
      private
        FButtons: THTMLElement;
      public
        Title: string;
//        LogoImage: string;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKind =[]; ARenderIt: Boolean =True); override;
        destructor Destroy; override;
        property Buttons: THTMLElement read FButtons;
      end;

      THeader = class(THTMLComponent)
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
        Wide: Boolean;
      public
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
      TSideBar = class(THTMLComponent)
      protected
        procedure Created; override;
      public
        function CanRender: Boolean; override;
      end;

      TMain = class(THTMLElement)
      protected
        procedure Created; override;
      public
        Margin: Integer;
        Padding: Integer;
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

      TClickType = (clickNavigate, clickAction, clickNone);

      { TClickable }

      TClickable = class abstract(THTMLControl)
      private
        FCaption: string;
        procedure SetCaption(const AValue: string);
      protected
        procedure ReceiveMessage(JSON: TDON_Pair); override;
      public
        ClickType: TClickType;
        property Caption: string read FCaption write SetCaption;
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
        Caption: string;
        Expanded: Boolean;
      end;

      TAccordionItem = class(TClickable)
      public
      end;

      [TID_Extension]
      THTMLCaptionComponent =class abstract(THTMLComponent)
      public
        Caption: string;
      end;

      { TCard }

      TCard = class(THTMLCaptionComponent)
      protected
        procedure Created; override;
      public
        Collapse: Boolean;
      end;

      TPanel = class(THTMLCaptionComponent)
      public
      end;

      TBar = class(THTMLLayout)
      public
      end;

      [TID_Extension]
      TCollapseCaption = class(THTMLCaptionComponent)
      protected
        procedure DoCompose; override;
      public
      end;

      TList = class(THTMLControl)
      public
      end;

      TViewItem = class(THTMLCaptionComponent)
      public
      end;

      TThemeModeButton = class(THTMLCaptionComponent)
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
        procedure DoAction(const AContext: TmnwContext; AResponse: TmnwResponse); override;
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
      end;

      { TLink }

			TLink = class(TClickable)
      public
        Location: string;
        constructor Create(AParent: TmnwElement; const ALocation: string; ACaption: string = ''); reintroduce;
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

      { TButtons }

      TButtons = class(THTMLLayout)
      protected
        procedure Added(Item: TmnwElement); override;
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

      [TID_Extension]
      TImage = class(THTMLComponent)
      protected
        procedure DoCompose; override;
      public
        Source: TLocation;
        AltText: string;
        //Width, Height: double;
      end;

      { TMemoryImage }

      [TID_Extension]
      TMemoryImage = class(TImage)
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
    function GetContentType(Route: string): string; override;
    property Document: TDocument read FDocument;
  end;

  { TmnwHTMLRenderer }

  TmnwHTMLRenderer = class(TmnwRenderer)
  protected
  public
  type

      { TElement }

      THTMLElement = class abstract(TmnwElementRenderer)
      protected
        procedure AddHead(const Scope: TmnwScope; const Context: TmnwContext); virtual;
        procedure DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext); override;
      end;

      { THTMLComponent }

      THTMLComponent = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
      end;

      { THTMLControl }

      THTMLControl = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
      end;

      { TComment }

      TComment = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TDocument }

      TDocument = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBody }

      TBody = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TFile }

      TFile = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCSSFile }

      TCSSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TDynamicCompose }

      TDynamicCompose = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TIntervalCompose }

      TIntervalCompose = class(TDynamicCompose)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
      end;

      { THeader }

      THeader = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TNavBar }

      TNavBar = class(THTMLComponent)
      private
      protected
        procedure DoRenderBrand(Scope: TmnwScope; Context: TmnwContext); virtual;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      public
      end;

      { TMenuBar }

      TMenuBar = class(THTMLComponent)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TLink }

      TLink = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSpan }

      TSpan = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TFooter }

      TFooter = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TToast }

      TToast = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TContent }

      TContent = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSideBar }

      TSideBar = class(THTMLComponent)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TMain }

      TMain = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TRow }

      TRow = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TColumn }

      TColumn = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordion }

      TAccordion = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordionSection }

      TAccordionSection = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TAccordionItem }

      TAccordionItem = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCard }

      TCard = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TPanel = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBar }

      TBar = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TCollapseCaption }

      TCollapseCaption = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TThemeModeButton }

      TThemeModeButton = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TForm }

      TForm = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TParagraph }

      TParagraph = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TBreak }

      TBreak = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TButton }

      TButton = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TNavItem }

      TNavItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TMenuItem }

      TMenuItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TSubbMenu }

      TSubMenu = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TInput }

      TInput = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      TInputPassword = class(TInput)
      end;

      { TImage }

      TImage = class(THTMLComponent)
      protected
        procedure DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer); override;
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

      { TMemoryImage }

      TMemoryImage = class(THTMLComponent)
      protected
        procedure DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer); override;
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse); override;
      end;

  protected
    procedure Created; override;
    class constructor RegisterObjects;
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  { TmnwRendererRegister }

  TmnwRendererRegister = class(TObject)
  public
    ElementClass: TmnwElementClass;
    RendererClass: TmnwElementRendererClass;
    Renderers: array of TmnwElementRendererClass;
    Extensions: TClassList;
    Level: Integer;
    constructor Create;
    destructor Destroy; override;
  end;

  { TmnwElementRenderers }

  TmnwElementRenderers = class(TmnObjectList<TmnwRendererRegister>)
  protected
    function Compare(Item1, Item2: TmnwRendererRegister): Integer; override;
  public
    Sorted: Boolean;
    procedure QuickSort; override;
    function Find(AElementClass: TmnwElementClass; Nearst: Boolean = False): TmnwRendererRegister;
  end;

  TmnwResponse = class(TmodHttpRespond)
  private
    FResume: Boolean;
  public
    SessionID: string;
    Location: string; //* New location to forward
    property Resume: Boolean read FResume write FResume;
  end;

  { TUIWebCommand }

  TUIWebCommand = class(TmodHttpCommand)
  private
    function GetModule: TUIWebModule;
    function GetRespond: TmnwResponse;
  protected
    function CreateRespond: TmodRespond; override;
  public
    RendererID: Integer;
    procedure RespondResult(var Result: TmodRespondResult); override;
    property Module: TUIWebModule read GetModule;
    property Respond: TmnwResponse read GetRespond;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TmnwSchema)
  private
  protected
    FLogo: THTML.TMemory;
    procedure DoPrepare; override;
    procedure DoCompose; override;
    procedure Created; override;
  public
    property Logo: THTML.TMemory read FLogo;
  end;

  { TUIWebModule }

  TUIWebModule = class(TmodWebModule)
  private
    FWebApp: TmnwApp;
  protected
    function CreateRenderer: TmnwRenderer; virtual;
    procedure CreateItems; override;
    procedure DoPrepareRequest(ARequest: TmodRequest); override;
    procedure Created; override;
    procedure Start; override;
    procedure Stop; override;
  public
    destructor Destroy; override;
    constructor Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules =nil); override;
    property WebApp: TmnwApp read FWebApp;
  end;

function LevelStr(vLevel: Integer): String;

{$ifdef RTTI_OBJECTS}
type
  TCacheClassObject = class(TObject)
  public
    ObjectClass: TClass;
  end;

  { TmnwRendererRegisters }

  TCacheClassObjects = class(TmnObjectList<TCacheClassObject>)
  public
    procedure AddClass(ObjectClass: TClass);
  end;

var
  CacheClassObjects: TCacheClassObjects = nil;

procedure CacheClasses;
{$endif}

//* You need to compile it by brcc32 mnWebElements.rc or wait another 100 years till Delphi/FPC auto compile it
{$R 'mnWebElements.res' 'mnWebElements.rc'}

const
  woFullTag = [woOpenIndent, woCloseIndent];

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSContentAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
function DirectionToStr(Direction: TDirection): string;
function GetTimeStamp: Int64;

implementation

function GetTimeStamp: Int64;
var
  t: Double absolute Result;
begin
  t := Now;
end;

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  if Align = alignStart then
    Result := 'align-self-start'

  else if Align = alignCenter then
    Result := 'align-self-center'
  else if Align = alignStreach then
    Result := 'align-self-streach'
  else if Align = alignEnd then
    Result := 'align-self-end'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSContentAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  if Align = alignStart then
    Result := 'justify-content-start'
  else if Align = alignCenter then
    Result := 'justify-content-center'
  else if Align = alignStreach then
    Result := 'justify-content-streach'
  else if Align = alignEnd then
    Result := 'justify-content-end'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
begin
  if Fixed = fixedTop then
    Result := 'fixed-top'
  else if Fixed = fixedBottom then
    Result := 'fixed-bottom'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
begin
  case Size of
    szUndefined: Result := '';
	  szVerySmall: Result := 'xs';
		szSmall: Result := 'sm';
		szNormal: Result := 'md';
		szLarge: Result := 'lg';
		szVeryLarge: Result := 'xl';
    else
      Result := '';
  end;
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

procedure NewID(Element: TmnwElement);
var
  s: string;
  p: Integer;
begin
  if Element.ID = '' then
  begin
    s := Element.ClassName;
    p := ReversePos('.', s);
    if p > 0 then
      s := Copy(s, p + 2, MaxInt) //* skip T
    else
      s := Copy(s, 2, MaxInt); //* skip T
    Element.ID := LowerCase(s + '-' + Element.GenHandle.ToString);
  end;
end;

procedure NewName(Element: TmnwElement; AddNumber: Boolean = True);
var
  s: string;
  p: Integer;
begin
  if Element.Name = '' then
  begin
    s := Element.ClassName;
    p := ReversePos('.', s);
    if p > 0 then
      s := LowerCase(Copy(s, p + 2, MaxInt)) //* skip T
    else
      s := LowerCase(Copy(s, 2, MaxInt)); //* skip T
    if AddNumber then
      Element.Name := s + '-' + Element.GenHandle.ToString
    else
      Element.Name := s;
  end;
end;

procedure NewRoute(Element: TmnwElement); inline;
var
  s: string;
  p: Integer;
begin
  if Element.Route = '' then
  begin
    s := Element.ClassName;
    p := ReversePos('.', s);
    if p > 0 then
      s := Copy(s, p + 2, MaxInt) //* skip T
    else
      s := Copy(s, 2, MaxInt); //* skip T
    Element.Route := LowerCase(s + '-' + Element.GenHandle.ToString);
  end;
end;

var
  //*Should be by base class categoried
  ElementRenderers: TmnwElementRenderers = nil;

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

{ TmnwOutput }

procedure TmnwOutput.Write(const Target, S: string; Options: TmnwWriterOptions);
var
  Writer: TmnwWriter;
begin
  Writer := Find(Target);
  if Writer <> nil then
    Writer.Write(S, Options);
end;

procedure TmnwOutput.WriteLn(const Target, S: string; Options: TmnwWriterOptions);
begin
  Write(Target, S, Options + [woEndLine]);
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
      if s.StartsWith('{') then
        Schema.ProcessMessage(s)
      else if s = 'attach' then
        Stream.WriteUTF8Line('attached')
      else
        Schema.ProcessMessage(s);
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
var
  Attachment: TmnwAttachment;
begin
  Lock.Enter;
  try
    for Attachment in Self do
    begin
      Attachment.SendMessage(Message);
    end;
  finally
    Lock.Leave;
  end;
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
begin
  idItem := IndexOfName('id');
  if (idItem > 0) then
    Move(idItem, 0);

  Result := '';
  for a in Self do
  begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + a.Name + '='+ QuoteStr(a.Value, '"');
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

{ TmnwElementRenderer }

procedure TmnwElementRenderer.RenderChilds(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  o: TmnwElement;
begin
  Context.ParentRenderer := Self;
  for o in Scope.Element do
    if (priorityStart = o.Priority) and not (elInternal in o.Kind) then
        o.Render(Context, AResponse);

  for o in Scope.Element do
    if (priorityNormal = o.Priority) and not (elInternal in o.Kind) then
        o.Render(Context, AResponse);

  for o in Scope.Element do
    if (priorityEnd = o.Priority) and not (elInternal in o.Kind) then
        o.Render(Context, AResponse);
end;

procedure TmnwElementRenderer.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  RenderChilds(Scope, Context, AResponse);
end;

procedure TmnwElementRenderer.DoAfterRender(Scope: TmnwScope; const Context: TmnwContext);
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

procedure TmnwElementRenderer.DoCollectAttributes(var Scope: TmnwScope);
begin
end;

procedure TmnwElementRenderer.DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer);
begin
end;

procedure TmnwElementRenderer.Prepare(AElement: TmnwElement; ARenderer: TmnwRenderer);
var
  o: TmnwElement;
  r: TmnwElementRenderer;
begin
  DoPrepare(AElement, ARenderer);
  for o in AElement do
  begin
    r := ARenderer.CreateRenderer(o);
    try
      r.Prepare(AElement, ARenderer);
    finally
      r.Free;
    end;
  end;
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; const Context: TmnwContext; AResponse: TmnwResponse);
var
  aScope: TmnwScope;
begin
  aScope.Attributes := TmnwAttributes.Create;
  aScope.Element := AElement;
  try
    CollectAttributes(aScope);

    if Context.ParentRenderer <> nil then
      Context.ParentRenderer.DoEnterChildRender(aScope, Context);

    DoEnterInnerRender(aScope, Context);
    DoInnerRender(aScope, Context, AResponse);
    DoAfterRender(aScope, Context);

    if Context.ParentRenderer <> nil then
      Context.ParentRenderer.DoLeaveChildRender(aScope, Context);

  finally
    FreeAndNil(aScope.Attributes);
  end;
end;

constructor TmnwElementRenderer.Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwRendererRegister);
begin
  inherited Create;
  FRenderer := ARenderer;
  FRendererRegister:= ARendererRegister;
end;

procedure TmnwElementRenderer.CollectAttributes(var Scope: TmnwScope);
begin
  Scope.Attributes.Append(Scope.Element.Attributes);
  Scope.Classes := Scope.Element.ElementClass;

  if Scope.Element.ID <> '' then
    Scope.Attributes['id'] := Scope.Element.ID;
  if Scope.Element.Name <> '' then
    Scope.Attributes['name'] := Scope.Element.Name;

  DoCollectAttributes(Scope);
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
    er := CreateRender(Context);
    if er <> nil then
    begin
      try
        try
          er.Render(Self, Context, AResponse);
        except
          on E: Exception do
          begin
            raise Exception.Create('Error in '+ ClassName +': ' + E.Message);
          end;
        end;
      finally
        er.Free;
      end;
    end;
  end;
end;

function TmnwElement.CanRender: Boolean;
begin
  Result := RenderIt;
end;

function TmnwElement.CreateRender(const Context: TmnwContext): TmnwElementRenderer;
begin
  if (Context.Renderer <> nil) then
    Result := Context.Renderer.CreateRenderer(Self)
  else
    Result := nil;
end;

{ TmnwSchema.TmnwElementRenderers }

function TmnwElementRenderers.Compare(Item1, Item2: TmnwRendererRegister): Integer;
begin
  Result := Item2.Level - Item1.Level;
end;

procedure TmnwElementRenderers.QuickSort;
begin
  inherited;
  Sorted := True;
end;

function TmnwElementRenderers.Find(AElementClass: TmnwElementClass; Nearst: Boolean): TmnwRendererRegister;
var
  o: TmnwRendererRegister;
  i: Integer;
begin
  Result := nil;
  for i:= 0 to Count - 1 do
  begin
    o := Items[i];
    if AElementClass = o.ElementClass then
    begin
      Result := o;
      break;
    end
    else if Nearst and AElementClass.InheritsFrom(o.ElementClass) then
    begin
      Result := o;
      break; //* because it sorted down
    end;
  end;
end;

{function TmnwElementRenderers.FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
var
  o: TmnwRendererRegister;
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

{ TmnwApp }

destructor TmnwApp.Destroy;
begin
  inherited;
  FreeAndNil(FRegistered);
  FreeAndNil(FLock);
end;

procedure TmnwApp.Start;
begin
  FAssets.Prepare;
end;

procedure TmnwApp.Stop;
begin
  FShutdown := True;
  Clear;
end;

procedure TmnwApp.RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass);
var
  aSchemaItem: TmnwSchemaItem;
begin
  aSchemaItem := TmnwSchemaItem.Create;
  aSchemaItem.Name := AName;
  aSchemaItem.SchemaClass := SchemaClass;
  Registered.Add(aSchemaItem);
end;

function TmnwApp.FindBy(const aSchemaName: string; const aSessionID: string): TmnwSchema;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, aSchemaName) and (aSessionID = Items[i].SessionID) then
      Result := Items[i];
    if Result <> nil then
      break;
  end;
end;

function TmnwApp.CreateSchema(const aSchemaName: string): TmnwSchema;
var
  SchemaItem: TmnwSchemaItem;
begin
	SchemaItem := Registered.Find(aSchemaName);
  if SchemaItem <> nil then
  begin
    Result := SchemaItem.SchemaClass.Create(SchemaItem.Name, SchemaItem.Name);
    SchemaCreated(Result);
    //Add(SchemaObject); no, when compose it we add it
  end
  else
    Result := nil;
end;

function TmnwApp.ReleaseSchema(const aSchemaName: string; aSessionID: string): TmnwSchema;
var
  aSchema: TmnwSchema;
begin
  Lock.Enter;
  try
    aSchema := FindBy(aSchemaName, aSessionID);
    if aSchema <> nil then
    begin
      Extract(aSchema);
      aSchema.FPhase := scmpReleased;
    end;
  finally
    Lock.Leave
  end;
end;

function TmnwApp.GetElement(var AContext: TmnwContext; out Schema: TmnwSchema; out Element: TmnwElement): Boolean;
var
  aElement: TmnwElement;
  Routes: TStringList;
  i: Integer;
  aSchemaName, aRoute: string;
begin
  Element := nil;
  Schema := nil;
  Result := False;
  Routes := TStringList.Create;
  try
    StrToStrings(AContext.Route, Routes, ['/']);
    if (Routes.Count > 0) then
    begin
      aSchemaName := Routes[0];
      Routes.Delete(0);
      AContext.Route := DeleteSubPath(aSchemaName, AContext.Route);
      Lock.Enter;
      try
        Schema := FindBy(aSchemaName, AContext.SessionID);

        if Schema = nil then
          Schema := CreateSchema(aSchemaName);
        if Schema <> nil then
          Inc(Schema.Usage);
			finally
        Lock.Leave;
      end;
    end
    else
      Schema := nil;

    if Schema = nil then
      Schema := First; //* fallback

    if (Schema <> nil) then
    begin
      if schemaSession in Schema.GetCapabilities then
      begin
        Schema.Lock.Enter;
        try
          Schema.SessionID := AContext.SessionID;
        finally
          Schema.Lock.Leave;
        end;
      end;

      if Schema.Accept then
      begin
        if not (estComposed in Schema.State) then
        begin
          Schema.Lock.Enter;
          try
            try
              Schema.Compose; //Compose
            except
              Schema.Lock.Leave;
              FreeAndNil(Schema);
              raise;
            end;
          finally
            if Schema <> nil then
              Schema.Lock.Leave;
          end;
        end;

        if (estComposed in Schema.State) then
        begin
          aElement := Schema;

          if aElement <> nil then
          begin
            Result := True;
            Element := aElement;
            i := 0;
            while i < Routes.Count do
            begin
              aRoute := Routes[i];
              aElement := aElement.FindByRoute(aRoute);
              if aElement = nil then
              begin
                //if elFallback in Element.Kind then
                Result := False;
                break;
              end
              else
              begin
                AContext.Route := DeleteSubPath(aRoute, AContext.Route);
                Element := aElement;
                Result := True;
              end;
              inc(i);
            end;
          end;
        end;
      end;
    end;
  finally
    Routes.Free;
  end;
end;

procedure TmnwApp.Respond(var AContext: TmnwContext; AResponse: TmnwResponse);

  function SessionCookies(const vData: string): string;
  var
    aDate: TDateTime;
    aPath, aDomain: string;
  begin
    aDate := IncSecond(Now, SessionTimeout);
    aPath := '';

    if vData<>'' then
      Result := Format('%s; Expires=%s; SameSite=None; Domain=%s; Path=/%s; Secure', [vData, FormatHTTPDate(aDate), Domain, aPath.ToLower])
    else
      Result := Format('; max-age=0; SameSite=None; Domain=%s; Path=/%s; Secure', [Domain, aPath.ToLower]);
  end;

var
  aSchema: TmnwSchema;
  aElement: TmnwElement;
begin
  if Shutdown then
    exit;

  try
    GetElement(AContext, aSchema, aElement);
    if aElement <> nil then
    begin
      aContext.Schema := aSchema;
      aContext.Element := aElement;

      AResponse.ContentType := aElement.GetContentType(AContext.Route);

      aElement.Action(AContext, AResponse);

      if AResponse.Location <> '' then
        AResponse.Header['Location'] := AResponse.Location;

      if AResponse.SessionID<>'' then
        AResponse.Cookies.Values['session'] := SessionCookies(AResponse.SessionID);

      //* We will render it now
      if AResponse.Resume then
        aElement.Respond(AContext, AResponse);

      if not (AResponse.IsHeaderSent) then
      begin
        if (AResponse.HttpResult = hrOK) and (AResponse.Resume = False) then
        begin
          AResponse.HttpResult := hrNoContent;
          AResponse.ContentLength := 0;
        end
        else if AResponse.HttpResult = hrNotFound then
        begin
          AResponse.ContentType := 'text/html';
          AContext.Writer.WriteLn('404 Not Found');
        end;

        if not (AResponse.IsHeaderSent) and (AResponse.HttpResult > hrNone) then
          AResponse.SendHeader;
      end;
    end
    else
    begin
      if not (AResponse.IsHeaderSent) then
      begin
        AResponse.HttpResult := hrNotFound;
        AResponse.ContentType := 'text/html';
        AContext.Writer.WriteLn('404 Not Found');
      end;
    end;

    if aSchema <> nil then
    begin
      Lock.Enter;
      try
        aSchema.LastAccess := Now;
        Dec(aSchema.Usage);
        if (aSchema.Usage = 0) and (aSchema.Released) then
          FreeAndNil(aSchema)
        else
        begin
          if aSchema.Phase = scmpNew then
          begin
            aSchema.FPhase := scmpNormal;
            Add(aSchema);
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
        AResponse.HttpResult := hrError;
        AResponse.ContentType := 'text/html';
      end;
      AContext.Writer.WriteLn('Server Error: ' + E.Message);
    end;
    {$else}
      raise;
    {$endif}
  end;
end;

function TmnwApp.Attach(const AContext: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;
var
  Routes: TStringList;
  i: Integer;
  aRoute: string;
  aSchema: TmnwSchema;
begin
  if Shutdown then
    exit(nil);

  aSchema := nil;
  Routes := TStringList.Create;
  try
    i := 0;
    StrToStrings(AContext.Route, Routes, ['/']);
    if (i<Routes.Count) then
    begin
      aRoute := Routes[i];
      inc(i);
      aSchema := FindBy(aRoute, '');
    end;

    if aSchema = nil then
      aSchema := First; //* fallback

    if aSchema <> nil then
    begin
      DeleteSubPath(aRoute, AContext.Route);
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

function TmnwApp.GetPath: string;
begin
  Result := '/' + IncludeURLDelimiter(IncludeURLDelimiter(Directory) + Alias);
end;

procedure TmnwApp.SchemaCreated(Schema: TmnwSchema);
begin
  Schema.FApp := Self;
  if Schema.HomePath = '' then
    Schema.HomePath := HomePath;
end;

procedure TmnwApp.Created;
begin
  inherited;
  RegisterSchema('assets', TAssetsSchema);
  FAssets := CreateSchema('assets') as TAssetsSchema;
  FAssets.FPhase := scmpNormal;
  Add(FAssets);
end;

constructor TmnwApp.Create;
begin
  FLock := TCriticalSection.Create;
  FRegistered := TRegisteredSchemas.Create;
  inherited;
end;

function TmnwApp.GetAssetsURL: string;
begin
  Result := GetPath + 'assets/'; //relative
end;

function TmnwApp.GetHostURL: string;
begin
  Result := IncludeURLDelimiter(ComposeHttpURL(IsSSL, Domain, Port));
end;

function TmnwApp.GetHomeURL: string;
begin
  Result := GetHostURL + IncludeURLDelimiter(IncludeURLDelimiter(Directory) + Alias);
end;

{ TmnwHTMLWriterHelper }

procedure TmnwHTMLWriterHelper.OpenTag(const Tag: string);
begin
  WriteLn('<'+Tag+'>', [woOpenIndent])
end;

procedure TmnwHTMLWriterHelper.OpenTag(const TagName, TagAttributes: string; TagText: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '>' + TagText, [woOpenIndent])
end;

procedure TmnwHTMLWriterHelper.ReadFromFile(FileName: string);
var
  stream: TmnBufferStream;
  s: UTF8String;
begin
  stream := TmnWrapperStream.Create(TFileStream.Create(FileName, fmShareDenyWrite or fmOpenRead), True);
  try
    while not (cloRead in stream.State) do
    begin
        if stream.ReadLine(s) then
        begin
          WriteLn(s);
        end;
    end;
  finally
    stream.Free;
  end;
end;

procedure TmnwHTMLWriterHelper.OpenInlineTag(const TagName: string; TagAttributes: string; TagText: string);
begin
  Write('<'+TagName + ' ' + TagAttributes + '>' + TagText, [woOpenIndent])
end;

procedure TmnwHTMLWriterHelper.CloseTag(const Tag: string);
begin
  WriteLn('</'+Tag+'>', [woCloseIndent])
end;

procedure TmnwHTMLWriterHelper.AddShortTag(const TagName: string; TagAttributes: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwHTMLWriterHelper.AddInlineShortTag(const TagName: string; TagAttributes: string);
begin
  Write('<'+TagName + ' ' + TagAttributes + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwHTMLWriterHelper.AddTag(const TagName, TagAttributes: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '></' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwHTMLWriterHelper.AddTag(const TagName, TagAttributes, Value: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '>' + Value + '</' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwHTMLWriterHelper.AddInlineTag(const TagName, TagAttributes, Value: string);
begin
  Write('<'+TagName + ' ' + TagAttributes + '>' + Value + '</' + TagName + '>', [woOpenIndent, woCloseIndent]);
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

{ THTML }

procedure THTML.Created;
begin
  inherited;
  FDocument := TDocument.Create(Self, [elEmbed], True);
end;

function THTML.GetContentType(Route: string): string;
begin
  if Route = '' then
    Result := inherited GetContentType(Route)
  else
    Result := DocumentToContentType(Route);
end;

{ THTML.THTMLComponent }

procedure THTML.THTMLComponent.Created;
begin
  inherited;
  Margin := 0;
  Padding := 0;
  Size := szUndefined;
end;

{ THTML.THTMLComponent }

procedure TmnwHTMLRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary('JQuery', False, TJQuery_Library);
  Libraries.RegisterLibrary('JQuery', False, TJQuery_LocalLibrary);
  Libraries.RegisterLibrary('WebElements', False, TWebElements_Library);
  Libraries.Use('WebElements');
end;

procedure TmnwHTMLRenderer.AddHead(const Context: TmnwContext);
begin
end;

class constructor TmnwHTMLRenderer.RegisterObjects;
begin
  //RegisterClasses(THTML);
  RegisterRenderer(THTML.TDynamicCompose, TDynamicCompose);
  RegisterRenderer(THTML.TIntervalCompose, TIntervalCompose);
  RegisterRenderer(THTML.TFile, TFile);
  RegisterRenderer(THTML.TJSFile, TJSFile);
  RegisterRenderer(THTML.TCSSFile, TCSSFile);

  RegisterRenderer(THTML.TComment ,TComment);
  RegisterRenderer(THTML.TDocument ,TDocument);
  RegisterRenderer(THTML.TBody ,TBody);
  RegisterRenderer(THTML.TParagraph, TParagraph);
  RegisterRenderer(THTML.TBreak, TBreak);
  RegisterRenderer(THTML.TNavBar, TNavBar);
  RegisterRenderer(THTML.TMenuBar, TMenuBar);
  RegisterRenderer(THTML.THeader, THeader);
  RegisterRenderer(THTML.TContent, TContent);
  RegisterRenderer(THTML.TSideBar, TSideBar);
  RegisterRenderer(THTML.TAccordion, TAccordion);
  RegisterRenderer(THTML.TAccordionSection, TAccordionSection);
  RegisterRenderer(THTML.TAccordionItem, TAccordionItem);
  RegisterRenderer(THTML.TMain, TMain);
  RegisterRenderer(THTML.TFooter, TFooter);
  RegisterRenderer(THTML.TToast, TToast);
  RegisterRenderer(THTML.TLink, TLink);
  RegisterRenderer(THTML.TSpan, TSpan);
  RegisterRenderer(THTML.TButton, TButton);
  RegisterRenderer(THTML.TNavItem, TNavItem);
  RegisterRenderer(THTML.TMenuItem, TMenuItem);
  RegisterRenderer(THTML.TInput, TInput);
  RegisterRenderer(THTML.TInputPassword, TInputPassword);
  RegisterRenderer(THTML.TImage, TImage);
  RegisterRenderer(THTML.TMemoryImage, TMemoryImage);
  RegisterRenderer(THTML.TCard, TCard);
  RegisterRenderer(THTML.TCollapseCaption, TCollapseCaption);
  RegisterRenderer(THTML.TForm, TForm);
  RegisterRenderer(THTML.TRow, TRow);
  RegisterRenderer(THTML.TColumn, TColumn);
  RegisterRenderer(THTML.TPanel, TPanel);
  RegisterRenderer(THTML.TBar, TBar);

  RegisterRenderer(THTML.TThemeModeButton, TThemeModeButton);
end;

{ TmnwHTMLRenderer.THTMLElement }

procedure TmnwHTMLRenderer.THTMLElement.AddHead(const Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwHTMLRenderer.THTMLElement.DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext);
begin
  if Scope.Element.Comment <> '' then
    Context.Writer.WriteLn('<!-- ' + Scope.Element.Comment + ' -->');
  inherited;
end;

{ TmnwHTMLRenderer.THTMLComponent }

procedure TmnwHTMLRenderer.THTMLComponent.DoCollectAttributes(var Scope: TmnwScope);
var
  e: THTML.THTMLComponent;
begin
  e := Scope.Element as THTML.THTMLComponent;
  inherited;
  if e.Shadow then
    Scope.Classes.Add('shadow-thin');
  if e.Margin > 0 then
    Scope.Classes.Add('m-'+e.Margin.ToString);
  if e.Padding > 0 then
    Scope.Classes.Add('p-'+e.Padding.ToString);
  if e.Size > szUndefined then
    Scope.Classes.Add('max-w-'+BSSizeToStr(e.Size));
end;

{ TmnwHTMLRenderer.THTMLControl }

procedure TmnwHTMLRenderer.THTMLControl.DoCollectAttributes(var Scope: TmnwScope);
var
  e: THTML.THTMLControl;
begin
  e := Scope.Element as THTML.THTMLControl;
  if e.Hint <> '' then
  begin
    Scope.Attributes['data-bs-toggle'] := 'tooltip';
    Scope.Attributes['data-bs-placement'] := 'top';
    Scope.Attributes['title'] := e.Hint;
  end;
  inherited;
end;

{ TmnwHTMLRenderer.TComment }

procedure TmnwHTMLRenderer.TComment.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TComment;
begin
  inherited;
  e := Scope.Element as THTML.TComment;
  Context.Writer.WriteLn('<!--' + e.Comment + '-->', [woOpenIndent, woCloseIndent]);
end;

{ TmnwHTMLRenderer.TDocumentHTML }

procedure TmnwHTMLRenderer.TDocument.DoCollectAttributes(var Scope: TmnwScope);
var
  e: THTML.TDocument;
begin
  e := Scope.Element as THTML.TDocument;
  if e.Schema.Direction = dirRightToLeft then
    Scope.Attributes['dir'] := 'rtl'
  else if e.Schema.Direction = dirLeftToRight then
    Scope.Attributes['dir'] := 'ltr';
  Scope.Attributes['lang'] := 'en';
end;

procedure TmnwHTMLRenderer.TDocument.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TDocument;
  aLibrary: TmnwLibrary;
//  o: TmnwElement;
//  r: THTMLElement;
begin
  e := Scope.Element as THTML.TDocument;
  Context.Writer.WriteLn('<!DOCTYPE html>');
  Context.Writer.OpenTag('html', Scope.ToString);
  Context.Writer.OpenTag('head');
  Context.Writer.AddTag('title', '', e.Title);
  Context.Writer.AddShortTag('link', 'rel="shortcut icon" href="#"');
  Context.Writer.AddShortTag('meta', 'charset="UTF-8"');
  Context.Writer.AddShortTag('meta', 'name="viewport" content="width=device-width, initial-scale=1"');
  if e.Parent <> nil then // Only root have head
  begin
    AddHead(Scope, Context);
    for aLibrary in Renderer.Libraries do
    begin
      if aLibrary.Usage > 0 then
        aLibrary.AddHead(Context);
    end;
    (Renderer as TmnwHTMLRenderer).AddHead(Context);
  end;

  //* Collect head from childs
  {for o in Scope.Element do
  begin
    if o is THTML.THTMLElement then
    begin
      r := Renderer.CreateRenderer(o) as THTMLElement;
      try
        r.AddHeader(o, Context);
      finally
        r.free;
      end;
    end;
  end;}
  Context.Writer.CloseTag('head');
  e.Body.Render(Context, AResponse);
  Context.Writer.CloseTag('html');
end;

{ TmnwHTMLRenderer.THeaderHTML }

procedure TmnwHTMLRenderer.THeader.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Scope.Classes.AddClasses('header sticky-top d-flex align-items-center navbar-dark bg-dark py-0 px-1');
  Context.Writer.OpenTag('header', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('header');
end;

{ TmnwHTMLRenderer.TFooterHTML }

procedure TmnwHTMLRenderer.TFooter.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TFooter;
begin
  e := Scope.Element as THTML.TFooter;
  Context.Writer.OpenTag('footer', 'class="text-center"');
  inherited;
  Context.Writer.CloseTag('footer');
end;

{ TmnwHTMLRenderer.TToast }

procedure TmnwHTMLRenderer.TToast.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TToast;
begin
  e := Scope.Element as THTML.TToast;
  Context.Writer.OpenTag('div aria-live="polite" aria-atomic="true"');
  Context.Writer.OpenTag('div id="toast-container" class ="toast-container position-absolute p-3" style="z-index:9;"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TContent }

procedure TmnwHTMLRenderer.TContent.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TContent;
begin
  e := Scope.Element as THTML.TContent;
  if e.Wide then
    Scope.Classes.Add('container-fluid')
  else
    Scope.Classes.Add('container');
  Context.Writer.OpenTag('div', Scope.ToString);
  Context.Writer.OpenTag('div id="content" class="content row"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TMainHTML }

procedure TmnwHTMLRenderer.TMain.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMain;
  classes: TElementClasses;
begin
  e := Scope.Element as THTML.TMain;
  //Context.Writer.OpenTag('div class="row"');
  classes.Init('main');
  if (e.Schema as THTML).Document.Body.Header.CanRender  then
    classes.Add('max-content-height');
  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
    classes.Add('col-md');
  classes.Add('p-0');
  classes.Add('m-0');

  Context.Writer.OpenTag('main', classes.ToString);

  Scope.Classes.Add('main-content');
  if e.Margin > 0 then
    Scope.Classes.Add('m-md-' + e.Margin.ToString);
  if e.Padding > 0 then
    Scope.Classes.Add('p-' + e.Padding.ToString);
  if e.Gap > 0 then
    //Scope.Classes.Add('gap-' + e.Gap.ToString);
    Scope.Classes.Add('m-childs-' + e.Gap.ToString);

  //Scope.Classes.Add('d-flex');
  //Scope.Classes.Add('flex-column');

  //Scope.Classes.Add('flex-wrap');
  Scope.Classes.Add('justify-content-center');
//container-fluid for full width, container not full width
  Context.Writer.OpenTag('div',  Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');

  Context.Writer.CloseTag('main');
end;

{ TmnwHTMLRenderer.TCardHTML }

//https://disjfa.github.io/bootstrap-tricks/card-collapse-tricks/
//https://bootstrapbrain.com/tutorial/bootstrap-accordion-with-plus-minus-icon/

procedure TmnwHTMLRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');
  Scope.Classes.Add(BSFixedToStr(e.Fixed));
  Scope.Classes.Add(BSAlignToStr(e.Align));
  Scope.Classes.Add('shadow-sm');
  if e.Anchor then
    Scope.Classes.Add('mx-auto');

  Context.Writer.OpenTag('div', Scope.ToString);
  if e.Caption <> '' then
  begin
    Context.Writer.OpenTag('h5', 'class="card-header d-flex" id="' + e.id + '-header"');
    Context.Writer.WriteLn(e.Caption);
    if e.Collapse then
    begin
      Context.Writer.Write('<span class="ms-auto my-auto icon-animate icon bi-chevron-down"');
      if e.Collapse then
          Context.Writer.Write(' role="button" data-bs-toggle="collapse" data-bs-target="#'+e.id+'-body" aria-expanded="true" aria-controls="'+e.id+'-body"');
      Context.Writer.WriteLn('></span>');
    end;
    Context.Writer.CloseTag('h5');
  end;

  Context.Writer.OpenTag('div', 'class="card-body collapse show" aria-labelledby="'+e.id+'-header" id="'+e.id+'-body"');
//  collapse
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TFormHTML }

procedure TmnwHTMLRenderer.TForm.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Scope.Classes.Add('form-control');
  inherited;
end;

procedure TmnwHTMLRenderer.TForm.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TmnwHTMLRenderer.TForm.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TForm;
  aPostTo: string;
begin
  e := Scope.Element as THTML.TForm;
  if e.PostTo.Custom <> '' then
    aPostTo := e.PostTo.Custom
  else if e.PostTo.Where = toSchema then
    aPostTo := e.Schema.GetPath
  else if e.PostTo.Where = toElement then
    aPostTo := e.GetPath
  else if e.PostTo.Where = toHome then
    aPostTo := '/';
  Context.Writer.OpenTag('form', 'method="post"'+ NV('action', aPostTo) + ' enctype="multipart/form-data"' + Scope.GetText);
  inherited;
  if e.RedirectTo <> '' then
    Context.Writer.AddShortTag('input', 'type="hidden" name="redirect" value="' + e.RedirectTo);
  Context.Writer.AddShortTag('input', 'type="hidden" name="execute" value="true"');
  Context.Writer.CloseTag('form');

  if e.Submit.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-success" type="submit" form="'+e.ID+'" value="Submit"', e.Submit.Caption);
  if e.Cancel.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-primary" type="cancel" form="'+e.ID+'" value="Cancel"', e.Cancel.Caption);
  if e.Reset.Caption <> '' then
    Context.Writer.AddTag('button', 'class="btn btn-primary" type="reset" form="'+e.ID+'" value="Reset"', e.Reset.Caption);
end;

{ TmnwHTMLRenderer.TParagraphHTML }

procedure TmnwHTMLRenderer.TParagraph.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TParagraph;
begin
  e := Scope.Element as THTML.TParagraph;
  Context.Writer.OpenInlineTag('p', Scope.ToString);
  if e.Text <> '' then
    Context.Writer.Write(e.Text);
  inherited;
  Context.Writer.CloseTag('p');
end;

{ TmnwHTMLRenderer.TBreakHTML }

procedure TmnwHTMLRenderer.TBreak.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('br');
end;

{ TmnwHTMLRenderer.TTButton }

procedure TmnwHTMLRenderer.TButton.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited;
  //Scope.Attributes['type'] := (Scope.Element as THTML.TButton).EditType;
end;

procedure TmnwHTMLRenderer.TButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TButton;
  event: string;
begin
  e := Scope.Element as THTML.TButton;
  Scope.Classes.Add('btn');
  Scope.Classes.Add('btn-primary');
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Context.Writer.AddTag('button', 'type="button"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TmnwHTMLRenderer.TNavItem }

procedure TmnwHTMLRenderer.TNavItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavItem;
  event: string;
begin
  e := Scope.Element as THTML.TNavItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Scope.Classes.Add('nav-link');
  Context.Writer.AddTag('a', 'href="'+e.LinkTo+'"' + event + Scope.GetText, e.Caption);
  inherited;
end;

{ TmnwHTMLRenderer.TMenuItem }

procedure TmnwHTMLRenderer.TMenuItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMenuItem;
  event: string;
begin
  e := Scope.Element as THTML.TMenuItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Context.Writer.AddTag('button', 'role="menu" type="button"' + event + Scope.Attributes.GetText, e.Caption);
  inherited;
end;

{ TmnwHTMLRenderer.TSubbMenu }

procedure TmnwHTMLRenderer.TSubMenu.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited DoCollectAttributes(Scope);
end;

procedure TmnwHTMLRenderer.TSubMenu.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TmnwHTMLRenderer.TInputHTML }

procedure TmnwHTMLRenderer.TInput.DoCollectAttributes(var Scope: TmnwScope);
begin
  Scope.Attributes['placeholder'] := (Scope.Element as THTML.TInput).PlaceHolder;
  Scope.Attributes['type'] := (Scope.Element as THTML.TInput).EditType;
  inherited;
end;

procedure TmnwHTMLRenderer.TInput.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TInput;
  event: string;
  isFormChild: Boolean;
begin
  e := Scope.Element as THTML.TInput;
  isFormChild := True;
  if isFormChild then
    Scope.Classes.Add('form-control');

  if e.Caption <> '' then
    Context.Writer.AddTag('label', When(isFormChild, 'class="form-label"') + ' for="' + e.ID + '"', e.Caption);

  if Context.Schema.Interactive then
    event := ' onchange="mnw.send(' + SQ(e.ID) + ', '+ SQ('change') + ',' + 'this.value' + ')"';

  Context.Writer.AddShortTag('input', event + When(e.Required, 'required') + Scope.GetText); //TODO need to generate less spaces
  if e.HelpText <> '' then
    Context.Writer.AddTag('div', 'class="form-text"', e.HelpText);
  inherited;
end;

{ TmnwHTMLRenderer.TImageHTML }

procedure TmnwHTMLRenderer.TImage.DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer);
begin
  inherited;
  ARenderer.Libraries.Use('JQuery');
end;

procedure TmnwHTMLRenderer.TImage.DoCollectAttributes(var Scope: TmnwScope);
begin
  Scope.Attributes['src'] := (Scope.Element as THTML.TImage).Source;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText; //* always set
  inherited;
end;

procedure TmnwHTMLRenderer.TImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TmnwHTMLRenderer.TMemoryImageHTML }

procedure TmnwHTMLRenderer.TMemoryImage.DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer);
begin
  inherited;
  ARenderer.Libraries.Use('JQuery');
end;

procedure TmnwHTMLRenderer.TMemoryImage.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited;
  Scope.Attributes['src'] := Scope.Element.GetPath;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText;
end;

procedure TmnwHTMLRenderer.TMemoryImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TMemoryImage;
begin
  e := Scope.Element as THTML.TMemoryImage;
  Context.Writer.AddShortTag('img', Scope.ToString);
  inherited;
end;

{ TmnwRendererRegister }

constructor TmnwRendererRegister.Create;
begin
  inherited Create;
  Extensions := TClassList.Create;
end;

destructor TmnwRendererRegister.Destroy;
begin
  FreeAndNil(Extensions);
  inherited;
end;

{ TmnwSchema }

constructor TmnwSchema.Create(AName: string; ARoute: string);
begin
  inherited Create(nil);
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

procedure TmnwSchema.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  fs: TFileStream;
  aFileName: string;
  aHomePath: string;
  files: TStringList;
  s: string;
begin
  inherited;
  if ServeFiles and (AContext.Route <> '') then
  begin
    aHomePath := When(HomePath, App.FHomePath);
    if aHomePath <> '' then
    begin
      if WebExpandFile(aHomePath, AContext.Route, aFileName) then
      begin
        if EndsDelimiter(aFileName) and AllowIndex then
        begin
          AResponse.ContentType := DocumentToContentType('html');
          files := TStringList.Create;
          try
            AContext.Writer.WriteLn('<!DOCTYPE html>');
            AContext.Writer.OpenTag('html');
            AContext.Writer.OpenTag('head');
            AContext.Writer.AddTag('title', '', 'Index of ' + aFileName);
            AContext.Writer.AddTag('style', '', 'body { font-family: monospace; }');
            AContext.Writer.CloseTag('head');
            AContext.Writer.OpenTag('body');
            EnumFiles(files, aFileName, '*.*', [efDirectory]);
            AContext.Writer.AddTag('h1', '', 'Index of ' + AContext.Route);
            AContext.Writer.AddTag('h2', '', 'Folders');
            AContext.Writer.OpenTag('ul', '', '');
            for s in files do
            begin
              if not StartsText('.', s) then
              begin
                AContext.Writer.OpenInlineTag('ui');
                AContext.Writer.AddInlineTag('a', 'href="' + s + '\"', s);
                AContext.Writer.AddInlineShortTag('br');
                AContext.Writer.CloseTag('ui');
              end;
            end;
            AContext.Writer.CloseTag('ul');
            AContext.Writer.AddTag('h2', '', 'Files');
            files.Clear;
            EnumFiles(files, aFileName, '*.*', [efFile]);
            AContext.Writer.OpenTag('ul', '', '');
            for s in files do
            begin
              if not StartsText('.', s) then
              begin
                AContext.Writer.OpenInlineTag('ui');
                AContext.Writer.AddInlineTag('a', 'href="' + s + '"', s);
                AContext.Writer.AddInlineShortTag('br');
                AContext.Writer.CloseTag('ui');
              end;
            end;
            AContext.Writer.CloseTag('ul');
            AContext.Writer.CloseTag('body');
            AContext.Writer.CloseTag('html');
          finally
            files.Free;
          end;
        end
        else if FileExists(aFileName) and not StartsText('.', ExtractFileName(aFileName)) then //no files starts with dots
        begin
          AResponse.ContentType := DocumentToContentType(aFileName);
          fs := TFileStream.Create(aFileName, fmShareDenyWrite or fmOpenRead);
          try
            AContext.Writer.WriteStream(fs, 0);
          finally
            fs.Free;
          end;
        end
        else
          AResponse.HttpResult := hrNotFound;
      end
      else
        AResponse.HttpResult := hrUnauthorized;
    end
    else
      Render(AContext, AResponse);
  end
  else
    Render(AContext, AResponse);
end;

procedure TmnwSchema.DoAccept(var Resume: Boolean);
begin
end;

procedure TmnwSchema.ProcessMessage(const s: string);
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

function TmnwSchema.Accept: Boolean;
begin
  Result := True;
  DoAccept(Result);
end;

{function TmnwSchema.Interactive: Boolean;
begin
  Result := schemaInteractive in GetCapabilities;
end;}

procedure TmnwSchema.Compose;
begin
  AddState([estComposing]);
  inherited;
  RemoveState([estComposing]);
  AddState([estComposed]);
end;

function TmnwSchema.GetReleased: Boolean;
begin
  Result := (FPhase = scmpReleased) or (schemaDynamic in GetCapabilities);
end;

function TmnwSchema.NewHandle: THandle;
begin
  AtomicIncrement(FNamingLastNumber);
  Result := FNamingLastNumber;
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

{ TmnwSchema.TElement }

constructor TmnwSchema.TElement.Create(AParent: TmnwElement; ARoute: string);
begin
  inherited Create(AParent);
  Route := ARoute;
end;

{$ifdef rtti_objects}
procedure TmnwRenderer.RegisterClasses(ASchemaClass: TmnwSchemaClass);
var
  aObjectClass: TCacheClassObject;
  aName, aClassName: string;
begin
  aClassName := ClassName;
  for aObjectClass in CacheClassObjects do
  begin
    aName := SubStr(aObjectClass.ObjectClass.ClassName, '.', 1);
    if (aName = aClassName) then
    begin
    end;
  end;
end;
{$endif}

class procedure TmnwRenderer.RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; How: TmnwRegisterHow);
var
  aRendererRegister: TmnwRendererRegister;
begin
  aRendererRegister := ElementRenderers.Find(AElementClass);
  if aRendererRegister <> nil then
  begin
    if (How = Replace) and (AElementClass.InheritsFrom(aRendererRegister.ElementClass)) then
      aRendererRegister.RendererClass := ARendererClass
    else if (How = Extend) and (AElementClass.InheritsFrom(aRendererRegister.ElementClass)) then
      aRendererRegister.Renderers := aRendererRegister.Renderers + [ARendererClass]
    else
      raise Exception.Create('You can''t re-register same class: '+ AElementClass.ClassName);
  end
  else
  begin
    if (How = Extend) then
      raise Exception.Create('Ops we can''t add extended, we need to optimize code: '+ AElementClass.ClassName);
    aRendererRegister := TmnwRendererRegister.Create;
    aRendererRegister.ElementClass := AElementClass;
    aRendererRegister.RendererClass := ARendererClass;
    aRendererRegister.Level := AElementClass.ClassLevel;
    rttiCollectExtensions(aRendererRegister.ElementClass, aRendererRegister.Extensions);
    ElementRenderers.Add(aRendererRegister);
  end;
end;

function TmnwRenderer.CreateRenderer(AElementClass: TmnwElementClass): TmnwElementRenderer;
var
  aRendererRegister: TmnwRendererRegister;
begin
  aRendererRegister := ElementRenderers.Find(AElementClass, True);
  if aRendererRegister <> nil then
    Result := aRendererRegister.RendererClass.Create(Self, aRendererRegister)
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
  FBody := TBody.Create(Self, [elEmbed, elInternal], True);
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
    SendMessage('"command": "change", "content": ' + DQ(Value));
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

{ THTML.TMemoryImage }

procedure THTML.TMemoryImage.Created;
begin
  inherited;
  FData := TMemoryStream.Create;
end;

destructor THTML.TMemoryImage.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function THTML.TMemoryImage.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

procedure THTML.TMemoryImage.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  Data.Seek(0, soBeginning);
  AContext.Writer.WriteStream(Data, 0);
end;

procedure THTML.TMemoryImage.LoadFromFile(const AFileName: string);
begin
  Data.LoadFromFile(AFileName);
  FileName := ExtractFilePath(ExtractFileName(AFileName));
end;

procedure THTML.TMemoryImage.LoadFromStream(AStream: TStream);
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

function TmnwElement.GetPath(Full: Boolean): string;
begin
  if Self = nil then
    exit('');

  if (Parent <> nil) then
  begin
    if Route <> '' then
      Result := IncludeURLDelimiter(Parent.GetPath(False)) + Route
    else
      Result := Parent.GetPath(False);
  end
  else
    Result := Route;
  if Full then
    Result := IncludeURLDelimiter(Schema.App.GetPath + Result)
  else
    Result := IncludeURLDelimiter(Result);
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
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, Name) then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TmnwElement.FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean): TmnwElement;
var
  o: TmnwElement;
begin
  Result := nil;
  for o in Self do
  begin
    if (o.InheritsFrom(ObjectClass) and (SameText(o.Name, AName))) then
    begin
      Result := o;
      exit;
    end;
  end;
  for o in Self do
  begin
    Result := o.FindObject(ObjectClass, AName);
    if Result <> nil then
      exit;
  end;
  if RaiseException and (Result = nil) then
    raise Exception.Create(ObjectClass.ClassName + ': ' + AName +  ' not exists in ' + Name);
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
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].ID, aID) then
      Result := Items[i]
    else
      Result := Items[i].FindByID(aID);
    if Result <> nil then
      break;
  end;
end;

function TmnwElement.FindByName(const aName: string): TmnwElement;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if SameText(Items[i].Name, aName) then
      Result := Items[i]
    else
      Result := Items[i].FindByID(aName);
    if Result <> nil then
      break;
  end;
end;

function TmnwElement.FindByRoute(const Route: string): TmnwElement;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Route <> '') then
    begin
      if SameText(Items[i].Route, Route) then
        Result := Items[i];
    end
    else
      Result := Items[i].FindByRoute(Route);
    if Result <> nil then
      break;
  end;
  {if (Result = nil) and (Route <> '') then
    Result := Self;}
end;

procedure TmnwElement.DoCompose;
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
  DoChanged;
end;

procedure TmnwElement.Changed;
begin
end;

procedure TmnwElement.Prepare;
begin
  if not FPrepared then
  begin
    DoPrepare;
    FPrepared := True;
  end;
end;

procedure TmnwElement.SendMessage(AMessage: string);
begin
  if Schema <> nil then
    Schema.Attachments.SendMessage('{"element": ' + DQ(ID) + ', ' + AMessage + '}');
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

procedure TmnwElement.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
end;

procedure TmnwElement.DoRespondHeader(AContext: TmnwContext);
begin
end;

procedure TmnwElement.DoAction(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
end;

constructor TmnwElement.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited Create;
  FTimeStamp := GetTimeStamp;
  FEnabled := True;
  FVisible := True;
  FRenderIt := ARenderIt;
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

class function TmnwElement.ClassLevel: Integer;
var
  c: TClass;
begin
  Result := 0;
  c := Self;
  while c <> nil do
  begin
    c := c.ClassParent;
    inc(Result);
  end;
end;

procedure TmnwElement.Add(O: TmnwElement);
begin
  O.FParent := Self;
  O.FSchema := FSchema;
  inherited Add(O);
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
  if vName <> '' then
    for i := 0 to Count - 1 do
    begin
      if SameText(Items[i].Name, vName) then
      begin
        Result := i;
        break;
      end;
    end;
end;

function LevelStr(vLevel: Integer): String;
begin
  Result := StringOfChar(' ', vLevel * cIndentSpaces);
end;

procedure TmnwElement.Respond(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  DoRespond(AContext, AResponse);
end;

procedure TmnwElement.Compose;
var
  o: TmnwElement;
begin
//  Clear; //*Should not clear here
  Prepare;
  DoCompose;
  UpdateElement(Self);
  for o in Self do
  begin
    o.Compose; //Compose
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

procedure TmnwElement.Action(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  AResponse.PutHeader('Content-Type', GetContentType(AContext.Route));
  DoRespondHeader(AContext);
  AResponse.Resume := True;
  DoAction(AContext, AResponse);
  if Assigned(FOnAction) then
    FOnAction(AContext, AResponse);
end;

constructor TmnwWriter.Create(AName: string; AStream: TmnBufferStream);
begin
  inherited Create;
  Name := AName;
  FStream := AStream;
end;

procedure TmnwWriter.Write(S: string; Options: TmnwWriterOptions);
begin
  if (woCloseIndent in Options) and not (woOpenIndent in Options) then
    Dec(Level);

  if not Compact then
  begin
    if (NewLine) then
      S := LevelStr(Level) + S;
  end;

  NewLine := False;

  if (woEndLine in Options) then
  begin
    NewLine := True;
    if not Compact then
    begin
      s := S + sWinEndOfLine;
    end;
  end;

  FStream.WriteUtf8String(S);

  if (woOpenIndent in Options) and not (woCloseIndent in Options) then
    Inc(Level);
end;

procedure TmnwWriter.WriteLn(const S: string; Options: TmnwWriterOptions);
begin
  Write(S, Options + [woEndLine]);
end;

procedure TmnwWriter.WriteLines(const S: string; Options: TmnwWriterOptions);
begin
  Write(S, Options + [woEndLine]); //TODO
end;

function TmnwWriter.WriteStream(AStream: TStream; Count: TFileSize): TFileSize;
begin
  Result := Stream.WriteStream(AStream, Count);
end;

{ TmnwRenderer }

procedure TmnwRenderer.BeginRender;
begin
  DoBeginRender;
end;

constructor TmnwRenderer.Create(AModule: TmodWebModule; IsLocal: Boolean);
{var
  o: TmnwRenderer.TmnwRendererRegister;}
begin
  FLibraries := TmnwLibraries.Create;
  FLibraries.Local := IsLocal;
  inherited Create;
  FModule := AModule;
  FParams := TmnwAttributes.Create;
  //Renderers := TmnwElementRenderers.Create();
{  for o in Renderers do
    log.WriteLn(o.ObjectClass.ClassName);}
  if not ElementRenderers.Sorted then
    ElementRenderers.QuickSort;
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

class destructor TmnwRenderer.Destroy;
begin
end;

procedure TmnwRenderer.EndRender;
begin
  DoEndRender;
end;

procedure TmnwRenderer.DoBeginRender;
begin
end;

procedure TmnwRenderer.DoEndRender;
begin
end;

class constructor TmnwRenderer.RegisterObjects;
begin
  ElementRenderers := TmnwElementRenderers.Create;
  RegisterRenderer(TmnwElement, TmnwElementRenderer);
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
  aStream: TStream;
  aDocSize: Int64;
  aDate: TDateTime;
  aEtag, aFTag: string;
begin
  inherited;
  if ftResource in Options then
  begin
    {$ifdef FPC}
    aStream := TResourceStream.Create(hInstance, ChangeFileExt(FileName, ''), 'RT_RCDATA'); //* remove extension
    {$else}
    aStream := TResourceStream.Create(hInstance, ChangeFileExt(FileName, ''), RT_RCDATA); //* remove extension
    {$endif}
    try
      AContext.Writer.WriteStream(aStream, 0);
    finally
      aStream.Free;
    end;
  end
  else
  begin
    if not FileExists(FileName) then
    begin
      AResponse.HttpResult := hrNotFound;
      AResponse.Resume := False;
      exit;
    end
    else
    begin
      FileAge(FileName, aDate);
      aFtag := DateTimeToUnix(aDate).ToString;
      aEtag := AContext.Stamp;
      if (aEtag<>'') and (aEtag = aFtag) then
      begin
        AResponse.HttpResult := hrNotModified;
        AResponse.Resume := False;
        exit;
      end;

      aStream := TFileStream.Create(FileName, fmShareDenyNone or fmOpenRead);
      try
        aDocSize := aStream.Size;

        AResponse.ETag := aFTag;
        AResponse.Header['Cache-Control']  := 'max-age=600';
        AResponse.Header['Last-Modified']  := FormatHTTPDate(aDate);
        AResponse.Header['Content-Length'] := IntToStr(aDocSize);

        AContext.Writer.WriteStream(aStream, 0);
      finally
        aStream.Free;
      end;
    end;
  end;
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
  if FileDate <> 0 then
    AResponse.Header['Last-Modified']  := FormatHTTPDate(FileDate);
  AResponse.Header['Content-Length'] := IntToStr(Data.Size);
  AResponse.Header['Cache-Control']  := 'public, max-age=3600';
  Data.Seek(0, soBeginning);
  AContext.Writer.WriteStream(Data, 0);
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
  FileName := '';
  FilePath := '';
end;

{ THTML.TAssets }

procedure THTML.TAssets.DoRespond(const AContext: TmnwContext; AResponse: TmnwResponse);
var
  fs: TFileStream;
  aFileName: string;
begin
  inherited;
  if HomePath <> '' then
  begin
    if WebExpandFile(HomePath, AContext.Route, aFileName) then
    begin
      if FileExists(aFileName) then
      begin
        fs := TFileStream.Create(aFileName, fmShareDenyWrite or fmOpenRead);
        try
          AContext.Writer.WriteStream(fs, 0);
        finally
          fs.Free;
        end;
      end;
    end;
  end;
end;

function THTML.TAssets.GetContentType(Route: string): string;
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
    Inner.Compose;

    Inner.Render(AContext, AResponse);
  finally
    Inner.Free;
  end;
end;

procedure THTML.TDynamicCompose.InnerCompose(Inner: TmnwElement; AResponse: TmnwResponse);
begin
end;

{ TmnwLibrary }

procedure TmnwLibrary.DecUsage;
begin
  FUsage := FUsage - 1;
end;

procedure TmnwLibrary.IncUsage;
begin
  FUsage := FUsage + 1;
end;

{ TmnwLibraries }

function TmnwLibraries.Find(ALibrary: string; OnlyLocal: Boolean): TmnwLibrary;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if (SameText(Items[i].Name, ALibrary)) and (not OnlyLocal or Items[i].IsLocal)  then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TmnwLibraries.RegisterLibrary(ALibraryName: string; IsLocal: Boolean; ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := ALibraryClass.Create;
  ALibrary.Name := ALibraryName;
  ALibrary.IsLocal := IsLocal;
  Add(ALibrary);
end;

procedure TmnwLibraries.Use(ALibrary: TmnwLibrary);
begin
  if ALibrary <> nil then
  begin
    ALibrary.IncUsage;
    Move(IndexOf(ALibrary), 0);
  end
  else
    raise Exception.Create('library is nil');
end;

procedure TmnwLibraries.RegisterLibrary(ALibraryName: string; IsLocal: Boolean; Source: string);
var
  lib: TmnwCustomLibrary;
begin
  lib := TmnwCustomLibrary.Create;
  lib.Name := ALibraryName;
  lib.IsLocal := IsLocal;
  lib.Source := Source;
  Add(lib);
end;

procedure TmnwLibraries.Use(ALibraryName: string);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryName, Local);
  if (ALibrary = nil) and Local then
    ALibrary := Find(ALibraryName, False);
  if ALibrary <> nil then
    Use(ALibrary)
  else
    raise Exception.Create('There is no library: ' + ALibraryName);
end;

{ TJQuery_Library }

procedure TJQuery_Library.AddHead(const Context: TmnwContext);
begin
  Context.Writer.AddTag('script', 'src="' + 'https://cdn.jsdelivr.net/npm/jquery@3.7.1/dist/' + 'jquery.min.js" defer crossorigin="anonymous"');
end;

{ TJQuery_LocalLibrary }

procedure TJQuery_LocalLibrary.AddHead(const Context: TmnwContext);
begin
  Context.Writer.AddTag('script', 'src="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'jquery.min.js?v=' + IntToStr(Context.Schema.TimeStamp) + '" defer crossorigin="anonymous"');
end;

{ TWebElements_Library }

procedure TWebElements_Library.AddHead(const Context: TmnwContext);
begin
  Context.Writer.AddTag('script', 'src="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'WebElements.js?v=' + IntToStr(Context.Schema.TimeStamp) + '" defer crossorigin="anonymous"');
  Context.Writer.AddShortTag('link', 'rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'WebElements.css?v=' + IntToStr(Context.Schema.TimeStamp) + '" defer crossorigin="anonymous"');
end;

{ THTML }

{ THTML.TImage }

procedure THTML.TImage.DoCompose;
begin
  inherited;
end;

{ TmnwHTMLRenderer.TFile }

procedure TmnwHTMLRenderer.TFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TFile;
begin
  e := Scope.Element as THTML.TFile;
  if ftEmbed in e.Options then
    Scope.Element.Respond(Context, AResponse);
  inherited;
end;

{ THTML.TBody }

constructor THTML.TBody.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited;
  //This object auto free by parents
  FHeader := THeader.Create(Self, [elEmbed], False);
  FHeader.Priority := priorityStart;

  FContent := TContent.Create(Self, [elEmbed], True);
  with FContent do
  begin
    FSideBar := TSideBar.Create(This, [elEmbed], True);
    FSideBar.Priority := priorityStart;
    FMain := TMain.Create(This, [elEmbed], True);
  end;

  FFooter := TFooter.Create(Self, [elEmbed], False);
  FFooter.Priority := priorityEnd;
  FToast := TToast.Create(Self, [elEmbed], False);
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

constructor THTML.TNavBar.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited;
  FButtons := THTMLElement.Create(This, [elInternal, elEmbed], True);
end;

destructor THTML.TNavBar.Destroy;
begin
  inherited;
end;

{ THTML.THeader }

function THTML.THeader.GetMenuBar: TMenuBar;
begin
  if FMenuBar = nil then
    FMenuBar := TMenuBar.Create(Self, [elEmbed], True);
  Result := FMenuBar;
end;

function THTML.THeader.GetNavBar: TNavBar;
begin
  if FNavBar = nil then
    FNavBar := TNavBar.Create(Self, [elEmbed], True);
  Result := FNavBar;
end;

procedure THTML.THeader.Created;
begin
  inherited;
  Shadow := True;
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
end;

{ TmnwHTMLRenderer.TBody }

procedure TmnwHTMLRenderer.TBody.DoCollectAttributes(var Scope: TmnwScope);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  inherited;
  if e.Schema.RefreshInterval <> 1 then //* not default, 0 Disable it
    Scope.Attributes['data-mnw-refresh-interval'] := e.Schema.RefreshInterval.ToString;
  if e.Theme <> '' then
    Scope.Attributes['data-bs-theme'] := e.Theme;
end;

procedure TmnwHTMLRenderer.TBody.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBody;
  function GetAttach: string;
  begin
    if Context.Schema.Interactive then
    begin
      Result := ' data-mnw-interactive="true"';
    end;
  end;
begin
  e := Scope.Element as THTML.TBody;
  Context.Writer.OpenTag('body', Scope.ToString + GetAttach);
  inherited;
  Context.Writer.CloseTag('body');
end;

{ TmnwHTMLRenderer.TPanel }

procedure TmnwHTMLRenderer.TPanel.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Context.Writer.OpenTag('div', 'class="panel fit-content"');
  if e.Caption <> '' then
    Context.Writer.AddTag('div', 'class="panel-header"', e.Caption);

  Scope.Classes.Add('panel-body');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TBar }

procedure TmnwHTMLRenderer.TBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TBar;
begin
  e := Scope.Element as THTML.TBar;
  Scope.Classes.Add('py-2');
  Scope.Classes.Add('fit-contents');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TCollapseCaption }

procedure TmnwHTMLRenderer.TCollapseCaption.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCollapseCaption;
begin
  e := Scope.Element as THTML.TCollapseCaption;
  Context.Writer.OpenTag('p', 'class="panel d-flex m-0" data-bs-toggle="collapse" role="button" data-bs-target="#'+e.ID+'-text" aria-expanded="false" aria-controls="'+e.ID+'-text"');
  Context.Writer.WriteLn(e.Caption);
  Context.Writer.AddTag('span', 'class="ms-auto p-0 align-bottom icon mw-three-dots"');
  Context.Writer.CloseTag('p');
  Context.Writer.OpenTag('div', 'id="'+e.ID+'-text" class="panel-body m-0 collapse"');
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TThemeModeButton }

procedure TmnwHTMLRenderer.TThemeModeButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TThemeModeButton;
begin
  e := Scope.Element as THTML.TThemeModeButton;
  Context.Writer.OpenTag('button', 'class="bg-dark me-0 ms-0 py-0 px-1 border-0" type="button" aria-label="Toggle navigation" onclick="mnw.switch_theme(event)"');
  Context.Writer.AddTag('span', 'class="invert icon mw-moon-stars"');
  inherited;
  Context.Writer.CloseTag('button');
end;

{ TmnwHTMLRenderer.TRow }

procedure TmnwHTMLRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add(BSContentAlignToStr(e.ContentAlign));
  Context.Writer.OpenTag('div class="row' + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align)+' flex-md-nowrap"');
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TColumn }

procedure TmnwHTMLRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Writer.OpenTag('div', 'class="col-md-'+e.Size.ToString + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align) + '"' + Scope.Attributes.GetText);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TAccordion }

procedure TmnwHTMLRenderer.TAccordion.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
//  Context.Writer.OpenTag('div', 'class="accordion"');
  inherited;
end;

procedure TmnwHTMLRenderer.TAccordion.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  //Context.Writer.CloseTag('div');
end;

procedure TmnwHTMLRenderer.TAccordion.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TAccordion;
begin
  e := Scope.Element as THTML.TAccordion;
  Scope.Classes.Add('accordion');
  Scope.Classes.Add('col');
  Scope.Classes.Add('accordion-flush');
  Context.Writer.OpenTag('div', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TAccordionSection }

procedure TmnwHTMLRenderer.TAccordionSection.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Context.Writer.OpenTag('li', 'class="list-group-item"');
  inherited;
end;

procedure TmnwHTMLRenderer.TAccordionSection.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('li');
end;

procedure TmnwHTMLRenderer.TAccordionSection.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TAccordionSection;
begin
  e := Scope.Element as THTML.TAccordionSection;
  Context.Writer.OpenTag('div', 'class="accordion-item"');
  Context.Writer.OpenTag('h', 'id="'+e.id+'-header" class="accordion-header"');
  Context.Writer.OpenTag('button ', 'class="accordion-button p-2" type="button" data-bs-toggle="collapse" data-bs-target="#' + e.ID + '" aria-expanded="'+When(e.Expanded, 'true', 'false')+'" aria-controls="' + e.ID + '"');
  Context.Writer.WriteLn(e.Caption);
  Context.Writer.CloseTag('button');
  Context.Writer.CloseTag('h');

  Scope.Classes.Add('accordion-collapse');
  Scope.Classes.Add('collapse');
  if e.Expanded then
    Scope.Classes.Add('show');
  if (e.Parent is THTML.TAccordion) and //* Should be
    not (e.Parent as THTML.TAccordion).AlwaysOpen then
      Scope.Attributes.Add('data-bs-parent', '#'+e.Parent.ID);
  Context.Writer.OpenTag('div', Scope.ToString + ' aria-labelledby="' + e.ID + '-header"');
  Context.Writer.OpenTag('ul', 'class="accordion-body list-group list-group-flush p-0"');
  inherited;
  Context.Writer.CloseTag('ul');

  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TAccordionItem }

procedure TmnwHTMLRenderer.TAccordionItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TAccordionItem;
begin
  e := Scope.Element as THTML.TAccordionItem;
  //Scope.Classes.Add('');
  inherited;
end;

{ THTML.TMain }

procedure THTML.TMain.Created;
begin
  inherited;
  Gap := 1;
end;

{ THTML.TCard }

procedure THTML.TCard.Created;
begin
  inherited;
  Shadow := True;
end;

{ THTML.TForm }

procedure THTML.TForm.DoAction(const AContext: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
  if (RedirectTo <> '') and (AResponse.HttpResult = hrNone) then
  begin
    AResponse.HttpResult := hrRedirect;
    AResponse.Location := RedirectTo;
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

{ THTML.TClickable }

procedure THTML.TClickable.SetCaption(const AValue: string);
begin
  if FCaption =AValue then Exit;
  FCaption :=AValue;
  if (estComposed in State) and (Schema <> nil) and Schema.Attached then
    SendMessage('"command": "change", "content": ' + DQ(Caption));
end;

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

{ THTML.TButtons }

procedure THTML.TButtons.Added(Item: TmnwElement);
begin
  if not (Item is TClickable) then
    raise Exception.Create('Buttons accepts only TClickable');
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

{ TmnwHTMLRenderer.TDynamicCompose }

procedure TmnwHTMLRenderer.TDynamicCompose.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  Context.Writer.OpenTag('div', Scope.Attributes.ToString);
  inherited;
  Scope.Element.Respond(Context, AResponse);
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TIntervalCompose }

procedure TmnwHTMLRenderer.TIntervalCompose.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited;
  Scope.Attributes['data-mnw-refresh-url'] := Scope.Element.GetPath;
end;

{ TmnwHTMLRenderer.TNavBar }

procedure TmnwHTMLRenderer.TNavBar.DoRenderBrand(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TNavBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Context.Writer.OpenTag('a', 'class="logo navbar-brand align-items-center me-auto" href="' + e.GetPath+'"');
  if e.Schema.App.Assets.Logo.Data.Size > 0 then
    Context.Writer.AddShortTag('img', 'src="' + e.Schema.App.Assets.Logo.GetPath+ '"');
  if e.Title <> '' then
    Context.Writer.AddTag('span', 'class="navbar-brand"', e.Title);
  Context.Writer.CloseTag('a');
end;

procedure TmnwHTMLRenderer.TNavBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Context.Writer.OpenTag('li', 'class="nav-item"');
  inherited;
end;

procedure TmnwHTMLRenderer.TNavBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.CloseTag('li');
end;

procedure TmnwHTMLRenderer.TNavBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TNavBar;
  sb: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Scope.Classes.Add('navbar');
  if e.Fixed = fixedTop then
    Scope.Classes.Add('fixed-top');
  Scope.Classes.Add('navbar-expand-md');
  Scope.Classes.Add('navbar-dark');
  Scope.Classes.Add('bg-dark');
  Scope.Classes.AddClasses('flex-nowrap navbar-expand-md navbar-dark bg-dark w-100 py-0 px-1');

  Context.Writer.OpenTag('nav', Scope.ToString);

  if (e.Schema as THTML).Document.Body.SideBar.CanRender then
  begin
    sb := (e.Schema as THTML).Document.Body.SideBar;
    Context.Writer.OpenTag('button', 'class="navbar-toggler me-0 ms-0 py-0 px-1 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#' + sb.id + '-items' + '" aria-controls="' + sb.id + '-items' + '" aria-expanded="false" aria-label="Toggle Sidebar"');
    Context.Writer.AddTag('span', 'class="invert icon mw-chevron-right"');
    Context.Writer.CloseTag('button');
  end;

	DoRenderBrand(Scope, Context);

  Context.Writer.OpenTag('div', 'id="'+e.id+'-items'+'" class="offcanvas offcanvas-top'+When((e.Schema as THTML).Document.Body.Header.CanRender, ' content-top') + ' navbar-dark bg-dark" data-bs-scroll="true" data-bs-backdrop="keyboard, static" tabindex="-1"');
  //Context.Writer.WriteLn('<div class="offcanvas-body">', [woOpenIndent]);
  Context.Writer.OpenTag('ul', 'class="navbar-nav mr-auto m-2 m-md-0"');
  inherited;
  Context.Writer.CloseTag('ul');
  Context.Writer.CloseTag('div');
  //Context.Writer.WriteLn('</div>', [woCloseIndent]);
  e.Buttons.Render(Context, AResponse); // Render buttons

  if e.Count > 0 then
  begin
    Context.Writer.OpenTag('button', 'class="navbar-toggler p-0 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#'+e.ID+'-items'+'" aria-controls="'+e.ID+'-items'+'" aria-expanded="false" aria-label="Toggle navigation"');
    Context.Writer.AddTag('span', 'class="invert icon mw-list"');
    Context.Writer.CloseTag('button');
  end;
  Context.Writer.CloseTag('nav');
end;

{ TmnwHTMLRenderer.TMenuBar }

procedure TmnwHTMLRenderer.TMenuBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TmnwHTMLRenderer.TMenuBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
end;

procedure TmnwHTMLRenderer.TMenuBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
begin
  inherited;
end;

{ TmnwHTMLRenderer.TLink }

procedure TmnwHTMLRenderer.TLink.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TLink;
  event: string;
begin
  e := Scope.Element as THTML.TLink;
  if e.ClickType = clickAction then
    event :=' onclick="mnw.click(event, this)"';
  Context.Writer.OpenInlineTag('a', 'href="'+When(e.Location, '#') + '"'+ event + Scope.GetText, e.Caption);
  inherited;
  Context.Writer.CloseTag('a');
end;

{ TmnwHTMLRenderer.TJSFile }

procedure TmnwHTMLRenderer.TJSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TJSFile;
  src: string;
begin
  e := Scope.Element as THTML.TJSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.OpenTag('script', 'type="text/javascript"' + Scope.GetText);
    inherited;
    Context.Writer.WriteLn('');
    Context.Writer.CloseTag('script');
  end
  else
  begin
    src := e.GetPath;
    Context.Writer.AddTag('script', 'type="text/javascript"' + When(e.Defer, ' defer') +' src='+ DQ(src)+'?v='+IntToStr(Context.Schema.TimeStamp));
    inherited;
  end;
end;

{ TmnwHTMLRenderer.TCSSFile }

procedure TmnwHTMLRenderer.TCSSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TCSSFile;
  src: string;
begin
  e := Scope.Element as THTML.TCSSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.OpenTag('style', 'type="text/css"'+ Scope.GetText);
    inherited;
    Context.Writer.WriteLn();
    Context.Writer.CloseTag('style');
  end
  else
  begin
    src := e.GetPath;
    Context.Writer.AddTag('link', 'rel="stylesheet" href='+ DQ(src) + '?v=' + IntToStr(Context.Schema.TimeStamp));
    inherited;
  end;
end;

{ TbsHttpGetHomeCommand }

function TUIWebCommand.CreateRespond: TmodRespond;
begin
  Result := TmnwResponse.Create(Request);
end;

function TUIWebCommand.GetModule: TUIWebModule;
begin
  Result := (inherited Module) as TUIWebModule;
end;

function TUIWebCommand.GetRespond: TmnwResponse;
begin
  Result := inherited Respond as TmnwResponse;
end;

procedure TUIWebCommand.RespondResult(var Result: TmodRespondResult);
var
  aContext: TmnwContext;
  aDomain, aPort: string;
begin
  inherited;
  AtomicIncrement(RendererID);
  InitMemory(aContext, SizeOf(aContext));

  aContext.Route := DeleteSubPath('', Request.Path);
  aContext.Sender := Self;

  if Module.Domain <> '' then
  begin
    aDomain := Module.Domain;
    aPort := Module.Port;
  end
  else
    SpliteStr(Request.Header['Host'], ':', aDomain, aPort);

  Module.WebApp.Lock.Enter; //smart huh, first connection will setup the domain name, i don't like it
  try
    if Module.WebApp.Domain = '' then
    begin
      Module.WebApp.Domain := aDomain;
      Module.WebApp.Port := aPort;
    end;
  finally
    Module.WebApp.Lock.Leave;
  end;

  if aDomain='' then
    raise Exception.Create('Domain is not defined');

  if Request.ConnectionType = ctWebSocket then
  begin
    (Module as TUIWebModule).WebApp.Attach(aContext, Self, Respond.Stream); //Serve the websocket
    //Result.Status := Result.Status - [mrKeepAlive]; // Disconnect
  end
  else
  begin
    aContext.SessionID := Request.Params.Values['session'];
    if aContext.SessionID = '' then
      aContext.SessionID := Request.Cookies.Values['session'];
    aContext.Data := TmnMultipartData.Create; //yes always created, i maybe pass params that come from Query (after ? )
    if Request.ConnectionType = ctFormData then
    begin
      aContext.Data.Boundary := Request.Header.Field['Content-Type'].SubValue('boundary');
      aContext.Data.TempPath := (Module as TUIWebModule).WorkPath + 'temp';
      aContext.Data.Read(Request.Stream);
    end;
    Respond.PutHeader('Content-Type', DocumentToContentType('html'));
    Respond.HttpResult := hrOK;
    aContext.Renderer := (Module as TUIWebModule).CreateRenderer;
    aContext.Renderer.RendererID := RendererID;
    aContext.Writer := TmnwWriter.Create('html', Respond.Stream);
    aContext.Writer.Compact := Module.WebApp.CompactMode;
    try
      aContext.Stamp := Request.Header['If-None-Match'];

      Respond.SessionID := Request.GetCookie('', 'session');
      Respond.HttpResult := hrOK;
      Respond.Location := '';

      (Module as TUIWebModule).WebApp.Respond(aContext, Respond);

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
  Kind := Kind + [elFallback];
  FLogo := THTML.TMemory.Create(This);
  FLogo.Name := 'logo';
  FLogo.Route := 'logo';
  ServeFiles := True;
end;

procedure TAssetsSchema.DoPrepare;
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
    if FileExists(HomePath + 'mnWebElements.js') then
    begin
      TFile.Create(This, [], HomePath + 'mnWebElements.js', 'WebElements.js');
      TFile.Create(This, [], HomePath + 'mnWebElements.css', 'WebElements.css');
    end
    else
    begin
      TFile.Create(This, [ftResource], 'WebElements_CSS.css', 'WebElements.css');
      TFile.Create(This, [ftResource], 'WebElements_JS.js', 'WebElements.js');
    end;
  end
  else
  begin
    TFile.Create(This, [], ExpandFileName(IncludePathDelimiter(minilib) + '/socket/source/mnWebElements.js'), 'WebElements.js');
    TFile.Create(This, [], ExpandFileName(IncludePathDelimiter(minilib) + '/socket/source/mnWebElements.css'), 'WebElements.css');
  end;

  with TElement.Create(This, 'resource') do
  begin

    TFile.Create(This, [ftResource], 'WebElements_CSS.css', 'WebElements.css');
    TFile.Create(This, [ftResource], 'WebElements_JS.js', 'WebElements.js');
  end;
end;

procedure TAssetsSchema.DoCompose;
begin
  inherited;
end;

{ TUIWebModule }

procedure TUIWebModule.DoPrepareRequest(ARequest: TmodRequest);
begin
  inherited;
  if StartsStr('.', ARequest.Route[ARequest.Route.Count - 1]) then
    ARequest.Command := ARequest.Route[ARequest.Route.Count - 1]
  else
    ARequest.Command := ARequest.Route[1];
  //ARequest.Path := DeleteSubPath(ARequest.Command, ARequest.Path);
end;

procedure TUIWebModule.Start;
begin
  inherited;
//  AssetsURL := '/' + AliasName + '/' + WebApp.Assets.Route;
  if WebApp.HomePath = '' then
    WebApp.HomePath := HomePath;
  if WebApp.Domain = '' then
    WebApp.Domain := Domain;
  if WebApp.Port = '' then
    WebApp.Port := Port;
  if WebApp.WorkPath = '' then
    WebApp.WorkPath := WorkPath;
  if WebApp.Alias = '' then
    WebApp.Alias := AliasName;

  WebApp.Assets.HomePath := WebApp.HomePath;

  WebApp.Start;
end;

procedure TUIWebModule.Stop;
begin
  WebApp.Stop;
  inherited;
end;

procedure TUIWebModule.CreateItems;
begin
  inherited;
  RegisterCommand('', TUIWebCommand, true);
end;

procedure TUIWebModule.Created;
begin
  inherited;
end;

function TUIWebModule.CreateRenderer: TmnwRenderer;
begin
  Result := TmnwHTMLRenderer.Create(Self, WebApp.IsLocal);
end;

destructor TUIWebModule.Destroy;
begin
  inherited;
  FreeAndNil(FWebApp); //keep behind inherited
end;

constructor TUIWebModule.Create(const AName, AAliasName: String; AProtocols: TArray<String>; AModules: TmodModules);
begin
  FWebApp := TmnwApp.Create;
  inherited;
end;

{ TElementClasses }

function TElementClasses.Add(const Name: string): Integer;
begin
  if Name = '' then
    exit(-1);

  Result:= Find(Name);
  if Result<0 then
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

class operator TElementClasses.Subtract(A: TElementClasses; B: string): TElementClasses;
var
  i: Integer;
begin
  i := A.Find(B);
  if i>=0 then
    Delete(A.Items,i, 0);
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

{ TmnwHTMLRenderer.TSideBar }

procedure TmnwHTMLRenderer.TSideBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
{  Context.Writer.WriteLn('<li class="sidebar-item">', [woOpenIndent]);
  if (Scope.Element is THTML.TLink) then
    Scope.Classes.Add('sidebar-link');}
  inherited;
end;

procedure TmnwHTMLRenderer.TSideBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
//  Context.Writer.WriteLn('</li>', [woCloseIndent]);
end;

procedure TmnwHTMLRenderer.TSideBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TSideBar;
  Context.Writer.OpenTag('aside id="'+e.ID+'" class="sidebar '+When((e.Schema as THTML).Document.Body.Header.CanRender, 'min-content-height') + ' p-0 m-0 shadow-thin navbar-expand-md"');
  Context.Writer.OpenTag('div id="' + e.ID + '-content' + '" class="sidebar-content ' + When((e.Schema as THTML).Document.Body.Header.CanRender, 'min-content-height') + ' fixed"');
  Context.Writer.OpenTag('div id="' + e.ID + '-body" class="sidebar-body offcanvas offcanvas-start p-2" data-bs-scroll="true" data-bs-backdrop="keyboard, static" aria-controls="header"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('aside');
end;

{ THTML.TLink }

constructor THTML.TLink.Create(AParent: TmnwElement; const ALocation: string; ACaption: string = '');
begin
  inherited Create(AParent);
  Location := ALocation;
  FCaption := ACaption;
end;

{ THTML.TCollapseCaption }

procedure THTML.TCollapseCaption.DoCompose;
begin
  inherited;

end;

{ TmnwCustomLibrary }

procedure TmnwCustomLibrary.AddHead(const Context: TmnwContext);
begin
  Context.Writer.AddTag('script', 'src="' + Source + '" defer crossorigin="anonymous"');
end;

{ THTML.TSpan }

constructor THTML.TSpan.Create(AParent: TmnwElement; const AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

{ TmnwHTMLRenderer.TSpan }

procedure TmnwHTMLRenderer.TSpan.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; AResponse: TmnwResponse);
var
  e: THTML.TSpan;
  event: string;
begin
  e := Scope.Element as THTML.TSpan;
  Context.Writer.OpenInlineTag('span', Scope.ToString, e.Text);
  inherited;
  Context.Writer.CloseTag('span');
end;

initialization

finalization
  FreeAndNil(ElementRenderers);
{$ifdef rtti_objects}
  FreeAndNil(CacheClassObjects);
{$endif}
end.
