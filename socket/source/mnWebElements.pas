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

    Application
     Document

┌──────┬──────────────────────────────────────┐  ─┐
│ Logo │ Brand NavBar                         │   │
├──────┴──────────────────────────────────────┤   ├─ Header
│ MenuBar                                     │   │
├────────────┬────────────────────────────────┤  ─┤
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
  LCLType, //* for RT_RCDATA
  {$endif}
  syncobjs, mnDON, mnJSON,
  mnUtils, mnClasses, mnStreams, mnLogs, mnMIME, mnParams,
  mnMultipartData, mnModules, mnWebModules;

{.$define rtti_objects}

type

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwElementRenderer = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwElementClass = class of TmnwElement;
  TElementExtension = class;
  TElementExtensionClass = class of TElementExtension;

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
    Renderer: TmnwRenderer;

    SessionID: string;
    ETag: string; //IfNone-Match
    Route: string;

    ParentRenderer: TmnwElementRenderer;
    Writer: TmnwWriter;
    Data: TmnMultipartData;
  end;

  TmnwReturn = record
    //ContentType: string;
    SessionID: string;
    Location: string; //* New location to forward

    Respond: TmodHttpRespond;
    Resume: Boolean;
  end;

  TmnwObject = class(TmnNamedObject);

  TmnwLibrary = class abstract(TmnNamedObject)
  private
    FUsage: Integer;
  protected
  public
    IsLocal: Boolean;
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); virtual; abstract;
    procedure IncUsage;
    procedure DecUsage;
    property Usage: Integer read FUsage;
  end;

  TmnwLibraryClass = class of TmnwLibrary;

  { TmnwLibraries }

  TmnwLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  public
    Local: Boolean; //* when find lib, find local first
    procedure Use(ALibrary: TmnwLibrary); overload;
    procedure Use(ALibraryName: string); overload;
    function Find(ALibrary: string; OnlyLocal: Boolean = False): TmnwLibrary; overload;
    procedure RegisterLibrary(ALibraryName: string; IsLocal: Boolean; ALibraryClass: TmnwLibraryClass);
  end;

  TJQuery_Library = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
  end;

  { TJQuery_LocalLibrary }

  TJQuery_LocalLibrary = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
  end;

  { TWebElements_Library }

  TWebElements_Library = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
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
    elHighLevel, //Rendered first like scripts
    elFallback //* if no child have the route name, it take the respond if have a name
  );


  TmnwAlign = (alignDefault, alignStart, alignCenter, alignStreach, alignEnd);
  TmnwFixed= (fixedDefault, fixedTop, fixedBottom);

  TActionProc = reference to procedure (const AContext: TmnwContext; var AReturn: TmnwReturn);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FHandle: Integer;
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
    FState: TmnwElementState;
    FOnExecute: TElementExecute;
    FOnAction: TActionProc;
    FPrepared: Boolean;
    procedure SetState(const AValue: TmnwElementState);
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;

    procedure DoPrepare; virtual;
    procedure DoCompose(const AContext: TmnwContext); virtual;
    procedure DoComposed; virtual;
    procedure DoRespondHeader(AContext: TmnwContext); virtual;
    procedure DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn); virtual;
    procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); virtual;

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

    function GetPath: string; virtual;

    function CreateRender(const Context: TmnwContext): TmnwElementRenderer;
    procedure Compose(const AContext: TmnwContext); virtual;
    procedure AddState(AState: TmnwElementState);
    procedure RemoveState(AState: TmnwElementState);

    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif} //* see TmnObjectList

    function GetContentType(Route: string): string; virtual;


    procedure Action(const AContext: TmnwContext; var AReturn: TmnwReturn);
    procedure Respond(const AContext: TmnwContext; var AReturn: TmnwReturn);

    //* Original Render
    procedure Render(const Context: TmnwContext; var AReturn: TmnwReturn); overload;

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

    property Attributes: TmnwAttributes read FAttributes;
    property Kind: TmnwElementKind read FKind write FKind;
    property State: TmnwElementState read FState write SetState;

    property OnExecute: TElementExecute read FOnExecute write FOnExecute;
    property OnAction: TActionProc read FOnAction write FOnAction;
    property Handle: Integer read FHandle;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (woEndLine, woOpenIndent, woCloseIndent);

  TmnwWriter = class(TmnNamedObject)
  private
    Level: Integer;
    NewLine: Boolean;
    FStream: TmnBufferStream;
  public
    constructor Create(AName: string; AStream: TmnBufferStream);
    procedure Write(S: string; Options: TmnwWriterOptions = []); virtual;
    procedure WriteLn(const S: string = ''; Options: TmnwWriterOptions = []);
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
    function GetReleased: Boolean;
  protected
    Usage: Integer;
    NameingLastNumber: Integer;
    procedure UpdateAttached;
    procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
    procedure ProcessMessage(const s: string);
  public
    LastAccess: TDateTime;
    IsManual: Boolean;
    Direction: TDirection;
    RefreshInterval: Integer; //* in seconds, for refresh elements that need auto refresh
    HomePath: string;
    ServeFiles: Boolean;
    SessionID: string;
    Interactive: Boolean;
    constructor Create(AName:string; ARoute: string = ''); reintroduce;
    destructor Destroy; override;

    class function GetCapabilities: TmnwSchemaCapabilities; virtual;

    //* Attaching cap
    //function Interactive: Boolean;

    procedure Compose(const AContext: TmnwContext); override;

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
      procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
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
      procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
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

    procedure RenderChilds(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);

    //* Called to parent to wrap the child rendering, each chiled will wrap it with this render
    //* This method exists in parent render
    procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Called only if have parent but exists in a child
    procedure DoEnterOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoLeaveOuterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    //* Content render
    procedure DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext); virtual;
    procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); virtual;
    procedure DoAfterRender(Scope: TmnwScope; const Context: TmnwContext); virtual;

    property Renderer: TmnwRenderer read FRenderer;
    property RendererRegister: TmnwRendererRegister read FRendererRegister;
  public
    procedure Render(AElement: TmnwElement; const Context: TmnwContext; var AReturn: TmnwReturn);
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

    function GetHostURL: string; virtual;
    function GetHomeURL: string; virtual;
    function GetAssetsURL: string; virtual;

    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); virtual; abstract;
  public
    IsSSL: Boolean;
    Domain: string; //localhost
    Port: string;
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
  protected
    procedure SchemaCreated(Schema: TmnwSchema); virtual;
    procedure Created; override;
  public
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
    function Respond(var AContext: TmnwContext; var AReturn: TmnwReturn): TmnwElement;
    //for WebSocket
    function Attach(const AContext: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;

    property Lock: TCriticalSection read FLock;
    property SessionTimeout: Integer read FSessionTimeout write FSessionTimeout; //in seconds
    property Assets: TAssetsSchema read FAssets;
    property HomePath: string read FHomePath write FHomePath;
    property AppPath: string read FAppPath write FAppPath;
    property Shutdown: Boolean read FShutdown;
  end;

  TSize = (
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
    procedure OpenTag(const TagName, TagAttributes: string); overload;
    procedure CloseTag(const Tag: string);
    procedure AddTag(const TagName, TagAttributes: string); overload;
    procedure AddTag(const TagName, TagAttributes, Value: string); overload;
  end;

  { THTML }

  THTML =class(TmnwSchema)
  private
  public
    type

      { THTMLElement }

      THTMLElement = class abstract(TmnwElement)
      protected
      public
      end;

      THTMLLayout = class abstract(THTMLElement)
      public
        Align: TmnwAlign;
        Fixed: TmnwFixed;
        Margin: Integer;
        Padding: Integer;
        Size: TSize;
      end;

      { THTMLComponent }

      THTMLComponent = class abstract(THTMLLayout)
      protected
        procedure Created; override;
      public
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
        procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
      public
        HomePath: string;
        function GetContentType(Route: string): string; override;
      end;

      TContentComposeProc = reference to procedure(Inner: TmnwElement);

      { TContentCompose }

      TContentCompose = class(THTMLElement)
      protected
        type

          { TInnerComposer }

          TInnerComposer = class(THTMLElement)
          public
          end;

        procedure InnerCompose(Inner: TmnwElement); virtual;

        procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
      public
        OnCompose: TContentComposeProc;
        constructor Create(AParent: TmnwElement; AOnCompose: TContentComposeProc = nil); reintroduce;
      end;

      [TID_Extension]
      [TRoute_Extension]
      TIntervalCompose = class(TContentCompose)
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
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKind =[]; ARenderIt: Boolean =True); override;
        destructor Destroy; override;
        property Header: THeader read FHeader;
        property SideBar: TSideBar read FSideBar;
        property Main: TMain read FMain;
        property Footer: TFooter read FFooter;
        property Toast: TToast read FToast;
        property Wide: Boolean read GetWide write SetWide;
      end;

      { THeader }

      [TID_Extension]
      TNavBar = class(THTMLComponent)
      public
        Title: string;
//        LogoImage: string;
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
        Shadow: Boolean;
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
      TSideBar = class(THTMLControl)
      protected
        procedure Created; override;
      public
        function CanRender: Boolean; override;
      end;

      TMain = class(THTMLComponent)
      protected
        procedure Created; override;
      public
        Size: TSize;
      end;

      TRow = class(THTMLLayout)
      public
        ContentAlign: TmnwAlign;
      end;

      TColumn = class(THTMLLayout)
      public
        Size: Integer;
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

      [TID_Extension]
      TPanel = class(THTMLCaptionComponent)
      public
      end;

      TList = class(THTMLControl)
      public
      end;

      TViewItem = class(THTMLCaptionComponent)
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
        procedure DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
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

      [TRoute_Extension]
      TAction = class(THTMLElement)
      protected
        procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
      public
        procedure Loop; virtual;
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

      { TLink }

			TLink = class(TClickable)
      public
        Location: string;
        constructor Create(AParent: TmnwElement; const ALocation, ACaption: string); reintroduce;
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
        procedure DoCompose(const AContext: TmnwContext); override;
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
        procedure DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn); override;
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

  protected
  public
    function GetContentType(Route: string): string; override;
  end;

  { TmnwHTMLRenderer }

  TmnwHTMLRenderer = class(TmnwRenderer)
  protected
  public
  type

      { TElement }

      THTMLElement = class abstract(TmnwElementRenderer)
      protected
        procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); virtual;
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
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TDocument }

      TDocument = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TBody }

      TBody = class(THTMLElement)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TFile }

      TFile = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TCSSFile }

      TCSSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TContentCompose }

      TContentCompose = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TIntervalCompose }

      TIntervalCompose = class(TContentCompose)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
      end;

      { THeader }

      THeader = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TNavBar }

      TNavBar = class(THTMLComponent)
      protected
        procedure DoRenderBrand(Scope: TmnwScope; Context: TmnwContext); virtual;
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TMenuBar }

      TMenuBar = class(THTMLComponent)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TLink }

      TLink = class(THTMLControl)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TFooter }

      TFooter = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TToast }

      TToast = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TContent }

      TContent = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TSideBar }

      TSideBar = class(THTMLControl)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TMain }

      TMain = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TRow }

      TRow = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TColumn }

      TColumn = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TCard }

      TCard = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      TPanel = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TForm }

      TForm = class(THTMLElement)
      protected
        procedure DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TParagraph }

      TParagraph = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TBreak }

      TBreak = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TButton }

      TButton = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TNavItem }

      TNavItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TMenuItem }

      TMenuItem = class(THTMLComponent)
      protected
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TSubbMenu }

      TSubMenu = class(THTMLControl)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TInput }

      TInput = class(THTMLComponent)
      protected
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      TInputPassword = class(TInput)
      end;

      { TImage }

      TImage = class(THTMLComponent)
      protected
        procedure DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer); override;
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

      { TMemoryImage }

      TMemoryImage = class(THTMLComponent)
      protected
        procedure DoPrepare(AElement: TmnwElement; ARenderer: TmnwRenderer); override;
        procedure DoCollectAttributes(var Scope: TmnwScope); override;
        procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
      end;

  protected
    procedure Created; override;
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
    class constructor RegisterObjects;
  public
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

  { TUIWebCommand }

  TUIWebCommand = class(TmodHttpCommand)
  private
    function GetModule: TUIWebModule;
  protected
  public
    procedure RespondResult(var Result: TmodRespondResult); override;
    property Module: TUIWebModule read GetModule;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TmnwSchema)
  private
  protected
    FLogo: THTML.TMemory;
    procedure DoPrepare; override;
    procedure DoCompose(const AContext: TmnwContext); override;
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
    IsLocal: Boolean;
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

implementation

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
begin
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

procedure TmnwElementRenderer.RenderChilds(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  o: TmnwElement;
begin
  Context.ParentRenderer := Self;
  for o in Scope.Element do
  begin
    if (elHighLevel in o.Kind) then
      if not (elInternal in o.Kind) then
        o.Render(Context, AReturn);
  end;

  for o in Scope.Element do
  begin
    if not (elHighLevel in o.Kind) then
      if not (elInternal in o.Kind) then
        o.Render(Context, AReturn);
  end;
end;

procedure TmnwElementRenderer.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterInnerRender(Scope: TmnwScope; const Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  RenderChilds(Scope, Context, AReturn);
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

procedure TmnwElementRenderer.Render(AElement: TmnwElement; const Context: TmnwContext; var AReturn: TmnwReturn);
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
    DoInnerRender(aScope, Context, AReturn);
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

procedure TmnwElement.Render(const Context: TmnwContext; var AReturn: TmnwReturn);
var
  Renderer: TmnwElementRenderer;
begin
  if CanRender then
  begin
    Renderer := CreateRender(Context);
    if Renderer <> nil then
    begin
      try
        try
          Renderer.Render(Self, Context, AReturn);
        except
          on E: Exception do
          begin
            raise Exception.Create('Error in '+ ClassName +': ' + E.Message);
          end;
        end;
      finally
        Renderer.Free;
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
  end;
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

    if Schema <> nil then
    begin
      if not (estComposed in Schema.State) then
      begin
        Schema.Lock.Enter;
        try
          try
            Schema.SessionID := AContext.SessionID;
            Schema.Compose(AContext);
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
    end;

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
    end
    else
      Result := False;

  finally
    Routes.Free;
  end;
end;

function TmnwApp.Respond(var AContext: TmnwContext; var AReturn: TmnwReturn): TmnwElement;

  function SessionCookies(const vData: string): string;
  var
    aDate: TDateTime;
    aPath, aDomain: string;
  begin
    aDate := IncSecond(Now, SessionTimeout);
    aDomain := AContext.Renderer.Domain;
    aPath := '';

    if vData<>'' then
      Result := Format('%s; Expires=%s; SameSite=None; Domain=%s; Path=/%s; Secure', [vData, FormatHTTPDate(aDate), aDomain, aPath.ToLower])
    else
      Result := Format('; max-age=0; SameSite=None; Domain=%s; Path=/%s; Secure', [aDomain, aPath.ToLower]);
  end;

var
  aSchema: TmnwSchema;
begin
  if Shutdown then
    exit(nil);

  try
    GetElement(AContext, aSchema, Result);
    if Result <> nil then
    begin
      aContext.Schema := aSchema;
      AReturn.Respond.ContentType := Result.GetContentType(AContext.Route);
      //resLatch
  //    (AContext.Sender as TmodHttpCommand).Respond.Latch := True;
      try
        //You should not send any text/data content only set headers or refuse to continue
        Result.Action(AContext, AReturn);
      finally
  //      (AContext.Sender as TmodHttpCommand).Respond.Latch := False;
      end;

      if AReturn.Location <> '' then
        AReturn.Respond.Header['Location'] := AReturn.Location;

      if AReturn.SessionID<>'' then
        AReturn.Respond.Cookies.Values['session'] := SessionCookies(AReturn.SessionID);

      //* We will render it now
      if AReturn.Resume then
        Result.Respond(AContext, AReturn);

      if not (AReturn.Respond.IsHeaderSent) then
      begin
        if (AReturn.Respond.HttpResult = hrOK) and (AReturn.Resume = False) then
        begin
          AReturn.Respond.HttpResult := hrNoContent;
          AReturn.Respond.ContentLength := 0;
        end
        else if AReturn.Respond.HttpResult = hrNotFound then
        begin
          AReturn.Respond.ContentType := 'text/html';
          AContext.Writer.WriteLn('404 Not Found');
        end;

        if not (AReturn.Respond.IsHeaderSent) and (AReturn.Respond.HttpResult > hrNone) then
          AReturn.Respond.SendHeader;
      end;
    end
    else
    begin
      if not (AReturn.Respond.IsHeaderSent) then
      begin
        AReturn.Respond.HttpResult := hrNotFound;
        AReturn.Respond.ContentType := 'text/html';
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
      if not (AReturn.Respond.IsHeaderSent) then
      begin
        AReturn.Respond.HttpResult := hrError;
        AReturn.Respond.ContentType := 'text/html';
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

{ TmnwHTMLWriterHelper }

procedure TmnwHTMLWriterHelper.OpenTag(const Tag: string);
begin
  WriteLn('<'+Tag+'>', [woOpenIndent])
end;

procedure TmnwHTMLWriterHelper.OpenTag(const TagName, TagAttributes: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes +'>', [woOpenIndent])
end;

procedure TmnwHTMLWriterHelper.CloseTag(const Tag: string);
begin
  WriteLn('</'+Tag+'>', [woCloseIndent])
end;

procedure TmnwHTMLWriterHelper.AddTag(const TagName, TagAttributes: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '></' + TagName + '>', [woOpenIndent, woCloseIndent]);
end;

procedure TmnwHTMLWriterHelper.AddTag(const TagName, TagAttributes, Value: string);
begin
  WriteLn('<'+TagName + ' ' + TagAttributes + '>' + Value + '</' + TagName + '>', [woOpenIndent, woCloseIndent]);
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
  Size := szNormal;
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

procedure TmnwHTMLRenderer.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
end;

class constructor TmnwHTMLRenderer.RegisterObjects;
begin
  //RegisterClasses(THTML);
  RegisterRenderer(THTML.TContentCompose, TContentCompose);
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
  RegisterRenderer(THTML.TMain, TMain);
  RegisterRenderer(THTML.TFooter, TFooter);
  RegisterRenderer(THTML.TToast, TToast);
  RegisterRenderer(THTML.TLink, TLink);
  RegisterRenderer(THTML.TButton, TButton);
  RegisterRenderer(THTML.TNavItem, TNavItem);
  RegisterRenderer(THTML.TMenuItem, TMenuItem);
  RegisterRenderer(THTML.TInput, TInput);
  RegisterRenderer(THTML.TInputPassword, TInputPassword);
  RegisterRenderer(THTML.TImage, TImage);
  RegisterRenderer(THTML.TMemoryImage, TMemoryImage);
  RegisterRenderer(THTML.TCard, TCard);
  RegisterRenderer(THTML.TForm, TForm);
  RegisterRenderer(THTML.TRow, TRow);
  RegisterRenderer(THTML.TColumn, TColumn);
  RegisterRenderer(THTML.TPanel, TPanel);
end;

{ TmnwHTMLRenderer.THTMLElement }

procedure TmnwHTMLRenderer.THTMLElement.AddHead(AElement: TmnwElement; const Context: TmnwContext);
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
    Scope.Classes.Add('shadow-sm');
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

procedure TmnwHTMLRenderer.TComment.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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
  Scope.Attributes['lang'] := 'en'
end;

procedure TmnwHTMLRenderer.TDocument.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TDocument;
  aLibrary: TmnwLibrary;
//  o: TmnwElement;
//  r: THTMLElement;
begin
  e := Scope.Element as THTML.TDocument;
  Context.Writer.WriteLn('<!DOCTYPE html>');
  Context.Writer.WriteLn('<html' + Scope.GetText + '>', [woOpenIndent]);
  Context.Writer.WriteLn('<head>', [woOpenIndent]);
  Context.Writer.WriteLn('<title>'+ e.Title + '</title>', [woOpenIndent, woCloseIndent]);
  Context.Writer.WriteLn('<link rel="shortcut icon" href="#" />', [woOpenIndent, woCloseIndent]);
  if e.Parent <> nil then // Only root have head
  begin
    AddHead(Scope.Element, Context);
    for aLibrary in Renderer.Libraries do
    begin
      if aLibrary.Usage > 0 then
        aLibrary.AddHead(Scope.Element, Context);
    end;
    (Renderer as TmnwHTMLRenderer).AddHead(Scope.Element, Context);
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
  Context.Writer.WriteLn('</head>', [woCloseIndent]);
  e.Body.Render(Context, AReturn);
  Context.Writer.WriteLn('</html>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.THeaderHTML }

procedure TmnwHTMLRenderer.THeader.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  Scope.Classes.AddClasses('header sticky-top d-flex align-items-center navbar-dark bg-dark shadow-sm py-0 px-1');
  Context.Writer.OpenTag('header', Scope.ToString);
  inherited;
  Context.Writer.CloseTag('header');
end;

{ TmnwHTMLRenderer.TFooterHTML }

procedure TmnwHTMLRenderer.TFooter.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TFooter;
begin
  e := Scope.Element as THTML.TFooter;
  Context.Writer.WriteLn('<footer class="bg-body-tertiary text-center text-lg-start">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</footer>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TToast }

procedure TmnwHTMLRenderer.TToast.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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

procedure TmnwHTMLRenderer.TContent.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TContent;
begin
  e := Scope.Element as THTML.TContent;
  if e.Wide then
    Scope.Classes.Add('container-fluid')
  else
    Scope.Classes.Add('container');
  Context.Writer.OpenTag('div'+Scope.GetText);
  Context.Writer.OpenTag('div id="content" class="content row flex-nowrap"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TMainHTML }

procedure TmnwHTMLRenderer.TMain.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TMain;
begin
  e := Scope.Element as THTML.TMain;
  Scope.Classes.Add('container');
  Context.Writer.WriteLn('<main'+Scope.GetText+'>', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</main>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TCardHTML }

procedure TmnwHTMLRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');
  Context.Writer.WriteLn('<div' + Scope.GetText + '>', [woOpenIndent]);
  if e.Caption <> '' then
    Context.Writer.AddTag('div', 'class="card-header"' + e.Caption);

  Context.Writer.WriteLn('<div class="card-body">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
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

procedure TmnwHTMLRenderer.TForm.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TForm;
  aPostTo: string;
begin
  e := Scope.Element as THTML.TForm;
  if e.PostTo.Custom <> '' then
    aPostTo := e.PostTo.Custom
  else if e.PostTo.Where = toSchema then
    aPostTo := IncludeURLDelimiter(Renderer.GetHomeUrl) + e.Schema.GetPath
  else if e.PostTo.Where = toElement then
    aPostTo := IncludeURLDelimiter(Renderer.GetHomeUrl) + e.GetPath
  else if e.PostTo.Where = toHome then
    aPostTo := IncludeURLDelimiter(Renderer.GetHomeUrl);
  Context.Writer.WriteLn('<form ajax="true" method="post"'+ NV('action', aPostTo) + ' enctype="multipart/form-data"' + Scope.GetText+'>', [woOpenIndent]);
  inherited;
  if e.RedirectTo <> '' then
    Context.Writer.WriteLn('<input type="hidden" name="redirect" value="' + e.RedirectTo + '">', [woOpenIndent, woCloseIndent]);
  Context.Writer.WriteLn('<input type="hidden" name="execute" value="true">', [woOpenIndent, woCloseIndent]);
  Context.Writer.WriteLn('</form>', [woCloseIndent]);

  if e.Submit.Caption <> '' then
    Context.Writer.WriteLn('<button class="btn btn-success" type="submit" form="'+e.ID+'" value="Submit">' + e.Submit.Caption + '</button>', [woOpenIndent, woCloseIndent]);
  if e.Cancel.Caption <> '' then
    Context.Writer.WriteLn('<button class="btn btn-primary" type="cancel" form="'+e.ID+'" value="Cancel">' + e.Cancel.Caption + '</button>', [woOpenIndent, woCloseIndent]);
  if e.Reset.Caption <> '' then
    Context.Writer.WriteLn('<button class="btn btn-primary" type="reset" form="'+e.ID+'" value="Reset">' + e.Reset.Caption + '</button>', [woOpenIndent, woCloseIndent]);
end;

{ TmnwHTMLRenderer.TParagraphHTML }

procedure TmnwHTMLRenderer.TParagraph.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TParagraph;
begin
  e := Scope.Element as THTML.TParagraph;
  Context.Writer.Write('<p>', [woOpenIndent]);
  if e.Text <> '' then
    Context.Writer.Write(e.Text, []);
  inherited;
  Context.Writer.WriteLn('</p>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TBreakHTML }

procedure TmnwHTMLRenderer.TBreak.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  Context.Writer.WriteLn('<br>');
end;

{ TmnwHTMLRenderer.TTButton }

procedure TmnwHTMLRenderer.TButton.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited;
  //Scope.Attributes['type'] := (Scope.Element as THTML.TButton).EditType;
end;

procedure TmnwHTMLRenderer.TButton.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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

procedure TmnwHTMLRenderer.TNavItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TNavItem;
  event: string;
begin
  e := Scope.Element as THTML.TNavItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Scope.Classes.Add('nav-link');
  Context.Writer.WriteLn('<a href="'+e.LinkTo+'"' + event + '' + Scope.GetText+'>'+e.Caption+'</a>', [woOpenIndent, woCloseIndent]);
  inherited;
end;

{ TmnwHTMLRenderer.TMenuItem }

procedure TmnwHTMLRenderer.TMenuItem.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TMenuItem;
  event: string;
begin
  e := Scope.Element as THTML.TMenuItem;
  if Context.Schema.Interactive then
    event := ' onclick="mnw.send(' + SQ(e.ID) + ', '+ SQ('click') + ')"';
  Context.Writer.WriteLn('<button role="menu" type="button"' + event + '' + Scope.Attributes.GetText+' >'+e.Caption+'</button>', [woOpenIndent, woCloseIndent]);
  inherited;
end;

{ TmnwHTMLRenderer.TSubbMenu }

procedure TmnwHTMLRenderer.TSubMenu.DoCollectAttributes(var Scope: TmnwScope);
begin
  inherited DoCollectAttributes(Scope);
end;

procedure TmnwHTMLRenderer.TSubMenu.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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

procedure TmnwHTMLRenderer.TInput.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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
    Context.Writer.WriteLn('<label'+When(isFormChild, ' class="form-label"')+' for="'+e.ID+'" >' + e.Caption + '</label>', [woOpenIndent, woCloseIndent]);

  if Context.Schema.Interactive then
    event := ' onchange="mnw.send(' + SQ(e.ID) + ', '+ SQ('change') + ',' + 'this.value' + ')"';

  Context.Writer.WriteLn('<input'+ event + When(e.Required, 'required') + Scope.GetText + ' >', [woOpenIndent, woCloseIndent]);
  if e.HelpText <> '' then
    Context.Writer.WriteLn('<div class="form-text">' + e.HelpText + '</div>', woFullTag);
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

procedure TmnwHTMLRenderer.TImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  Context.Writer.WriteLn('<img' + Scope.GetText+' >', [woOpenIndent, woCloseIndent]);
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
  Scope.Attributes['src'] := IncludeURLDelimiter(Renderer.GetHomeUrl) + Scope.Element.GetPath;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText;
end;

procedure TmnwHTMLRenderer.TMemoryImage.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TMemoryImage;
begin
  e := Scope.Element as THTML.TMemoryImage;
  Context.Writer.WriteLn('<img'+ Scope.GetText+' >', [woOpenIndent, woCloseIndent]);
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

procedure TmnwSchema.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
var
  fs: TFileStream;
  aFileName: string;
  aHomePath: string;
begin
  inherited;
  if ServeFiles and (AContext.Route <> '') then
  begin
    aHomePath := When(HomePath, App.FHomePath);
    if aHomePath <> '' then
    begin
      if WebExpandFile(aHomePath, AContext.Route, aFileName) then
      begin
        if FileExists(aFileName) then
        begin
          AReturn.Respond.ContentType := DocumentToContentType(aFileName);
          fs := TFileStream.Create(aFileName, fmShareDenyWrite or fmOpenRead);
          try
            AContext.Writer.Stream.WriteStream(fs, 0);
          finally
            fs.Free;
          end;
        end
        else
          AReturn.Respond.HttpResult := hrNotFound;
      end
      else
        AReturn.Respond.HttpResult := hrUnauthorized;
    end
    else
      Render(AContext, AReturn);
  end
  else
    Render(AContext, AReturn);
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

function TmnwSchema.GetReleased: Boolean;
begin
  Result := (FPhase = scmpReleased) or (schemaDynamic in GetCapabilities);
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

procedure THTML.TMemoryImage.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  Data.Seek(0, soBeginning);
  AContext.Writer.Stream.WriteStream(Data, 0);
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

function TmnwElement.GetPath: string;
begin
  if Self=nil then
    Exit('/');

  if (Parent <> nil) then
  begin
    if Route <> '' then
      Result := IncludeURLDelimiter(Parent.GetPath) + Route
    else
      Result := Parent.GetPath;
  end
  else
    Result := Route;
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
  p: TmnwElement;
begin
  p := Self;
  while p<>nil do
  begin
    if SameText(p.ID, aID) then
      Exit(p);

    p := p.Parent;
  end;

  Result := nil;
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
    Inc(Schema.NameingLastNumber);
    FHandle := Schema.NameingLastNumber
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

procedure TmnwElement.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
end;

procedure TmnwElement.DoRespondHeader(AContext: TmnwContext);
begin
end;

procedure TmnwElement.DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
end;

constructor TmnwElement.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited Create;
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
  Result := StringOfChar(' ', vLevel * 4);
end;

procedure TmnwElement.Respond(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  DoRespond(AContext, AReturn);
end;

procedure TmnwElement.Compose(const AContext: TmnwContext);
var
  o: TmnwElement;
begin
//  Clear; //*Should not clear here
  Prepare;
  DoCompose(AContext);
  UpdateElement(Self);
  for o in Self do
  begin
    o.Compose(AContext);
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

procedure TmnwElement.Action(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  AReturn.Respond.PutHeader('Content-Type', GetContentType(AContext.Route));
  DoRespondHeader(AContext);
  AReturn.Resume := True;
  DoAction(AContext, AReturn);
  if Assigned(FOnAction) then
    FOnAction(AContext, AReturn);
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

  if (NewLine) then
    S := LevelStr(Level) + S;

  NewLine := False;

  if (woEndLine in Options) then
  begin
    NewLine := True;
    s := S + sWinEndOfLine;
  end;

  FStream.WriteUtf8String(S);

  if (woOpenIndent in Options) and not (woCloseIndent in Options) then
    Inc(Level);
end;

procedure TmnwWriter.WriteLn(const S: string; Options: TmnwWriterOptions);
begin
  Write(S, Options + [woEndLine]);
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


function TmnwRenderer.GetAssetsURL: string;
begin
  Result := GetHomeURL + 'assets/';
end;

function TmnwRenderer.GetHomeURL: string;
begin
  Result := IncludeURLDelimiter(IncludeURLDelimiter(GetHostURL) + Module.AliasName);
end;

function TmnwRenderer.GetHostURL: string;
begin
  Result := ComposeHttpURL(IsSSL, Domain, Port);
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

procedure TmnwSchema.TFile.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
var
  aStream: TStream;
  aDocSize: Int64;
  aDate: TDateTime;
  aEtag, aFTag: string;
begin
  inherited;
  if ftResource in Options then
  begin
    aStream := TResourceStream.Create(hInstance, ChangeFileExt(FileName, ''), RT_RCDATA); //* remove extension
    try
      AContext.Writer.Stream.WriteStream(aStream, 0);
    finally
      aStream.Free;
    end;
  end
  else
  begin
    if not FileExists(FileName) then
    begin
      AReturn.Respond.HttpResult := hrNotFound;
      AReturn.Resume := False;
      exit;
    end
    else
    begin
      FileAge(FileName, aDate);
      aFtag := DateTimeToUnix(aDate).ToString;
      aEtag := AContext.ETag;
      if (aEtag<>'') and (aEtag = aFtag) then
      begin
        AReturn.Respond.HttpResult := hrNotModified;
        AReturn.Resume := False;
        exit;
      end;

      aStream := TFileStream.Create(FileName, fmShareDenyNone or fmOpenRead);
      try
        aDocSize := aStream.Size;

        AReturn.Respond.Header['Cache-Control']  := 'max-age=600';
        AReturn.Respond.Header['Last-Modified']  := FormatHTTPDate(aDate);
        AReturn.Respond.Header['ETag']           := aFTag;
        AReturn.Respond.Header['Content-Length'] := IntToStr(aDocSize);

        AContext.Writer.Stream.WriteStream(aStream, 0);
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

procedure TmnwSchema.TMemory.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  if FileDate <> 0 then
    AReturn.Respond.Header['Last-Modified']  := FormatHTTPDate(FileDate);
  AReturn.Respond.Header['Content-Length'] := IntToStr(Data.Size);
  AReturn.Respond.Header['Cache-Control']  := 'public, max-age=3600';
  Data.Seek(0, soBeginning);
  AContext.Writer.Stream.WriteStream(Data, 0);
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

procedure THTML.TAssets.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
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
          AContext.Writer.Stream.WriteStream(fs, 0);
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

{ THTML.TContentCompose }

constructor THTML.TContentCompose.Create(AParent: TmnwElement; AOnCompose: TContentComposeProc);
begin
  inherited Create(AParent);
  OnCompose := AOnCompose;
end;

procedure THTML.TContentCompose.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
var
  InnerComposer: TInnerComposer;
begin
  inherited;
  //Clear; //here not in Compose
  //Compose;
  {InnerComposer := TInnerComposer.Create(nil);
  InnerComposer.FParent := Self;
  TContentCompose(InnerComposer).DoCompose(const AContext: TmnwContext);
  //InnerComposer.OnCompose;
  Render(ARenderer, Sender, AStream);}

  InnerComposer := TInnerComposer.Create(nil);
  try
    InnerComposer.FSchema := Schema;
    InnerComposer.FParent := Self; //Fake Parent do not add it to the list;
    InnerCompose(InnerComposer);
    if Assigned(OnCompose) then
      OnCompose(InnerComposer);
    InnerComposer.Render(AContext, AReturn);
  finally
    InnerComposer.Free;
  end;

end;

procedure THTML.TContentCompose.InnerCompose(Inner: TmnwElement);
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

procedure TJQuery_Library.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<script src="' + 'https://cdn.jsdelivr.net/npm/jquery@3.7.1/dist/' + 'jquery.min.js" crossorigin="anonymous"></script>');
end;

{ TJQuery_LocalLibrary }

procedure TJQuery_LocalLibrary.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<script src="' + IncludeURLDelimiter(Context.Renderer.GetAssetsURL) + 'jquery.min.js" crossorigin="anonymous"></script>');
end;

{ TWebElements_Library }

procedure TWebElements_Library.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<script src="' + IncludeURLDelimiter(Context.Renderer.GetAssetsURL) + 'WebElements.js" crossorigin="anonymous"></script>');
  Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Renderer.GetAssetsURL) + 'WebElements.css" crossorigin="anonymous">');
end;

{ THTML }

{ THTML.TImage }

procedure THTML.TImage.DoCompose(const AContext: TmnwContext);
begin
  inherited;
end;

{ TmnwHTMLRenderer.TFile }

procedure TmnwHTMLRenderer.TFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TFile;
begin
  e := Scope.Element as THTML.TFile;
  if ftEmbed in e.Options then
    Scope.Element.Respond(Context, AReturn);
  inherited;
end;

{ THTML.TBody }

constructor THTML.TBody.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited;
  //This object auto free by parents
  FHeader := THeader.Create(Self, [elEmbed], False);

  FContent := TContent.Create(Self, [elEmbed], True);
  with FContent do
  begin
    FSideBar := TSideBar.Create(This, [elEmbed], True);
    FMain := TMain.Create(This, [elEmbed], True);
  end;

  FFooter := TFooter.Create(Self, [elEmbed], False);
  FToast := TToast.Create(Self, [elEmbed], False);
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
end;

procedure TmnwHTMLRenderer.TBody.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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
  Context.Writer.WriteLn('<body' + Scope.GetText + GetAttach + '>', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</body>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TPanel }

procedure TmnwHTMLRenderer.TPanel.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Context.Writer.WriteLn('<div class="panel">', [woOpenIndent]);
  if e.Caption <> '' then
    Context.Writer.WriteLn('<div class="panel-header">' + e.Caption + '</div>', [woOpenIndent, woCloseIndent]);

  Context.Writer.WriteLn('<div class="panel-body">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TRow }

procedure TmnwHTMLRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Context.Writer.WriteLn('<div class="row">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TColumn }

procedure TmnwHTMLRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Writer.WriteLn('<div class="column">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ THTML.TMain }

procedure THTML.TMain.Created;
begin
  inherited;
end;

{ THTML.TCard }

procedure THTML.TCard.Created;
begin
  inherited;
  Shadow := True;
end;

{ THTML.TForm }

procedure THTML.TForm.DoAction(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  inherited;
  if (RedirectTo <> '') and (AReturn.Respond.HttpResult = hrNone) then
  begin
    AReturn.Respond.HttpResult := hrRedirect;
    AReturn.Location := RedirectTo;
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

procedure THTML.TAction.DoRespond(const AContext: TmnwContext; var AReturn: TmnwReturn);
begin
  inherited;
  try
    Execute;
    AContext.Writer.Stream.WriteUTF8Line('Executed');
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

{ TmnwHTMLRenderer.TContentCompose }

procedure TmnwHTMLRenderer.TContentCompose.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  Context.Writer.WriteLn('<div ' + Scope.Attributes.GetText+'>', [woOpenIndent]);
  inherited;
  Scope.Element.Respond(Context, AReturn);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TIntervalCompose }

procedure TmnwHTMLRenderer.TIntervalCompose.DoCollectAttributes(var Scope: TmnwScope);
var
  URL: string;
begin
  inherited;
  URL := IncludeURLDelimiter(Renderer.GetHomeUrl) + Scope.Element.GetPath;
  Scope.Attributes['data-mnw-refresh-url'] := URL;
end;

{ TmnwHTMLRenderer.TNavBar }

procedure TmnwHTMLRenderer.TNavBar.DoRenderBrand(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TNavBar;
begin
  e := Scope.Element as THTML.TNavBar;
  Context.Writer.WriteLn('<a class="logo navbar-brand d-flex align-items-center p-0 ms-0" href="'+Context.Renderer.GetHomeURL+'">', [woOpenIndent]);
  if e.Schema.App.Assets.Logo.Data.Size > 0 then
    Context.Writer.WriteLn('<img class="" src="' + Context.Renderer.GetHomeURL + e.Schema.App.Assets.Logo.GetPath + '">', [woOpenIndent, woCloseIndent]);
  if e.Title <> '' then
    Context.Writer.WriteLn('<span class="navbar-brand ms-1">'+e.Title+'</span>', [woOpenIndent, woCloseIndent]);
  Context.Writer.WriteLn('</a>', [woCloseIndent]);
end;

procedure TmnwHTMLRenderer.TNavBar.DoEnterChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<li class="nav-item">', [woOpenIndent]);
  inherited;
end;

procedure TmnwHTMLRenderer.TNavBar.DoLeaveChildRender(var Scope: TmnwScope; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.WriteLn('</li>', [woCloseIndent]);
end;

procedure TmnwHTMLRenderer.TNavBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
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
  Scope.Classes.Add('p-1');
  Scope.Classes.AddClasses('flex-nowrap shadow-md navbar-expand-md navbar-dark bg-dark w-100 px-1');

  Context.Writer.WriteLn('<nav'+Scope.GetText+'>', [woOpenIndent]);

  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
  begin
    sb := (e.Parent.Parent as THTML.TBody).SideBar;
    Context.Writer.OpenTag('button', 'class="navbar-toggler me-0 ms-0 py-0 px-1 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#' + sb.id + '-items' + '" aria-controls="' + sb.id + '-items' + '" aria-expanded="false" aria-label="Toggle Sidebar"');
    Context.Writer.AddTag('span', 'class="carousel-control-next-icon"');
    Context.Writer.CloseTag('button');
  end;

	DoRenderBrand(Scope, Context);

  if e.Count > 0 then
  begin
    Context.Writer.WriteLn('<button class="navbar-toggler p-0 border-0" type="button" data-bs-toggle="offcanvas" data-bs-target="#'+e.ID+'-items'+'" aria-controls="'+e.ID+'-items'+'" aria-expanded="false" aria-label="Toggle navigation">', [woOpenIndent]);
    Context.Writer.WriteLn('<span class="navbar-toggler-icon"></span>');
    Context.Writer.WriteLn('</button>', [woCloseIndent]);
  end;

  //Context.Writer.WriteLn('<div id="'+e.id+'-items'+'" class="collapse navbar-collapse">', [woOpenIndent]);
  Context.Writer.WriteLn('<div id="'+e.id+'-items'+'" class="offcanvas offcanvas-top text-bg-dark" data-bs-scroll="true" data-bs-backdrop="keyboard, static" tabindex="-1">', [woOpenIndent]);
  Context.Writer.WriteLn('<div class="offcanvas-body">', [woOpenIndent]);

  Context.Writer.WriteLn('<ul class="navbar-nav mr-auto mb-2 mb-md-0">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</ul>', [woCloseIndent]);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
  Context.Writer.WriteLn('</nav>', [woCloseIndent]);
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

procedure TmnwHTMLRenderer.TMenuBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
begin
  inherited;
end;

{ TmnwHTMLRenderer.TLink }

procedure TmnwHTMLRenderer.TLink.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TLink;
  event: string;
begin
  e := Scope.Element as THTML.TLink;
  if e.ClickType = clickAction then
    event :=' onclick="mnw.click(event, this)"';
  Context.Writer.Write('<a' + Scope.GetText + ' href="'+When(e.Location, '#') + '"'+ event + '>', [woOpenIndent]);
  if e.Caption <> '' then
    Context.Writer.Write(e.Caption)
  else
    Context.Writer.Write('#');
  inherited;
  Context.Writer.WriteLn('</a>', [woCloseIndent]);
end;

{ TmnwHTMLRenderer.TJSFile }

procedure TmnwHTMLRenderer.TJSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TJSFile;
  src: string;
begin
  e := Scope.Element as THTML.TJSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.WriteLn('<script type="text/javascript"'+ Scope.Attributes.GetText + '>', [woOpenIndent]);
    inherited;
    Context.Writer.WriteLn('');
    Context.Writer.WriteLn('</script>', [woCloseIndent]);
  end
  else
  begin
    src := IncludeURLDelimiter(Renderer.GetHomeUrl) + Scope.Element.GetPath;
    Context.Writer.WriteLn('<script type="text/javascript"' + When(e.Defer, ' defer') +' src='+ DQ(src) +' ></script>', [woOpenIndent, woCloseIndent]);
    inherited;
  end;
end;

{ TmnwHTMLRenderer.TCSSFile }

procedure TmnwHTMLRenderer.TCSSFile.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TCSSFile;
  src: string;
begin
  e := Scope.Element as THTML.TCSSFile;
  if ftEmbed in e.Options then
  begin
    Context.Writer.WriteLn('<style type="text/css"'+ Scope.Attributes.GetText + '>', [woOpenIndent]);
    inherited;
    Context.Writer.WriteLn('');
    Context.Writer.WriteLn('</style>', [woCloseIndent]);
  end
  else
  begin
    src := IncludeURLDelimiter(Renderer.GetHomeUrl) + Scope.Element.GetPath;
    Context.Writer.WriteLn('<link rel="stylesheet" href='+ DQ(src) +' ></link>', [woOpenIndent, woCloseIndent]);
    inherited;
  end;
end;

{ TbsHttpGetHomeCommand }

function TUIWebCommand.GetModule: TUIWebModule;
begin
  Result := (inherited Module) as TUIWebModule;
end;

procedure TUIWebCommand.RespondResult(var Result: TmodRespondResult);
var
  aContext: TmnwContext;
  aReturn: TmnwReturn;
  aDomain, aPort: string;
begin
  InitMemory(aContext, SizeOf(aContext));
  InitMemory(aReturn, SizeOf(aReturn));
  inherited;
  aContext.Route := DeleteSubPath('', Request.Path);
  aContext.Sender := Self;

  if Module.Domain<>'' then
  begin
    aDomain := Module.Domain;
    aPort := Module.Port;
  end
  else
    SpliteStr(Request.Header['Host'], ':', aDomain, aPort);

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
    aReturn.Respond := Respond;
    Respond.HttpResult := hrOK;
    aContext.Renderer := (Module as TUIWebModule).CreateRenderer;
    aContext.Writer := TmnwWriter.Create('html', Respond.Stream);
    try
      aContext.Renderer.IsSSL := Respond.Request.IsSSL;
      aContext.Renderer.Domain := aDomain;
      aContext.Renderer.Port := aPort;
      aContext.ETag := Request.Header['If-None-Match'];

      aReturn.SessionID := Request.GetCookie('', 'session');
      aReturn.Respond.HttpResult := hrOK;
      aReturn.Location := '';

      (Module as TUIWebModule).WebApp.Respond(aContext, aReturn);

      //SessionID
    finally
      FreeAndNil(aContext.Writer);
      aContext.Renderer.Free;
      aContext.Data.Free;
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
    TFile.Create(This, [ftResource], 'WebElements_CSS.css', 'WebElements.css');
    TFile.Create(This, [ftResource], 'WebElements_JS.js', 'WebElements.js');
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

procedure TAssetsSchema.DoCompose(const AContext: TmnwContext);
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
  WebApp.Assets.HomePath := HomePath;
  WebApp.Assets.ServeFiles := True;
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
  Result := TmnwHTMLRenderer.Create(Self, IsLocal);
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
  s := Classes.ToString;
  if s <> '' then
    s := 'class="'+s+'"';
  Result := Space(s, Attributes.ToString);
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

procedure TmnwHTMLRenderer.TSideBar.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TSideBar;
begin
  e := Scope.Element as THTML.TSideBar;
  Context.Writer.OpenTag('aside id="'+e.ID+'" class="sidebar navbar-expand-md text-bg-dark"');
  Context.Writer.OpenTag('nav id="' + e.ID + '-content' + '" class="sidebar-content fixed"');
  Context.Writer.OpenTag('div id="' + e.ID + '-items" class="sidebar-items p-2 offcanvas offcanvas-start text-bg-dark" data-bs-scroll="true" data-bs-backdrop="keyboard, static" tabindex="-1"');
  inherited;
  Context.Writer.CloseTag('div');
  Context.Writer.CloseTag('nav');
  Context.Writer.CloseTag('aside');
end;

{ THTML.TLink }

constructor THTML.TLink.Create(AParent: TmnwElement; const ALocation, ACaption: string);
begin
  inherited Create(AParent);
  Location := ALocation;
  FCaption := ACaption;
end;

initialization

finalization
  FreeAndNil(ElementRenderers);
{$ifdef rtti_objects}
  FreeAndNil(CacheClassObjects);
{$endif}
end.
