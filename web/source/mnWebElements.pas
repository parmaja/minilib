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
   Protocol UserInfo       Host                Path
    ┌─┴─┐   ┌──┴───┐ ┌──────┴──────────┐┌───────┴─────────────────────┐
GET https://john.doe@www.example.com:123/forum/user/ask/questions/q/10/?tag=networking&order=newest#top
└┬┘                  └──────┬──────┴─┬─┘└───────────────┬─────────────┘└────────────┬─────────────┘└─┬─┘
Method                    Domain    Port└──┬──────┘└┬──┘└───┬────┴──┬─┘           Query           Fragment
                                         BasePath Module   Schema  CurrentPath    └─┬─┘
WebElement:                              NameSpace                                Params
    └────────────┬──────────────────────┘ ─ ─ ┘        |              |     
    |          HostURL (From Request or Config)        |              |     
    └────────────────────────┬─────────────────────────┘              |
    |             HomeURL/URL (WebApp)                                |      
    └────────────────────────┬────────────────────────────────────────┘     
                            URL                                      

## Application ##
    
                    Document
┌──────────────────────┴───────────────────────┐
┌──────┬───────────────────────────────────────┐  ─┐
│>Logo │ Brand NavBar                       c =│   ├─ Header
├──────┴───────────────────────────────────────┤  ─┤
│ MenuBar                                      │   │
├────────────┬─────────────────────────────────┤   │
│  Sidebar   │ Main                            │   │
│    ─┬─     │                                 │   ├─ Container
│ Accordion  │ ┌─ TabControl ──────┐ ┌───────┐ │   │
│            │ │ Tab │ Tab │       │ │ Card  │ │   │
│            │ ├─────┴─────┴───────┤ ├───────┤ │   │
│            │ │ ┌─ Control ────┐  │ │       │ │   │
│            │ │ └──────────────┘  │ │       │ │   │
│            │ └───────────────────┘ └───────┘ │   │
│            │ ┌─ Form ────────────┐ ┌───────┐ │   │
│            │ │ ┌─ Control ────┐  │ │ Panel │ │   │
│            │ │ └──────────────┘  │ │       │ │   │
│            │ │ ┌─ Control ────┐  │ │       │ │   │
│            │ │ └──────────────┘  │ │       │ │   │
│            │ │ ┌──┐┌──┐          │ │       │ │   │
│            │ │ └──┘└──┘          │ │       │ │   │
│            │ │                   │ │       │ │   │
│            │ └───────────────────┘ └───────┘ │   │
│            │                                 │   │
├────────────┴── Footer ───────────────────────┤  ─┤
│                                              │   ├─ Footer
└──────────────────────────────────────────────┘  ─┘

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

{NOTICES
  NO MARGINE for `main`
}

{.$define LOG}
{$ifopt D+}
{$define LOCAL_RESOURCE}
{$endif}

{.$define Warn}

interface

uses
  Classes, SysUtils, StrUtils, DateUtils, Contnrs, Variants, Types, RTTI,
  {$ifdef FPC}
  resource, //* for RT_RCDATA
  {$endif}
  syncobjs, mnDON, mnJSON,
  mnUtils, mnClasses, mnStreams, mnStreamUtils, mnLogs, mnMIME, mnParams, mnTypes,
  mnMultipartData, mnModules, mnWebModules;

const
  cVersion = '1.85';

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

  TDirection = (dirUndefined, dirLeftToRight, dirRightToLeft);

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

  TLocationRelative = (
    toNone,
    toElement,
    toSchema,
    toHome,
    toDefault,
    toCustom
  );

  { TLocation }

  TLocation = record
    Where: TLocationRelative;
    Custom: string;
    WithQuery: Boolean;
    class operator Explicit(const Source: string): TLocation;
    class operator Implicit(Source : string) : TLocation;
    class operator Implicit(Source : TLocation): string;
    class operator Implicit(Source : TLocationRelative) : TLocation;
    function IsDefined: Boolean;
  end;

  TImageLocationType = (
    imgSymbol, 
    imgPath,
    imgMemory
  );

  { TImageLocation }

  TImageLocation = record
  private
    FLocation: TImageLocationType;
    FValue: string;

    FData: TBytes;
    FContentType: string;
    FFileDate: TDateTime;    
    function GetSymbol: string;
    function GetPath: string;
    procedure SetSymbol(const AValue: string);
    procedure SetPath(const AValue: string);
    procedure SetData(const Value: TBytes);
  public    
    property Location: TImageLocationType read FLocation;
    property Path: string read GetPath write SetPath;
    property Symbol: string read GetSymbol write SetSymbol;

    //TODO, Not rendered yet
    procedure LoadFromFile(const AFileName: string);
    property Data: TBytes read FData write SetData;
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

  TAttributeArea = (ssOuter, ssInner);
  TAttributeAreas = set of TAttributeArea;

  { TmnwAttribute }

  TmnwAttribute = class(TmnNameValueObject)
  public
    IsProperty: Boolean; //that dosnt have value
    Area: TAttributeArea;
    Used: Boolean;
    function CreateSubValues(vSeparators: TSysCharSet = [' ']): TStringList;
  end;


  TmnwScopeState = (
    sstSize //Size classes/attributes added
  );

  TmnwScopeStates = set of TmnwScopeState;

  { TmnwAttributes }

  TmnwAttributes = class(TmnNameValueObjectList<TmnwAttribute>)
  protected
    procedure Created; override;
  public
    function ToString(Area: TAttributeAreas = [ssOuter, ssInner]): string; reintroduce;
    function GetText: string;
    function AddProp(const Name: string; Area: TAttributeArea = ssOuter): TmnwAttribute;
    function Add(const Name: string; const Value: string = ''; Area: TAttributeArea = ssOuter): TmnwAttribute; overload;
    function AddIf(Condition: Boolean; const Name: string; const Value: string = ''; Area: TAttributeArea = ssOuter): TmnwAttribute; overload;
    procedure Delete(const Name: string); overload;
    function HaveSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function SetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function UnsetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    procedure Append(AAttributes: TmnwAttributes);
  end;

  TElementClass = record
  public
    Name: string;
    Area: TAttributeArea;
    Used: Boolean;
    constructor Create(AName: string; AArea: TAttributeArea);
  end;

  { TElementClasses }

  TElementClasses = record
    Items: TArray<TElementClass>;
    function IndexOf(const Name: string): Integer;
    function Exists(const Name: string): Boolean;
    //Add one item
    function Add(const Name: string; Area: TAttributeArea = ssOuter): Integer; overload;
    function AddIf(Condition: Boolean; const Name: string; Area: TAttributeArea = ssOuter): Integer; {$ifopt D-}inline;{$endif} overload;
    function AddIf(const Name: string; Area: TAttributeArea = ssOuter): Integer;  {$ifopt D-}inline;{$endif} overload;
    function Add(const AClass: TElementClass): Integer; overload;
    //Add multiple items in on string
    procedure Append(const S: string; Area: TAttributeArea = ssOuter); overload;
    procedure Append(A: TElementClasses); overload;
    function Remove(const Name: string): Boolean;
    //function ToString(const Initial: string = ''): string; overload;
    function ToString(Area: TAttributeAreas = [ssOuter, ssInner]): string; overload;
    function ToFullString(Area: TAttributeAreas = [ssOuter, ssInner]): string; overload;
    procedure Clear;

    class operator Add(A: TElementClasses; B: string): TElementClasses;
    class operator Subtract(A: TElementClasses; B: string): TElementClasses;
    class operator Explicit(const Source: string): TElementClasses;
    class operator Implicit(Source : string) : TElementClasses;
    class operator Implicit(Source : TElementClasses): string;
    procedure Init(classes: string = '');
  end;

  TmnwWeb = class;

  TmnwSession = class(TObject)
  private
    FID: string;
    FInstance: TObject;
    FChanged: Boolean;
    FDomain: string;
    FPath: string;
    FAge: Integer;
    procedure SetID(const Value: string);
    procedure SetDomain(const Value: string);
    procedure SetPath(const Value: string);
    procedure SetAge(const Value: Integer);
    
    procedure SetInteralInstance(const Value: TObject);
  public    
    constructor Create;
    procedure Reset;
    procedure SetInstance(const AInstance: TObject);
    property ID: string read FID write SetID;
    property Domain: string read FDomain write SetDomain;
    property Path: string read FPath write SetPath;
    property Age: Integer read FAge write SetAge;
    property Instance: TObject read FInstance write SetInteralInstance;
    property Changed: Boolean read FChanged;
  end;  

  { TmnwScope }

  TmnwScope = class(TmnObject)
  public
    Element: TmnwElement;
    Attributes: TmnwAttributes;
    Classes: TElementClasses;
    WrapClasses: TElementClasses; //WrapClass is a class used of what parent wrapped it
  public
    State: TmnwScopeStates;
    function ToString(Area: TAttributeAreas = [ssOuter, ssInner]; WithSpace: Boolean = False): string; overload;
    function ToString(WithSpace: Boolean): string; overload;

    constructor Create(AElement: TmnwElement);
    procedure Free;
  end;

  TmnwLibraryClass = class of TmnwLibrary;

  TmnwContext = record
  private
    FResponse: TmnwResponse;
    FWeb: TmnwWeb;
    FRenderer: TmnwRenderer;
    FWriter: TmnTidyWriter;
    function GetDomain: string;
    function GetPort: string;
    function GetSession: TmnwSession;
    function GetRequest: TwebRequest;
  public
    Sender: TObject;

    Schema: TmnwSchema;    
    Element: TmnwElement;
    
    ParentRenderer: TmnwElementRenderer;
    //
    Data: TDON_Value;
    // For
    CurrentPath: string;   

    Language: string;
    Direction: TDirection;    

    property Domain: string read GetDomain;
    //Need to review
    property Port: string read GetPort;

    // http://host:80/
    function GetHostURL: string; overload;
    // /basepath/module/
    function GetBasePath: string; overload;
    // http://host:80/basepath/module/
    function GetHomeURL: string; overload;
    // http://host:80/basepath/module/schema
    function GetSchemaURL: string; overload;

    function GetRequestURL: string;

    // With Schema
    // /basepath/module/schema
    function GetPath: string; overload;
    // /module/basepath/schema/element    
    function GetPath(e: TmnwElement): string; overload;    

    //this get absolute path http://host:80/module/basepath/schema/element
    function GetURL(e: TmnwElement): string; overload;
    //this get absolute path http://host:80/module/basepath/schema
    function GetURL: string; overload;

    //this get path relative requested path /element1/element2
    function GetRelativePath(e: TmnwElement): string; overload;    

    // http://host:80/default_schema/basepath/
    function GetDefaultPath: string; overload;    
    //Schema URL with http://host:80/basepath/assets/schema
    function GetAssetsPath: string;
    function GetAssetsURL: string;
    //Dir of PublicPath of assets
    function GetAssetDir: string;
    function GetLocationPath(AElement: TmnwElement; Location: TLocation): string; overload;
    
    property Request: TwebRequest read GetRequest;
    property Response: TmnwResponse read FResponse;
    property Session: TmnwSession read GetSession;
    property Web: TmnwWeb read FWeb;
    property Renderer: TmnwRenderer read FRenderer;
    property Writer: TmnTidyWriter read FWriter;

    function _T(const Key: string; const Default: string = ''): string;
  public
    procedure Require(ALibraryClass: TmnwLibraryClass; Priority: Integer = 0);
  end;

  TmnwObject = class(TmnNamedObject);

  TLibraryOption = (
    libDefer, 
    libCross
  );
  
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
  
  TmnwLibrarySource = class(TmnNamedObject)
  public    
    SourceType: TLibrarySourceType;
    Where: TLibrarySourceWhere;
    OnlineFile: string;
    LocalFile: string;    
    Text: string; //For Embed
    Integrity: string;
    Direction: TDirection;
    Language: string;
    Options: TLibraryOptions;    
    constructor Create; virtual;
  end;

  TLibrarySources = class(TmnNamedObjectList<TmnwLibrarySource>)
  private
  public
    //LocalFile: from assets, only file name, not with path
    //OnlineFile: if OnlineFile ended with / LocalFile will added
    function Add(SourceType: TLibrarySourceType; Where: TLibrarySourceWhere; const OnlineFile, LocalFile: string; Integrity: string = ''; Options: TLibraryOptions = [libDefer, libCross]): TmnwLibrarySource; overload;
    function AddEmbed(const SourceType: TLibrarySourceType; const AName: string; const EmbedText: string): TmnwLibrarySource; overload;

    function Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFile: string; Direction: TDirection = dirUndefined): TmnwLibrarySource; overload;
    function Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFile: string; Integrity: string; Options: TLibraryOptions = [libDefer, libCross]): TmnwLibrarySource; overload;

    function AddStyle(const EmbedText: string; AName: string; Direction: TDirection = dirUndefined): TmnwLibrarySource; overload;
  end;
  
  TmnwLibrary = class abstract(TmnNamedObject)
  private
    FPriority: Integer;
    FDependsOn: TmnwLibrary;
    FSources: TLibrarySources;
  protected
    function CheckOffline(const Ctx: TmnwContext; const FileName: string): Boolean;
    procedure Created; override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
     
    procedure AddHead(const Ctx: TmnwContext); virtual;
    
    property Priority: Integer read FPriority write FPriority;
    property DependsOn: TmnwLibrary read FDependsOn write FDependsOn;
    property Sources: TLibrarySources read FSources;
  end;

  { TmnwLibraries }

  TmnwLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  private
    FLock: TMREWSync;
  public
    constructor Create; virtual;
    destructor Destroy; override;     
    function Find(ALibraryName: string): TmnwLibrary; overload;
    function Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary; overload;
    function RegisterLibrary(ALibraryClass: TmnwLibraryClass; Priority: Integer = 0): TmnwLibrary; overload;
    property Lock: TMREWSync read FLock;
  end;

  TmnwRequires = class(TmnNamedObjectList<TmnwLibrary>)
  protected
    function Compare(Item1, Item2: TmnwLibrary): Integer; override;
  public    
    function Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary; overload;

    procedure Use(ALibraryClass: TmnwLibraryClass); overload;
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

  TDarklyTheme_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

  TCustomTheme_Library = class(TmnwLibrary)
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
  TmnwShadow = (shadowUndefined, shadowHairline, shadowThin, shadowThick, shadowEnd, ShadowBottom);

  TmnwAlign = (alDefault, alFirst, alCenter, alStreach, alLast);
  TmnwJustify = (jstDefault, jstStart, jstCenter, ralBetween, jstAround, jstEvenly, jstEnd);

  TmnwFixed = (fixedNone, fixedTop, fixedBottom, fixedStart, fixedEnd);

  TmnwSticky = (stickyNone, stickyTop, stickyBottom);

  TGap = 0..5;

  TGapHelper = record helper for TGap
    function ToString: string;
  end;

  TColCount = 0..12;

  TColCountHelper = record helper for TColCount
    function ToString: string;
  end;

  TWidthSize = (
        szUndefined,
        szExtraSmall,
        szVerySmall,
        szSmall,
        szMedium,
        szLarge,
        szVeryLarge,
        szExtraLarge
    );

  TWidth = 0..6;

  TWidthHelper = record helper for TWidth
    function ToString: string;
  end;

  TColSizeKind = (cskUndefined, cskNumber, cskAuto, cskFit, cskMax);

  TColSizeNumber = 0..12;
  TColSize = record
    Kind: TColSizeKind;
    Columns: TColSizeNumber;
    Max: TWidthSize;
    class operator Explicit(const Source: Integer): TColSize;
    class operator Explicit(const Source: TWidthSize): TColSize;
    class operator Implicit(const Source: TColSize): Boolean;
    class operator Implicit(Source : Integer) : TColSize;
    class operator Implicit(Source : TWidthSize) : TColSize;
    class operator Implicit(Source : TColSize): Integer;
    class operator Implicit(Source : TColSizeKind) : TColSize;
  end;

  TmnwBindAction = (
    bindNone, //Only for master
    bindVisible,
    bindEnabled
  );

  TmnwBind = record
    //Group name, only under this group
    Group: string;
    //What action to do
    Action: TmnwBindAction;
  end;

  //Keep it as DoRespond form
  TRespondProc = reference to procedure (const Ctx: TmnwContext);
  TRenderProc = reference to procedure(Scope: TmnwScope; const Ctx: TmnwContext);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FHandle: THandle;
    FVisible: Boolean;
    FSchema: TmnwSchema;
    FParent: TmnwElement;

    FRoute: String;
    FComment: String;
    FID: String;
    FName: String;
    FKind: TmnwElementKinds;
    FPriority: TmnwPriority;
    FState: TmnwElementState;
    FOnExecute: TElementExecute;
    FOnRespond: TRespondProc;
    FPrepared: Boolean;
    FIsRoot: Boolean;
    FTimeStamp: Int64;
    FData: String;
    FEndRoute: Boolean;
    procedure SetState(const AValue: TmnwElementState);
    function GetRespondIt: Boolean;
    function GetRenderIt: Boolean;
    procedure SetRenderIt(const Value: Boolean);
    procedure SetOnRespond(const Value: TRespondProc);
  protected    
    function GetRoute: String; virtual;

    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;

    procedure ServeDir(APath: string; Options: TmodServeFiles; const Ctx: TmnwContext);
    function ServeFile(PublicPath: string; DefaultDocuments: TStringList; Options: TmodServeFiles; const Ctx: TmnwContext): Boolean; overload;
    function ServeFile(PublicPath: string; Options: TmodServeFiles; const Ctx: TmnwContext): Boolean; overload;

    procedure DoRequired(const Ctx: TmnwContext); virtual;
    procedure DoPrepare; virtual;
    
    procedure DoCompose(const Ctx: TmnwContext); virtual;
    procedure DoComposed; virtual;

    procedure DoRespondHeader(const Ctx: TmnwContext); virtual;
    procedure DoRespond(const Ctx: TmnwContext); virtual;

    procedure PrepareRenderer(const Ctx: TmnwContext);
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

    function GetPathClasses: string;
    
    function CreateRenderer(const Ctx: TmnwContext): TmnwElementRenderer;
    procedure Compose(const Ctx: TmnwContext); virtual;
    procedure AddState(AState: TmnwElementState);
    procedure RemoveState(AState: TmnwElementState);

    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif} //* see TmnObjectList
    function CountComposed: Integer;

    function GetContentType(Route: string = ''): string; virtual;

    procedure RespondInit(const Ctx: TmnwContext);
    procedure Respond(const Ctx: TmnwContext);

    //* Original Render
    procedure Render(const Ctx: TmnwContext); overload;

    function CanRender: Boolean; virtual;

    property IsRoot: Boolean read FIsRoot write FIsRoot;

    property ID: String read FID write FID;
    property Name: String read FName write FName;
    property Data: String read FData write FData;
    property Route: String read GetRoute write FRoute; 
    property Comment: String read FComment write FComment;

    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;

    property RespondIt: Boolean read GetRespondIt; // false: do not use respond
    property RenderIt: Boolean read GetRenderIt write SetRenderIt;

    property Kind: TmnwElementKinds read FKind write FKind;
    property Priority: TmnwPriority read FPriority write FPriority;
    property State: TmnwElementState read FState write SetState;

    property OnExecute: TElementExecute read FOnExecute write FOnExecute;
    property OnRespond: TRespondProc read FOnRespond write SetOnRespond;
    property Handle: THandle read FHandle;

    property TimeStamp: Int64 read FTimeStamp;
    //* FindRoute stop at this element, NOT TESTED YET
    property EndRoute: Boolean read FEndRoute write FEndRoute;
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
    procedure SetTerminated; virtual;
    procedure Terminate; 
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
    FLock: TMREWSync;
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
    property Lock: TMREWSync read FLock;
  end;

  TmnwSchemaCapability = (
    schemaStatic, //* Not deleted when restart server
//    schemaDynamic,  //* dynamic, do not add it to the list, not cached, becareful
    schemaSession,
    schemaStartup, //* Create it when registered
    schemaAttach //Allow/Accepts websocket connections, Interactive also allow websocket
  );

  TmnwSchemaCapabilities = set of TmnwSchemaCapability;

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
    FWeb: TmnwWeb;
    FPhase: TmnwSchemaPhase;
    FNamingLastNumber: THandle;
    FPublicPath: string;

    FInternalLock: TCriticalSection; //Can be nil
    function GetReleased: Boolean;
    procedure SetDefaultDocuments(AValue: TStringList);
    procedure SetPublicPath(const Value: string);
  protected
    Usage: Integer;
    procedure UpdateAttached;
    class procedure Registered; virtual;
    procedure DoRespond(const Ctx: TmnwContext); override;
    procedure DoAccept(var Ctx: TmnwContext; var Resume: Boolean); virtual;
    procedure DoPrepare; override;
    procedure DoChildRespond(AElement: TmnwElement; const Ctx: TmnwContext); virtual;
    procedure AttachedMessage(const s: string); virtual; //from websocket
    procedure InteractiveMessage(const s: string);
    property DefaultDocuments: TStringList read FDefaultDocuments write SetDefaultDocuments;
  public
    Reference: string; //To find it
    LastAccess: TDateTime;
    IsManual: Boolean;
    RefreshInterval: Integer; //* in seconds, for refresh elements that need auto refresh
    ServeFiles: TmodServeFiles;
    Interactive: Boolean;
    constructor Create(AWeb: TmnwWeb; AName:string; ARoute: string = ''); reintroduce;
    destructor Destroy; override;

    class function GetCapabilities: TmnwSchemaCapabilities; virtual;
    function GetPublicPath: string; virtual;
    function NewHandle: THandle;

    property PublicPath: string read GetPublicPath write SetPublicPath;

    procedure Enter; //Lock if lock not nil (Static)
    procedure Leave;

    //* Attaching cap
    //function Interactive: Boolean;

    procedure Start; virtual;
    function Accept(var Ctx: TmnwContext): Boolean;
    procedure Compose(const Ctx: TmnwContext); override;

    // Executed from a thread of connection of WebSocket, it stay inside until the disconnect or terminate
    procedure Attach(Route: string; Sender: TObject; AStream: TmnBufferStream); // in connection thread

    property Attachments: TmnwAttachments read FAttachments;
    property Attached: Boolean read FAttached;
    property Released: Boolean read GetReleased;
    property Phase: TmnwSchemaPhase read FPhase;
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
      procedure DoRespond(const Ctx: TmnwContext); override;
    public
      FileName: string;
      Options: TFileOptions;
      constructor Create(AParent: TmnwElement; AOptions: TFileOptions = []; AFileName: string = ''; ARoute: string = ''); reintroduce;
      function GetContentType(Route: string): string; override;
    end;

    { TMemory }

    [TID_Extension]
    TMemory = class(TmnwElement)
    private
      ContentType: string;
      FData: TMemoryStream;
    protected
      procedure DoRespond(const Ctx: TmnwContext); override;
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
    function CanRenderChilds: Boolean; virtual;
    procedure RenderChilds(Scope: TmnwScope; Ctx: TmnwContext);

    //This function called one time
    procedure AddHead(const Scope: TmnwScope; const Ctx: TmnwContext); virtual;
    //* Called to parent to wrap the child rendering, each chiled will wrap it with this render
    //* This method exists in parent render
    //* Keep `var`
    procedure DoEnterChildRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;
    procedure DoLeaveChildRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;

    //* Called only if have parent but exists in a child
    procedure DoEnterOuterRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;
    procedure DoLeaveOuterRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;

    //* Keep `var` to allow descents child takes new attributes
    procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); virtual;
    //* Content render
    //Scope will not inherited to descents child
    procedure DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;
    procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;
    procedure DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext); virtual;

    property Renderer: TmnwRenderer read FRenderer;
    property RendererRegister: TmnwElementRendererRegister read FRendererRegister;
  public
    procedure Render(AElement: TmnwElement; const Ctx: TmnwContext);
    constructor Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwElementRendererRegister); virtual; //useful for creating it by RendererClass.Create
    procedure CollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
  end;

  TmnwElementRendererClass = class of TmnwElementRenderer;

  { TmnwRenderer }

  TmnwRenderer = class abstract(TmnwObject)
  private
    FModule: TmodWebModule;
    FRequires: TmnwRequires;
    FParams: TmnwAttributes;
  protected
    {$ifdef rtti_objects}
    procedure RegisterClasses(ASchemaClass: TmnwSchemaClass);
    {$endif}
    procedure DoBeginRender; virtual;
    procedure DoEndRender; virtual; 
    
    procedure Created; override;          
  public
    constructor Create(AModule: TmodWebModule); virtual;
    destructor Destroy; override;

    class function ElementRenderers: TmnwElementRenderers; virtual; abstract; 
    class function RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = False): TmnwElementRendererRegister;

    class procedure RegisterElements; virtual;
    
    procedure BeginRender;
    procedure EndRender;
    
    function CreateRenderer(AElementClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property Params: TmnwAttributes read FParams;
    property Requires: TmnwRequires read FRequires;
    property Module: TmodWebModule read FModule;

    procedure Require(ALibraryClass: TmnwLibraryClass); overload;
    procedure AddHead(const Ctx: TmnwContext); virtual;
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

  TmnwHTMLRenderer = class(TmnwRenderer)
  private
    class constructor Register;
  protected
  public
    type
      { TElement }

      THTMLElement = class abstract(TmnwElementRenderer)
      protected         
        procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;
    
      { TComment }

      TComment = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;   

      { TJSScript }

      TJSScript = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TFile }

      TFile = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TJSFile }

      TJSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TCSSFile }

      TCSSFile = class(TFile)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      //* Write at render time
      TOutput = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TCompose }

      //* Dynamic compose at render time with fake parent
      TCompose = class(THTMLElement)
      protected
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TIntervalCompose }

      TIntervalCompose = class(TCompose)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
      end;
    
      { TDocument }

      TDocument = class(THTMLElement)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;

      { TBody }

      TBody = class(THTMLElement)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
        procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
        procedure DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
      end;
    
    class procedure RegisterElements; override;
  end;
  
  { TmnwRegisterdSchema }

  TmnwRegisterdSchema = class(TmnNamedObject)
  public
    SchemaClass: TmnwSchemaClass;
    destructor Destroy; override;
  end;

  TRegisteredSchemas = class(TmnNamedObjectList<TmnwRegisterdSchema>)
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
    FPublicPath: string;
    FAppPath: string;
    FPrivatePath: string;
    FAssets: TAssetsSchema;
    FDefaultSchema: TmnwRegisterdSchema;
    FShutdown: Boolean;
    FLock: TMREWSync;
    FRegistered: TRegisteredSchemas;
    FTimeStamp: Int64;
    FOnlineFiles: TOnlineFiles;
    FLanguage: string;
    FVersion: string;
    FShowVersion: Boolean;
    procedure SetLanguage(const Value: string);
  protected
    procedure SchemaCreated(Schema: TmnwSchema); virtual;
    procedure Created; override;
    procedure CleanSchemas;

    procedure DoCompose(const Ctx: TmnwContext); virtual;

  public
    Started: Boolean;

    IsSecure: Boolean;
    Domain: string; //localhost
    Port: string;
    
    ModuleName: string; //Module Name

    CompactMode: Boolean;
    SessionAge: Integer; //* in ms
    PasswordToken: string;

    //SchameName if root path requested without schema = ''
    FallbackTo: string;
    JWTmode: Boolean;

    constructor Create;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;

    function RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass; AsDefaultSchema: Boolean = False): TmnwSchema;
    property Registered: TRegisteredSchemas read FRegistered;

    function FindBy(const aSchemaName: string; const aSessionID: string): TmnwSchema;
    function CreateSchema(const aSchemaName: string): TmnwSchema; overload;
    function CreateSchema(const SchemaClass: TmnwSchemaClass; AName: string): TmnwSchema; overload;
    function CreateSchema(SchemaItem: TmnwRegisterdSchema): TmnwSchema; overload;
    function ReleaseSchema(const aSchemaName: string; aSessionID: string): TmnwSchema;
    
    function InquireElement(var Ctx: TmnwContext; FindNested: Boolean): Boolean;
    //for HTML
    procedure Respond(var Ctx: TmnwContext);
    //for WebSocket
    function Attach(var Ctx: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;

    property Lock: TMREWSync read FLock;
    property Assets: TAssetsSchema read FAssets;
    property DefaultSchema: TmnwRegisterdSchema read FDefaultSchema;
    //Public Web Files
    property PublicPath: string read FPublicPath write FPublicPath;
    //Private Files
    property PrivatePath: string read FPrivatePath write FPrivatePath;
    //Exe path //TODO do really need it?
    property AppPath: string read FAppPath write FAppPath;
    property Shutdown: Boolean read FShutdown;
    property Options: TmnwAppOptions read FOptions write FOptions;
    property OnlineFiles: TOnlineFiles read FOnlineFiles write FOnlineFiles;
    property Language: string read FLanguage write SetLanguage;
    property TimeStamp: Int64 read FTimeStamp;
    property Version: string read FVersion write FVersion;
    property ShowVersion: Boolean read FShowVersion write FShowVersion;
  end;

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

      { TJSScript }
      
      TJSScript = class(THTMLElement)
      public
        Script: string;
        constructor Create(AParent: TmnwElement; AScript: string); reintroduce;
      end;
      
      { THTMLComponent }

      THTMLComponent = class abstract(THTMLElement)
      protected
        procedure Created; override;
      public
        Active: Boolean;
        Disabled: Boolean;
      end;

      { THTMLControl }

      THTMLControl = class abstract(THTMLComponent)
      protected
        procedure Created; override;
      public
//        Width: TWidth;
        Size: TColSize;
        Shadow: TmnwShadow;
        Hint: string;
        Style: TItemStyle;
        Bind: TmnwBind;
      end;

      TmnwLabelLayout = (lfUndefined, lfSide, lfAbove, lfFloating);

      TCustomFormControl = class abstract(THTMLControl)
      public
        Required: Boolean;
      end;

      THTMLFormControl = class abstract(TCustomFormControl)
      private
        FCaption: string;
        procedure SetCaption(const AValue: string);
      public
        //* Layout of the caption label: clTop (above input) or clSide (left of input)
        LabelLayout: TmnwLabelLayout;
        property Caption: string read FCaption write SetCaption;
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
        procedure DoRespond(const Ctx: TmnwContext); override;
      public
        PublicPath: string;
        ServeFiles: TmodServeFiles;
        function GetContentType(Route: string): string; override;
      end;

      TFolder = class(THTMLElement)
      protected
        procedure DoRespond(const Ctx: TmnwContext); override;
      public
        PublicPath: string;
        ServeFiles: TmodServeFiles;
        function GetContentType(Route: string): string; override;
      end;

      { TOutput }

      //* Write at render time
      TOutput = class(THTMLElement)
      protected
      public
        OnOutput: TRenderProc;
        constructor Create(AParent: TmnwElement; AOnOutput: TRenderProc = nil); reintroduce;
      end;

      TComposeProc = reference to procedure(Inner: TmnwElement; const Ctx: TmnwContext);

      { TCompose }

      TCompose = class(THTMLElement)
      protected
        type

          { TInner}

          TInner = class(TmnwElement)
          public
          end;

        procedure InnerCompose(Inner: TmnwElement; const Ctx: TmnwContext); virtual;

        procedure DoRespond(const Ctx: TmnwContext); override;
      public
        OnCompose: TComposeProc;
        constructor Create(AParent: TmnwElement; AOnCompose: TComposeProc = nil); reintroduce;
      end;

      [TID_Extension]
      [TRoute_Extension]
      TIntervalCompose = class(TCompose)
      public
        Code: string;
      end;

      { TDocument }

      TDocument = class(TAssets)
      private
        FTitle: string;
        FBody: TBody;
      protected
        procedure Created; override;
      public
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

      TDropdownOptions = set of (dropArrow, dropSplit, dropEnd);

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
        FLogo: TImageFile;
      public
        Fixed: TmnwFixed;
        Title: string;
//        LogoImage: string;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKinds =[]); override;
        destructor Destroy; override;
        property Logo: TImageFile read FLogo;
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
        Theme: TTheme; //Deprecated
        function CanRender: Boolean; override;
      end;

      TMain = class(THTMLElement)
      protected
        procedure Created; override;
      public
      end;

      THTMLLayout = class abstract(THTMLElement)
      protected
        procedure Created; override;
      public
        Size: TColSize;
        Gap: TGap;
        Padding: Integer;
      end;

      TBox = class(THTMLLayout)
      protected
        procedure Created; override;
      public
        Columns: TColCount;
      end;

      TBar = class(THTMLLayout)
      protected
        procedure Created; override;
      public
        Fixed: TmnwFixed;
        Wrap: Boolean;
        AlignItems: TmnwAlign;
        JustifyItems: TmnwJustify;
      end;

      TRow = class(THTMLLayout)
      protected
        procedure Created; override;
      public
        Fixed: TmnwFixed;
        Wrap: Boolean;
        AlignItems: TmnwAlign;
        JustifyItems: TmnwJustify;
      end;

      TColumn = class(THTMLLayout)
      public
        Fixed: TmnwFixed;
        Reverse: Boolean;
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

      TAccordion = class(THTMLElement)
      protected
        procedure Created; override;
      public
        AlwaysOpen: Boolean;
      end;

      [TID_Extension]
      TAccordionSection = class(THTMLElement)
      protected
      public
        Image: TImageLocation;
        Caption: string;
        Expanded: Boolean;
        SaveState: Boolean;
        function CanRender: Boolean; override;
      end;

      TAccordionItem = class(TClickable)
      public
      end;

      TPanelMode = (emdUndefined, emdColumn, emdRow);

      { TPanel }

      TCustomPanel = class(THTMLControl)
      protected
        procedure Created; override;
      public
        Direction: TDirection;
        Gap: TGap;
        Solitary: Boolean; //* Single in Row
        AlignItems: TmnwAlign;
//        JustifyItems: TmnwJustify;
        NoWrap: Boolean;
        Mode: TPanelMode;
      end;

      TPanel = class(TCustomPanel)
      public
        Sticky: TmnwSticky;
      end;

      { TCard }

      TCardHeader = class(THTMLElement)
      public
      end;

      TCardFooter = class(THTMLElement)
      public
        Sticky: Boolean;
      end;

      [TID_Extension]
      TCard = class(TCustomPanel)
      private
        FHeader: TCardHeader;
        FFooter: TCardFooter;
      protected
        procedure Created; override;
      public
        Caption: string;
        Collapse: Boolean;
        constructor Create(AParent: TmnwElement; AKind: TmnwElementKinds =[]); override;
        property Footer: TCardFooter read FFooter;
        property Header: TCardHeader read FHeader;
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
        procedure DoCompose(const Ctx: TmnwContext); override;
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

      { TPopupMenu }

      [TID_Extension]
      TPopupMenu = class(THTMLControl)
      protected
        procedure Created; override;
      public
        Items: TStringList;
        Caption: string;
        Image: TImageLocation;
        destructor Destroy; override;
      end;

      TDropdownItem = class(TLink)
      private
      protected
      public
      end;

      { THTMLGroup }

      THTMLGroup = class(THTMLControl)
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

      TGroup = class(THTMLGroup)        
      end;
      
      { TForm }

      [TID_Extension]
      [TRoute_Extension]
      TForm = class(THTMLElement)
      private
      protected
        procedure DoRespondHeader(const Ctx: TmnwContext); override;
        procedure Created; override;
        procedure DoComposed; override;
      public
        type
          TFormButton = record
            Caption: string;
          end;
      public
        Gap: TGap;
        Endpoint: TLocation;
        CancelTo: TLocation;
        CallScript: string;

        //RedirectTo: TLocation;
        RedirectTo: string;

        Submit: TFormButton;
        Reset: TFormButton;
        Cancel: TFormButton;
      end;

      TParagraph = class(THTMLElement)
      public
        Text: string;
        constructor Create(AParent: TmnwElement; AText: string = ''); reintroduce;
      end;

      THeadingStyle = set of (hsMuted);
      THeadingLevel = 1..6;

      THeading = class(THTMLElement)
      public
        Level: THeadingLevel;
        Text: string;
        Style: THeadingStyle;
        constructor Create(AParent: TmnwElement; ALevel: Integer; AText: string = ''; AStyle: THeadingStyle = []); reintroduce;
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
        procedure DoRespond(const Ctx: TmnwContext); override;
      public        
        procedure Loop; virtual;
        constructor Create(AParent: TmnwElement; AName: string; ARoute: string = ''; ActionProc: TRespondProc = nil); reintroduce; overload;
      end;

      TSpan = class(THTMLElement)
      public
        Text: string;
        constructor Create(AParent: TmnwElement; const AText: string); reintroduce;
      end;

      TBadge = class(THTMLElement)
      public
        Text: string;
        Style: TItemStyle;
        constructor Create(AParent: TmnwElement; const AText: string; AStyle: TItemStyle); reintroduce;
      end;

      TSpanButton = class(TSpan)
      public
      end;
      
      { TCustomButton }

      TCustomButton = class(TClickable)
      private
      protected
        procedure Created; override;
      public
        CallScript: string;
        ConfirmMessage: string;
        Outline: Boolean;
        constructor Create(AParent: TmnwElement; const ACaption: string); reintroduce; overload;
      end;

      TButton = class(TCustomButton)
      end;

      TToolButton = class(TButton)
      end;

      TThemeButton = class(TToolButton)
      protected
        procedure Created; override;
      public
      end;

      TFormButton = class(TCustomButton)
      public
        FormID: string;
      end;
      
      TSubmitForm = class(TFormButton)
      public
      end;

      TResetForm = class(TFormButton)
      public
      end;

      TLinkButton = class(TFormButton) //TODO
      public
        Location: string;
        constructor Create(AParent: TmnwElement; const ALocation: string; const ACaption: string); reintroduce; overload;
      end;

      TActionForm = class(TFormButton)
      public
        Action: string;
      end;

      TCookieButton = class(TCustomButton)
      public
        Value: string;
      end;     

      TCookieButtons = class(TGroupButtons)
      protected
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

      { TCustomInput }

      [TID_Extension]
      TCustomInput = class(THTMLFormControl)
      private
        FValue: string;
        procedure SetValue(const AValue: string);
      protected
        procedure Created; override;
        procedure ReceiveMessage(JSON: TDON_Pair); override;
      public
        PlaceHolder: string;
        EditType: string;
      public
        property Value: string read FValue write SetValue;
      end;

      TInput = class(TCustomInput)      
      public
        AutoFocus: Boolean;
        AutoComplete: Boolean; //default True
        constructor Create(AParent: TmnwElement; ACaption: string = ''; AValue: string = ''); reintroduce;      
      end;

      [TID_Extension]
      TUsername = class(TInput)
      protected
        procedure Created; override;
      public
      end;

      { TInputPassword }

      [TID_Extension]
      TPassword = class(TInput)
      protected
      public
        Token: string;
      end;

      TNewPassword = class(TPassword)
      protected
      public
      end;      

      { TIntegerInput }

      [TID_Extension]
      TIntegerInput = class(TInput)
      protected
      public
      end;

      { TCountInput }

      [TID_Extension]
      TCountInput = class(TIntegerInput)
      protected
        procedure Created; override;
      public
        Min: Integer; //Default 0
        Max: Integer; //Default 100
      end;

      { TDateInput }

      [TID_Extension]
      TDateInput = class(TInput)
      protected
      public
      end;

      { TTimeInput }

      [TID_Extension]
      TTimeInput = class(TInput)
      protected
      public
      end;

      { TDateTimeInput }

      [TID_Extension]
      TDateTimeInput = class(TInput)
      protected
      public
      end;

      { TMaskInput }

      //* A text input that formats the value while typing
      //* Format is rendered as data-mask attribute, e.g. '99/99/9999' (date), '00:00' (time),
      //* '(999) 999-9999' (phone), '999999.99' (number) or a preset name: date, time, datetime, phone, number, zip
      //* '9' required digit, '0' optional digit, '#' optional digit, 'A' required letter,
      //* '*' required alphanumeric, '.' and ',' decimal separator slots
      //* Time masks use unit letters with ':' separators: 'hh' 12-hour (01-12),
      //* 'HH' 24-hour (00-23), 'mm' minutes (00-59), 'ss' seconds (00-59),
      //* e.g. 'hh:mm', 'HH:mm:ss', 'mm:ss'
      [TID_Extension]
      TMaskInput = class(TInput)
      private
        FFormat: string;
      public
        property Format: string read FFormat write FFormat;
      end;

      { TSelect }

      [TID_Extension]
      TSelect = class(THTMLFormControl)
      private
        FItems: TmnNameValueObjectList<TmnNameValueObject>;
        FSelectedValue: string;
      protected
        procedure Created; override;
      public
        Multiple: Boolean;
        ChangeScript: string;
        destructor Destroy; override;
        property Items: TmnNameValueObjectList<TmnNameValueObject> read FItems;
        //* Value of the selected option
        property SelectedValue: string read FSelectedValue write FSelectedValue;
      end;

      { TTextArea }

      [TID_Extension]
      TTextArea = class(THTMLFormControl)
      public
        Text: string;
        Rows: Integer;
        constructor Create(AParent: TmnwElement; ACaption: string = ''; AText: string = ''); reintroduce;
      end;

      { TCheckbox }

      [TID_Extension]
      TCheckbox = class(TCustomFormControl)
      private
        FChecked: Boolean;
        FValue: string;
      protected
        procedure Created; override;
      public
        Caption: string;
        property Checked: Boolean read FChecked write FChecked;
        //* Value submitted when checked, default 'true'
        property Value: string read FValue write FValue;
      end;

      [TName_Extension]
      THiddenInput = class(THTMLElement)
      protected
      public
        Value: string;
        constructor Create(AParent: TmnwElement; const AName: string; const AValue: string = ''); reintroduce;
      end;      

      TCustomImage = class(THTMLComponent)
      public
        AltText: string;
        //Width, Height: double;
      end;

      [TID_Extension]
      TImage = class(TCustomImage)
      protected
        procedure DoCompose(const Ctx: TmnwContext); override;
      public
        Source: TLocation;
      end;

      { TImageFile }

      [TRoute_Extension]
      TImageFile = class(TCustomImage)
      private
        FFileName: string;
        procedure SetFileName(const Value: string);
      protected
        function GetRoute: String; override;
        procedure DoRespond(const Ctx: TmnwContext); override;
      public        
        function CanRender: Boolean; override;
        function GetContentType(Route: string): string; override;
        property FileName: string read FFileName write SetFileName;
      end;

      { TImageMemory }

      [TRoute_Extension]
      TImageMemory = class(TCustomImage)
      private
        FData: TMemoryStream;
      protected
        procedure DoRespond(const Ctx: TmnwContext); override;
      protected
        function GetRoute: String; override;
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

      THorzLine = class(THTMLElement)
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
    function Replace(AElementClass: TmnwElementClass; ReplaceWith: TmnwElementRendererRegister): Integer;
    function FindByParents(AElementClass: TmnwElementClass): TmnwElementRendererRegister;
    function RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; ReplaceIfExists: Boolean = False): TmnwElementRendererRegister; overload;
  end;

  TmnwRendererRegister = class(TmnNamedObject)  
  public    
    RendererClass: TmnwRendererClass;
  end;  
  
  TmnwRenderers = class(TmnNamedObjectList<TmnwRendererRegister>)
  private
    FCurrent: TmnwRendererRegister;
    FStarted: Boolean;
  public
    function RegisterRenderer(AName: string; ARendererClass: TmnwRendererClass): TmnwRendererRegister; overload;
    function FindBy(ARendererClass: TmnwRendererClass): TmnwRendererRegister; overload;
    procedure Switch(AName: string); overload;
    procedure Switch(ARendererClass: TmnwRendererClass); overload;
    property Current: TmnwRendererRegister read FCurrent;
    property Started: Boolean read FStarted;
  end;
  
  TmnwResponse = class(TwebResponse)
  private
    FSession: TmnwSession;
  protected
    procedure DoSetCookies; override;     
    procedure DoSendHeader; override;     
    
  public    
    constructor Create(ARequest: TmodRequest); override;
    destructor Destroy; override;     
    property Session: TmnwSession read FSession;
  end;

  { TAssetsSchema }

  TAssetsSchema = class(TmnwSchema)
  private
    FLogoFile: string;
  protected
    //FLogo: THTML.TMemory;  
    procedure Created; override;
    procedure DoRespond(const Ctx: TmnwContext); override;
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
    procedure Start; override;
    //property Logo: THTML.TMemory read FLogo;
    property LogoFile: string read FLogoFile write FLogoFile;
    function GetPublicPath: string; override;
  end;

  //Return error as json if fail with message of error, so we need JS to post
  TAuthForm = class(THTML.THTMLItem)
  private
    FForm: THTML.TForm;
  protected
    procedure DoCompose(const Ctx: TmnwContext); override;
    procedure Created; override;
  public
    JWTMode: Boolean;
    property Form: THTML.TForm read FForm;
  end;

  TAuthSchema = class(THTML)
  private
    FAuth: TAuthForm;
    FLoginCard: THTML.TCard;
  public
  protected
    procedure DoLogin(const Ctx: TmnwContext; var Success: Boolean; var Message: string; var SessionID: string); virtual; //use `var` no `out` because `inherited` reset it
    procedure DoLogout(const Ctx: TmnwContext); virtual;

    procedure UserLogin(const Ctx: TmnwContext);
    procedure UserLogout(const Ctx: TmnwContext);
    
    procedure DoChildRespond(AElement: TmnwElement; const Ctx: TmnwContext); override;
    procedure DoRespondHeader(const Ctx: TmnwContext); override;
    procedure DoCompose(const Ctx: TmnwContext); override;
    procedure Created; override;     
  public
    property Auth: TAuthForm read FAuth;
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
    procedure DoMatched(ARequest: TmodRequest); override;
    procedure Start; override;
    procedure Stop; override;
  public
    destructor Destroy; override;
    constructor Create(AServer: TmodModuleServer; const AName: string; const AAliasName: String); override;
    function GetDefaultURL: string; override;
    property Web: TmnwWeb read FWeb;
  end;

{ Controls } 

type

  { TZoomButtons }

  TZoomButtons = class(THTML.TGroupButtons)
  protected
    FButtonSmall: THTML.TToolButton;
    FButtonNormal: THTML.TToolButton;
    FButtonLarge: THTML.TToolButton;
    procedure Created; override;
  public
  end;

  [TRoute_Extension]
  TLangDropdown = class(THTML.TDropdown)
  protected
    procedure Created; override; 
    procedure DoRespond(const Ctx: TmnwContext); override;
    procedure DoCompose(const Ctx: TmnwContext); override;
  end;

  
{$ifdef FPC}
{$R 'mnWebElements.rc'}
{$else}
//* You need to compile it by brcc32 mnWebElements.rc or wait another 100 years till Delphi auto compile it
{$R 'mnWebElements.res' 'mnWebElements.rc'}
{$endif}

const
  woFullTag = [woOpenIndent, woCloseIndent];

function DirectionToStr(Direction: TDirection): string;
function BindActionToStr(BindType: TmnwBindAction): string;
function ThemeToStr(Theme: TTheme): string;

//Short functions
//Single Quote
function SQ(const s: string): string; inline;
//Double Quote
function DQ(const s: string): string; inline;

function Attr(const s: string): string; overload; inline;
function Attr(Value: Integer): string; overload; inline;

//Name Value with Quote
function NV(const Name, Value: string): string; overload; inline;
function NV(const Name, Value, Default: string): string; overload; inline;

function AddIf(const Value: string; Add: string): string; overload; inline;
function When(Value: Boolean; Kind: TmnwElementKind): TmnwElementKinds; overload;

function StartURL(const Path: string): string; inline;
function EndURL(const Path: string): string; inline;
function EscapeAttr(const S: string): string;

function Renderers: TmnwRenderers;
function Libraries: TmnwLibraries;

function _T(const Key: string; const Lang: string; const Default: string = ''): string;
procedure InitLanguages(const APath: string);

implementation

uses  
  Generics.Collections,
  mnBase64, mnHttpClient;

function DirectionToStr(Direction: TDirection): string;
begin
  if Direction = dirRightToLeft then
    Result := 'rtl'
  else if Direction = dirLeftToRight then
    Result := 'ltr';
end;

function BindActionToStr(BindType: TmnwBindAction): string;
begin
  case BindType of
    bindVisible: Result := 'visible';
    bindEnabled: Result := 'enabled';
  end;
end;

function ThemeToStr(Theme: TTheme): string;
begin
  case Theme of
    themeUndefined: Result := '';
    themeLight: Result := 'light';
    themeDark: Result := 'dark';
  end;
end;

function SQ(const s: string): string; inline;
begin
  Result := QuoteStr(s, '''');
end;

function DQ(const s: string): string; inline;
begin
  Result := QuoteStr(s, '"');
end;

function Attr(const s: string): string; inline;
begin
  Result := DQ(EscapeAttr(s));
end;

function Attr(Value: Integer): string; inline;
begin
  Result := Attr(Value.ToString);
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
    Result := ' ' + Name + '=' + DQ(Value)
  else if Default <> '' then
    Result := ' ' + Name + '=' + DQ(Default)
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

function EscapeAttr(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
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

var
  Languages: TDictionary<string, TDON_Value> = nil; //Move to TmnwWeb

procedure InitLanguages(const APath: string);
var
  SR: TSearchRec;
  LangCode: string;
  LangData: TDON_Value;
begin
  if Languages = nil then
    Languages := TDictionary<string, TDON_Value>.Create
  else
    Languages.Clear;

  if FindFirst(APath + '*.json', 0, SR) = 0 then
  try
    repeat
      LangCode := SubStr(SR.Name, '.');
      LangData := JsonLoadFile(APath + SR.Name, [jsoModern, jsoModernPlus]);
      if LangData <> nil then
      begin
//        Languages.AddOrSetValue(LangCode, LangData);
        Languages.Add(LangCode, LangData);
      end;
    until FindNext(SR) <> 0;
  finally
    FindClose(SR);
  end;
end;

function _T(const Key: string; const Lang: string; const Default: string = ''): string;
var
  LangData: TDON_Value;
begin
  if Languages = nil then
    Exit(Default);
  if Languages.TryGetValue(Lang, LangData) then
  begin
    if LangData[Key].IsExists then
      Result := LangData[Key].AsString
    else if Default = '' then
      Result := Key
    else
      Result := Default;
  end
  else if Default = '' then
    Result := Key
  else
    Result := Default;
end;

{$ifdef rtti_objects}
procedure CacheClasses;
var
  Ctx: TRTTIContext;
  rTypes: TArray<TRttiType>;
  rType: TRttiType;
begin
  if CacheClassObjects <> nil then
    exit;
  CacheClassObjects := TCacheClassObjects.Create;
  Ctx := TRTTIContext.Create;
  try
    rTypes := Ctx.GetTypes;
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
    Ctx.Free;
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

procedure TmnwAttachment.SetTerminated;
begin
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
    SplitStr(lCmd, ' ', lCmd, lValue);
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
  Stream.Close;
  FTerminated := True;
  SetTerminated;
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
  FLock := TMREWSync.Create;
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
  List: TList<TmnwAttachment>;
begin
  List := TList<TmnwAttachment>.Create;
  try
    Lock.BeginWrite;
    try
      for Attachment in Self do
        List.Add(Attachment);
    finally
      Lock.EndWrite;
    end;
    for Attachment in List do
      Attachment.Terminate;
  finally
    List.Free;
  end;
end;

procedure TmnwAttachments.SendMessage(const Message: string);
begin
  SendMessage('', Message);
end;

procedure TmnwAttachments.Add(AAttachment: TmnwAttachment);
begin
  Lock.BeginWrite;
  try
    inherited Add(AAttachment);
  finally
    Lock.EndWrite;
  end;
end;

procedure TmnwAttachments.Remove(AAttachment: TmnwAttachment);
begin
  if Lock = nil then
    raise Exception.Create('Lock is nil in Attachments');
  Lock.BeginWrite;
  try
    inherited Remove(AAttachment);
  finally
    Lock.EndWrite;
  end;
end;

procedure TmnwAttachments.SendMessage(const AttachmentName, Message: string);
var
  Attachment: TmnwAttachment;
begin
  Lock.BeginRead;
  try
    for Attachment in Self do
    begin
      if (Attachment.Name = '') or SameText(AttachmentName, Attachment.Name) then
        Attachment.SendMessage(Message);
    end;
  finally
    Lock.EndRead;
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
    aAttribute := Add(AName, '');
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

function TmnwAttributes.ToString(Area: TAttributeAreas = [ssOuter, ssInner]): string;
var
  itm: TmnwAttribute;
  idItem: Integer;
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    idItem := IndexOfName('id');
    if (idItem >= 0) then
    begin
      itm := Items[idItem];
      if (Area = []) or (itm.Area in Area) then
        if not itm.Used then
        begin
          sb.Append(itm.Name).Append('=').Append(DQ(itm.Value));
          itm.Used := True;
        end;
    end;

    for itm in Self do
    if (Area = []) or (itm.Area in Area) then
      if not itm.Used then
      begin
        if sb.Length > 0 then
          sb.Append(' ');
        if itm.IsProperty and (itm.Value = '') then
          sb.Append(itm.Name)
        else if not SameText(itm.name, 'id') then
          sb.Append(itm.Name).Append('=').Append(DQ(itm.Value));
        itm.Used := True;
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

function TmnwAttributes.AddIf(Condition: Boolean; const Name, Value: string; Area: TAttributeArea): TmnwAttribute;
begin
  if Condition then
    Result := Add(Name, Value, Area)
  else
    Result := nil;
end;

function TmnwAttributes.AddProp(const Name: string; Area: TAttributeArea = ssOuter): TmnwAttribute;
begin
  Result := Add(Name, '', Area);
  Result.IsProperty := True;
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

function TmnwAttributes.Add(const Name, Value: string; Area: TAttributeArea): TmnwAttribute;
begin
  Result := Find(Name);
  if Result = nil then
    Result := inherited Add(Name, Value)
  else
    Result.Value := Value;
  Result.Area := Area;
end;

procedure TmnwAttributes.Created;
begin
  inherited;
  //AutoRemove := True; //no AltTxt in image should writen even if it empty
end;

procedure TmnwAttributes.Delete(const Name: string);
var
  i: Integer;
begin
  i:= IndexOfName(Name);
  if i>=0 then
    Delete(i);
end;

{ TmnwElementRenderer }

procedure TmnwElementRenderer.RenderChilds(Scope: TmnwScope; Ctx: TmnwContext);
var
  o: TmnwElement;
  ParentRenderer: TmnwElementRenderer;
  StartElements, NormalElements, EndElements: TList<TmnwElement>;
begin
  ParentRenderer := Ctx.ParentRenderer;
  Ctx.ParentRenderer := Self;
  
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
      o.Render(Ctx);
    for o in NormalElements do
      o.Render(Ctx);
    for o in EndElements do
      o.Render(Ctx);
  finally
    EndElements.Free;
    NormalElements.Free;
    StartElements.Free;
    Ctx.ParentRenderer := ParentRenderer;
  end;
end;

procedure TmnwElementRenderer.DoEnterChildRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  if CanRenderChilds then
    RenderChilds(Scope, Ctx);
end;

procedure TmnwElementRenderer.DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoLeaveChildRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoEnterOuterRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoLeaveOuterRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; const Ctx: TmnwContext);
var
  aScope: TmnwScope;
begin
  aScope := TmnwScope.Create(AElement);
  try
    CollectAttributes(aScope, Ctx);

    if Ctx.ParentRenderer <> nil then
      Ctx.ParentRenderer.DoEnterChildRender(aScope, Ctx);

    DoEnterRender(aScope, Ctx);
    DoInnerRender(aScope, Ctx);
    DoLeaveRender(aScope, Ctx);

    if Ctx.ParentRenderer <> nil then
      Ctx.ParentRenderer.DoLeaveChildRender(aScope, Ctx);

  finally
    aScope.Free;
  end;
end;

constructor TmnwElementRenderer.Create(ARenderer: TmnwRenderer; ARendererRegister: TmnwElementRendererRegister);
begin
  inherited Create;
  FRenderer := ARenderer;
  FRendererRegister:= ARendererRegister;
end;

procedure TmnwElementRenderer.AddHead(const Scope: TmnwScope; const Ctx: TmnwContext);
begin
end;

function TmnwElementRenderer.CanRenderChilds: Boolean;
begin
  Result := True;
end;

procedure TmnwElementRenderer.CollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  if Scope.Element.ID <> '' then
    Scope.Attributes.add('id', Scope.Element.ID, ssInner);
  if Scope.Element.Name <> '' then
    Scope.Attributes.add('name', Scope.Element.Name, ssInner);

  DoCollectAttributes(Scope, Ctx);
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

procedure TmnwElement.Render(const Ctx: TmnwContext);
var
  er: TmnwElementRenderer;
begin
  if CanRender then
  begin
    er := CreateRenderer(Ctx);
    if er <> nil then
    try
      try
        er.Render(Self, Ctx);
      except
        on E: Exception do
          raise Exception.Create('Error in '+ ClassName +': ' + E.Message);
      end;
    finally
      er.Free;
    end;
  end;
end;

procedure TmnwElement.PrepareRenderer(const Ctx: TmnwContext);
var
  o: TmnwElement;
begin
  DoRequired(Ctx);
  for o in Self do
  begin
    o.PrepareRenderer(Ctx);
  end;
end;

function TmnwElement.CanRender: Boolean;
begin
  Result := RenderIt;
end;

function TmnwElement.CreateRenderer(const Ctx: TmnwContext): TmnwElementRenderer;
begin
  if (Ctx.Renderer <> nil) then
  begin
    Result := Ctx.Renderer.CreateRenderer(Self);
    //PrepareRenderer(Ctx);
  end
  else
    Result := nil;
end;

function TmnwElementRenderers.Replace(AElementClass: TmnwElementClass; ReplaceWith: TmnwElementRendererRegister): Integer;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AElementClass = Items[i].ElementClass then
    begin
      Items[i] := ReplaceWith;
      Exit(i);
    end;
  raise Exception.Create('Cannot replace renderer for ' + AElementClass.ClassName);
end;

{ TmnwSchema.TmnwElementRenderers }

function TmnwElementRenderers.RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; ReplaceIfExists: Boolean): TmnwElementRendererRegister;
  procedure CreateIt;
  begin
    Result := TmnwElementRendererRegister.Create;
    Result.ElementClass := AElementClass;
    Result.RendererClass := ARendererClass;
    rttiCollectExtensions(Result.ElementClass, Result.Extensions);
  end;
begin
  if Renderers.Started then
    raise Exception.Create('Once web app started you can''t register Renderer');

  if not AElementClass.InheritsFrom(TmnwElement) then
    raise Exception.Create('Element should inherited from THTML');

  if not ARendererClass.InheritsFrom(TmnwElementRenderer) then
    raise Exception.Create('Renderer should inherited from TmnwElementRenderer');

  Result := Find(AElementClass);
  if Result <> nil then
  begin
    if ReplaceIfExists and (AElementClass = Result.ElementClass) then
    begin
      CreateIt;
      Result.Index := Replace(AElementClass, Result);
    end
    else
      raise Exception.Create('You can''t re-register same class: ' + AElementClass.ClassName + ' with ' + Result.ElementClass.ClassName);
  end;
  
  if Result = nil then
  begin
    CreateIt;
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
      {if aClass <> AElementClass then
        Result := RegisterRenderer(AElementClass, Result.RendererClass);}
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

{ TmnwWeb }

destructor TmnwWeb.Destroy;
begin
  FreeAndNil(FRegistered);
  FreeAndNil(FLock);
  inherited;
end;

procedure TmnwWeb.DoCompose(const Ctx: TmnwContext);
begin

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
  CleanSchemas;
  Started := False;
end;

function TmnwWeb.RegisterSchema(const AName: string; SchemaClass: TmnwSchemaClass; AsDefaultSchema: Boolean = False): TmnwSchema;
var
  aSchemaItem: TmnwRegisterdSchema;
begin
  aSchemaItem := TmnwRegisterdSchema.Create;
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
    if SameText(Items[i].Name, aSchemaName) and (not (schemaSession in Items[i].GetCapabilities) or (aSessionID = Items[i].Reference)) then
      Result := Items[i];
    if Result <> nil then
      break;
  end;
end;

function TmnwWeb.CreateSchema(const aSchemaName: string): TmnwSchema;
var
  SchemaItem: TmnwRegisterdSchema;
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
  Lock.BeginWrite;
  try
    Result := FindBy(aSchemaName, aSessionID);
    if Result <> nil then
    begin
      Extract(Result);
      Result.FPhase := scmpReleased;
    end;
  finally
    Lock.EndWrite
  end;
end;

//Main
function TmnwWeb.InquireElement(var Ctx: TmnwContext; FindNested: Boolean): Boolean;
var
  aElement: TmnwElement;
  aRoutes: TStringList;
  i: Integer;
  aSchemaName, aRoute: string;
  aSchema: TmnwSchema; 
begin  
  aSchema := nil;
  Result := False;
  aRoutes := TStringList.Create;
  try
    StrToStrings(Ctx.CurrentPath, aRoutes, [URLDelimiter]);
    if (aRoutes.Count > 0) then
      aSchemaName := aRoutes[0]
    else
      aSchemaName := '';

    //Find already exists Schema
    Lock.BeginRead;
    try
       aSchema := FindBy(aSchemaName, Ctx.Session.ID);
    finally
      Lock.EndRead;
    end;

    if aSchema = nil then // Not cached, create it.
    begin
      aSchema := CreateSchema(aSchemaName);
      if aSchema = nil then  //* Fallback
      begin
        Lock.BeginRead;
        try
          aSchema := FindBy('', Ctx.Session.ID);
        finally
          Lock.EndRead;
        end;
        if aSchema = nil then
          aSchema := CreateSchema('');

        if (aSchema = nil) and (aSchemaName = '') then
        begin
          Ctx.Response.RespondRedirectTo(EndURL(FallbackTo));
          exit;
        end;
          
        if aSchema <> nil then
          aSchemaName := '';
      end;

      if (aSchema <> nil) and (schemaSession in aSchema.GetCapabilities) then
        aSchema.Reference := Ctx.Session.ID;
    end;

    if aSchemaName <> '' then
    begin
      if (aRoutes.Count > 0) then
      begin
        aRoutes.Delete(0);
        Ctx.CurrentPath := DeleteSubPath(aSchemaName, Ctx.CurrentPath);
      end;
    end;

    Lock.BeginRead;
    try
      if aSchema <> nil then
        AtomicIncrement(aSchema.Usage);
    finally
      Lock.EndRead;
    end;

    if (aSchema <> nil) then
    begin
      Ctx.Schema := aSchema;

      Ctx.Session.ID := Ctx.Request.Params['session'];
      if Ctx.Session.ID = '' then
        Ctx.Session.ID := Ctx.Request.Cookies['session'];
      Ctx.Session.Age := SessionAge;
      Ctx.Session.Domain := Ctx.Request.Domain;
      Ctx.Session.Path := Ctx.GetBasePath;
      //AResponse.Session.Path := StartURL(Alias, True);
      Ctx.Session.Reset;
      
      Ctx.Language := Ctx.Request.Params['language'];
      if Ctx.Language = '' then
        Ctx.Language := Ctx.Request.Cookies['language'];
      if Ctx.Language = '' then
        Ctx.Language := Ctx.Web.Language;
        
      if SameText(Ctx.Language, 'ar') then
        Ctx.Direction := dirRightToLeft
      else
        Ctx.Direction := dirLeftToRight;

      if aSchema.Accept(Ctx) then
      begin
        if not (estComposed in aSchema.State) then
        begin
          aSchema.Enter;
          try
            if not (estComposed in aSchema.State) then //Check again after Enter, while waiting can be composed
            try
              aSchema.Compose(Ctx); //Compose
            except
              aSchema.Leave;
              Ctx.Schema := nil;
              FreeAndNil(aSchema);
              raise;
            end;
          finally
            if aSchema <> nil then
                aSchema.Leave;
          end;
        end;

        if (estComposed in aSchema.State) then
        begin                 
          Ctx.Element := aSchema;

          Result := True;

          if FindNested then
          begin
            //Finding nested element inside Schema
            aElement := aSchema;
            i := 0;
            while i < aRoutes.Count do
            begin
              aRoute := aRoutes[i];
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
                  Ctx.Element := aElement;
                  Result := True;
                  Ctx.CurrentPath := DeleteSubPath(aRoute, Ctx.CurrentPath);
                end;
              end;
              inc(i);
            end;
          end;
        end;
      end;
    end;
  finally
    aRoutes.Free;
  end;
end;

procedure TmnwWeb.Respond(var Ctx: TmnwContext);
begin
  if Shutdown then
    exit;

  try
    InquireElement(Ctx, True);
    if Ctx.Element <> nil then
    begin      
      Ctx.Response.Answer := hrOK;
      Ctx.Response.Redirect := '';
//      AResponse.Header['access-control-allow-origin'] := AResponse.Request.Host;
      Ctx.Response.Header['access-control-allow-origin'] := '*';
      Ctx.Response.PutHeader('Access-Control-Allow-Headers', 'Location, Content-Type, Authorization, Accept, Origin, X-PINGOTHER');

      //AResponse.Header['Access-Control-Allow-Headers'] := ' X-PINGOTHER, Content-Type';
      //AResponse.Header['Access-Control-Allow-Methods'] := 'HEAD,POST,GET,OPTIONS,PUT,DELETE,CONNECT,TRACE,PATCH';
      //AResponse.Header['Access-Control-Expose-Headers'] := ' Content-Encoding, Kuma-Revision';     

      if not Ctx.Response.IsResponded then
        Ctx.Element.RespondInit(Ctx); //For check Login in header before redirecting if needed

      //* If you call schema name without ending by /
      if not Ctx.Response.IsResponded then
      begin
        if (Ctx.Element = Ctx.Schema) and (Ctx.Schema.Name <> '') and (Ctx.CurrentPath = '') then
          Ctx.Response.RespondRedirectTo(IncludeURLDelimiter(Ctx.GetPath(Ctx.Schema)), True)
        else
          Ctx.Response.ContentType := Ctx.Element.GetContentType(Ctx.CurrentPath);
      end;

      //* Resume maybe come false in action
      //* We will render it now
      if not Ctx.Response.IsResponded then
      begin
        Ctx.Element.PrepareRenderer(Ctx);
        if not Ctx.Response.IsResponded then
          Ctx.Element.Respond(Ctx);
      end;

      if not (Ctx.Response.IsHeaderSent) then
      begin
        if (Ctx.Response.Answer =hrOK) and (not Ctx.Response.IsResponded) then
          Ctx.Response.RespondNoContent
        else if Ctx.Response.Answer = hrNotFound then
          Ctx.Response.RespondNotFound;
      end;
    end
    else
    begin
      if not Ctx.Response.IsHeaderSent then
        Ctx.Response.RespondNotFound;
    end;

    if Ctx.Schema <> nil then
    begin
      Lock.BeginWrite;
      try
        Ctx.Schema.LastAccess := Now;
        AtomicDecrement(Ctx.Schema.Usage);
        if (Ctx.Schema.Usage = 0) and (Ctx.Schema.Released) then
          FreeAndNil(Ctx.Schema)
        else
        begin
          if Ctx.Schema.Phase = scmpNew then
          begin
            Ctx.Schema.FPhase := scmpNormal;
            Add(Ctx.Schema);
          end;
        end;
      finally
        Lock.EndWrite;
      end;
    end;
  except
    {$ifdef DEBUG}
    on E: Exception do
    begin
      if not (Ctx.Response.IsHeaderSent) then
      begin
        Ctx.Response.RespondText('Server Error: ' + E.Message, hrError);
      end;
      raise;
    end;
    {$else}
      raise;
    {$endif}
  end;
end;

function TmnwWeb.Attach(var Ctx: TmnwContext; Sender: TObject; AStream: TmnBufferStream): TmnwAttachment;
begin
  Result := nil;
  
  if Shutdown then
    exit(nil);

  InquireElement(Ctx, False);
  if Ctx.Schema <> nil then
  begin
    if Ctx.Schema.Interactive or (schemaAttach in Ctx.Schema.GetCapabilities) then
      Ctx.Schema.Attach(Ctx.CurrentPath, Sender, AStream)
  end
end;

procedure TmnwWeb.SchemaCreated(Schema: TmnwSchema);
begin
end;

procedure TmnwWeb.SetLanguage(const Value: string);
begin
  FLanguage := Value;
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

procedure TmnwWeb.CleanSchemas;
var
  i: Integer;
begin
  i := Count-1;
  while i>=0 do
  begin
    if not (schemaStatic in Items[i].GetCapabilities) then
      Delete(i);
    Dec(i);
  end;
end;

constructor TmnwWeb.Create;
begin
  FTimeStamp := GetTimeStamp;
  FLock := TMREWSync.Create;
  FRegistered := TRegisteredSchemas.Create;
  SessionAge := msOneHour; //Forever
  FShowVersion := True;
  FLanguage := 'en';
  FAppPath := ExtractFilePath(ParamStr(0));
  inherited;
end;

function TmnwWeb.CreateSchema(SchemaItem: TmnwRegisterdSchema): TmnwSchema;
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

function TLocation.IsDefined: Boolean;
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
  FLocation := imgPath;
end;

procedure TImageLocation.SetData(const Value: TBytes);
begin
  FData := Value;
end;

procedure TImageLocation.SetSymbol(const AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
  FLocation := imgSymbol;
end;

function TImageLocation.GetSymbol: string;
begin
  if FLocation = imgSymbol then
    Result := FValue
  else
    Result := '';
end;

function TImageLocation.GetPath: string;
begin
  if FLocation = imgPath then
    Result := FValue
  else
    Result := '';
end;

procedure TImageLocation.LoadFromFile(const AFileName: string);
begin
  FValue := AFileName;
  FileAge(AFileName, FFileDate);
  FContentType := DocumentToContentType(AFileName);  
  FData := LoadFileBytes(AFileName);
  FLocation := imgMemory;
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
//  Width := szUndefined;
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
  if schemaStatic in GetCapabilities then
    FInternalLock := TCriticalSection.Create
  else
    FInternalLock := nil;
  RefreshInterval := 1;
  {$ifdef rtti_objects}
  CacheClasses;
  {$endif}
end;

destructor TmnwSchema.Destroy;
var
  LLock: TCriticalSection;
begin
  FAttachments.Terminate;

  // Drain: take the lock away first so any new caller sees nil,
  // then acquire/release to wait for any thread currently inside.
  LLock := FInternalLock;
  FInternalLock := nil;
  if LLock <> nil then
  begin
    LLock.Enter;
    LLock.Leave;
  end;

  FAttachments.Clear;
  FreeAndNil(FAttachments);

  FreeAndNil(LLock);
  FreeAndNil(FDefaultDocuments);
  inherited;
end;

// Executed from a thread of connection of WebSocket, it stay inside until the disconnect or terminate
procedure TmnwSchema.Attach(Route: string; Sender: TObject; AStream: TmnBufferStream);
var
  aAttachment: TmnwAttachment;
begin
  if FAttachments = nil then //Maybe shutdowning
    exit;
  aAttachment := TmnwAttachment.Create;
  aAttachment.Schema := Self;
  aAttachment.Stream := AStream;
  Attachments.Add(aAttachment);
  UpdateAttached;
  if Attachments <> nil then
  try
    aAttachment.Loop;
  finally
    if not aAttachment.Terminated then
      aAttachment.Terminate;
    Attachments.Remove(aAttachment);//Already do Lock.BeginWrite
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

function TmnwElement.ServeFile(PublicPath: string; DefaultDocuments: TStringList; Options: TmodServeFiles; const Ctx: TmnwContext): Boolean;
var
  aDocument, aRequestDocument, aFile: string;
  IsDocument, IsDirectory, Expanded: Boolean;
begin
  Result := True;
  if PublicPath = '' then
  begin
    Result := False;
    Exit;
  end;

  WebExpandFile(PublicPath, Ctx.CurrentPath, aRequestDocument, False);
  Expanded := WebExpandFile(PublicPath, Ctx.CurrentPath, aDocument, serveSmart in Options);

  if not Expanded then
  begin
    if (Ctx.CurrentPath = '') or IsStrInArray(Ctx.CurrentPath, ['\', '/']) then
    begin
      if (serveIndexRoot in Options) and EndsDelimiter(aDocument) and DirectoryExists(aDocument) then
      begin
        if StartsStr(PublicPath, aDocument) then
          ServeDir(aDocument, Options, Ctx)
        else
          Ctx.Response.RespondUnauthorized;
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

  if ((Ctx.CurrentPath = '') and not IsDocument) or (not EndsDelimiter(aRequestDocument) and IsDirectory) then
  begin
    Ctx.Response.RespondRedirectTo(IncludeURLDelimiter(Ctx.Request.Path)); //TODO short it
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
      if StartsStr(PublicPath, aDocument) then
        ServeDir(aDocument, Options, Ctx)
      else
        Ctx.Response.RespondUnauthorized;
      Exit;
    end;
  end;

  if StartsText('.', ExtractFileName(aDocument)) then
    Ctx.Response.RespondForbidden
  else if IsDocument then
  begin
    if StartsText(PublicPath, aDocument) then
      Ctx.Response.SendFile(aDocument)
    else
      Ctx.Response.RespondUnauthorized;
  end
  else
    Result := False;
end;

function TmnwElement.ServeFile(PublicPath: string; Options: TmodServeFiles; const Ctx: TmnwContext): Boolean;
begin
  Result := ServeFile(PublicPath, nil, Options, Ctx);
end;

procedure TmnwElement.ServeDir(APath: string; Options: TmodServeFiles; const Ctx: TmnwContext);
var
  Files: TStringList;

  procedure AddLink(const s: string);
  begin
    Ctx.Writer.OpenInlineTag('li');
    Ctx.Writer.AddInlineTag('a', 'href="' + s + '"', s);
    Ctx.Writer.CloseTag('li');
  end;

  procedure WriteSection(const ACaption: string; AFilter: TEnumFilesOptions; const AExtra: string = '');
  var
    s: string;
  begin
    Files.Clear;
    EnumFiles(Files, APath, '*.*', AFilter);
    Ctx.Writer.AddTag('h2', '', ACaption);
    Ctx.Writer.OpenTag('ul');
    if AExtra <> '' then
      AddLink(AExtra);
    for s in Files do
      if not StartsText('.', s) then
        AddLink(s);
    Ctx.Writer.CloseTag('ul');
  end;

begin
  Ctx.Response.ContentType := DocumentToContentType('html');
  Files := TStringList.Create;
  try
    Ctx.Writer.WriteLn('<!DOCTYPE html>');
    Ctx.Writer.OpenTag('html');
    Ctx.Writer.OpenTag('head');
    Ctx.Writer.AddTag('title', '', 'Index of ' + APath);
    Ctx.Writer.AddShortTag('link', 'rel="icon" href="data:,"'); //disable call favicon.ico
    Ctx.Writer.AddShortTag('meta', 'charset="UTF-8"');
    Ctx.Writer.AddShortTag('meta', 'name="viewport" content="width=device-width, initial-scale=1"');
    Ctx.Writer.AddTag('style', '', 'body { font-family: monospace; }');
    Ctx.Writer.CloseTag('head');
    Ctx.Writer.OpenTag('body');
    Ctx.Writer.AddTag('h1', '', 'Index of ' + Ctx.CurrentPath);
    WriteSection('Dirs', [efDirectory], '..');
    WriteSection('Files', [efFile]);
    Ctx.Writer.CloseTag('body');
    Ctx.Writer.CloseTag('html');
  finally
    Files.Free;
  end;
end;

procedure TmnwSchema.DoRespond(const Ctx: TmnwContext);
begin
  if not (serveEnabled in ServeFiles) or not ServeFile(GetPublicPath, DefaultDocuments, ServeFiles, Ctx) then
    Render(Ctx);
end;

procedure TmnwSchema.Enter;
begin
  if FInternalLock <> nil then
    FInternalLock.Enter;
end;

procedure TmnwSchema.DoAccept(var Ctx: TmnwContext; var Resume: Boolean);
begin
end;

procedure TmnwSchema.DoChildRespond(AElement: TmnwElement; const Ctx: TmnwContext);
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
    Json := JsonParsePairString(s, Error, [jsoSafe]);
    try
      elementID := Json['element'].AsString;
      element := FindByID(elementID);
      if element <> nil then
      begin
        Attachments.Lock.BeginRead;
        try
          element.ReceiveMessage(Json);
        finally
          Attachments.Lock.EndRead;
        end;
      end;
    finally
      Json.Free;
    end;
  end
end;

procedure TmnwSchema.Leave;
begin
  if FInternalLock <> nil then
    FInternalLock.Leave;
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

function TmnwSchema.Accept(var Ctx: TmnwContext): Boolean;
begin
  Result := True;
  DoAccept(Ctx, Result);
end;

{function TmnwSchema.Interactive: Boolean;
begin
  Result := schemaInteractive in GetCapabilities;
end;}

procedure TmnwSchema.Compose(const Ctx: TmnwContext);
begin
  inherited;
end;

procedure TmnwSchema.DoPrepare;
begin
  inherited; // hook for descendants
end;

function TmnwSchema.GetReleased: Boolean;
begin
  Result := (FPhase = scmpReleased) or not (schemaStatic in GetCapabilities);
end;

procedure TmnwSchema.SetDefaultDocuments(AValue: TStringList);
begin
  FDefaultDocuments.Assign(AValue);
end;

procedure TmnwSchema.SetPublicPath(const Value: string);
begin
  FPublicPath := Value;
end;

procedure TmnwSchema.Start;
begin
end;

function TmnwSchema.NewHandle: THandle;
begin
  AtomicIncrement(FNamingLastNumber);
  Result := FNamingLastNumber;
end;

function TmnwSchema.GetPublicPath: string;
begin
  if FPublicPath <> '' then
    Result := FPublicPath
  else
    Result := Web.PublicPath;
end;

procedure TmnwSchema.UpdateAttached;
begin
  // Add/Remove already serialize list access; no need for a separate lock here.
  FAttached := Attachments.Count > 0;
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

{ THTML.TCustomInput }

procedure THTML.TCustomInput.SetValue(const AValue: string);
begin
  if FValue =AValue then Exit;
  FValue :=AValue;
  if (estComposed in State) and (Schema <> nil) and Schema.Attached then
    SendInteractive('"command": "change", "content": ' + DQ(Value));
end;

procedure THTML.TCustomInput.Created;
begin
  inherited;
  EditType := 'text';
end;

procedure THTML.TCustomInput.ReceiveMessage(JSON: TDON_Pair);
begin
  if JSON['command'].AsString = 'change' then
  begin
    if JSON['content'].IsExists then
      FValue := JSON['content'].AsString;
    if JSON['caption'].IsExists then
      FCaption := JSON['caption'].AsString;
  end;
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

function THTML.TImageMemory.GetRoute: String;
begin
  Result := ExtractFileName(FileName);
  if Result = '' then
    Result := inherited GetRoute;
end;

procedure THTML.TImageMemory.DoRespond(const Ctx: TmnwContext);
begin
  Data.Seek(0, soBeginning);
  Ctx.Response.SendStream(Data, FileName, Data.Size, InstanceDate);
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

function TmnwElement.GetPathClasses: string;
begin
  if (Parent <> nil) then
    Result := AddEndURLDelimiter(Parent.GetPathClasses) + ClassName
  else
    Result := ClassName;
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

function TmnwElement.GetRoute: String;
begin
  Result := FRoute;
end;

procedure TmnwElement.SetOnRespond(const Value: TRespondProc);
begin
  FOnRespond := Value;
  {$ifopt D+}
  {$ifdef Warn}
  if (Schema <> nil) and (schemaDynamic in Schema.GetCapabilities) then
    log.WriteLn(lglWarning, 'You are using OnRespond in dynamic schema:' + GetPathClasses);
  {$endif}
  {$endif}
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
    raise Exception.Create(ObjectClass.ClassName + ': ' + AName + ' does not exist in ' + Name);
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
  if EndRoute then //Not tested yet
    exit(self);   
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

procedure TmnwElement.DoCompose(const Ctx: TmnwContext);
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
  if FHandle = 0 then
    FHandle := Schema.NewHandle;
  Result := FHandle;
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

procedure TmnwElement.DoRequired(const Ctx: TmnwContext);
begin
end;

procedure TmnwElement.DoRespond(const Ctx: TmnwContext);
begin
end;

procedure TmnwElement.DoRespondHeader(const Ctx: TmnwContext);
begin
end;

constructor TmnwElement.Create(AParent: TmnwElement; AKind: TmnwElementKinds);
begin
  inherited Create;
  FTimeStamp := GetTimeStamp;
  FEnabled := True;
  FVisible := True;
  FName := '';
  FKind := AKind;
  FParent := AParent;
  if FParent <> nil then
  begin
    FSchema:= FParent.FSchema;
    FParent.Add(Self);
  end;
{$ifopt D+}
{
  if (Schema <> nil) and (schemaDynamic in Schema.GetCapabilities) then
  begin
    if (Schema <> Self) then
      if Self.MethodAddress('DoRespond') <> @TmnwElement.DoRespond then
        log.WriteLn(lglWarning, 'You are using DoRespond in dynamic schema: ' + GetPathClasses);
  end;
}
{$endif}
end;

destructor TmnwElement.Destroy;
begin
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

procedure TmnwElement.Respond(const Ctx: TmnwContext);
begin
  if (Schema <> nil) and (Schema <> Self) then
    Schema.DoChildRespond(Self, Ctx);
  if not Ctx.Response.IsResponded and Assigned(OnRespond) then
    OnRespond(Ctx);
//  if not Ctx.Response.IsResponded then
  DoRespond(Ctx);
end;

procedure TmnwElement.Compose(const Ctx: TmnwContext);
var
  o: TmnwElement;
begin
//  Clear; //*Should not clear here
//  Prepare;
  AddState([estComposing]);
  UpdateElement(Self);
  DoCompose(Ctx);
  for o in Self do
  begin
    if not (estComposed in o.State) then    
      o.Compose(Ctx); //Compose
  end;
  RemoveState([estComposing]);

  AddState([estComposed]);
  DoComposed;
end;

function TmnwElement.CountComposed: Integer;
var
  e: TmnwElement;
begin
  Result := 0;
  for e in Self do
    if not (elEmbed in e.Kind) then
      Inc(Result);  
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

procedure TmnwElement.RespondInit(const Ctx: TmnwContext);
begin
  DoRespondHeader(Ctx);
end;

{ TmnwRenderer }

procedure TmnwRenderer.AddHead(const Ctx: TmnwContext);
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
  FRequires := TmnwRequires.Create(False);
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

procedure TmnwRenderer.Created;
begin
  inherited;
  Require(TWebElements_Library);
end;

destructor TmnwRenderer.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FRequires);
  inherited;
end;

procedure TmnwRenderer.EndRender;
begin
  DoEndRender;
end;

class procedure TmnwRenderer.RegisterElements;
begin
end;

class function TmnwRenderer.RegisterRenderer(AElementClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean): TmnwElementRendererRegister;
begin
  Result := ElementRenderers.RegisterRenderer(AElementClass, ARendererClass, Replace);
end;

procedure TmnwRenderer.Require(ALibraryClass: TmnwLibraryClass);
begin
  Requires.Use(ALibraryClass);
end;

procedure TmnwRenderer.DoBeginRender;
begin
end;

procedure TmnwRenderer.DoEndRender;
begin
end;

{ TmnwRegisterdSchema }

destructor TmnwRegisterdSchema.Destroy;
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

procedure TmnwSchema.TFile.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
  if ftResource in Options then
    Ctx.Response.SendResource(FileName, Route)
  else
    Ctx.Response.SendFile(FileName);
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

procedure TmnwSchema.TMemory.DoRespond(const Ctx: TmnwContext);
begin
  Data.Seek(0, soBeginning);
  Ctx.Response.SendStream(Data, FileName, Data.Size, FileDate);
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

procedure THTML.TAssets.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
  ServeFile(Schema.GetPublicPath, [serveDefault], Ctx);
end;

function THTML.TAssets.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TFolder }

procedure THTML.TFolder.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
  ServeFile(PublicPath, ServeFiles, Ctx);
end;

function THTML.TFolder.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TCompose }

constructor THTML.TCompose.Create(AParent: TmnwElement; AOnCompose: TComposeProc);
begin
  inherited Create(AParent);
  OnCompose := AOnCompose;
end;

procedure THTML.TCompose.DoRespond(const Ctx: TmnwContext);
var
  Inner: TInner;
begin
  inherited;
  Inner:= TInner.Create(nil);
  try
    Inner.FSchema := Schema;
    Inner.FParent := Self; //Fake Parent do not add it to the list;
    Inner.IsRoot := Ctx.Element = Self; // if compused from Schema of parents, or just directly composed
    InnerCompose(Inner, Ctx);
    if Assigned(OnCompose) then
      OnCompose(Inner, Ctx);
    Inner.Compose(Ctx);

    Inner.Render(Ctx);
  finally
    Inner.Free;
  end;
end;

procedure THTML.TCompose.InnerCompose(Inner: TmnwElement; const Ctx: TmnwContext);
begin
end;

{ TmnwLibrary }

procedure TmnwLibrary.AddHead(const Ctx: TmnwContext);
var
  source: TmnwLibrarySource;
  url: string;  
  aDirection: TDirection;
  local: Boolean;
begin
  for source in Sources do
  begin
    aDirection := Ctx.Direction;
    if aDirection = dirUndefined then
      aDirection := dirLeftToRight;      
    
    if ((source.Direction = dirUndefined) or (source.Direction = aDirection)) and
       ((Source.Language = '') or (Source.Language = Ctx.Language)) then
    begin
      if source.Where in [stOnline, stResource]  then           
      begin
        if (source.OnlineFile = '') or (source.Where = stResource) or CheckOffline(Ctx, source.Name) then
        begin
          url := EndUrl(Ctx.GetAssetsURL) + source.Name;
          local := True;
        end
        else 
        begin
          url := source.OnlineFile;
          local:= False;
        end;

        if not StartsText('http', url) and not StartsStr('//', url) then
        begin
          if Ctx.Request.IsSecure then
            url := 'https://' + url
          else
            url := 'http://' + url;
        end;
        
        case source.SourceType of
          stStyle: Ctx.Writer.AddLinkStyle(url, When(not local, source.Integrity), libCross in source.Options);
          stScript: Ctx.Writer.AddLinkScript(url, When(not local, source.Integrity), libDefer in source.Options, libCross in source.Options);
        end;
      end
      else
      begin
        case source.SourceType of
          stStyle: Ctx.Writer.AddEmbedStyle(source.Text);
          stScript: Ctx.Writer.AddEmbedScript(source.Text, libDefer in source.Options);
        end;
      end;
    end;    
  end;
end;

function TmnwLibrary.CheckOffline(const Ctx: TmnwContext; const FileName: string): Boolean;
begin
  with Ctx.Schema do
    Result := (Web.OnlineFiles = olfOffline) or ((Web.OnlineFiles = olfSmart) and FileExists(IncludePathDelimiter(Ctx.GetAssetDir) + FileName));
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

destructor TmnwLibrary.Destroy;
begin
  inherited;
  FreeAndNil(FSources);
end;

{ TmnwLibraries }

function TmnwLibraries.Find(ALibraryName: string): TmnwLibrary;
var
  i: Integer;
begin
  Result := nil;
  Lock.BeginRead;
  try
    for i := 0 to Count - 1 do
      if (SameText(Items[i].Name, ALibraryName)) then
      begin
        Result := Items[i];
        break;
      end;
  finally
    Lock.EndRead;
  end;
end;

constructor TmnwLibraries.Create;
begin
  inherited Create;
  FLock := TMREWSync.Create;
end;

destructor TmnwLibraries.Destroy;
begin
  FreeAndNil(FLock);
  inherited;
end;

function TmnwLibraries.Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary;
var
  i: Integer;
begin
  Lock.BeginRead;
  try
    Result := nil;
    for i := 0 to Count - 1 do
      if Items[i] is ALibraryClass then
      begin
        Result := Items[i];
        break;
      end;
  finally
    Lock.EndRead;
  end;
end;

function TmnwLibraries.RegisterLibrary(ALibraryClass: TmnwLibraryClass; Priority: Integer): TmnwLibrary;
begin
  Lock.BeginWrite;
  try
    Result := Find(ALibraryClass);
    if Result <> nil then
      raise Exception.Create(ALibraryClass.ClassName + ' is already registered');
    Result := ALibraryClass.Create;
    Result.Priority := Priority;
    Add(Result);
  finally
    Lock.EndWrite;
  end;
end;

{ TJQuery_Library }

procedure TJQuery_Library.Created;
begin
  inherited;
  Sources.Add(stScript, 'https://cdn.jsdelivr.net/npm/jquery@4.0.0/dist/', 'jquery.min.js', '', []); //* no Differ
end;

{ THTML }

procedure TWebElements_Library.Created;
begin
  inherited;
  Sources.Add(stScript, 'https://cdn.jsdelivr.net/npm/js-sha256@0.11.1/src/', 'sha256.min.js', '', []);
//  Sources.Add(stScript, '', 'web-elements.js?v=' + IntToStr(GlobalTimeStamp));
//  Sources.Add(stStyle, '', 'web-elements.css?v=' + IntToStr(GlobalTimeStamp));
  Sources.Add(stScript, stResource, 'web-elements.js', '?minilib\web\source\mnWebElements.js');
  Sources.Add(stStyle, stResource, 'web-elements.css', '?minilib\web\source\mnWebElements.css');
end;

{ THTML.TImage }

procedure THTML.TImage.DoCompose(const Ctx: TmnwContext);
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
  FLogo := TImageFile.Create(This, [elInternal, elEmbed]);
  FLogo.Route := 'logo';
  FLogo.FileName := Schema.Web.Assets.LogoFile;
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
  Shadow := ShadowBottom;
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
  Shadow := shadowEnd;
  Theme := themeUndefined;
end;

{ THTML.TMain }

procedure THTML.TMain.Created;
begin
  inherited;
end;

{ THTML.TBar }

procedure THTML.TBar.Created;
begin
  inherited;
//  Padding := 1;
end;

{ THTML.TCard }

constructor THTML.TCard.Create(AParent: TmnwElement; AKind: TmnwElementKinds);
begin
  inherited;
  FFooter := TCardFooter.Create(Self, [elEmbed, elInternal]);
  FHeader := TCardHeader.Create(Self, [elEmbed, elInternal]);
end;

procedure THTML.TCard.Created;
begin
  inherited;
end;

{ THTML.TForm }

procedure THTML.TForm.DoRespondHeader(const Ctx: TmnwContext);
begin
  inherited;
  if (RedirectTo <> '') and (Ctx.Response.Answer = hrNone) then
  begin
    Ctx.Response.Answer := hrRedirect;
    Ctx.Response.Redirect := RedirectTo;
  end;
end;

procedure THTML.TForm.Created;
begin
  inherited;
  Endpoint.Where := toElement;
  CallScript := 'mnw.formPost(event)';
end;

procedure THTML.TForm.DoComposed;
begin
  inherited;
  if Endpoint.Where = toElement then
    NewRoute(Self);
end;

{ THTML.TParagraph }

constructor THTML.TParagraph.Create(AParent: TmnwElement; AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

{ THTML.THeading }

constructor THTML.THeading.Create(AParent: TmnwElement; ALevel: Integer; AText: string; AStyle: THeadingStyle);
begin
  inherited Create(AParent);
  Level := ALevel;
  Text := AText;
  Style := AStyle;
end;

{ THTML.TAction }

constructor THTML.TAction.Create(AParent: TmnwElement; AName, ARoute: string; ActionProc: TRespondProc);
begin
  inherited Create(AParent);
  Name := AName;
  Route := ARoute;
  OnRespond := ActionProc;
end;

procedure THTML.TAction.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
  try
    Execute;
    Ctx.Writer.WriteLn('Executed');
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
  Ctx: TmnwContext;
  aDomain, aPort: string;
  aContent: string;
begin
  inherited;
  if (Request.CurrentPath = '') and (Request.URI <> '') then
  begin
    Response.RespondRedirectTo(IncludeURLDelimiter(Request.URI));
    exit;
  end;
  AtomicIncrement(RendererID);
  InitMemory(Ctx, SizeOf(Ctx));

  //Remove leading /
  Ctx.CurrentPath := RemoveStartURLDelimiter(Request.CurrentPath);
  Ctx.Sender := Self;

  Ctx.FResponse := Response;
  Ctx.FWeb := Module.Web;

  if Module.Domain <> '' then
  begin
    aDomain := Module.Domain;
    aPort := Module.Server.UsedPort;
  end
  else
    SplitStr(Request.Header['Host'], ':', aDomain, aPort);

  if (aDomain='') and Request.Connected then
    raise Exception.Create('Domain is not defined');

  if Request.RequestType = rtWebSocket then
  begin
    //Serve the websocket
    if (Module as TmnwWebModule).Web.Attach(Ctx, Self, Response.Stream) = nil then
      Result.Status := []; // Disconnect
  end
  else
  begin
    Ctx.FRenderer := (Module as TmnwWebModule).CreateRenderer;
    Ctx.Renderer.RendererID := RendererID;
    Ctx.Renderer.Requires.QuickSort;
    Ctx.FWriter := TmnTidyWriter.Create('html', Response.Stream);
    Ctx.Writer.Compact := Module.Web.CompactMode;

    //yes always created, i maybe pass params that come from Query (after ? )
    if Request.RequestType = rtFormData then
    begin
      Ctx.Data := TmnMultipartData.Create(Request.Header.Field['Content-Type'].SubValue('boundary'), (Module as TmnwWebModule).PrivatePath + 'temp');
      (Ctx.Data as TmnMultipartData).Read(Request.Stream);
    end
    else if Request.RequestType = rtJSONData then
    begin
      if Ctx.Request.ReadString(aContent) then
        Ctx.Data := JsonParseValueString(aContent, [])
      else
        Ctx.Data := TDON_Pair.Create(nil);
    end
    else
      Ctx.Data := TDON_Pair.Create(nil);
    
    try          
//      Response.ContentType := DocumentToContentType('html');
      Module.Web.Respond(Ctx); //Main
    finally
      FreeAndNil(Ctx.Writer);
      FreeAndNil(Ctx.Renderer);
      FreeAndNil(Ctx.Data);
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
  aLibrary: TmnwLibrary;
  aSource: TmnwLibrarySource;
  {$ifdef LOCAL_RESOURCE}
  aLocalFile: string;
  {$endif}
  aName: string;
begin
  inherited;
  Name := 'Assets';
  Route := 'assets';
  
  Libraries.Lock.BeginRead;
  try
    for aLibrary in Libraries do
    begin
      for aSource in aLibrary.Sources do
      begin
        if stResource = aSource.Where then
        begin    
          aName := SubPath(aSource.LocalFile, -1);
          {$ifdef LOCAL_RESOURCE}
          //from original source
          aLocalFile := ExpandFileName(VarEnvReplace(aSource.LocalFile));
          if FileExists(aLocalFile) then
            TFile.Create(This, [], aLocalFile, aSource.Name)
          else
          {$endif}          
          // From local Dir
          if FileExists(IncludePathDelimiter(PublicPath) + aName) then          
          begin
            TFile.Create(This, [], IncludePathDelimiter(PublicPath) + aName, aSource.Name)          
          end
          else //From resources
            TFile.Create(This, [ftResource], StringReplace(aName, '.', '_', [rfReplaceAll]), aSource.Name);
        end;
      end;
    end;    
  finally
    Libraries.Lock.EndRead;
  end;
end;

procedure TAssetsSchema.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
end;

class function TAssetsSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  Result := inherited + [schemaStartup, schemaStatic];
end;

function TAssetsSchema.GetPublicPath: string;
begin
  if FPublicPath <> '' then
    Result := FPublicPath
  else
    Result := IncludePathDelimiter(Web.PublicPath) + Route;
end;

{ TmnwWebModule }

procedure TmnwWebModule.DoMatched(ARequest: TmodRequest);
begin
  inherited;
  if (ARequest.Route.Count > 0) then
  begin
    // Files with extensions are treated as commands (e.g. favicon.ico)
    if StartsStr('.', ARequest.Route[ARequest.Route.Count - 1]) then
      ARequest.Command := ARequest.Route[ARequest.Route.Count - 1]
    else
      ARequest.Command := ARequest.Route[0]; //TODO or 1?
  end
  else
    ARequest.Command := '';
end;

function TmnwWebModule.GetDefaultURL: string;
begin
  Result := inherited;
  if Web.DefaultSchema <> nil then
     Result := Result+ AddStartURLDelimiter(Web.DefaultSchema.Name);
end;

procedure TmnwWebModule.Start;
begin
  inherited;
  if Web.PublicPath = '' then
    Web.PublicPath := PublicPath;
  if Web.PrivatePath = '' then
    Web.PrivatePath := PrivatePath;
  if Web.ModuleName = '' then
    Web.ModuleName := AliasName;
  //Web.Assets.PublicPath := Web.PublicPath;
  Web.IsSecure := Server.IsSecure;

//Maybe leave it to extract it from request
{  if Web.Domain = '' then
    Web.Domain := Domain;
  if Web.Port = '' then
    Web.Port := Server.UsedPort;}

  Web.Start;
  Renderers.FStarted := True;
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

constructor TmnwWebModule.Create(AServer: TmodModuleServer; const AName: string; const AAliasName: String);
begin
  FWeb := TmnwWeb.Create;
  inherited;
end;

{ TElementClasses }

function TElementClasses.Add(const Name: string; Area: TAttributeArea): Integer;
begin
  if Name = '' then
    raise Exception.Create('Classs.Add needs a name');

  Result:= IndexOf(Name);
  if Result < 0 then
  begin
    Items := Items + [TElementClass.Create(Name, Area)];
    Result := Length(Items) - 1;
  end
  else
  begin
    Items[Result].Area := Area;
    Items[Result].Used := False;
  end;
end;

procedure Classes_StrToStringsExCallbackProc(Sender: Pointer; Index, CharIndex, NextIndex: Integer; S: string; var Resume: Boolean);
type
  PElementClasses = ^TElementClasses;
begin
  PElementClasses(Sender)^.Add(S);
end;

procedure TElementClasses.Append(const S: string; Area: TAttributeArea);
var
  strings: TStringList;
  itm: string;
begin
  strings := TStringList.Create;
  try
    StrToStrings(S, strings, [' '], [], []);
    for itm in strings do
    begin
      Add(itm, Area);
    end;
  finally
    strings.Free;
  end;
end;

class operator TElementClasses.Add(A: TElementClasses; B: string): TElementClasses;
begin
  A.Add(B);
  Result := A;
end;

function TElementClasses.AddIf(const Name: string; Area: TAttributeArea): Integer;
begin
  if Name <> '' then
    Result := Add(Name, Area);
end;

function TElementClasses.Add(const AClass: TElementClass): Integer;
begin
  Items := Items + [AClass];
  Result := Length(Items) - 1;
end;

function TElementClasses.AddIf(Condition: Boolean; const Name: string; Area: TAttributeArea): Integer;
begin
  if Condition then
    Result := Add(Name, Area)
  else
    Result := -1;
end;

procedure TElementClasses.Append(A: TElementClasses);
var
 itm : TElementClass;
begin
  for itm in A.Items do
  begin
    Add(itm);
  end;
end;

procedure TElementClasses.Clear;
begin
  Items := nil;
end;

function TElementClasses.Exists(const Name: string): Boolean;
begin
  Result := IndexOf(Name) >= 0;
end;

class operator TElementClasses.Explicit(const Source: string): TElementClasses;
begin
  InitMemory(Result, SizeOf(Result));
  Result.Append(Source)
end;

class operator TElementClasses.Implicit(Source: string): TElementClasses;
begin
  InitMemory(Result, SizeOf(Result));
  Result.Append(Source)
end;

class operator TElementClasses.Implicit(Source: TElementClasses): string;
begin
  Result := Source.ToString
end;

function TElementClasses.IndexOf(const Name: string): Integer;
var
 i: Integer;
begin
  for i := 0 to Length(Items) -1 do
  begin
    if SameText(Name, Items[i].Name) then
      exit(i)
  end;
  Result := -1
end;

procedure TElementClasses.Init(classes: string);
begin
  InitMemory(Self, SizeOf(Self));
  Append(classes);
end;

function TElementClasses.Remove(const Name: string): Boolean;
var
  index: integer;
begin
  index := IndexOf(Name);
  Result := index >= 0;
  if Result then
    Delete(Items, index, 1);
end;

class operator TElementClasses.Subtract(A: TElementClasses; B: string): TElementClasses;
var
  i: Integer;
begin
  i := A.IndexOf(B);
  if i>=0 then
    Delete(A.Items, i, 1);
  Result := A;
end;

function TElementClasses.ToFullString(Area: TAttributeAreas): string;
begin
  Result := ToString(Area);
  if Result <> '' then
    Result := 'class=' + DQ(Result);
end;

function TElementClasses.ToString(Area: TAttributeAreas): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to Length(Items) -1 do
  begin
    if (Area = []) or (Items[i].Area in Area) then
      if not Items[i].Used then
      begin
        if Result <> '' then
          Result := Result + ' ' + Items[i].Name
        else
          Result := Items[i].Name;
        Items[i].Used := True; //keep Items[i]
      end;
  end;
end;

{ TmnwScope }

procedure TmnwScope.Free;
begin  
  FreeAndNil(Attributes);
  Element := nil;
  Classes := Default(TElementClasses);
  WrapClasses := Default(TElementClasses);
end;

function TmnwScope.ToString(Area: TAttributeAreas; WithSpace: Boolean): string;
begin
  Result := Classes.ToString(Area);

  if Result <> '' then
    Result := 'class=' + DQ(Result);

  Result := SpaceIf(Result, Attributes.ToString(Area));
    
  if WithSpace and (Result <> '') then
    Result := ' ' + Result;
end;

constructor TmnwScope.Create(AElement: TmnwElement);
begin
  inherited Create;
  Attributes := TmnwAttributes.Create;
  Element := AElement;
end;

function TmnwScope.ToString(WithSpace: Boolean): string;
begin
  Result := ToString([ssOuter, ssInner], WithSpace);
end;

{ THTML.TLink }

constructor THTML.TLink.Create(AParent: TmnwElement; const ALocation: string; ACaption: string);
begin
  inherited Create(AParent);
  Location := ALocation;
  FCaption := ACaption;
end;

{ THTML.TCollapseCaption }

procedure THTML.TCollapseCaption.DoCompose(const Ctx: TmnwContext);
begin
  inherited;
end;

{ THTML.TDropdown }

procedure THTML.TDropdown.Created;
begin
  inherited;
  Options := [dropArrow];
end;

{ THTML.TPopupMenu }

procedure THTML.TPopupMenu.Created;
begin
  inherited;
  Items := TStringList.Create;
end;

destructor THTML.TPopupMenu.Destroy;
begin
  FreeAndNil(Items);
  inherited;
end;

{ TZoomButtons }

procedure TZoomButtons.Created;
begin
  inherited;
  FButtonSmall := THTML.TToolButton.Create(Self, [elEmbed]);
  FButtonSmall.Data := 'small';
  FButtonSmall.Style := styleUndefined;
  FButtonSmall.Image.Symbol := 'icon mnw-scale-down';
  FButtonSmall.CallScript := 'mnw.switch_zoom(event)';

  FButtonNormal := THTML.TToolButton.Create(Self, [elEmbed]);
  FButtonNormal.Data := 'normal';
  FButtonNormal.Style := styleUndefined;
  FButtonNormal.Image.Symbol := 'icon mnw-scale-reset';
  FButtonNormal.CallScript := 'mnw.switch_zoom(event)';

  FButtonLarge := THTML.TToolButton.Create(Self, [elEmbed]);
  FButtonLarge.Data := 'large';
  FButtonLarge.Style := styleUndefined;
  FButtonLarge.Image.Symbol := 'icon mnw-scale-up';
  FButtonLarge.CallScript := 'mnw.switch_zoom(event)';
end;

{ THTML.THTMLGroup }

function THTML.THTMLGroup.CanRender: Boolean;
begin
  Result := inherited CanRender and (Count > 0);
end;

{ THTML.TSpan }

constructor THTML.TSpan.Create(AParent: TmnwElement; const AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

constructor THTML.TCustomButton.Create(AParent: TmnwElement; const ACaption: string);
begin
  inherited Create(AParent);
  Caption := ACaption;
end;

procedure THTML.TCustomButton.Created;
begin
  inherited;
  Style := stylePrimary;
end;

{ TmnwContext }

function TmnwContext.GetPath: string;
begin
  Result := GetPath(Schema);
end;

function TmnwContext.GetPath(e: TmnwElement): string;
begin
  Result := GetBasePath + StartURL(e.GetPath);
end;

function TmnwContext.GetPort: string;
begin
  Result := Web.Port;
  if Result = '' then
    Result := Request.Port;
end;

function TmnwContext.GetRelativePath(e: TmnwElement): string;
begin
  if Element = nil then  
    Result := e.GetPathTo(Schema)
  else
    Result := e.GetPathTo(Element);
end;

function TmnwContext.GetRequest: TwebRequest;
begin
  Result := Response.Request;
end;

function TmnwContext.GetRequestURL: string;
begin
  Result := EndUrl(GetHostURL) + Request.URI;
end;

function TmnwContext.GetSchemaURL: string;
begin
  Result := GetHostURL + GetPath(Schema);
end;

function TmnwContext.GetSession: TmnwSession;
begin
  if Response <> nil then
    exit(Response.Session);
  Result := nil;  
end;

function TmnwContext.GetURL: string;
begin
  Result := GetURL(Schema);
end;

procedure TmnwContext.Require(ALibraryClass: TmnwLibraryClass; Priority: Integer);
begin
  Renderer.Require(ALibraryClass);
end;

function TmnwContext._T(const Key, Default: string): string;
begin
  Result := mnWebElements._T(Key, Language, Default);
end;

function TmnwContext.GetBasePath: string;
begin
  Result := RemoveEndURLDelimiter(StartURL(Request.BasePath));
end;

function TmnwContext.GetHomeURL: string;
begin
  Result := GetHostURL + GetBasePath;
end;

function TmnwContext.GetHostURL: string;
begin
  Result := ComposeHttpURL(Request.IsSecure, Domain, Port);
end;

function TmnwContext.GetLocationPath(AElement: TmnwElement; Location: TLocation): string;
begin
  if Location.Where = toSchema then
    Result := EndURL(GetPath(Schema))
  else if Location.Where = toElement then
    Result := EndURL(GetPath(AElement))
  else if Location.Where = toHome then
    Result := EndURL(GetBasePath)
  else if Location.Where = toDefault then
    Result := GetDefaultPath;
    
  if Location.Where = toCustom then
    Result := EndURL(Location.Custom)
  else if Location.Custom <> '' then
    Result := EndURL(Result) + EndURL(Location.Custom);
    
  if Location.WithQuery and (Request.Query <> '') then
    Result := Result + '?' + Request.Query;
end;

function TmnwContext.GetURL(e: TmnwElement): string;
begin
  Result := GetHomeURL + StartURL(e.GetPath);
end;

function TmnwContext.GetAssetDir: string;
begin
  if Schema.Web.Assets <> nil then
    Result := Schema.Web.Assets.PublicPath
  else
    Result := Schema.Web.PublicPath;
end;

function TmnwContext.GetAssetsPath: string;
begin
  Result := GetPath(Schema.Web.Assets);
end;

function TmnwContext.GetAssetsURL: string;
begin
  Result := GetURL(Schema.Web.Assets);
end;

function TmnwContext.GetDefaultPath: string;
begin
  Result := GetHomeURL;
  if Schema.Web.DefaultSchema <> nil then  
    Result := Result + StartURL(Schema.Web.DefaultSchema.Name);
end;

function TmnwContext.GetDomain: string;
begin
  Result := Web.Domain;
  if Result = '' then
    Result := Request.Domain;
end;

{ TAuthSchema }

procedure TAuthSchema.UserLogin(const Ctx: TmnwContext);
var
  aSuccess: Boolean;
  aMessage: string;
  aSessionID: string;
begin
  aSuccess := False;
  aSessionID := '';
  aMessage := '';
  DoLogin(Ctx, aSuccess, aMessage, aSessionID);

  if aSuccess then
  begin
    Ctx.Session.ID := aSessionID;
    if Ctx.Request.RequestType = rtJSONData then
       Ctx.Response.RespondJSON('{"type": "success", "state": "200", "message": "Login successed.", "redirect": "'+Ctx.GetDefaultPath+'" }')
    else
      Ctx.Response.RespondRedirectTo(Ctx.GetDefaultPath);
  end
  else
  begin
    Ctx.Response.RespondJSON('{"type": "error", "state": "301", "message": "'+aMessage+'" }', hrUnauthorized);
  end;    
end;

procedure TAuthSchema.UserLogout(const Ctx: TmnwContext);
begin
  Ctx.Session.ID := '';
  Ctx.Response.RespondRedirectTo(Ctx.GetDefaultPath);
end;

procedure TAuthSchema.DoRespondHeader(const Ctx: TmnwContext);
begin
  if (Ctx.Data <> nil) and Ctx.Data.Values['password'].IsExists then
  begin
    UserLogin(Ctx);
  end;
  inherited;
end;

procedure TAuthSchema.Created;
begin
  inherited;
  with Document.Body.Main do
  begin
    FLoginCard := TCard.Create(this);
    with FLoginCard do
    begin
      FAuth := TAuthForm.Create(This);              
      Auth.Form.CancelTo := toDefault;
  end;
  end;
end;

procedure TAuthSchema.DoChildRespond(AElement: TmnwElement; const Ctx: TmnwContext);
begin
  inherited;
  if (AElement.Name = 'login-form') and (Ctx.Data <> nil) and (Ctx.Data.Values['password'].IsExists) then
  begin
    UserLogin(Ctx);
  end;
end;

procedure TAuthSchema.DoCompose(const Ctx: TmnwContext);
begin
  inherited;
  with Document do
  begin
    Title := 'Login';
    //Direction := dirLeftToRight; We take it from language

    TAction.Create(This, 'login', 'login', UserLogin);
    TAction.Create(This, 'logout', 'logout', UserLogout);

    with Body do
    begin
      Header.RenderIt := False;
      
      with Main do
      begin
        with FLoginCard do
        begin
          Solitary := True;
          Size := szSmall;
          Mode := emdColumn;
          Caption := Ctx._T('login', 'Login');
//          Auth.Compose(Ctx);
        end;
      end;
    end;
  end;
end;

procedure TAuthSchema.DoLogin(const Ctx: TmnwContext; var Success: Boolean; var Message: string; var SessionID: string);
begin
end;

procedure TAuthSchema.DoLogout(const Ctx: TmnwContext);
begin
end;

{ THTML.TImageFile }

function THTML.TImageFile.CanRender: Boolean;
begin
  Result := (inherited CanRender) and (FileName <> '');
end;

procedure THTML.TImageFile.DoRespond(const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Response.SendFile(FileName);
end;

function THTML.TImageFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

function THTML.TImageFile.GetRoute: String;
begin
    Result := ExtractFileName(FFileName);
  if Result = '' then
    Result := inherited GetRoute;
end;

procedure THTML.TImageFile.SetFileName(const Value: string);
begin
  FFileName := Value;
  //Route := ExtractFileName(FFileName);
end;

{ TAuthForm }

procedure TAuthForm.Created;
begin
  inherited;
  FForm := THTML.TForm.Create(This);
end;

procedure TAuthForm.DoCompose(const Ctx: TmnwContext);
begin
  Caption := Ctx._T('login', 'Login');

  with THTML, Self do
  begin
    with Form do
    begin
      Route := 'login';
      Name := 'login-form';
      Endpoint.Where := toElement;

      CallScript := 'mnw.formPost(event)';

      with TUsername.Create(This) do
      begin
        LabelLayout := lfFloating;
        ID := 'username';
        Name := 'username';
        Caption := Ctx._T('username', 'Username');
        PlaceHolder := Ctx._T('type.user.name', 'Type user name');
        AutoFocus := True;
        Required := True;
      end;

      with TPassword.Create(This) do
      begin
        LabelLayout := lfFloating;
        ID := 'password';
        Name := 'password';
        Caption := Ctx._T('password', 'Password');
        Token := Ctx.Web.PasswordToken;
      end;

      TParagraph.Create(This, Ctx._T('you.need.numbers', 'You need to use letters numbers'));

      if JWTMode or Ctx.Web.JWTmode then
      begin
        THiddenInput.Create(This, 'JWTMode', 'True');
      end;

      Submit.Caption := Ctx._T('submit',  'Submit');
      Reset.Caption := Ctx._T('reset',  'Reset');
      Cancel.Caption := Ctx._T('cancel', 'Cancel') ;
    end;
  end;
  inherited;
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
  if Started then
    raise Exception.Create('Once web app started you can''t register Renderer');

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

function TLibrarySources.Add(SourceType: TLibrarySourceType; Where: TLibrarySourceWhere; const OnlineFile, LocalFile: string; Integrity: string; Options: TLibraryOptions): TmnwLibrarySource; 
begin
  Result := TmnwLibrarySource.Create;

{ From where we get Name

  online = 'min.file.js', local='file.min.js'
                  ^
  online = 'file.js', local='c:\assets\file.min.js'
               ^
  online = 'https://online.com/min.file.js', local='file.min.js'
                                                        ^
  online = 'https://online.com/', local='file.min.js'
                                              ^
  online = '', local='file.min.js'
                            ^
  online = 'https://online.com/file.js', local=''
                                  ^
  online = 'https://online.com/file.js', local='c:\assets\file.min.js'
                                  ^
}
  Result.OnlineFile := OnlineFile;
  Result.LocalFile := CorrectPath(VarEnvReplace(LocalFile, [vrPathValues]));

  //online = 'https://online.com/min.file.js', local=''
  if (OnlineFile <> '') and (Result.LocalFile = '') then
  begin
    Result.Name := SubStr(OnlineFile, PathDelimiters, -1);
  end
  //online = '', local='file.min.js'
  //online = '', local='/path/file.min.js'
  else if (OnlineFile = '') and (Result.LocalFile <> '') then
  begin
    Result.Name := SubStr(LocalFile, PathDelimiters, -1);
    //Result.OnlineFile := OnlineFile;
  end
  //online = 'https://online.com/min.file.js', local='path/to/file/'
  else if not EndsDelimiter(OnlineFile) and EndsDelimiter(Result.LocalFile) then
  begin
    Result.Name := SubStr(OnlineFile, PathDelimiters, -1);
  end
  //online = 'https://online.com/path/', local='path/to/file/file.js'
  else if EndsDelimiter(OnlineFile) and not EndsDelimiter(Result.LocalFile) then
  begin
    Result.Name := SubStr(LocalFile, PathDelimiters, -1);
    Result.OnlineFile := OnlineFile + Result.Name; //Fix online to full url
  end
  //online = 'https://online.com/path/file.js', local='path/to/file/file.js'
  else
  begin
    if HaveChar(OnlineFile, PathDelimiters) then
      Result.Name := SubStr(LocalFile, PathDelimiters, -1)
    else
      Result.Name := SubStr(OnlineFile, PathDelimiters, -1);
  end;

  if Result.Name = '' then
    raise Exception.Create('Library: We need can''t guess alias name of source');

  Result.SourceType := SourceType;
  Result.Where := Where;
  Result.Integrity := Integrity;
  Result.Options := Options;
  inherited Add(Result);
end;

function TLibrarySources.Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFile: string; Integrity: string; Options: TLibraryOptions): TmnwLibrarySource;
begin
  Result := Add(SourceType, stOnline, OnlineFile, LocalFile, Integrity, Options);
end;

function TLibrarySources.AddEmbed(const SourceType: TLibrarySourceType; const AName: string; const EmbedText: string): TmnwLibrarySource;
begin
  Result := TmnwLibrarySource.Create;
  Result.Name := AName;
  Result.Text := EmbedText;
 
  Result.SourceType := SourceType;
  Result.Where := stEmbed;
  inherited Add(Result);
end;

function TLibrarySources.Add(SourceType: TLibrarySourceType; const OnlineFile, LocalFile: string; Direction: TDirection): TmnwLibrarySource;
begin
  Result := Add(SourceType, stOnline, OnlineFile, LocalFile, '');
end;

function TLibrarySources.AddStyle(const EmbedText: string; AName: string; Direction: TDirection): TmnwLibrarySource;
begin
  Result := AddEmbed(stStyle, AName, EmbedText);
  Result.Direction := Direction;
end;

{ TmnwLibrarySource }

constructor TmnwLibrarySource.Create;
begin
  inherited;
  Options := [libDefer, libCross];
end;

{ TmnwRequires }

procedure TmnwRequires.Use(ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryClass);
  if ALibrary <> nil then
    exit; //Already used
  ALibrary := Libraries.Find(ALibraryClass);
  if ALibrary = nil then
    ALibrary := Libraries.RegisterLibrary(ALibraryClass);
  if ALibrary <> nil then
    Add(ALibrary)
  else
    raise Exception.Create('Can''t register library: ' + ALibraryClass.ClassName);
end;

function TmnwRequires.Compare(Item1, Item2: TmnwLibrary): Integer;
begin
  if Item1.Priority < Item2.Priority then
    Result := -1
  else if Item1.Priority > Item2.Priority then
    Result := 1
  else
    Result := 0;
  //TODO use DependsOn
end;

function TmnwRequires.Find(ALibraryClass: TmnwLibraryClass): TmnwLibrary;
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

procedure TmnwRequires.Use(ALibraryName: string);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryName);
  if ALibrary <> nil then
    exit; //Already used
  ALibrary := Libraries.Find(ALibraryName);
  if ALibrary <> nil then
    Add(ALibrary)
  else
    raise Exception.Create('There is no library such: ' + ALibraryName);
end;

{ TmnwHTMLRenderer.THTMLElement }

procedure TmnwHTMLRenderer.THTMLElement.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  {$ifopt D+}
  Scope.Attributes.Add('data-mnw-class', Scope.Element.ClassName, ssOuter);
  {$endif}
  if Scope.Element.Data <> '' then
    Scope.Attributes.Add('data-mnw-value', Scope.Element.Data, ssInner);
end;

procedure TmnwHTMLRenderer.THTMLElement.DoEnterRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  if Scope.Element.Comment <> '' then
    Ctx.Writer.WriteLn('<!-- ' + Scope.Element.Comment + ' -->');
  inherited;
end;


{ TmnwHTMLRenderer }

class constructor TmnwHTMLRenderer.Register;
begin
  //with ElementRenderers do
  begin 
    //RegisterRenderer(THTML.THTMLElement, THTMLElement);
  end;
end;

class procedure TmnwHTMLRenderer.RegisterElements;
begin
  with ElementRenderers do
  begin
    RegisterRenderer(THTML.THTMLElement, THTMLElement);
    RegisterRenderer(THTML.TComment ,TComment);
    RegisterRenderer(THTML.TJSScript,TJSScript);    

    RegisterRenderer(THTML.TDocument, TDocument);
    RegisterRenderer(THTML.TBody, TBody);

    RegisterRenderer(THTML.TOutput, TOutput);
    RegisterRenderer(THTML.TCompose, TCompose);
    RegisterRenderer(THTML.TIntervalCompose, TIntervalCompose);
    
    RegisterRenderer(THTML.TFile, TFile);
    RegisterRenderer(THTML.TJSFile, TJSFile);
    RegisterRenderer(THTML.TCSSFile, TCSSFile);
  end;
end;

{ TmnwHTMLRenderer.TComment }

procedure TmnwHTMLRenderer.TComment.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TComment;
begin
  inherited;
  e := Scope.Element as THTML.TComment;
  Ctx.Writer.AddComment(e.Comment);
end;

{ TmnwHTMLRenderer.TFile }

procedure TmnwHTMLRenderer.TFile.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TFile;
begin
  e := Scope.Element as THTML.TFile;
  if ftEmbed in e.Options then
    Scope.Element.Respond(Ctx);
  inherited;
end;

{ TmnwHTMLRenderer.TJSFile }

procedure TmnwHTMLRenderer.TJSFile.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TJSFile;
  src: string;
begin
  e := Scope.Element as THTML.TJSFile;
  if ftEmbed in e.Options then
  begin
    Ctx.Writer.OpenTag('script', 'type="text/javascript"' + Scope.ToString(True));
    inherited;
    Ctx.Writer.WriteLn('');
    Ctx.Writer.CloseTag('script');
  end
  else
  begin
    src := Ctx.GetPath(e);
    Ctx.Writer.AddTag('script', 'type="text/javascript"' + When(e.Defer, ' defer') +' src='+ DQ(src+'?v='+IntToStr(Ctx.Schema.Web.TimeStamp)));
    inherited;
  end;
end;

{ TmnwHTMLRenderer.TCSSFile }

procedure TmnwHTMLRenderer.TCSSFile.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TCSSFile;
  src: string;
begin
  e := Scope.Element as THTML.TCSSFile;
  if ftEmbed in e.Options then
  begin
    Ctx.Writer.OpenTag('style', 'type="text/css"'+ Scope.ToString(True));
    inherited;
    Ctx.Writer.WriteLn();
    Ctx.Writer.CloseTag('style');
  end
  else
  begin
    src := Ctx.GetPath(e);
    Ctx.Writer.AddTag('link', 'rel="stylesheet" href='+ DQ(src+'?v='+IntToStr(Ctx.Schema.Web.TimeStamp)));
    inherited;
  end;
end;

{ TmnwHTMLRenderer.TCompose }

procedure TmnwHTMLRenderer.TCompose.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  Ctx.Writer.OpenTag('div', Scope.Attributes.ToString);
  inherited;
  Scope.Element.Respond(Ctx);
  Ctx.Writer.CloseTag('div');
end;

{ TmnwHTMLRenderer.TIntervalCompose }

procedure TmnwHTMLRenderer.TIntervalCompose.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Attributes['data-mnw-refresh-url'] := Ctx.GetPath(Scope.Element);
end;

{ TmnwHTMLRenderer.TBody }

procedure TmnwHTMLRenderer.TBody.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  inherited;
  Scope.Attributes.Delete('Name'); //* Not for HTML tag
  if e.Schema.RefreshInterval <> 1 then //* not default, 0 Disable it
    Scope.Attributes['data-mnw-refresh-interval'] := e.Schema.RefreshInterval.ToString;
  if Ctx.Schema.Interactive then
    Scope.Attributes['data-mnw-interactive'] := 'true';
  if e.FontName<>'' then
    Scope.Attributes['style'] := 'font-family: '+SQ(e.FontName)+'!important;';    
end;

procedure TmnwHTMLRenderer.TBody.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;  
end;

procedure TmnwHTMLRenderer.TBody.DoLeaveRender(Scope: TmnwScope; const Ctx: TmnwContext);
begin
  inherited;
end;

{ TmnwHTMLRenderer.TDocument }

procedure TmnwHTMLRenderer.TDocument.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  if Ctx.Direction = dirRightToLeft then
    Scope.Attributes['dir'] := 'rtl'
  else if Ctx.Direction = dirLeftToRight then
    Scope.Attributes['dir'] := 'ltr';
  Scope.Attributes['lang'] := When(Ctx.Language <> '', Ctx.Language, 'en');
end;

procedure TmnwHTMLRenderer.TDocument.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TDocument;
  aLibrary: TmnwLibrary;
//  o: TmnwElement;
//  r: THTMLElement;
begin
  e := Scope.Element as THTML.TDocument;
  Scope.Attributes.Delete('Name'); //* Not for HTML tag
  Ctx.Writer.WriteLn('<!DOCTYPE html>');
  Ctx.Writer.OpenTag('html', Scope.ToString);
  Ctx.Writer.OpenTag('head');
  Ctx.Writer.AddTag('title', '', e.Title);
  //Ctx.Writer.AddShortTag('link', 'rel="shortcut icon" href="#"');
  Ctx.Writer.AddShortTag('link', 'rel="icon" href="data:,"'); //disable call favicon.ico
  Ctx.Writer.AddShortTag('meta', 'charset="UTF-8"');
  Ctx.Writer.AddShortTag('meta', 'name="viewport" content="width=device-width, initial-scale=1"');
  if e.Parent <> nil then // Only root have head
  begin
    AddHead(Scope, Ctx);
    //* Library Head
    for aLibrary in Renderer.Requires do
    begin
      aLibrary.AddHead(Ctx);
    end;
    //* Renderer Head
    Renderer.AddHead(Ctx);
  end;

  //* Collect head from childs
  {for o in Scope.Element do
  begin
    if o is THTML.THTMLElement then
    begin
      r := Renderer.CreateRenderer(o) as THTMLElement;
      try
        r.AddHeader(o, Ctx);
      finally
        r.free;
      end;
    end;
  end;}
  Ctx.Writer.CloseTag('head');
  e.Body.Render(Ctx);
  Ctx.Writer.CloseTag('html');
end;

{ TLangDropdown }

procedure TLangDropdown.Created;
begin
  inherited;  
//  Route := 'LLL';
end;

procedure TLangDropdown.DoCompose(const Ctx: TmnwContext);
begin
  inherited;
  if Ctx.Language = 'ar' then
    Image.Symbol := 'icon mnw-lang-arabic'             
  else
    Image.Symbol := 'icon mnw-lang-english';              
//  Caption := Ctx.Language.ToUpper;
//  Hint := Ctx.Language.ToUpper;    TODO fix it

  with THTML.TDropdownItem.Create(this, Ctx.GetURL(Self) + '?lang=ar', 'عربي') do
    Image.Symbol := 'icon mnw-lang-arabic';              
  with THTML.TDropdownItem.Create(this, Ctx.GetURL(Self) + '?lang=en', 'English') do
    Image.Symbol := 'icon mnw-lang-english';              
end;

procedure TLangDropdown.DoRespond(const Ctx: TmnwContext);
var
  Lang: string;
  Referer: string;
  Cookie: TmnwCookie;
begin
  Lang := Ctx.Request.Params['lang'];
  if Lang = '' then
    Lang := Ctx.Web.Language;

  Cookie := Ctx.Response.SetCookie('language', Lang);
  if Cookie <> nil then
  begin
    Cookie.Domain := Ctx.Domain;
    Cookie.Path := Ctx.GetBasePath;
    Cookie.Age := 365 * 24 * 60 * 60; // 1 year
  end;

  Referer := Ctx.Request.Header['Referer'];
  if Referer <> '' then
    Ctx.Response.RespondRedirectTo(Referer)
  else
    Ctx.Response.RespondRedirectTo(Ctx.GetURL);
end;

{ TmnwSession }

constructor TmnwSession.Create;
begin
  FAge := 86400;
end;

procedure TmnwSession.Reset;
begin
  FChanged := False;
end;

procedure TmnwSession.SetAge(const Value: Integer);
begin
  if FAge = Value then
    exit;
  FAge := Value;
  FChanged := True;
end;

procedure TmnwSession.SetDomain(const Value: string);
begin
  if FDomain = Value then
    exit;
  FDomain := Value;
  FChanged := True;
end;

procedure TmnwSession.SetID(const Value: string);
begin
  if FID = Value then
    exit;
  FID := Value;
  FChanged := True;
end;

procedure TmnwSession.SetInstance(const AInstance: TObject);
begin
  SetInteralInstance(AInstance);
end;

procedure TmnwSession.SetInteralInstance(const Value: TObject);
begin
  FInstance := Value;
end;

procedure TmnwSession.SetPath(const Value: string);
begin
  if FPath = Value then
    exit;
  FPath := Value;
  FChanged := True;  
end;

{ TmnwResponse }

constructor TmnwResponse.Create(ARequest: TmodRequest);
begin
  inherited;
  FSession := TmnwSession.Create;
end;

destructor TmnwResponse.Destroy;
begin
  FreeAndNil(FSession);
  inherited;
end;

procedure TmnwResponse.DoSendHeader;
begin
  inherited;
end;

procedure TmnwResponse.DoSetCookies;
var
  aOptions: TmnwCookieOptions;
begin
  inherited;
  if Session.Changed then  
  begin
    aOptions := [HttpOnly];
    if Request.IsSecure then
      aOptions := aOptions + [Secured];

    Cookies.SetCookie(Session.Domain, Session.Path, 'session', Session.ID, aOptions, When(Session.ID <> '', Session.Age, 0));
    Session.Reset;
  end;
end;

{ TmnwHTMLRenderer.TJSScript }

procedure TmnwHTMLRenderer.TJSScript.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TJSScript;
begin
  e := Scope.Element as THTML.TJSScript;

  Ctx.Writer.OpenTag('script', 'type="text/javascript"' + Scope.ToString(True));
  inherited;
  Ctx.Writer.WriteLines(e.Script);
  Ctx.Writer.CloseTag('script');
end;

{ THTML.TJSScript }

constructor THTML.TJSScript.Create(AParent: TmnwElement; AScript: string);
begin
  inherited Create(AParent);
  Script := AScript;
end;

{ THTML.TInput }

constructor THTML.TInput.Create(AParent: TmnwElement; ACaption: string; AValue: string);
begin
  inherited Create(AParent);
  Value := AValue;
  Caption := ACaption;
  AutoComplete := True;
end;

{ THTML.TSelect }

procedure THTML.TSelect.Created;
begin
  inherited;
  FItems := TmnNameValueObjectList<TmnNameValueObject>.Create;
end;

destructor THTML.TSelect.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

{ THTML.TTextArea }

constructor THTML.TTextArea.Create(AParent: TmnwElement; ACaption: string; AText: string);
begin
  inherited Create(AParent);
  Caption := ACaption;
  Text := AText;
  Rows := 3;
end;

{ THTML.TCheckbox }

procedure THTML.TCheckbox.Created;
begin
  inherited;
  FValue := 'true';
end;

{ THTML.THTMLFormControl }

procedure THTML.THTMLFormControl.SetCaption(const AValue: string);
begin
  if FCaption =AValue then Exit;
  FCaption :=AValue;
end;

{ THTML.TUsername }

procedure THTML.TUsername.Created;
begin
  inherited;
end;

{ THTML.TCountInput }

procedure THTML.TCountInput.Created;
begin
  inherited;
  Min := 0; //Defaults
  Max := 100;
end;

{ THTML.TAccordionSection }

function THTML.TAccordionSection.CanRender: Boolean;
begin
  Result := Count > 0;
end;

{ THTML.THiddenInput }

constructor THTML.THiddenInput.Create(AParent: TmnwElement; const AName: string; const AValue: string);
begin
  inherited Create(AParent);
  Name := AName;
  Value := AValue;
end;

{ THTML.TThemeButton }

procedure THTML.TThemeButton.Created;
begin
  inherited;
  Style := styleUndefined;
  Image.Symbol := 'icon mnw-theme';
  CallScript := 'mnw.switch_theme(event)';
end;

{ TDarklyTheme_Library }

procedure TDarklyTheme_Library.Created;
begin
  inherited;
  Sources.Add(stStyle, stResource, 'darkly.css', '?minilib\web\source\bs_darkly.css', '', []);
end;

{ TCustomTheme_Library }

procedure TCustomTheme_Library.Created;
begin
  inherited;
  Sources.Add(stStyle, stResource, 'custom.css', '?minilib\web\source\bs_custom.css', '', []);
end;

{ TmnwHTMLRenderer.TOutput }

procedure TmnwHTMLRenderer.TOutput.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: THTML.TOutput;
begin
  e := Scope.Element as THTML.TOutput;
  if Assigned(e.OnOutput) then
    e.OnOutput(Scope, Ctx);
  inherited;
end;

{ THTML.TOutput }

constructor THTML.TOutput.Create(AParent: TmnwElement; AOnOutput: TRenderProc);
begin
  inherited Create(AParent);
  OnOutput := AOnOutput;
end;

{ TGapHelper }

function TGapHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ THTML.THTMLLayout }

procedure THTML.THTMLLayout.Created;
begin
  inherited;
end;

{ THTML.TBox }

procedure THTML.TBox.Created;
begin
  inherited;
  Columns := 3;
end;

{ THTML.TLinkButton }

constructor THTML.TLinkButton.Create(AParent: TmnwElement; const ALocation, ACaption: string);
begin
  inherited Create(AParent, ACaption);
  Location := ALocation;
end;

{ TWidthHelper }

function TWidthHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ TElementClass }

constructor TElementClass.Create(AName: string; AArea: TAttributeArea);
begin
  Name := AName;
  Area := AArea;
end;

{ THTML.TRow }

procedure THTML.TRow.Created;
begin
  inherited;
  Wrap := True;
end;

{ TColSize }

class operator TColSize.Explicit(const Source: Integer): TColSize;
begin
  Result.Kind := cskNumber;
  Result.Columns := Source;
end;

class operator TColSize.Implicit(const Source: TColSize): Boolean;
begin
  Result := Source.Kind > cskUndefined;
end;

class operator TColSize.Implicit(Source: TColSize): Integer;
begin
  Result := Source.Columns;
end;

class operator TColSize.Implicit(Source: Integer): TColSize;
begin
  Result.Kind := cskNumber;
  Result.Columns := Source;
end;

class operator TColSize.Explicit(const Source: TWidthSize): TColSize;
begin
  Result.Max := Source;
  Result.Columns := 12;
  Result.Kind := cskMax;
end;

class operator TColSize.Implicit(Source: TWidthSize): TColSize;
begin
  Result.Max := Source;
  Result.Columns := 12;
  Result.Kind := cskMax;
end;

class operator TColSize.Implicit(Source: TColSizeKind): TColSize;
begin
  Result.Kind := Source;
  if Source <> cskNumber then
    Result.Columns := 0;
end;

{ TColCountHelper }

function TColCountHelper.ToString: string;
begin
  Result := IntToStr(Self);
end;

{ THTML.TCustomPanel }

procedure THTML.TCustomPanel.Created;
begin
  inherited;
//  Width := szMedium;
  Size := 8;
  Shadow := shadowHairline;
end;

{ THTML.TBadge }

constructor THTML.TBadge.Create(AParent: TmnwElement; const AText: string; AStyle: TItemStyle);
begin
  inherited Create(Parent);
  Text := AText;
  Style := AStyle;
end;

initialization
  Libraries.RegisterLibrary(TWebElements_Library, 2000);
  Libraries.RegisterLibrary(TDarklyTheme_Library, 2000);
  Libraries.RegisterLibrary(TCustomTheme_Library, 2050);
  Libraries.RegisterLibrary(TJQuery_Library);
finalization
  FreeAndNil(FRenderers);
  FreeAndNil(FLibraries);
  FreeAndNil(Languages);
end.
