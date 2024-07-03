unit mnWebElements;//* BETA
{$IFDEF FPC}
{$mode delphi}
{$modeswitch prefixedattributes}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$ENDIF}
{$H+}{$M+}
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *
 *}

{

    Application
      Document

┌──────┬──────── Header ──────────────────────┐
│ Column                                      │
│      │                                      │
├──────┴──────────────────────────────────────┤
│ Navigator/Menu                              │
├────────────┬─ Content ─────────────┬────────┤
│ ASideColumn │                      │ PeerColumn
│            │ ┌─ TabControl ──────┐ │        │
│            │ │ Tab │    │        │ │        │
│            │ ├─────┴────┴────────┤ │        │
│            │ │                   │ │        │
│            │ ├─ Form ────────────┤ │        │
│            │ │                   │ │        │
│            │ │ ┌─ Control ────┐  │ │        │
│            │ │ └──────────────┘  │ │        │
│            │ │                   │ │        │
│            │ │                   │ │        │
│            │ │                   │ │        │
│            │ │                   │ │        │
│            │ └───────────────────┘ │        │
│            │                       │        │
├────────────┴── Footer ─────────────┴────────┤
│                                             │
│                                             │
└─────────────────────────────────────────────┘

}

interface

uses
  Classes, SysUtils, Contnrs, Variants, Types, RTTI,
  {$ifdef FPC}
  LCLType, //* for RT_RCDATA
  {$endif}
	mnUtils, mnClasses, mnStreams, mnLogs,
  mnMultipartData, mnModules, mnWebModules;

{.$define rtti_objects}

type

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwOutput = class;
  TmnwElementRenderer = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwElementClass = class of TmnwElement;
  TrttiElementAttribute = class;
  TrttiElementAttributeClass = class of TrttiElementAttribute;

  { TrttiElementAttribute }

  TrttiElementAttribute = class(TCustomAttribute)
  public
    class procedure Update(Element: TmnwElement); virtual; abstract;
    constructor Create; //* Leave it
  end;

  { TrttiIDAttribute }

  TrttiIDAttribute = class(TrttiElementAttribute)
  private
  public
    class procedure Update(Element: TmnwElement); override;
  end;

  { TrttiRouteAttribute }

  TrttiRouteAttribute = class(TrttiElementAttribute)
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
  public
    function GetText(WithExtraSpace: Boolean = False): string;
    function HaveSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function SetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    function UnsetSubValue(const AName, AValue: String; vSeparators: TSysCharSet = [' ']): Boolean;
    procedure Append(AAttributes: TmnwAttributes);
    procedure Created; override;
  end;

  { TmnwContext }

  TmnwContext = record
    Sender: TObject;
    Renderer: TmnwRenderer;
    Output: TmnwOutput;
  end;

  { TmnwScope }

  TmnwScope = record
    Schema: TmnwSchema;
    Element: TmnwElement;
    Attributes: TmnwAttributes;
  end;

  TmnwObject = class(TmnNamedObject);

  TmnwLibrary = class abstract(TmnNamedObject)
  private
    FUsage: Integer;
  protected
    function GetSource(url: string): string; inline;
  public
    Source: string;
    procedure AddHead(AElement: TmnwElement; Context: TmnwContext); virtual; abstract;
    procedure IncUsage;
    procedure DecUsage;
    property Usage: Integer read FUsage;
  end;

  TmnwLibraryClass = class of TmnwLibrary;

  TmnwLibraries = class(TmnNamedObjectList<TmnwLibrary>)
  public
    procedure Use(ALibrary: TmnwLibrary); overload;
    procedure Use(ALibraryName: string); overload;
    procedure Use(ALibraryClass: TmnwLibraryClass); overload;
    function Find(ALibrary: TmnwLibraryClass): TmnwLibrary; overload;
    function ChangeSource(ALibraryClass: TmnwLibraryClass; NewSource: string): Boolean;
    procedure RegisterLibrary(ALibraryName: string; ALibraryClass: TmnwLibraryClass);
  end;

  TJQuery_Library = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; Context: TmnwContext); override;
  end;

  TmnwRequestState = (rsBeforeRequest, rsAfterRequest);

  TmnwElementKind = set of(
//    elRender,
    elEmbed, //* created by parent
    elInternal, //* do not render we will call it manually
    elHighLevel, //Rendered first like scripts
    elFallback //* if no child have the route name, it take the respond if have a name
  );

  TmnwAlign = (alignDefault, alignStart, alignCenter, alignStreach, alignEnd);
  TmnwFixed= (fixedDefault, fixedTop, fixedBottom);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FVisible: Boolean;
    FRenderIt: Boolean;
    FRoot: TmnwSchema;
    FParent: TmnwElement;

    FRoute: String;
    FComment: String;
    FID: String;
    FName: String;
    FStyleClass: String;
    FStyle: String;
    FAttributes: TmnwAttributes;
    FKind: TmnwElementKind;
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;
    procedure DoState(RequestState: TmnwRequestState); virtual;
    procedure State(RequestState: TmnwRequestState);
    procedure DoBeforeRender(Renderer: TmnwRenderer); virtual;
    procedure DoCompose; virtual;
    procedure DoRespondHeader(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); virtual;
    procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); virtual;
    procedure BeforeRender(Renderer: TmnwRenderer);
  public
    Composed: Boolean;
    constructor Create(AParent: TmnwElement; AKind: TmnwElementKind = []; ARenderIt: Boolean = True); virtual;
    destructor Destroy; override;
    procedure Add(O: TmnwElement); overload;
    function Add<O: TmnwElement>(const AID: String = ''; const AName: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function FindByRoute(const Route: string): TmnwElement;
    function FindByPath(const APath: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwElement read FParent;

    function GetPath: string; virtual;

    function CreateRender(Context: TmnwContext): TmnwElementRenderer;
    procedure Compose;
    procedure Clear; {$ifdef FPC} override; {$else} virtual; {$endif} //* see TmnObjectList

    function GetContentType(Route: string): string; virtual;

    procedure Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);

    procedure Render(Context: TmnwContext); overload;

    function Render(Renderer: TmnwRenderer; Sender: TObject; AOutput: TmnwOutput): Boolean; overload;
    function Render(Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): Boolean; overload;
    function Render(Renderer: TmnwRenderer; Sender: TObject; AStrings: TStrings): Boolean; overload;

    property Route: String read FRoute write FRoute; //TODO change it to Alias
    property Name: String read FName write FName;
    property StyleClass: String read FStyleClass write FStyleClass;
    property Style: String read FStyle write FStyle;
    property ID: String read FID write FID;
    property Comment: String read FComment write FComment;
    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;

    property RenderIt: Boolean read FRenderIt write FRenderIt;
    //* Embed render it directly not by loop like THeader
    property Attributes: TmnwAttributes read FAttributes;
    property Kind: TmnwElementKind read FKind write FKind;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (woEndLine, woOpenTag, woCloseTag);

  TmnwWriter = class(TmnNamedObject)
  private
    Level: Integer;
    NewLine: Boolean;
    FStream: TmnBufferStream;
  public
    constructor Create(AName: string; AStream: TmnBufferStream);
    procedure Write(S: string; Options: TmnwWriterOptions = []); virtual;
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

  { TmnwSchema }

  TmnwSchema = class(TmnwElement)
  private
    FCached: Boolean;
  protected
    NameingLastNumber: Integer;
    procedure GenID(Element: TmnwElement); inline;
    procedure GenRoute(Element: TmnwElement); inline;
    procedure DoRespond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
  public
    constructor Create(AParent: TmnwElement; AKind: TmnwElementKind = []; ARenderIt: Boolean = True); override;
    destructor Destroy; override;
    property Cached: Boolean read FCached write FCached;
  end;

  TmnwSchemaClass = class of TmnwSchema;

  { TmnwElementRenderer }

  TmnwElementRenderer = class(TObject)
  private
    FRenderer: TmnwRenderer;
  protected
    procedure DoCollectAttributes(Scope: TmnwScope); virtual;
    procedure DefaultRender(Scope: TmnwScope; Context: TmnwContext);

		procedure DoBeforeChildRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    procedure DoBeforeRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    procedure DoRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    procedure DoAfterRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    procedure DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    property Renderer: TmnwRenderer read FRenderer;
  public
    procedure Render(AElement: TmnwElement; Context: TmnwContext);
    constructor Create(ARenderer: TmnwRenderer); virtual; //useful for creating it by RendererClass.Create
    procedure CollectAttributes(Scope: TmnwScope);
  end;

  TmnwElementRendererClass = class of TmnwElementRenderer;

  { TmnwRenderer }

  TmnwRenderer = class(TmnwObject)
  public
    type

      TRegObject = class(TObject)
      public
        ObjectClass: TmnwElementClass;
        RendererClass: TmnwElementRendererClass;
      end;

      { TRegObjects }

      TRegObjects = class(TmnObjectList<TRegObject>)
      protected
        function Compare(Item1, Item2: TRegObject): Integer; override;
      public
        function FindDerived(AObjectClass: TmnwElementClass): TmnwElementClass;
        function Find(AObjectClass: TmnwElementClass; Nearst: Boolean = False): TRegObject;
        function FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
      end;
  private
    FLibraries: TmnwLibraries;
    FObjectClasses: TRegObjects;
    FParams: TmnwAttributes;
  protected
    {$ifdef rtti_objects}
    procedure RegisterClasses(ASchemaClass: TmnwSchemaClass);
    {$endif}
    procedure DoBeginRender; virtual;
    procedure DoEndRender; virtual;
    procedure InitObjects; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure BeginRender;
    procedure EndRender;

    procedure RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = True);
    function FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
    function CreateRenderer(AObjectClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property ObjectClasses: TRegObjects read FObjectClasses;
    property Params: TmnwAttributes read FParams;
    property Libraries: TmnwLibraries read FLibraries;
  end;

  { TmnwSchemaObject }

  TmnwSchemaObject = class(TmnNamedObject)
  public
    SchemaClass: TmnwSchemaClass;
    Schema: TmnwSchema;
    destructor Destroy; override;
  end;

  { TmnwSchemas }

  TmnwSchemas = class(TmnNamedObjectList<TmnwSchemaObject>)
  protected
    procedure SchemaCreated(Schema: TmnwSchema); virtual;
  public
    destructor Destroy; override;
    procedure RegisterSchema(AName: string; SchemaClass: TmnwSchemaClass);
    function Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): TmnwElement;
    //function Render(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): TmnwSchema;
  end;

  TDirection = (dirUnkown, dirLTR, dirRTL);

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  { THTML }

  THTML =class(TmnwSchema)
  private
  public
    type

      { THTMLElement }

      THTMLElement = class(TmnwElement)
      protected
      public
      end;

      { TContent }

      TContent = class abstract(THTMLElement)
      protected
        procedure Added(Item: TmnwElement); override;
      public
        Align: TmnwAlign;
        Fixed: TmnwFixed;
      end;

      TBody = class;
      THeader = class;
      TFooter = class;
      TContainer = class;

      [TrttiIDAttribute]

      { TDirectFile }

      TDirectFile = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        FileName: string;
        constructor Create(AParent: TmnwElement; AFileName: string = ''); reintroduce;
        function GetContentType(Route: string): string; override;
      end;

      { TEmbedFile }

      TEmbedFile = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        FileName: string;
        procedure Created; override;
        constructor Create(AParent: TmnwElement; AFileName: string = ''); reintroduce;
        function GetContentType(Route: string): string; override;
      end;

      { TJSResource }

      TJSResource = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        ResName: string;
        procedure Created; override;
        constructor Create(AParent: TmnwElement; AResName: string); reintroduce;
        function GetContentType(Route: string): string; override;
      end;

      { TJSEmbedFile }

      TJSEmbedFile = class(TEmbedFile)
      end;

      [TrttiIDAttribute]

      { TFile }

      TFile = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        function GetContentType(Route: string): string; override;
      end;

      { TAssets }

      TAssets = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        HomePath: string;
        function GetContentType(Route: string): string; override;
      end;

      { TCompose }

      TCompose = class(THTMLElement)
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        OnCompose: TProc;
        constructor Create(AParent: TmnwElement; AOnCompose: TProc = nil); reintroduce;
      end;

      [TrttiIDAttribute]
      [TrttiRouteAttribute]
      TIntervalCompose = class(TCompose)
      end;

      { TDocument }

      TDocument = class(TAssets)
      private
        FTitle: string;
        FVersion: integer;
        FBody: TBody;
      public
        Direction: TDirection;
        property Version: integer read FVersion write FVersion;
        property Title: string read FTitle write FTitle;
        procedure Created; override;
        destructor Destroy; override;
        property Body: TBody read FBody;
      end;

      { TBody }

      TBody = class(TContent)
      private
        function GetContainer: TContainer;
        function GetFooter: TFooter;
        function GetHeader: THeader;
      protected
        FHeader: THeader;
        FFooter: TFooter;
        FContainer: TContainer;
      public
        property Header: THeader read GetHeader;
        property Footer: TFooter read GetFooter;
        property Container: TContainer read GetContainer;
        procedure Created; override;
        destructor Destroy; override;
      end;

      THeader = class(TContent)
      public
        Text: string;
      end;

      TFooter = class(TContent)
      public
        Text: string;
      end;

      TContainer = class(TContent)
      protected
        procedure Created; override;
      public
        Margin: Integer;
        Size: Integer;
      end;

      TRow = class(TContent)
      public
        Size: Integer;
      end;

      TColumn = class(TContent)
      public
        Size: Integer;
      end;

      [TrttiIDAttribute]
      TCard = class(TContent)
      public
        Collapse: Boolean;
        Caption: string;
      end;

      [TrttiIDAttribute]
      TPanel = class(TContent)
      public
        Caption: string;
      end;

      TFormButton = (fbSubmit, fbCancel, fbReset);
      TFormButtons = set of TFormButton;

      { TForm }

      [TrttiIDAttribute]
      TForm = class(TContent)
      private
        FButtons: TFormButtons;
      public
        property Buttons: TFormButtons read FButtons write FButtons;
      end;

      TParagraph = class(TContent)
      public
        Text: string;
        constructor Create(AParent: TmnwElement; AText: string = ''); reintroduce;
      end;

      { TEdit }

      [TrttiIDAttribute]
      TInput = class(THTMLElement)
      protected
        procedure Created; override;
      public
        Caption: string;
        Text: string;
        PlaceHolder: string;
        EditType: string;
        Width, Height: double;
      end;

      { TInputPassword }

      [TrttiIDAttribute]
      TInputPassword = class(TInput)
      protected
        procedure Created; override;
      end;

      [TrttiIDAttribute]
      TImage = class(THTMLElement)
      protected
        procedure DoBeforeRender(Renderer: TmnwRenderer); override;
        procedure DoCompose; override;
      public
        Source: string;
        AltText: string;
        Width, Height: double;
      end;

      { TMemoryImage }

      [TrttiIDAttribute]
      TMemoryImage = class(TImage)
      private
        FData: TMemoryStream;
      protected
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream); override;
      public
        FileName: string;
        FilePath: string;
        procedure Created; override;
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
      TTag = class(THTMLElement)
      public
      end;
  protected
  public
  end;

  { TmnwHTMLRenderer }

  TmnwHTMLRenderer = class(TmnwRenderer)
  protected
  public
  type

      { TElement }

      TElementHTML = class abstract(TmnwElementRenderer)
      protected
        procedure AddHead(AElement: TmnwElement; Context: TmnwContext); virtual;
        procedure DoBeforeRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TDocument }

      TDocument = class(TElementHTML)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TBody }

      TBody = class(TElementHTML)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TDirectFile }

      TDirectFile = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TEmbedFile }

      TEmbedFile = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TJSResource = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TJSEmbedFile }

      TJSEmbedFile = class(TEmbedFile)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TCompose }

      TCompose = class(TElementHTML)
      protected
        function AddScript(Scope: TmnwScope; Context: TmnwContext): string; virtual;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TIntervalCompose }

      TIntervalCompose = class(TCompose)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { THeader }

      THeader = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TFooter }

      TFooter = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TContainer }

      TContainer = class abstract(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TRow = class abstract(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TColumn = class abstract(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TCard }

      TCard = class abstract(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TPanel = class abstract(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TForm }

      TForm = class abstract(TElementHTML)
      protected
        procedure DoBeforeChildRender(Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TParagraph }

      TParagraph = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TBreak }

      TBreak = class(TElementHTML)
      protected
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TInput }

      TInput = class(TElementHTML)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TInputPassword = class(TInput)
      end;

      { TImage }

      TImage = class(TElementHTML)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TMemoryImage }

      TMemoryImage = class(TElementHTML)
      protected
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

    protected
      procedure AddHead(AElement: TmnwElement; Context: TmnwContext); virtual;
      procedure InitObjects; override;
    public
      HomeUrl: string;
  end;

function LevelStr(vLevel: Integer): String;

{$ifdef RTTI_OBJECTS}
type
  TCacheClassObject = class(TObject)
  public
    ObjectClass: TClass;
  end;

  { TRegObjects }

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

implementation

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

{ TmnwAttributes }

function TmnwAttributes.GetText(WithExtraSpace: Boolean): string;
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

{ TmnwScope }

{ TmnwElementRenderer }

procedure TmnwElementRenderer.DefaultRender(Scope: TmnwScope; Context: TmnwContext);
var
  o: TmnwElement;
begin
  for o in Scope.Element do
  begin
    if (elHighLevel in o.Kind) then
      if not (elInternal in o.Kind) then
        o.Render(Context);
  end;

  for o in Scope.Element do
  begin
    if not (elHighLevel in o.Kind) then
      if not (elInternal in o.Kind) then
        o.Render(Context);
  end;
end;

procedure TmnwElementRenderer.DoBeforeChildRender(Scope: TmnwScope; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoBeforeRender(Scope: TmnwScope; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  DefaultRender(Scope, Context);
end;

procedure TmnwElementRenderer.DoAfterRender(Scope: TmnwScope; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoCollectAttributes(Scope: TmnwScope);
begin
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; Context: TmnwContext);
var
  aParent: TmnwElementRenderer;
  aScope: TmnwScope;
begin
  aScope.Attributes := TmnwAttributes.Create;
  aScope.Element := AElement;
  try
    CollectAttributes(aScope);

    if AElement.Parent <> nil then
      aParent := AElement.Parent.CreateRender(Context)
    else
      aParent := nil;

    if aParent <> nil then
      aParent.DoBeforeChildRender(aScope, Context);

    DoBeforeRender(aScope, Context);
    DoRender(aScope, Context);
    DoAfterRender(aScope, Context);

    if aParent <> nil then
      aParent.DoAfterChildRender(aScope, Context);

    if aParent <> nil then
      aParent.Free;
  finally
    FreeAndNil(aScope.Attributes);
  end;
end;

constructor TmnwElementRenderer.Create(ARenderer: TmnwRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

procedure TmnwElementRenderer.CollectAttributes(Scope: TmnwScope);
begin
  Scope.Attributes.Append(Scope.Element.Attributes);

  if Scope.Element.ID <> '' then
    Scope.Attributes['id'] := Scope.Element.ID;
  if Scope.Element.Name <> '' then
    Scope.Attributes['name'] := Scope.Element.Name;
  if Scope.Element.Style <> '' then
    Scope.Attributes['style'] := Scope.Element.Style;
  if Scope.Element.StyleClass <> '' then
    Scope.Attributes.SetSubValue('class', Scope.Element.StyleClass);

  DoCollectAttributes(Scope);
end;

{ TrttiElementAttribute }

constructor TrttiElementAttribute.Create;
begin
  inherited Create;
end;

function TmnwAttribute.CreateSubValues(vSeparators: TSysCharSet): TStringList;
begin
  Result := TStringList.Create;
  if Self <> nil then
    StrToStrings(Value, Result, vSeparators, []);
end;

procedure TmnwElement.Render(Context: TmnwContext);
var
  Renderer: TmnwElementRenderer;
begin
  if RenderIt then
  begin
    Renderer := CreateRender(Context);
    if Renderer <> nil then
    begin
      try
        Renderer.Render(Self, Context);
      finally
        Renderer.Free;
      end;
    end;
  end;
end;

function TmnwElement.CreateRender(Context: TmnwContext): TmnwElementRenderer;
begin
  if (Context.Renderer <> nil) then
    Result := Context.Renderer.CreateRenderer(Self)
  else
    Result := nil;
end;

{ TmnwSchema.TRegObjects }

function TmnwRenderer.TRegObjects.FindDerived(AObjectClass: TmnwElementClass): TmnwElementClass;
var
  o: TRegObject;
begin
  Result := nil;
  for o in Self do
  begin
    if o.ObjectClass.ClassParent = AObjectClass then
    begin
      Result := o.ObjectClass;
      break;
    end;
  end;
end;

function TmnwRenderer.TRegObjects.Compare(Item1, Item2: TRegObject): Integer;
begin
  if Item2.ObjectClass.InheritsFrom(Item1.ObjectClass) then
    Result := 1
  else if Item1.ObjectClass.InheritsFrom(Item2.ObjectClass) then
    Result := -1
  else
    Result := 0;
end;

function TmnwRenderer.TRegObjects.Find(AObjectClass: TmnwElementClass; Nearst: Boolean): TRegObject;
var
  o: TRegObject;
  i: Integer;
begin
  Result := nil;
  for i:= Count - 1  downto 0 do
  begin
    o := Items[i];
    if AObjectClass = o.ObjectClass then
    begin
      Result := o;
      break;
    end
    else if Nearst and AObjectClass.InheritsFrom(o.ObjectClass) then
    begin
      Result := o;
    end;
  end;
end;

function TmnwRenderer.TRegObjects.FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
var
  o: TRegObject;
begin
  o := Find(AObjectClass, True);
  if o <> nil then
    Result := o.RendererClass
  else
    Result := TmnwElementRenderer;
end;

{ TmnwSchemaObject }

destructor TmnwSchemaObject.Destroy;
begin
  FreeAndNil(Schema);
  inherited;
end;

{ TmnwSchemas }

destructor TmnwSchemas.Destroy;
begin
  inherited;
end;

procedure TmnwSchemas.RegisterSchema(AName: string; SchemaClass: TmnwSchemaClass);
var
  SchemaObject: TmnwSchemaObject;
begin
  SchemaObject := TmnwSchemaObject.Create;
  SchemaObject.Name := AName;
	SchemaObject.SchemaClass := SchemaClass;
  inherited Add(SchemaObject);
end;

function TmnwSchemas.Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): TmnwElement;
var
  SchemaObject: TmnwSchemaObject;
  aElement: TmnwElement;
  Routes: TStringList;
  i: Integer;
  aRoute: string;
  aSchema: TmnwSchema;
begin
  aSchema := nil;
  Routes := TStringList.Create;
  try
    i := 0;
    StrToStrings(Route, Routes, ['/']);
    if (i<Routes.Count) then
    begin
      aRoute := Routes[i];
      inc(i);
      SchemaObject := Find(aRoute);
    end
    else
      SchemaObject := nil;

    if SchemaObject = nil then
      SchemaObject := First;

    if SchemaObject <> nil then
    begin
      DeleteSubPath(aRoute, Route);
      if SchemaObject.Schema = nil then
      begin
        aSchema := SchemaObject.SchemaClass.Create(nil);
        aSchema.Route := Route;
        SchemaCreated(aSchema);
        aSchema.Compose;
        if aSchema.Cached then
          SchemaObject.Schema := aSchema;
      end
      else
        aSchema := SchemaObject.Schema;
      aElement := aSchema;
    end
    else
      aElement := nil;

    if aElement <> nil then
    begin
      Result := aElement;
      while i < Routes.Count do
      begin
        aRoute := Routes[i];
        aElement := aElement.FindByRoute(aRoute);
        if aElement = nil then
          break
        else
        begin
          DeleteSubPath(aRoute, Route);
          Result := aElement;
        end;
        inc(i);
      end;
    end;
  finally
    Routes.Free;
  end;

  if Result <> nil then
  begin
    Result.Respond(aRoute, Renderer, Sender, AStream);
  end;

  if (aSchema <> nil) and not aSchema.Cached then
    aSchema.Free;
end;

procedure TmnwSchemas.SchemaCreated(Schema: TmnwSchema);
begin
end;

{function TmnwSchemas.Render(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): TmnwSchema;
var
  SchemaObject: TmnwSchemaObject;
begin
  SchemaObject := Find(Route);
  if SchemaObject <> nil then
  begin
    Result := SchemaObject.Schema;
  end
  else
    Result := First.Schema;

  Result.Render(Renderer, Sender, AStream);
end;}

procedure TmnwHTMLRenderer.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
end;

procedure TmnwHTMLRenderer.InitObjects;
begin
  inherited;
  //RegisterClasses(THTML);
  RegisterRenderer(THTML.TCompose, TCompose);
  RegisterRenderer(THTML.TIntervalCompose, TIntervalCompose);
  RegisterRenderer(THTML.TDirectFile,TDirectFile);
  RegisterRenderer(THTML.TEmbedFile, TEmbedFile);
  RegisterRenderer(THTML.TJSResource, TJSResource);
  RegisterRenderer(THTML.TJSEmbedFile, TJSEmbedFile);

  RegisterRenderer(THTML.TDocument ,TDocument);
  RegisterRenderer(THTML.TBody ,TBody);
  RegisterRenderer(THTML.TParagraph, TParagraph);
  RegisterRenderer(THTML.TBreak, TBreak);
  RegisterRenderer(THTML.TInput, TInput);
  RegisterRenderer(THTML.TInputPassword, TInputPassword);
  RegisterRenderer(THTML.TImage, TImage);
  RegisterRenderer(THTML.TMemoryImage, TMemoryImage);
  RegisterRenderer(THTML.THeader, THeader);
  RegisterRenderer(THTML.TFooter, TFooter);
  RegisterRenderer(THTML.TContainer, TContainer);
  RegisterRenderer(THTML.TCard, TCard);
  RegisterRenderer(THTML.TForm, TForm);
  RegisterRenderer(THTML.TRow, TRow);
  RegisterRenderer(THTML.TColumn, TColumn);
  RegisterRenderer(THTML.TPanel, TPanel);

  Libraries.RegisterLibrary('JQuery', TJQuery_Library);
end;

{ TmnwHTMLRenderer.TElementHTML }

procedure TmnwHTMLRenderer.TElementHTML.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
end;

procedure TmnwHTMLRenderer.TElementHTML.DoBeforeRender(Scope: TmnwScope; Context: TmnwContext);
begin
  if Scope.Element.Comment <> '' then
    Context.Output.WriteLn('html', '<!-- ' + Scope.Element.Comment + ' -->');
  inherited;
end;

{ TmnwHTMLRenderer.TDocumentHTML }

procedure TmnwHTMLRenderer.TDocument.DoCollectAttributes(Scope: TmnwScope);
var
  e: THTML.TDocument;
begin
  e := Scope.Element as THTML.TDocument;
  if e.Direction = dirRTL then
    Scope.Attributes['dir'] := 'rtl'
  else if E.Direction = dirLTR then
    Scope.Attributes['dir'] := 'ltr';
  Scope.Attributes['lang'] := 'en'
end;

procedure TmnwHTMLRenderer.TDocument.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TDocument;
  aLibrary: TmnwLibrary;
//  o: TmnwElement;
//  r: TElementHTML;
begin
  e := Scope.Element as THTML.TDocument;
  //Log.WriteLn(ClassName);
  Context.Output.WriteLn('html', '<!DOCTYPE html>');
  Context.Output.WriteLn('html', '<html' + Scope.Attributes.GetText(True) + '>', [woOpenTag]);
  Context.Output.WriteLn('html', '<head>', [woOpenTag]);
  Context.Output.WriteLn('html', '<title>'+ e.Title + '</title>', [woOpenTag, woCloseTag]);
  AddHead(Scope.Element, Context);

  for aLibrary in Renderer.Libraries do
  begin
    if aLibrary.Usage > 0 then
      aLibrary.AddHead(Scope.Element, Context);
  end;

  (Renderer as TmnwHTMLRenderer).AddHead(Scope.Element, Context);

  //* Collect head from childs
  {for o in Scope.Element do
  begin
    if o is THTML.THTMLElement then
    begin
      r := Renderer.CreateRenderer(o) as TElementHTML;
      try
        r.AddHeader(o, Context);
      finally
        r.free;
      end;
    end;
  end;}
  Context.Output.WriteLn('html', '</head>', [woCloseTag]);
  e.Body.Render(Context);
  Context.Output.WriteLn('html', '</html>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.THeaderHTML }

procedure TmnwHTMLRenderer.THeader.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.THeader;
begin
  e := Scope.Element as THTML.THeader;
  Context.Output.WriteLn('html', '<header class="bg-primary text-white text-left py-3">', [woOpenTag]);
  inherited;
  if e.Text <> '' then
    Context.Output.WriteLn('html', '<h1>'+e.Text+'</h1>', [woOpenTag, woCloseTag]);
  Context.Output.WriteLn('html', '</header>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TFooterHTML }

procedure TmnwHTMLRenderer.TFooter.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TFooter;
begin
  e := Scope.Element as THTML.TFooter;
  Context.Output.WriteLn('html', '<footer class="bg-body-tertiary text-center text-lg-start">', [woOpenTag]);
  if e.Text <> '' then
    Context.Output.WriteLn('html', '<h6>'+e.Text+'</h6>', [woOpenTag, woCloseTag]);
  inherited;
  Context.Output.WriteLn('html', '</footer>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TContainerHTML }

procedure TmnwHTMLRenderer.TContainer.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TContainer;
begin
  e := Scope.Element as THTML.TContainer;
  Context.Output.WriteLn('html', '<main class="container">', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</main>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TCardHTML }

procedure TmnwHTMLRenderer.TCard.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Context.Output.WriteLn('html', '<div class="card">', [woOpenTag]);
  if e.Caption <> '' then
    Context.Output.WriteLn('html', '<div class="card-header">' + e.Caption + '</div>', [woOpenTag, woCloseTag]);

  Context.Output.WriteLn('html', '<div class="card-body">', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TFormHTML }

procedure TmnwHTMLRenderer.TForm.DoBeforeChildRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<div>', [woOpenTag]);
  Scope.Attributes.SetSubValue('class', 'form-control');
  inherited;
end;

procedure TmnwHTMLRenderer.TForm.DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  inherited;
end;

procedure TmnwHTMLRenderer.TForm.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TForm;
begin
  e := Scope.Element as THTML.TForm;
  Context.Output.WriteLn('html', '<form>', [woOpenTag]);
  inherited;
  //buttons
  Context.Output.WriteLn('html', '</form>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TParagraphHTML }

procedure TmnwHTMLRenderer.TParagraph.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TParagraph;
begin
  e := Scope.Element as THTML.TParagraph;
  Context.Output.Write('html', '<p>', [woOpenTag]);
  if e.Text <> '' then
    Context.Output.Write('html', e.Text, []);
  inherited;
  Context.Output.WriteLn('html', '</p>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TBreakHTML }

procedure TmnwHTMLRenderer.TBreak.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<br>');
end;

{ TmnwHTMLRenderer.TInputHTML }

procedure TmnwHTMLRenderer.TInput.DoCollectAttributes(Scope: TmnwScope);
begin
  Scope.Attributes['placeholder'] := (Scope.Element as THTML.TInput).PlaceHolder;
  Scope.Attributes['type'] := (Scope.Element as THTML.TInput).EditType;
  inherited;
end;

procedure TmnwHTMLRenderer.TInput.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TInput;
begin
  e := Scope.Element as THTML.TInput;
  if e.Caption <> '' then
    Context.Output.WriteLn('html', '<label for="'+e.ID+'" >' + e.Caption + '</label>', [woOpenTag, woCloseTag]);
  Context.Output.WriteLn('html', '<input'+ Scope.Attributes.GetText(True)+' >', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwHTMLRenderer.TImageHTML }

procedure TmnwHTMLRenderer.TImage.DoCollectAttributes(Scope: TmnwScope);
begin
  Scope.Attributes['src'] := (Scope.Element as THTML.TImage).Source;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText; //* always set
  inherited;
end;

procedure TmnwHTMLRenderer.TImage.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<img' + Scope.Attributes.GetText(True)+' >', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwHTMLRenderer.TMemoryImageHTML }

procedure TmnwHTMLRenderer.TMemoryImage.DoCollectAttributes(Scope: TmnwScope);
begin
  inherited;
  Scope.Attributes['src'] := IncludeURLDelimiter(TmnwHTMLRenderer(Renderer).HomeUrl) + Scope.Element.GetPath;
  Scope.Attributes['alt'] := (Scope.Element as THTML.TImage).AltText;
end;

procedure TmnwHTMLRenderer.TMemoryImage.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TMemoryImage;
begin
  e := Scope.Element as THTML.TMemoryImage;
  Context.Output.WriteLn('html', '<img'+ Scope.Attributes.GetText(True)+' >', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwSchema }

constructor TmnwSchema.Create(AParent: TmnwElement; AKind: TmnwElementKind; ARenderIt: Boolean);
begin
  inherited;
  FRoot := Self;
  FCached := True;
  {$ifdef rtti_objects}
  CacheClasses;
  {$endif}
end;

destructor TmnwSchema.Destroy;
begin
  inherited;
end;

procedure TmnwSchema.DoRespond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
begin
  BeforeRender(Renderer);
  Render(Renderer, Sender, AStream);
end;

procedure rttiCollectAttributes(rttiContext: TRttiContext; ElementClass: TClass; List: TClassList);
var
  rttiType: TRttiType;
  attribute: TCustomAttribute;
begin
  rttiType := rttiContext.GetType(ElementClass);
  for attribute in rttiType.GetAttributes do
    if List.IndexOf(attribute.ClassType)<0 then
      List.Add(attribute.ClassType);
  if ElementClass.ClassParent <> nil then
    rttiCollectAttributes(rttiContext, ElementClass.ClassParent, List);
end;

procedure UpdateElement(Element: TmnwElement);
var
  rttiContext: TRttiContext;
  attribute: TCustomAttributeClass;
  list: TClassList;
begin
  list := TClassList.Create;
  rttiContext := TRttiContext.Create;
  try
    rttiCollectAttributes(rttiContext, Element.ClassType, list);
    for attribute in list do
      if attribute.InheritsFrom(TrttiElementAttribute) then
        TrttiElementAttributeClass(attribute).Update(Element);
  finally
    rttiContext.Free;
    list.Free;
  end;
end;

procedure TmnwSchema.GenID(Element: TmnwElement);
var
  s: string;
  p: Integer;
begin
  if Element.ID = '' then
  begin
    Inc(NameingLastNumber);
    s := Element.ClassName;
    p := ReversePos('.', s);
    if p > 0 then
      s := Copy(s, p + 2, MaxInt) //* skip T
    else
      s := Copy(s, 2, MaxInt); //* skip T
    Element.ID := LowerCase(s + '-' + NameingLastNumber.ToString);
  end;
end;

procedure TmnwSchema.GenRoute(Element: TmnwElement);
var
  s: string;
  p: Integer;
begin
  if Element.Route = '' then
  begin
    Inc(NameingLastNumber);
    s := Element.ClassName;
    p := ReversePos('.', s);
    if p > 0 then
      s := Copy(s, p + 2, MaxInt) //* skip T
    else
      s := Copy(s, 2, MaxInt); //* skip T
    Element.Route := LowerCase(s + '-' + NameingLastNumber.ToString);
  end;
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

procedure TmnwRenderer.RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean);
var
  aRegObject: TRegObject;
begin
  aRegObject := ObjectClasses.Find(AObjectClass);
  if aRegObject <> nil then
  begin
//    log.WriteLn('Replacing : '+AObjectClass.ClassName);
    if Replace and (AObjectClass.InheritsFrom(aRegObject.ObjectClass)) then
      aRegObject.RendererClass := ARendererClass
    else
      raise Exception.Create('You can''t re-register same class: '+ AObjectClass.ClassName);
  end
  else
  begin
    //log.WriteLn(AObjectClass.ClassName);
    aRegObject := TRegObject.Create;
    aRegObject.ObjectClass := AObjectClass;
    aRegObject.RendererClass := ARendererClass;
    ObjectClasses.Add(aRegObject);
  end;
end;

function TmnwRenderer.FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
begin
  Result := ObjectClasses.FindRendererClass(AObjectClass);
end;

function TmnwRenderer.CreateRenderer(AObjectClass: TmnwElementClass): TmnwElementRenderer;
var
  RendererClass: TmnwElementRendererClass;
begin
  RendererClass := FindRendererClass(AObjectClass);
  if RendererClass <> nil then
    Result := RendererClass.Create(Self)
  else
    Result := nil;
end;

function TmnwRenderer.CreateRenderer(AObject: TmnwElement): TmnwElementRenderer;
begin
  Result := CreateRenderer(TmnwElementClass(AObject.ClassType));
end;

{ TContent }

procedure THTML.TContent.Added(Item: TmnwElement);
begin
  inherited Added(Item);
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

procedure THTML.TInput.Created;
begin
  inherited;
  EditType := 'text';
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

procedure THTML.TMemoryImage.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
begin
  Data.Seek(0, soBeginning);
  AStream.WriteStream(Data, 0);
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

procedure TmnwElement.Update;
begin

end;

procedure TmnwElement.Added(Item: TmnwElement);
begin
  inherited;
  Item.Update;
end;

procedure TmnwElement.BeforeRender(Renderer: TmnwRenderer);
var
  o: TmnwElement;
begin
  DoBeforeRender(Renderer);

  for o in Self do
    if (elHighLevel in o.Kind) then
      o.BeforeRender(Renderer);

  for o in Self do
    if not (elHighLevel in o.Kind) then
      o.BeforeRender(Renderer);
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

procedure TmnwElement.DoState(RequestState: TmnwRequestState);
begin

end;

procedure TmnwElement.State(RequestState: TmnwRequestState);
begin

end;

function TmnwElement.FindByPath(const APath: string): TmnwElement;
var
  o: TmnwElement;
begin
{  if (FRoot = nil) and (APath = '')
    exit(Self);}

  if SameText(GetPath, APath) then
    exit(Self);

  Result := nil;

  for o in Self do
  begin
    Result := o.FindByPath(APath);
    if Result <> nil then
      exit;
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
  if (Result = nil) and (Route <> '') and (elFallback in Kind) then
    Result := Self;
end;

procedure TmnwElement.DoBeforeRender(Renderer: TmnwRenderer);
begin
end;

procedure TmnwElement.DoCompose;
begin
end;

procedure TmnwElement.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
begin
end;

procedure TmnwElement.DoRespondHeader(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
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
    FRoot:= FParent.FRoot;
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
  O.FRoot := FRoot;
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

function TmnwElement.Render(Renderer: TmnwRenderer; Sender: TObject; AOutput: TmnwOutput): Boolean;
var
  Context: TmnwContext;
begin
  Result := False;
  Context.Output := AOutput;
  Context.Renderer := Renderer;
  Context.Sender := Sender;
  Render(Context);
  Result := True;
end;

function TmnwElement.Render(Renderer: TmnwRenderer; Sender: TObject; AStrings: TStrings): Boolean;
var
  AStringStream: TStringStream;
  AStream: TmnWrapperStream;
begin
  AStringStream := TStringStream.Create;
  AStream := TmnWrapperStream.Create(AStringStream);
  try
    Result := Render(Renderer, Sender, AStream);
    AStrings.Text := AStringStream.DataString;
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TmnwElement.Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
begin
  if Route <> '' then
  begin
    (Sender as TmodHttpCommand).Respond.PutHeader('Content-Type', GetContentType(Route)); //* move outside of mnWebElement.pas please
    DoRespondHeader(Route, Renderer, Sender, AStream);
  end;
  DoRespond(Route, Renderer, Sender, AStream);
end;

function TmnwElement.Render(Renderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream): Boolean;
var
  Writer: TmnwWriter;
  Output: TmnwOutput;
begin
  Writer := TmnwWriter.Create('html', AStream);
  Output := TmnwOutput.Create;
  Output.Add(Writer);
  try
    Result := Render(Renderer, Sender, Output);
  finally
    FreeAndNil(Output);
  end;
  Result := True;
end;

procedure TmnwElement.Compose;
var
  o: TmnwElement;
begin
//  Clear; //*Should not clear here
  DoCompose;
  for o in Self do
  begin
    o.Compose;
  end;
  UpdateElement(Self);
  Composed := True;
end;

procedure TmnwElement.Clear;
begin
  inherited;
  Composed := False;
end;

function TmnwElement.GetContentType(Route: string): string;
begin
  Result := 'text/html';
end;

constructor TmnwWriter.Create(AName: string; AStream: TmnBufferStream);
begin
  inherited Create;
  Name := AName;
  FStream := AStream;
end;

procedure TmnwWriter.Write(S: string; Options: TmnwWriterOptions);
begin
	if (woCloseTag in Options) and not (woOpenTag in Options) then
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

	if (woOpenTag in Options) and not (woCloseTag in Options) then
    Inc(Level);
end;

{ TmnwRenderer }

procedure TmnwRenderer.BeginRender;
begin
  DoBeginRender;
end;

constructor TmnwRenderer.Create;
var
  o: TmnwRenderer.TRegObject;
begin
  FLibraries := TmnwLibraries.Create;
  inherited;
  FObjectClasses := TRegObjects.Create;
  FParams := TmnwAttributes.Create;
  InitObjects;
  FObjectClasses.QuickSort;
end;

destructor TmnwRenderer.Destroy;
begin
  FreeAndNil(FObjectClasses);
  FreeAndNil(FParams);
  FreeAndNil(FLibraries);
  inherited;
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

procedure TmnwRenderer.InitObjects;
begin
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

{ THTML.TDirectFile }

procedure THTML.TDirectFile.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
var
  fs: TFileStream;
begin
  inherited;
  fs := TFileStream.Create(FileName, fmShareDenyWrite or fmOpenRead);
  try
    AStream.WriteStream(fs, 0);
  finally
    fs.Free;
  end;
end;

constructor THTML.TDirectFile.Create(AParent: TmnwElement; AFileName: string);
begin
  inherited Create(AParent);
  FileName := AFileName;
end;

function THTML.TDirectFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

{ THTML.TEmbedFile }

procedure THTML.TEmbedFile.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
var
  fs: TFileStream;
begin
  inherited;
  fs := TFileStream.Create(FileName, fmShareDenyWrite or fmOpenRead);
  try
    AStream.WriteStream(fs, 0);
  finally
    fs.Free;
  end;
end;

procedure THTML.TEmbedFile.Created;
begin
  inherited;
  Kind := Kind + [elHighLevel];
end;

constructor THTML.TEmbedFile.Create(AParent: TmnwElement; AFileName: string);
begin
  inherited Create(AParent);
  FileName := AFileName;
end;

function THTML.TEmbedFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(FileName);
end;

{ THTML.TFile }

procedure THTML.TFile.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
var
  fs: TFileStream;
begin
  inherited;
  if FileExists(Route) then
  begin
    fs := TFileStream.Create(Route, fmOpenRead);
    try
      AStream.WriteStream(fs, 0);
    finally
      fs.Free;
    end;
  end;
end;

function THTML.TFile.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TAssets }

procedure THTML.TAssets.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
var
  fs: TFileStream;
  aFileName: string;
begin
  inherited;
  if HomePath <> '' then
  begin
    aFileName := IncludePathDelimiter(HomePath) + Route;
    if FileExists(aFileName) then
    begin
      fs := TFileStream.Create(aFileName, fmShareDenyWrite or fmOpenRead);
      try
        AStream.WriteStream(fs, 0);
      finally
        fs.Free;
      end;
    end;
  end;
end;

function THTML.TAssets.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType(Route);
end;

{ THTML.TCompose }

constructor THTML.TCompose.Create(AParent: TmnwElement; AOnCompose: TProc);
begin
  inherited Create(AParent);
  OnCompose := AOnCompose;
end;

procedure THTML.TCompose.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
begin
  inherited;
  Clear; //here not in Compose
  Compose;
  if Assigned(OnCompose) then
    OnCompose();
  Render(ARenderer, Sender, AStream);
end;

{ TmnwLibrary }

procedure TmnwLibrary.DecUsage;
begin
  FUsage := FUsage - 1;
end;

function TmnwLibrary.GetSource(url: string): string;
begin
  if Source <> '' then
    Result := IncludeURLDelimiter(Source)
  else
    Result := IncludeURLDelimiter(url);
end;

procedure TmnwLibrary.IncUsage;
begin
  FUsage := FUsage + 1;
end;

{ TmnwLibraries }

function TmnwLibraries.ChangeSource(ALibraryClass: TmnwLibraryClass; NewSource: string): Boolean;
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryClass);
  Result := ALibrary <> nil;
  if Result then
    ALibrary.Source := NewSource;
end;

function TmnwLibraries.Find(ALibrary: TmnwLibraryClass): TmnwLibrary;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i].ClassType = ALibrary then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TmnwLibraries.RegisterLibrary(ALibraryName: string; ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := ALibraryClass.Create;
  ALibrary.Name := ALibraryName;
  Add(ALibrary);
end;

procedure TmnwLibraries.Use(ALibrary: TmnwLibrary);
begin
  if ALibrary <> nil then
    ALibrary.IncUsage
  else
    raise Exception.Create('library is nil');
end;

procedure TmnwLibraries.Use(ALibraryClass: TmnwLibraryClass);
var
  ALibrary: TmnwLibrary;
begin
  ALibrary := Find(ALibraryClass);
  if ALibrary <> nil then
    Use(ALibrary)
  else
    raise Exception.Create('There is no library: ' + ALibraryClass.ClassName);
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

procedure TJQuery_Library.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
  inherited;
  Context.Output.WriteLn('html', '<script src="' + GetSource('https://cdn.jsdelivr.net/npm/jquery@3.7.1/dist/') + 'jquery.min.js" crossorigin="anonymous"></script>');
end;

{ THTML }

{ THTML.TImage }

procedure THTML.TImage.DoBeforeRender(Renderer: TmnwRenderer);
begin
  inherited;
  Renderer.Libraries.Use('JQuery');
end;

procedure THTML.TImage.DoCompose;
begin
  inherited;
end;

{ TmnwHTMLRenderer.TDirectFile }

procedure TmnwHTMLRenderer.TDirectFile.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Scope.Element.Respond('', Context.Renderer, Context.Sender, Context.Output['html'].Stream);
end;

{ TmnwHTMLRenderer.TEmbedFile }

procedure TmnwHTMLRenderer.TEmbedFile.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.Element.Respond('', Context.Renderer, Context.Sender, Context.Output['html'].Stream);
end;

{ THTML.TBody }

function THTML.TBody.GetHeader: THeader;
begin
  if FHeader = nil then
    FHeader := THeader.Create(Self, [elEmbed], True);
  Result := FHeader
end;

function THTML.TBody.GetContainer: TContainer;
begin
  if FContainer = nil then
    FContainer := TContainer.Create(Self, [elEmbed], True);
   Result := FContainer;
end;

function THTML.TBody.GetFooter: TFooter;
begin
  if FFooter = nil then
    FFooter := TFooter.Create(Self, [elEmbed], True);
  Result := FFooter;
end;

procedure THTML.TBody.Created;
begin
  inherited;
end;

destructor THTML.TBody.Destroy;
begin
{  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  FreeAndNil(FContainer);}
  inherited;
end;

{ TmnwHTMLRenderer.TBody }

procedure TmnwHTMLRenderer.TBody.DoCollectAttributes(Scope: TmnwScope);
begin
  inherited;

end;

procedure TmnwHTMLRenderer.TBody.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TBody;
begin
  e := Scope.Element as THTML.TBody;
  Context.Output.WriteLn('html', '<body'+Scope.Attributes.GetText(True)+'>', [woOpenTag]);
//  e.Header.Render(Context);
//  e.Container.Render(Context);
  inherited;
//  e.Footer.Render(Context);
  Context.Output.WriteLn('html', '</body>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TPanel }

procedure TmnwHTMLRenderer.TPanel.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TPanel;
begin
  e := Scope.Element as THTML.TPanel;
  Context.Output.WriteLn('html', '<div class="panel">', [woOpenTag]);
  if e.Caption <> '' then
    Context.Output.WriteLn('html', '<div class="panel-header">' + e.Caption + '</div>', [woOpenTag, woCloseTag]);

  Context.Output.WriteLn('html', '<div class="panel-body">', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TRow }

procedure TmnwHTMLRenderer.TRow.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Context.Output.WriteLn('html', '<div class="row">', [woOpenTag]);
  inherited;
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TColumn }

procedure TmnwHTMLRenderer.TColumn.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Output.WriteLn('html', '<div class="column">', [woOpenTag]);
  inherited;
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ THTML.TContainer }

procedure THTML.TContainer.Created;
begin
  inherited;
  Margin := 3;
  Size := 1;
end;

{ THTML.TParagraph }

constructor THTML.TParagraph.Create(AParent: TmnwElement; AText: string);
begin
  inherited Create(AParent);
  Text := AText;
end;

{ TNameAttribute }

class procedure TrttiIDAttribute.Update(Element: TmnwElement);
begin
  Element.Root.GenID(Element);
end;

{ TrttiRouteAttribute }

class procedure TrttiRouteAttribute.Update(Element: TmnwElement);
begin
  Element.Root.GenRoute(Element);
end;

{ TmnwHTMLRenderer.TCompose }

function TmnwHTMLRenderer.TCompose.AddScript(Scope: TmnwScope; Context: TmnwContext): string;
begin
  Result := '';
end;

procedure TmnwHTMLRenderer.TCompose.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<div ' + Scope.Attributes.GetText(True)+'>', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TIntervalCompose }

procedure TmnwHTMLRenderer.TIntervalCompose.DoCollectAttributes(Scope: TmnwScope);
var
  URL: string;
begin
  inherited;
  URL := IncludeURLDelimiter(TmnwHTMLRenderer(Renderer).HomeUrl) + Scope.Element.GetPath;
  Scope.Attributes['data-refresh-url'] := URL;
end;

procedure TmnwHTMLRenderer.TIntervalCompose.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  inherited DoRender(Scope, Context);
end;

{ THTML.TJSResource }

constructor THTML.TJSResource.Create(AParent: TmnwElement; AResName: string);
begin
  inherited Create(AParent);
  ResName := AResName;
end;

function THTML.TJSResource.GetContentType(Route: string): string;
begin
  Result := DocumentToContentType('.js');
end;

procedure THTML.TJSResource.Created;
begin
  inherited;
  Kind := Kind + [elHighLevel];
end;

procedure THTML.TJSResource.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TmnBufferStream);
var
  ResStream: TResourceStream;
begin
  inherited;
  ResStream := TResourceStream.Create(hInstance, ResName, RT_RCDATA);
  try
    AStream.CopyFrom(ResStream, 0);
  finally
    ResStream.Free;
  end;
end;

{ TmnwHTMLRenderer.TJSResource }

procedure TmnwHTMLRenderer.TJSResource.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Context.Output.WriteLn('html', '<script type="text/javascript"'+ Scope.Attributes.GetText(True)+'>', [woOpenTag]);
  Scope.Element.Respond('', Context.Renderer, Context.Sender, Context.Output['html'].Stream);
  inherited;
  Context.Output.WriteLn('html', '');
  Context.Output.WriteLn('html', '</script>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TJSEmbedFile }

procedure TmnwHTMLRenderer.TJSEmbedFile.DoRender(Scope: TmnwScope; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<script type="text/javascript"'+ Scope.Attributes.GetText(True)+'>', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '');
  Context.Output.WriteLn('html', '</script>', [woCloseTag]);
end;

initialization

finalization
{$ifdef rtti_objects}
  FreeAndNil(CacheClassObjects);
{$endif}
end.
