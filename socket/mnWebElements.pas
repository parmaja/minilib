unit mnWebElements;//* BETA
{$IFDEF FPC}
{$mode delphi}
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
  Classes, SysUtils, Contnrs, Variants, RTTI,
	mnUtils, mnClasses, mnStreams, mnLogs,
  mnMultipartData, mnModules, mnWebModules;

type

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwOutput = class;
  TmnwElementRenderer = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwElementClass = class of TmnwElement;

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
    Element: TmnwElement;
    Attributes: TmnwAttributes;
  end;

  TmnwObject = class(TmnNamedObject);

  TmnwRequestState = (rsBeforeRequest, rsAfterRequest);

  TmnwElementKind = set of(
    elEmbed, //* created by parent
    elInternal, //* do not render we will call it manually
    elFallback //* if no child have the route name, it take the respond
  );

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEnabled: Boolean;
    FVisible: Boolean;
    FActive: Boolean;
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
    procedure DoCompose; virtual;
    procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream); virtual;
  public
    Composed: Boolean;
    constructor Create; overload; virtual;
    constructor Create(AKind: TmnwElementKind; Parent: TmnwElement = nil); overload;
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

    procedure Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream);

    procedure Render(Context: TmnwContext); overload;

    function Render(Renderer: TmnwRenderer; Sender: TObject; AOutput: TmnwOutput): Boolean; overload;
    function Render(Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): Boolean; overload;
    function Render(Renderer: TmnwRenderer; Sender: TObject; AStrings: TStrings): Boolean; overload;

    property Route: String read FRoute write FRoute;
    property Name: String read FName write FName;
    property StyleClass: String read FStyleClass write FStyleClass;
    property Style: String read FStyle write FStyle;
    property ID: String read FID write FID;
    property Comment: String read FComment write FComment;
    property Visible: Boolean read FVisible write FVisible;
    property Enabled: Boolean read FEnabled write FEnabled;
    //* Active render it
    property Active: Boolean read FActive write FActive;
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
    FStream: TStream;
  public
    constructor Create(AName: string; AStream: TStream);
    procedure Write(S: string; Options: TmnwWriterOptions = []); virtual;
    property Stream: TStream read FStream write FStream;
  end;

  { TmnwOutput }

  TmnwOutput = class(TmnNamedObjectList<TmnwWriter>)
  private
  public
    procedure Write(const Target, S: string; Options: TmnwWriterOptions = []); overload;
    procedure WriteLn(const Target, S: string; Options: TmnwWriterOptions = []); overload;
  end;

  { TmnwSchema }

  TmnwSchema = class(TmnwElement)
  protected
  public
  private

  protected
    procedure DoRespond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream); override;
  public
    constructor Create; override;
    destructor Destroy; override;

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
    procedure DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext); virtual;
    property Renderer: TmnwRenderer read FRenderer;
  public
    procedure Render(AElement: TmnwElement; Context: TmnwContext);
    procedure Respond(AElement: TmnwElement; AStream: TStream); virtual;
    constructor Create(ARenderer: TmnwRenderer); virtual; //usfull for creating it by RendererClass.Create
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
      public
        function FindDerived(AObjectClass: TmnwElementClass): TmnwElementClass;
        function Find(AObjectClass: TmnwElementClass; Nearst: Boolean = False): TRegObject;
        function FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
      end;
  protected
    FObjectClasses: TRegObjects;
    FParams: TmnwAttributes;
    {$ifndef FPC}
    procedure RegisterClasses(ASchemaClass: TmnwSchemaClass);
    {$endif}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = True);
    function FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
    function CreateRenderer(AObjectClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property ObjectClasses: TRegObjects read FObjectClasses;
    property Params: TmnwAttributes read FParams;
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
    function Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): TmnwElement;
    function Render(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): TmnwSchema;
  end;

  TDirection = (dirUnkown, dirLTR, dirRTL);

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  THTML =class(TmnwSchema)
  public
    type
      THTMLElement = class(TmnwElement)
      protected
      public
      end;

      { TContent }

      TContent = class abstract(THTMLElement)
      protected
        procedure Added(Item: TmnwElement); override;
      public
      end;

      THeader = class;
      TFooter = class;
      TContainer = class;

      TDirectFile = class(THTMLElement)
      public
        FileName: string;
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream); override;
      end;

      TFile = class(THTMLElement)
      public
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream); override;
      end;

      TAssets = class(THTMLElement)
      public
        HomePath: string;
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream); override;
      end;

      { TDocument }

      TDocument = class(THTMLElement)
      private
        FContainer: TContainer;
        FTitle: string;
        FVersion: integer;
        FHeader: THeader;
        FFooter: TFooter;
      public
        Direction: TDirection;
        property Version: integer read FVersion write FVersion;
        property Title: string read FTitle write FTitle;
        property Header: THeader read FHeader;
        property Footer: TFooter read FFooter;
        property Container: TContainer read FContainer;
        constructor Create; override;
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
      public
      end;

      TCard = class(TContent)
      public
        Caption: string;
      end;

      TFormButton = (fbSubmit, fbCancel, fbReset);
      TFormButtons = set of TFormButton;

      { TForm }

      TForm = class(TContent)
      private
        FButtons: TFormButtons;
      public
        property Buttons: TFormButtons read FButtons write FButtons;
      end;

      TParagraph = class(TContent)
      public
        Text: string;
      end;

      { TEdit }

      TInput = class(THTMLElement)
      public
        Caption: string;
        Text: string;
        PlaceHolder: string;
        EditType: string;
        Width, Height: double;
        procedure Created; override;
      end;

      { TInputPassword }

      TInputPassword = class(TInput)
      public
        procedure Created; override;
      end;

      TImage = class(THTMLElement)
      public
        Source: string;
        AltText: string;
        Width, Height: double;
      end;

      { TMemoryImage }

      TMemoryImage = class(TImage)
      protected
        Data: TMemoryStream;
        procedure DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream); override;
      public
        FileName: string;
        FilePath: string;
        constructor Create; override;
        destructor Destroy; override;
        procedure LoadFromFile(AFileName: string);
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

  end;

  { TmnwHTMLRenderer }

  TmnwHTMLRenderer = class(TmnwRenderer)
  protected
  public
  type

      { TElement }

      TElementHTML = class(TmnwElementRenderer)
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

      { THeader }

      THeader = class(TElementHTML)
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TFooter }

      TFooter = class(TElementHTML)
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TContainer }

      TContainer = class abstract(TElementHTML)
      protected
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TCard }

      TCard = class abstract(TElementHTML)
      protected
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TForm }

      TForm = class abstract(TElementHTML)
      protected
        procedure DoBeforeChildRender(Scope: TmnwScope; Context: TmnwContext); override;
        procedure DoAfterChildRender(Scope: TmnwScope; Context: TmnwContext); override;
			public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TParagraph }

      TParagraph = class(TElementHTML)
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TBreak }

      TBreak = class(TElementHTML)
      public
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TInput }

      TInput = class(TElementHTML)
      public
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      TInputPassword = class(TInput)
      end;

      { TImage }

      TImage = class(TElementHTML)
      public
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

      { TMemoryImage }

      TMemoryImage = class(TElementHTML)
      public
        procedure Respond(AElement: TmnwElement; AStream: TStream); override;
        procedure DoCollectAttributes(Scope: TmnwScope); override;
        procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
      end;

    protected
      procedure Created; override;
    public
      HomeUrl: string;
  end;

function LevelStr(vLevel: Integer): String;

{$ifndef FPC}
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

implementation

{$ifndef FPC}
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
    if aParent <> nil then
    begin
      aParent.DoAfterChildRender(aScope, Context);
      aParent.Free;
    end;
  finally
    aScope.Attributes.Free;
  end;
end;

procedure TmnwElementRenderer.Respond(AElement: TmnwElement; AStream: TStream);
begin
end;

constructor TmnwElementRenderer.Create(ARenderer: TmnwRenderer);
begin
  inherited Create;
  FRenderer := ARenderer;
end;

procedure TmnwElementRenderer.CollectAttributes(Scope: TmnwScope);
begin
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
  if Active then
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

function TmnwSchemas.Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): TmnwElement;
var
  SchemaObject: TmnwSchemaObject;
  aElement: TmnwElement;
  aPath: string;
  Routes: TStringList;
  i: Integer;
  aRoute: string;
begin
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
        SchemaObject.Schema := SchemaObject.SchemaClass.Create;
        SchemaObject.Schema.Route := Route;
        SchemaCreated(SchemaObject.Schema);
        SchemaObject.Schema.Compose;
      end;
      aElement := SchemaObject.Schema;
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
end;

procedure TmnwSchemas.SchemaCreated(Schema: TmnwSchema);
begin
end;

function TmnwSchemas.Render(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): TmnwSchema;
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
end;

procedure TmnwHTMLRenderer.Created;
begin
  inherited Created;
  //RegisterClasses(THTML);
  RegisterRenderer(THTML.TDocument ,TDocument);
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
  Context.Output.WriteLn('html', '<body>', [woOpenTag]);
  e.Header.Render(Context);
  e.Container.Render(Context);
  inherited;
  e.Footer.Render(Context);
  Context.Output.WriteLn('html', '</body>', [woCloseTag]);
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
  Context.Output.WriteLn('html', '<div class="container mt-3">', [woOpenTag]);
  //Context.Output.WriteLn('html', '<div class="col-md-9">', [woOpenTag]);
  Context.Output.WriteLn('html', '<main>', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</main>', [woCloseTag]);
  //Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
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

procedure TmnwHTMLRenderer.TMemoryImage.Respond(AElement: TmnwElement; AStream: TStream);
begin
  AStream.CopyFrom((AElement as THTML.TMemoryImage).Data, 0);
end;

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
  Context.Output.WriteLn('html', '<img '+ Scope.Attributes.GetText(True)+' >', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwSchema }

constructor TmnwSchema.Create;
begin
  inherited Create;
  FRoot := Self;
  {$ifndef FPC}
  CacheClasses;
  {$endif}
end;

destructor TmnwSchema.Destroy;
begin
  inherited;
end;

procedure TmnwSchema.DoRespond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream);
begin
  Render(Renderer, Sender, AStream);
end;

{$ifndef FPC}
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
      raise Exception.Create('You can''t reregister same class: '+ AObjectClass.ClassName);
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

constructor THTML.TDocument.Create;
begin
  inherited;
  FHeader := THeader.Create([elEmbed, elInternal], Self);
  FFooter := TFooter.Create([elEmbed, elInternal], Self);
  FContainer := TContainer.Create([elEmbed, elInternal], Self);
end;

destructor THTML.TDocument.Destroy;
begin
{  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  FreeAndNil(FContainer);}
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

constructor THTML.TMemoryImage.Create;
begin
  inherited;
  Data := TMemoryStream.Create;
end;

destructor THTML.TMemoryImage.Destroy;
begin
  FreeAndNil(Data);
  inherited;
end;

procedure THTML.TMemoryImage.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream);
begin
  (Sender as TmodHttpCommand).Respond.PutHeader('Content-Type', DocumentToContentType(FileName));
  Data.Seek(0, soBeginning);
  AStream.CopyFrom(Data, 0);
end;

procedure THTML.TMemoryImage.LoadFromFile(AFileName: string);
begin
  Data.LoadFromFile(AFileName);
  FileName := ExtractFileName(AFileName);
  FilePath := ExtractFilePath(AFileName);
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
  if (Result = nil) and (elFallback in Kind) then
    Result := Self;
end;

procedure TmnwElement.DoCompose;
begin
end;

procedure TmnwElement.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream);
begin
end;

constructor TmnwElement.Create;
begin
  inherited Create;
  FEnabled := True;
  FVisible := True;
  FActive := True;
  FName := '';
  FAttributes := TmnwAttributes.Create;
end;

constructor TmnwElement.Create(AKind: TmnwElementKind; Parent: TmnwElement = nil);
begin
  Create;
  FKind := AKind;
  if Parent <> nil then
    Parent.Add(Self);
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

function TmnwElement.Add<O>(const AID: String; const AName: String): O;
begin
  Result := O.Create;
  Result.FID := AID;
  Result.FName := AName;
  Add(Result);
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
begin
  AStringStream := TStringStream.Create;
  try
    Result := Render(Renderer, Sender, AStringStream);
    AStrings.Text := AStringStream.DataString;
  finally
    FreeAndNil(AStringStream);
  end;
end;

procedure TmnwElement.Respond(Route: string; Renderer: TmnwRenderer; Sender: TObject; AStream: TStream);
begin
  DoRespond(Route, Renderer, Sender, AStream);
end;

function TmnwElement.Render(Renderer: TmnwRenderer; Sender: TObject; AStream: TStream): Boolean;
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
  DoCompose;
  for o in Self do
    o.Compose;
  Composed := True;
end;

constructor TmnwWriter.Create(AName: string; AStream: TStream);
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

constructor TmnwRenderer.Create;
begin
  inherited;
  FObjectClasses := TRegObjects.Create;
  FParams := TmnwAttributes.Create;
end;

destructor TmnwRenderer.Destroy;
begin
  FreeAndNil(FObjectClasses);
  FreeAndNil(FParams);
  inherited;
end;

{$ifndef FPC}
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

procedure THTML.TDirectFile.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream);
var
  fs: TFileStream;
begin
  inherited;
  (Sender as TmodHttpCommand).Respond.PutHeader('Content-Type', DocumentToContentType(FileName));
  fs := TFileStream.Create(FileName, fmShareDenyWrite or fmOpenRead);
  try
    AStream.CopyFrom(fs, 0);
  finally
    fs.Free;
  end;
end;

{ THTML.TFile }

procedure THTML.TFile.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream);
var
  fs: TFileStream;
begin
  inherited;
  (Sender as TmodHttpCommand).Respond.PutHeader('Content-Type', DocumentToContentType(Route));
  if FileExists(Route) then
  begin
    fs := TFileStream.Create(Route, fmOpenRead);
    try
      AStream.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
  end;
end;

{ THTML.TAssets }

procedure THTML.TAssets.DoRespond(Route: string; ARenderer: TmnwRenderer; Sender: TObject; AStream: TStream);
var
  fs: TFileStream;
  aFileName: string;
begin
  inherited;
  (Sender as TmodHttpCommand).Respond.PutHeader('Content-Type', DocumentToContentType(Route));
  aFileName := IncludePathDelimiter(HomePath) + Route;
  if FileExists(aFileName) then
  begin
    fs := TFileStream.Create(aFileName, fmShareDenyWrite or fmOpenRead);
    try
      AStream.CopyFrom(fs, 0);
    finally
      fs.Free;
    end;
  end;
end;

initialization
finalization
end.
