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
│ SideColumn                                  │
│      │                                      │
├──────┴──────────────────────────────────────┤
│ Navigator/Menu                              │
├────────────┬─ Content ─────────────┬────────┤
│ SideColumn │                       │ PeerColumn
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
	mnUtils, mnClasses, mnStreams, mnLogs;

type

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwOutput = class;
  TmnwElementRenderer = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwElementClass = class of TmnwElement;

  TmnwAttribute = class(TmnNameValueObject)
  end;

  { TmnwAttributes }

  TmnwAttributes = class(TmnNameValueObjectList<TmnwAttribute>)
  public
    function GetText(WithExtraSpace: Boolean = False): string;
  end;

  TmnwContext = record
    DataObject: TObject;
    Renderer: TmnwRenderer;
    Output: TmnwOutput;
  end;

  TmnwObject = class(TmnNamedObject);

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FEmbed: Boolean;
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
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;
    procedure DoCompose; virtual;
  public
    Composed: Boolean;
    constructor Create; virtual; overload;
    constructor Create(AEmbed: Boolean); overload;
    destructor Destroy; override;
    function Add<O: TmnwElement>(const AName: String = ''; const AID: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwElement read FParent;

    function GetPath: string; virtual;

    function FindByPath(const APath: string): TmnwElement;
    function CreateRender(Context: TmnwContext): TmnwElementRenderer;
    procedure Render(Context: TmnwContext);
    procedure Compose;

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
    property Embed: Boolean read FEmbed;
    property Attributes: TmnwAttributes read FAttributes;
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
  public
    constructor Create; override;
    destructor Destroy; override;

    function Render(Renderer: TmnwRenderer; DataObject: TObject; AOutput: TmnwOutput): Boolean; overload;
    function Render(Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): Boolean; overload;
    function Render(Renderer: TmnwRenderer; DataObject: TObject; AStrings: TStrings): Boolean; overload;
  end;

  { TmnwElementRenderer }

  TmnwElementRenderer = class(TObject)
  private
    FRenderer: TmnwRenderer;
  protected
    procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); virtual;
    procedure DefaultRender(AElement: TmnwElement; Context: TmnwContext);

		procedure DoBeforeChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); virtual;
    procedure DoBeforeRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); virtual;
    procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); virtual;
    procedure DoAfterChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); virtual;
    property Renderer: TmnwRenderer read FRenderer;
  public
    procedure Render(AElement: TmnwElement; Context: TmnwContext);
    procedure Respond(AElement: TmnwElement; AStream: TStream); virtual;
    constructor Create(ARenderer: TmnwRenderer); virtual; //usfull for creating it by RendererClass.Create
    procedure CollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
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
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass; Replace: Boolean = False);
    function FindRendererClass(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
    function CreateRenderer(AObjectClass: TmnwElementClass): TmnwElementRenderer; overload;
    function CreateRenderer(AObject: TmnwElement): TmnwElementRenderer; overload;

    property ObjectClasses: TRegObjects read FObjectClasses;
    property Params: TmnwAttributes read FParams;
  end;

  TmnwSchemaClass = class of TmnwSchema;

  TmnwSchemaObject = class(TmnNamedObject)
  public
    Schema: TmnwSchema;
  end;

  { TmnwSchemas }

  TmnwSchemas = class(TmnNamedObjectList<TmnwSchemaObject>)
  protected
    procedure DoRespond(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream); virtual;
  public
    procedure RegisterSchema(AName: string; Schema: TmnwSchema);
    function Respond(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): TmnwSchema;
    function Render(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): TmnwSchema;
  end;

  TDirection = (dirUnkown, dirLTR, dirRTL);

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  THTML =class(TmnwSchema)
  public
    type
      THTMLElement = class(TmnwElement)
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
      public
        Data: TMemoryStream;
        constructor Create; override;
        destructor Destroy; override;
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

      { TElementHTML }

      TElementHTML = class(TmnwElementRenderer)
      protected
        procedure AddHead(AElement: TmnwElement; Context: TmnwContext); virtual;
        procedure DoBeforeRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TDocumentHTML }

      TDocumentHTML = class(TElementHTML)
      protected
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { THeaderHTML }

      THeaderHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TFooterHTML }

      TFooterHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TContainerHTML }

      TContainerHTML = class abstract(TElementHTML)
      protected
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TCardHTML }

      TCardHTML = class abstract(TElementHTML)
      protected
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TFormHTML }

      TFormHTML = class abstract(TElementHTML)
      protected
        procedure DoBeforeChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
        procedure DoAfterChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
			public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TParagraphHTML }

      TParagraphHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TBreakHTML }

      TBreakHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TInputHTML }

      TInputHTML = class(TElementHTML)
      public
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      TInputPasswordHTML = class(TInputHTML)
      end;

      { TImageHTML }

      TImageHTML = class(TElementHTML)
      public
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

      { TMemoryImageHTML }

      TMemoryImageHTML = class(TElementHTML)
      public
        procedure Respond(AElement: TmnwElement; AStream: TStream); override;
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext); override;
      end;

    protected
      procedure Created; override;
    public
      HomeUrl: string;
  end;

function LevelStr(vLevel: Integer): String;

implementation

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

{ TmnwElementRenderer }

procedure TmnwElementRenderer.DefaultRender(AElement: TmnwElement; Context: TmnwContext);
var
  o: TmnwElement;
begin
  for o in AElement do
  begin
    o.Render(Context);
  end;
end;

procedure TmnwElementRenderer.DoBeforeChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoBeforeRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  DefaultRender(AElement, Context);
end;

procedure TmnwElementRenderer.DoAfterChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
end;

procedure TmnwElementRenderer.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; Context: TmnwContext);
var
  lAttributes: TmnwAttributes;
  aParent: TmnwElementRenderer;
begin
  lAttributes := TmnwAttributes.Create;
  try
    CollectAttributes(AElement, lAttributes);
    if AElement.Parent <> nil then
      aParent := AElement.Parent.CreateRender(Context)
    else
      aParent := nil;

    if aParent <> nil then
      aParent.DoBeforeChildRender(AElement, lAttributes, Context);

    DoBeforeRender(AElement, lAttributes, Context);
    DoRender(AElement, lAttributes, Context);
    if aParent <> nil then
    begin
      aParent.DoAfterChildRender(AElement, lAttributes, Context);
      aParent.Free;
    end;
  finally
    lAttributes.Free;
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

procedure TmnwElementRenderer.CollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  if AElement.ID <> '' then
    Attributes.Values['id'] := AElement.ID;
  if AElement.Name <> '' then
    Attributes.Values['name'] := AElement.Name;
  if AElement.StyleClass <> '' then
    Attributes.Values['classname'] := AElement.StyleClass;
  if AElement.Style <> '' then
    Attributes.Values['style'] := AElement.Style;
  DoCollectAttributes(AElement, Attributes);
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

{ TmnwSchemas }

procedure TmnwSchemas.DoRespond(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream);
begin

end;

procedure TmnwSchemas.RegisterSchema(AName: string; Schema: TmnwSchema);
var
  SchemaObject: TmnwSchemaObject;
begin
  SchemaObject:=TmnwSchemaObject.Create;
  SchemaObject.Name := AName;
	SchemaObject.Schema := Schema;
  inherited Add(SchemaObject);
end;

function TmnwSchemas.Respond(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): TmnwSchema;
var
  SchemaObject: TmnwSchemaObject;
begin
  {    aPath := Request.Path;
      Delete(aPath, 1, 1);
      (Module as THomeModule).HomeSchema.FindByPath(aPath);}
  SchemaObject := Find(Route);
  if SchemaObject <> nil then
  begin
    Result := SchemaObject.Schema;
  end
  else
    Result := First.Schema;

  Result.Render(Renderer, DataObject, AStream);
end;

function TmnwSchemas.Render(Route: string; Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): TmnwSchema;
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

  Result.Render(Renderer, DataObject, AStream);
end;

procedure TmnwHTMLRenderer.Created;
begin
  inherited Created;
  RegisterRenderer(THTML.TDocument ,TDocumentHTML);
  RegisterRenderer(THTML.TParagraph, TParagraphHTML);
  RegisterRenderer(THTML.TBreak, TBreakHTML);
  RegisterRenderer(THTML.TInput, TInputHTML);
  RegisterRenderer(THTML.TInputPassword, TInputPasswordHTML);
  RegisterRenderer(THTML.TImage, TImageHTML);
  RegisterRenderer(THTML.TMemoryImage, TMemoryImageHTML);
  RegisterRenderer(THTML.THeader, THeaderHTML);
  RegisterRenderer(THTML.TFooter, TFooterHTML);
  RegisterRenderer(THTML.TContainer, TContainerHTML);
  RegisterRenderer(THTML.TCard, TCardHTML);
  RegisterRenderer(THTML.TForm, TFormHTML);
end;

{ TmnwHTMLRenderer.TElementHTML }

procedure TmnwHTMLRenderer.TElementHTML.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
end;

procedure TmnwHTMLRenderer.TElementHTML.DoBeforeRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  if AElement.Comment <> '' then
    Context.Output.WriteLn('html', '<!-- ' + AElement.Comment + ' -->');
  inherited;
end;

{ TmnwHTMLRenderer.TDocumentHTML }

procedure TmnwHTMLRenderer.TDocumentHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
var
  e: THTML.TDocument;
begin
  e := AElement as THTML.TDocument;
  if e.Direction = dirRTL then
    Attributes['dir'] := 'rtl'
  else if E.Direction = dirLTR then
    Attributes['dir'] := 'ltr';
  Attributes['language'] := 'en'
end;

procedure TmnwHTMLRenderer.TDocumentHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TDocument;
//  o: TmnwElement;
//  r: TElementHTML;
begin
  e := AElement as THTML.TDocument;
  //Log.WriteLn(ClassName);
  Context.Output.WriteLn('html', '<!DOCTYPE html>');
  Context.Output.WriteLn('html', '<html' + Attributes.GetText(True) + '>', [woOpenTag]);
  Context.Output.WriteLn('html', '<head>', [woOpenTag]);
  Context.Output.WriteLn('html', '<title>'+ e.Title + '</title>', [woOpenTag, woCloseTag]);
  AddHead(AElement, Context);
  //* Collect head from childs
  {for o in AElement do
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

procedure TmnwHTMLRenderer.THeaderHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.THeader;
begin
  e := AElement as THTML.THeader;
  Context.Output.WriteLn('html', '<header class="bg-primary text-white text-left py-3">', [woOpenTag]);
  inherited;
  if e.Text <> '' then
    Context.Output.WriteLn('html', '<h1>'+e.Text+'</h1>', [woOpenTag, woCloseTag]);
  Context.Output.WriteLn('html', '</header>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TFooterHTML }

procedure TmnwHTMLRenderer.TFooterHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TFooter;
begin
  e := AElement as THTML.TFooter;
  Context.Output.WriteLn('html', '<footer class="bg-body-tertiary text-center text-lg-start">', [woOpenTag]);
  if e.Text <> '' then
    Context.Output.WriteLn('html', '<h6>'+e.Text+'</h6>', [woOpenTag, woCloseTag]);
  inherited;
  Context.Output.WriteLn('html', '</footer>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TContainerHTML }

procedure TmnwHTMLRenderer.TContainerHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TContainer;
begin
  e := AElement as THTML.TContainer;
  Context.Output.WriteLn('html', '<div class="container mt-4">', [woOpenTag]);
  Context.Output.WriteLn('html', '<div class="col-md-9">', [woOpenTag]);
  Context.Output.WriteLn('html', '<main>', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</main>', [woCloseTag]);
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TCardHTML }

procedure TmnwHTMLRenderer.TCardHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TCard;
begin
  e := AElement as THTML.TCard;
  Context.Output.WriteLn('html', '<div class="card">', [woOpenTag]);
  if e.Caption <> '' then
    Context.Output.WriteLn('html', '<div class="card-header">' + e.Caption + '</div>', [woOpenTag, woCloseTag]);

  Context.Output.WriteLn('html', '<div class="card-body">', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TFormHTML }

procedure TmnwHTMLRenderer.TFormHTML.DoBeforeChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<div>', [woOpenTag]);
  Attributes['class'] := 'form-control';
  inherited;
end;

procedure TmnwHTMLRenderer.TFormHTML.DoAfterChildRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  inherited;
end;

procedure TmnwHTMLRenderer.TFormHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TForm;
begin
  e := AElement as THTML.TForm;
  Context.Output.WriteLn('html', '<form>', [woOpenTag]);
  inherited;
  //buttons
  Context.Output.WriteLn('html', '</form>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TParagraphHTML }

procedure TmnwHTMLRenderer.TParagraphHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TParagraph;
begin
  e := AElement as THTML.TParagraph;
  Context.Output.Write('html', '<p>', [woOpenTag]);
  if e.Text <> '' then
    Context.Output.Write('html', e.Text, []);
  inherited;
  Context.Output.WriteLn('html', '</p>', [woCloseTag]);
end;

{ TmnwHTMLRenderer.TBreakHTML }

procedure TmnwHTMLRenderer.TBreakHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<br />');
end;

{ TmnwHTMLRenderer.TInputHTML }

procedure TmnwHTMLRenderer.TInputHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  Attributes['placeholder'] := (AElement as THTML.TInput).PlaceHolder;
  Attributes['type'] := (AElement as THTML.TInput).EditType;
  inherited;
end;

procedure TmnwHTMLRenderer.TInputHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TInput;
begin
  e := AElement as THTML.TInput;
  if e.Caption <> '' then
    Context.Output.WriteLn('html', '<label for="'+e.Name+'" >' + e.Caption + '</label>', [woOpenTag, woCloseTag]);
  Context.Output.WriteLn('html', '<input '+ Attributes.GetText(True)+' />', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwHTMLRenderer.TImageHTML }

procedure TmnwHTMLRenderer.TImageHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  Attributes['src'] := (AElement as THTML.TImage).Source;
  if (AElement as THTML.TImage).AltText <> '' then
    Attributes['alt'] := (AElement as THTML.TImage).AltText;
  inherited;
end;

procedure TmnwHTMLRenderer.TImageHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<img '+Attributes.GetText(True)+' />', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwHTMLRenderer.TMemoryImageHTML }

procedure TmnwHTMLRenderer.TMemoryImageHTML.Respond(AElement: TmnwElement; AStream: TStream);
begin
  AStream.CopyFrom((AElement as THTML.TMemoryImage).Data, 0);
end;

procedure TmnwHTMLRenderer.TMemoryImageHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  inherited;
  Attributes['src'] := IncludeURLDelimiter(TmnwHTMLRenderer(Renderer).HomeUrl) + AElement.GetPath;
  if (AElement as THTML.TImage).AltText <> '' then
    Attributes['alt'] := (AElement as THTML.TImage).AltText;
end;

procedure TmnwHTMLRenderer.TMemoryImageHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext);
var
  e: THTML.TMemoryImage;
begin
  e := AElement as THTML.TMemoryImage;
  Context.Output.WriteLn('html', '<img '+Attributes.GetText(True)+' />', [woOpenTag, woCloseTag]);
  inherited;
end;

{ TmnwSchema }

constructor TmnwSchema.Create;
begin
  inherited Create;
  FRoot := Self;
end;

destructor TmnwSchema.Destroy;
begin
  inherited;
end;

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
  FHeader := THeader.Create(True);
  FFooter := TFooter.Create(True);
  FContainer := TContainer.Create(True);
end;

destructor THTML.TDocument.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FFooter);
  FreeAndNil(FContainer);
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

{ TmnwElement }

function TmnwElement.This: TmnwElement;
begin
  Result := Self;
end;

function TmnwElement.GetPath: string;
begin
  if (Parent <> nil) then
  begin
    Result := IncludeURLDelimiter(Parent.GetPath)+Route;
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

procedure TmnwElement.DoCompose;
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

constructor TmnwElement.Create(AEmbed: Boolean);
begin
  Create;
  FEmbed := True;
end;

destructor TmnwElement.Destroy;
begin
  FreeAndNil(FAttributes);
  inherited;
end;

function TmnwElement.Add<O>(const AName: String; const AID: String): O;
begin
  Result := O.Create;
  Result.FName := AName;
  Result.FID := AID;
  Result.FParent := Self;
  Result.FRoot := FRoot;
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

{ TmnwSchema }

function TmnwSchema.Render(Renderer: TmnwRenderer; DataObject: TObject; AOutput: TmnwOutput): Boolean;
var
  Context: TmnwContext;
begin
  Result := False;
  Context.Output := AOutput;
  Context.Renderer := Renderer;
  Context.DataObject := DataObject;
  Render(Context);
  Result := True;
end;

function TmnwSchema.Render(Renderer: TmnwRenderer; DataObject: TObject; AStrings: TStrings): Boolean;
var
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create;
  try
    Result := Render(Renderer, DataObject, AStringStream);
    AStrings.Text := AStringStream.DataString;
  finally
    FreeAndNil(AStringStream);
  end;
end;

function TmnwSchema.Render(Renderer: TmnwRenderer; DataObject: TObject; AStream: TStream): Boolean;
var
  Writer: TmnwWriter;
  Output: TmnwOutput;
begin
  Writer := TmnwWriter.Create('html', AStream);
  Output := TmnwOutput.Create;
  Output.Add(Writer);
  try
    Result := Render(Renderer, DataObject, Output);
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

end.
