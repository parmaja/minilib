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
    Renderer: TmnwRenderer;
    Output: TmnwOutput;
  end;

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FRoot: TmnwSchema;
    FParent: TmnwElement;

    FComment: String;
    FID: String;
    FName: String;
    FStyle: String;
    FAttributes: TmnwAttributes;
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;
    procedure DoCompose; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add<O: TmnwElement>(const AName: String = ''; const AID: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwElement read FParent;

    function GetPath: string; virtual;

    procedure Render(Context: TmnwContext; vLevel: Integer);
    procedure Compose;

    property Name: String read FName write FName;
    property Style: String read FStyle write FStyle;
    property ID: String read FID write FID;
    property Comment: String read FComment write FComment;
    property Attributes: TmnwAttributes read FAttributes;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TmnwWriter = class(TmnNamedObject)
  private
    FStream: TStream;
  public
    constructor Create(AName: string; AStream: TStream);
    procedure Write(const S: string; Options: TmnwWriterOptions = []); virtual;
    property Stream: TStream read FStream write FStream;
  end;

  { TmnwOutput }

  TmnwOutput = class(TmnNamedObjectList<TmnwWriter>)
  private
  public
    procedure Write(const Target, S: string; Options: TmnwWriterOptions = []); overload;
    procedure Write(const Target, S: string; Level: Integer; Options: TmnwWriterOptions); overload;
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

    function Render(RendererClass: TmnwRendererClass; AOutput: TmnwOutput): Boolean; overload;
    function Render(RendererClass: TmnwRendererClass; AStream: TStream): Boolean; overload;
    function Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean; overload;
  end;

  { TmnwElementRenderer }

  TmnwElementRenderer = class(TObject)
  private
  protected
    procedure DefaultRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
    procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); virtual;
    procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); virtual;
  public
    procedure Render(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
    procedure Respond(AElement: TmnwElement; AStream: TStream); virtual;
    constructor Create; virtual; //usfull for creating it by RendererClass.Create
    procedure CollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
  end;

  TmnwElementRendererClass = class of TmnwElementRenderer;

  TmnwRenderer = class(TmnwElement)
  public
    type

      TmnwContainerRenderer = class(TmnwElementRenderer)
      protected
      end;

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
        function FindRenderer(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
      end;
  protected
    FObjectClasses: TRegObjects;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass);
    property ObjectClasses: TRegObjects read FObjectClasses;

  end;

  TmnwSchemaClass = class of TmnwSchema;

  TDirection = (dirUnkown, dirLTR, dirRTL);

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  THTML =class(TmnwSchema)
  public
    type
      TContainer = class;

      { TDocument }

      TDocument = class(TmnwElement)
      private
        FTitle: string;
        FVersion: integer;
      public
        Direction: TDirection;
        property Version: integer read FVersion write FVersion;
        property Title: string read FTitle write FTitle;
      end;

      //* Custom Tag
      TTag = class(TmnwElement)
      public
      end;

      { TContainer }

      TContainer = class abstract(TmnwElement)
      protected
        procedure Added(Item: TmnwElement); override;
      public
      end;

      TPage = class(TContainer)
      public
      end;

      TParagraph = class(TmnwElement)
      public
        Text: string;
      end;

      TImage = class(TmnwElement)
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

      TBreak = class(TmnwElement)
      private
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
      end;

      { TDocumentHTML }

      TDocumentHTML = class(TElementHTML)
      protected
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TContainerHTML }

      TContainerHTML = class abstract(TmnwContainerRenderer)
      protected
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      public
        constructor Create; override;
      end;

      { TPageHTML }

      TPageHTML = class(TContainerHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TParagrapHTML }

      TParagrapHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TBreakHTML }

      TBreakHTML = class(TElementHTML)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TImageHTML }

      TImageHTML = class(TElementHTML)
      public
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TMemoryImageHTML }

      TMemoryImageHTML = class(TImageHTML)
      public
        procedure Respond(AElement: TmnwElement; AStream: TStream); override;
        procedure DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes); override;
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

    protected
      procedure Created; override;
  end;

function LevelStr(vLevel: Integer): String;

implementation

{ THTML.TContainerHTML }

constructor TmnwHTMLRenderer.TContainerHTML.Create;
begin
  inherited Create;
end;

procedure TmnwHTMLRenderer.TContainerHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
begin
  inherited;
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

procedure TmnwOutput.Write(const Target, S: string; Level: Integer; Options: TmnwWriterOptions);
begin
  Write(Target, LevelStr(Level) + S, Options);
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

procedure TmnwElementRenderer.DefaultRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
var
  o: TmnwElement;
begin
  for o in AElement do
  begin
    o.Render(Context, vLevel);
  end;
end;

procedure TmnwElementRenderer.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
begin
  DefaultRender(AElement, Context, vLevel);
end;

procedure TmnwElementRenderer.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
end;

procedure TmnwElementRenderer.Render(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
var
  lAttributes: TmnwAttributes;
begin
  lAttributes := TmnwAttributes.Create;
  try
    CollectAttributes(AElement, lAttributes);
    DoRender(AElement, lAttributes, Context, vLevel);
  finally
    lAttributes.Free;
  end;
end;

procedure TmnwElementRenderer.Respond(AElement: TmnwElement; AStream: TStream);
begin
end;

constructor TmnwElementRenderer.Create;
begin
  inherited Create;
end;

procedure TmnwElementRenderer.CollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  if AElement.ID <> '' then
    Attributes.Values['ID'] := AElement.ID;
  if AElement.Name <> '' then
    Attributes.Values['Name'] := AElement.Name;
  DoCollectAttributes(AElement, Attributes);
end;

procedure TmnwElement.Render(Context: TmnwContext; vLevel: Integer);
var
  Renderer: TmnwElementRenderer;
  RendererClass: TmnwElementRendererClass;
begin
  if (Context.Renderer <> nil) then
  begin
    RendererClass := Context.Renderer.ObjectClasses.FindRenderer(TmnwElementClass(ClassType));
    if RendererClass <> nil then
    begin
      Renderer := RendererClass.Create;
      try
        Renderer.Render(Self, Context, vLevel);
      finally
        Renderer.Free;
      end;
    end;
  end;
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

function TmnwRenderer.TRegObjects.FindRenderer(AObjectClass: TmnwElementClass): TmnwElementRendererClass;
var
  o: TRegObject;
begin
  Result := TmnwElementRenderer;
  for o in Self do
  begin
    if AObjectClass = o.ObjectClass then
    begin
      Result := o.RendererClass;
      break;
    end
    else if AObjectClass.InheritsFrom(o.ObjectClass) then
    begin
      Result := o.RendererClass;
    end;
  end;
end;

procedure TmnwHTMLRenderer.Created;
begin
  inherited Created;
  RegisterRenderer(THTML.TDocument ,TDocumentHTML);
  RegisterRenderer(THTML.TPage, TPageHTML);
  RegisterRenderer(THTML.TParagraph, TParagrapHTML);
  RegisterRenderer(THTML.TBreak, TBreakHTML);
  RegisterRenderer(THTML.TBreak, TBreakHTML);
  RegisterRenderer(THTML.TImage, TImageHTML);
  RegisterRenderer(THTML.TMemoryImage, TMemoryImageHTML);
end;

{ TmnwHTMLRenderer.TDocumentHTML }

procedure TmnwHTMLRenderer.TDocumentHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
var
  e: THTML.TDocument;
begin
  e := AElement as THTML.TDocument;
  if e.Direction = dirRTL then
    Attributes.Values['dir'] := 'rtl'
  else if E.Direction = dirLTR then
    Attributes.Values['dir'] := 'ltr'
end;

procedure TmnwHTMLRenderer.TDocumentHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
var
  e: THTML.TDocument;
begin
  e := AElement as THTML.TDocument;
  Context.Output.Write('html', '<html' + Attributes.GetText(True) + '>', [cboEndLine]);
  Context.Output.Write('html', '<title>'+ e.Title + '</title>', [cboEndLine]);
  Context.Output.Write('html', '<head>', [cboEndLine]);
  //* Collect head from childs
  Context.Output.Write('html', '</head>', [cboEndLine]);
  inherited;
  Context.Output.Write('html', '</html>', [cboEndLine]);
end;

{ TmnwHTMLRenderer.TPageHTML }

procedure TmnwHTMLRenderer.TPageHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
begin
  Context.Output.Write('html', '<body>', [cboEndLine]);
  inherited;
  Context.Output.Write('html', '</body>', [cboEndLine]);
end;

{ TmnwHTMLRenderer.TParagrapHTML }

procedure TmnwHTMLRenderer.TParagrapHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
var
  e: THTML.TParagraph;
begin
  e := AElement as THTML.TParagraph;
  Context.Output.Write('html', '<p>', []);
  Context.Output.Write('html', e.Text, []);
  inherited;
  Context.Output.Write('html', '</p>', [cboEndLine]);
end;

{ TmnwHTMLRenderer.TBreakHTML }

procedure TmnwHTMLRenderer.TBreakHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
begin
  Context.Output.Write('html', '</br>', [cboEndLine]);
end;

{ TmnwHTMLRenderer.TImageHTML }

procedure TmnwHTMLRenderer.TImageHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  Attributes['src'] := (AElement as THTML.TImage).Source;
  inherited;
end;

procedure TmnwHTMLRenderer.TImageHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
var
  e: THTML.TImage;
begin
  e := AElement as THTML.TImage;
  Context.Output.Write('html', '<img '+Attributes.GetText(True)+'>', []);
  Context.Output.Write('html', e.AltText, []);
  inherited;
  Context.Output.Write('html', '</img>', [cboEndLine]);
end;

{ TmnwHTMLRenderer.TMemoryImageHTML }

procedure TmnwHTMLRenderer.TMemoryImageHTML.Respond(AElement: TmnwElement; AStream: TStream);
begin
  AStream.CopyFrom((AElement as THTML.TMemoryImage).Data, 0);
end;

procedure TmnwHTMLRenderer.TMemoryImageHTML.DoCollectAttributes(AElement: TmnwElement; Attributes: TmnwAttributes);
begin
  inherited;
  Attributes['src'] := AElement.GetPath;
end;

procedure TmnwHTMLRenderer.TMemoryImageHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
var
  e: THTML.TMemoryImage;
begin
  e := AElement as THTML.TMemoryImage;
  Context.Output.Write('html', '<img '+Attributes.GetText(True)+'>', []);
  Context.Output.Write('html', e.AltText, []);
  inherited;
  Context.Output.Write('html', '</img>', [cboEndLine]);
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

procedure TmnwRenderer.RegisterRenderer(AObjectClass: TmnwElementClass; ARendererClass: TmnwElementRendererClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.RendererClass := ARendererClass;
  ObjectClasses.Add(aRegObject);
end;

{ TContainer }

procedure THTML.TContainer.Added(Item: TmnwElement);
begin
  inherited Added(Item);
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
  if (Parent <>nil) and (Name <> '') then
    Result := IncludeURLDelimiter(Parent.GetPath) + Name
  else
    Result := '';
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

procedure TmnwElement.DoCompose;
begin
end;

constructor TmnwElement.Create;
begin
  inherited Create;
  FName := '';
  FAttributes := TmnwAttributes.Create;
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

function TmnwSchema.Render(RendererClass: TmnwRendererClass; AOutput: TmnwOutput): Boolean;
var
  Context: TmnwContext;
begin
  Result := False;
  Context.Output := AOutput;
  Context.Renderer := RendererClass.Create;
  try
    Render(Context, 0);
    Result := True;
  finally
    FreeAndNil(Context.Renderer);
  end;
end;

function TmnwSchema.Render(RendererClass: TmnwRendererClass; AStream: TStream): Boolean;
var
  Writer: TmnwWriter;
  Output: TmnwOutput;
begin
  Writer := TmnwWriter.Create('html', AStream);
  Output := TmnwOutput.Create;
  Output.Add(Writer);
  try
    Result := Render(RendererClass, Output);
  finally
    FreeAndNil(Output);
  end;
  Result := True;
end;

function TmnwSchema.Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean;
var
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create;
  try
    Result := Render(RendererClass, AStringStream);
    AStrings.Text := AStringStream.DataString;
  finally
    FreeAndNil(AStringStream);
  end;
end;

procedure TmnwElement.Compose;
var
  o: TmnwElement;
begin
  DoCompose;
  for o in Self do
    o.Compose;
end;

constructor TmnwWriter.Create(AName: string; AStream: TStream);
begin
  inherited Create;
  Name := AName;
  FStream := AStream;
end;

procedure TmnwWriter.Write(const S: string; Options: TmnwWriterOptions);
begin
  if (cboEndLine in Options) then
    FStream.WriteUtf8String(S + sWinEndOfLine)
  else
    FStream.WriteUtf8String(S)
end;

{ TmnwRenderer }

constructor TmnwRenderer.Create;
begin
  inherited;
  FObjectClasses := TRegObjects.Create;
end;

destructor TmnwRenderer.Destroy;
begin
  FreeAndNil(FObjectClasses);
  inherited;
end;

end.
