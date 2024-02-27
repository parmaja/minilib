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
  mnClasses;

type

  TmnwSchema = class;

  TmnwObject = class;
  TmnwObjectClass = class of TmnwObject;

  { TmnwObject }

  TmnwObject = class(TmnObjectList<TmnwObject>)
  private
    FComment: String;
    FName: String;
    FParent: TmnwObject;
    FRoot: TmnwSchema;
    FTags: String;
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwObject); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwObjectClass; AName: string; RaiseException: Boolean = false): TmnwObject;
  public
    constructor Create; virtual;
    function Add<O: TmnwObject>(const AName: String = ''): O;
    procedure LinkTo(AParent: TmnwObject);
    function Find(const Name: string): TmnwObject;
    function IndexOfName(vName: string): Integer;

    property Comment: String read FComment write FComment;
    function This: TmnwObject; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwObject read FParent;

    property Name: String read FName write FName;
    property Tags: String read FTags write FTags; //etc: 'Key,Data'
  end;

  { TmnwWriterObject }

  TmnwWriterOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TmnwWriterObject = class(TObject)
  private
    FParams: TStringList;
    FElement: TObject;
  public
    Index: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Write(S: string; Options: TmnwWriterOptions = []); overload; virtual; abstract;
    procedure Write(Level: Integer; S: string; Options: TmnwWriterOptions = []); overload;
    property Element: TObject read FElement write FElement;
    property Params: TStringList read FParams;
  end;

  { TmnwSchema }

  TmnwSchema = class(TmnwObject)
  protected
  public
    type
      TmnwElement = class;

      TContainer = class;

      { TmnwRenderer }

      TmnwRenderer = class(TObject)
      private
      protected
        procedure DefaultRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer);
        function DoRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer): Boolean; virtual;
      public
        procedure Render(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer);
        constructor Create; virtual; //usfull for creating it by RendererClass.Create
      end;

      TmnwRendererClass = class of TmnwRenderer;

      TmnwContainerRenderer = class(TmnwRenderer)
      protected
      end;

      { TmnwElement }

      TmnwElement = class(TmnwObject)
      private
        FRendererClass: TmnwRendererClass;
      protected
        procedure Update; override;
        procedure SetRendererClass(AValue: TmnwRendererClass); virtual;
        property RendererClass: TmnwRendererClass read FRendererClass write SetRendererClass;
        procedure Created; override;
      public
        procedure Render(WriterObject: TmnwWriterObject; vLevel: Integer);
      end;

      { TDocument }

      TDocument = class(TmnwElement)
      private
        FVersion: integer;
      public
        property Version: integer read FVersion write FVersion;
      end;

      TParagraph = class(TmnwElement)
      public
      end;

      //* Custom Tag
      TTag = class(TmnwElement)
      public
      end;

      { TContainer }

      TContainer = class abstract(TmnwElement)
      protected
        procedure Added(Item: TmnwObject); override;
        procedure SetRendererClass(AValue: TmnwRendererClass); override;
      public
      end;

      TPage = class(TContainer)
      public
      end;

      { Break }

      TBreak = class(TmnwElement)
      private
      public
      end;

      { Trigger }


    public
      type

        TRegObject = class(TObject)
        public
          ObjectClass: TmnwObjectClass;
          RendererClass: TmnwRendererClass;
        end;

        { TRegObjects }

        TRegObjects = class(TmnObjectList<TRegObject>)
        public
          function FindDerived(AObjectClass: TmnwObjectClass): TmnwObjectClass;
          function FindRenderer(AObjectClass: TmnwObjectClass): TmnwRendererClass;
        end;

  private
    FImpact: Boolean;
    FObjectClasses: TRegObjects;
    FQuoteChar: string;

  protected
  public
    constructor Create(AName: String); overload;
    destructor Destroy; override;

    function Render(WriterObject: TmnwWriterObject): Boolean; overload;
    function Render(AStrings: TStrings): Boolean; overload;

    procedure RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwRendererClass);

    property ObjectClasses: TRegObjects read FObjectClasses;
    property QuoteChar: string read FQuoteChar write FQuoteChar; //Empty, it will be used with Callback
    property Impact: Boolean read FImpact write FImpact; //use inline peroperty of members
  end;

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  TmnwSchemaClass = class of TmnwSchema;

  { TmnwHTML }

  TmnwHTML = class(TmnwSchema)
  protected
  public
    type
      { TDocumentStd }

      TDocumentHtml = class(TmnwRenderer)
      public
        function DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean; override;
      end;

      { TContainerStd }

      TContainerHTML = class abstract(TmnwContainerRenderer)
      protected
        function DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean; override;
      public
        constructor Create; override;
      end;

      { TPageHTML }

      TPageHTML = class(TContainerHTML)
      public
        function DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean; override;
      end;

    public
      procedure Created; override;
  end;

function LevelStr(vLevel: Integer): String;

implementation

{ TmnwHTML.TContainerStd }

constructor TmnwHTML.TContainerHTML.Create;
begin
  inherited Create;
end;

function TmnwHTML.TContainerHTML.DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean;
var
  o: TmnwObject;
  AContainer: TContainer;
begin
  Result := True;
  AContainer := AObject as TContainer;
  with AContainer do
  begin
  end;
end;

{ TmnwHTML }

{ TmnwWriterObject }

constructor TmnwWriterObject.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TmnwWriterObject.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TmnwWriterObject.Write(Level: Integer; S: string; Options: TmnwWriterOptions);
begin
  Write(LevelStr(Level) + S, Options);
end;

{ TmnwSchema.TmnwRenderer }

procedure TmnwSchema.TmnwRenderer.DefaultRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer);
var
  o: TmnwObject;
begin
  for o in AElement do
    (o as TmnwElement).Render(WriterObject, vLevel);
end;

function TmnwSchema.TmnwRenderer.DoRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer): Boolean;
begin
  Result := False;
end;

procedure TmnwSchema.TmnwRenderer.Render(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer);
begin
  if not DoRender(AElement, WriterObject, vLevel) then
    DefaultRender(AElement, WriterObject, vLevel);
end;

constructor TmnwSchema.TmnwRenderer.Create;
begin
  inherited Create;
end;

{ TmnwSchema.TmnwElement }

procedure TmnwSchema.TmnwElement.Update;
begin
  inherited;
  if (FRoot <> nil) then
    RendererClass := (Root as TmnwSchema).ObjectClasses.FindRenderer(TmnwObjectClass(ClassType));
end;

procedure TmnwSchema.TmnwElement.SetRendererClass(AValue: TmnwRendererClass);
begin
  if FRendererClass =AValue then Exit;
  FRendererClass :=AValue;
end;

procedure TmnwSchema.TmnwElement.Created;
begin
  inherited Created;
  //Update;
end;

procedure TmnwSchema.TmnwElement.Render(WriterObject: TmnwWriterObject; vLevel: Integer);
var
  Renderer: TmnwRenderer;
begin
  if RendererClass <> nil then
  begin
    Renderer := RendererClass.Create;
    try
      Renderer.Render(self, WriterObject, vLevel);
    finally
      Renderer.Free;
    end;
  end;
end;

{ TmnwSchema.TRegObjects }

function TmnwSchema.TRegObjects.FindDerived(AObjectClass: TmnwObjectClass): TmnwObjectClass;
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

function TmnwSchema.TRegObjects.FindRenderer(AObjectClass: TmnwObjectClass): TmnwRendererClass;
var
  o: TRegObject;
begin
  Result := TmnwRenderer;
  for o in Self do
  begin
    if AObjectClass.InheritsFrom(o.ObjectClass) then
    begin
      Result := o.RendererClass;
      break;
    end;
  end;
end;

procedure TmnwHTML.Created;
begin
  inherited Created;
  RegisterRenderer(TDocument ,TDocumentHtml);
  RegisterRenderer(TPage, TPageHTML);
end;

{ TmnwHTML.TDocumentHtml }

function TmnwHTML.TDocumentHtml.DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean;
begin
  Result := inherited DoRender(AObject, Callback, vLevel);
end;

{ TmnwHTML.TPageHTML }

function TmnwHTML.TPageHTML.DoRender(AObject: TmnwElement; Callback: TmnwWriterObject; vLevel: Integer): Boolean;
begin
  Callback.Write('<Body>', [cboEndLine]);
  Result := inherited DoRender(AObject, Callback, vLevel);
  Callback.Write('</Body>', [cboEndLine]);
end;

{ TmnwSchema }

constructor TmnwSchema.Create(AName: String);
begin
  inherited Create;
  FRoot := Self;
  FObjectClasses := TRegObjects.Create;
end;

destructor TmnwSchema.Destroy;
begin
  FreeAndNil(FObjectClasses);
  inherited Destroy;
end;

procedure TmnwSchema.RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwRendererClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.RendererClass := ARendererClass;
  ObjectClasses.Add(aRegObject);
end;

{ TContainer }

procedure TmnwSchema.TContainer.Added(Item: TmnwObject);
begin
  inherited Added(Item);
end;

procedure TmnwSchema.TContainer.SetRendererClass(AValue: TmnwRendererClass);
begin
  if not (AValue.InheritsFrom(TmnwContainerRenderer)) then
    raise Exception.Create('Renderer should be ContainerRenderer');

  inherited SetRendererClass(AValue);
end;

{ TmnwObject }

function TmnwObject.This: TmnwObject;
begin
  Result := Self;
end;

procedure TmnwObject.Update;
begin

end;

procedure TmnwObject.Added(Item: TmnwObject);
begin
  inherited;
  Item.Update;
end;

procedure TmnwObject.Check;
var
  o: TmnwObject;
begin
  for o in Self do
    o.Check;
end;

function TmnwObject.Find(const Name: string): TmnwObject;
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

function TmnwObject.FindObject(ObjectClass: TmnwObjectClass; AName: string; RaiseException: Boolean): TmnwObject;
var
  o: TmnwObject;
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

constructor TmnwObject.Create;
begin
  inherited Create;
  FName := ClassName();
end;

function TmnwObject.Add<O>(const AName: String = ''): O;
begin
  Result := O.Create;
  Result.FName := AName;
  if Result.FName = '' then
    Result.FName := ClassName;
  Result.FParent := Self;
  Result.FRoot := FRoot;
  inherited Add(Result);
end;

function TmnwObject.IndexOfName(vName: string): Integer;
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

function ValueToStr(vValue: Variant): string;
begin
  if not VarIsEmpty(vValue) then
  begin
    if VarType(vValue) = varString then
      Result := '''' + vValue + ''''
    else
      Result := VarToStr(vValue);
  end
  else
    Result := '''''';
end;

procedure TmnwObject.LinkTo(AParent: TmnwObject);
begin
  if AParent <> nil then
  begin
    FParent := AParent;
//    AParent.Add(Self);
    FRoot := AParent.Root;
  end;
end;

{ TDocument }

type

  { TmnwWebCallbackObject }

  TmnwWebCallbackObject = class(TmnwWriterObject)
  private
    Buffer: string;
  public
    Callback: TStrings; //Reference to Callback
    constructor Create(ACallback: TStrings);
    destructor Destroy; override;
    procedure Write(S: string; Options: TmnwWriterOptions = []); override;
  end;

{ TmnwWebCallbackObject }

constructor TmnwWebCallbackObject.Create(ACallback: TStrings);
begin
  inherited Create;
  Callback := ACallback;
end;

destructor TmnwWebCallbackObject.Destroy;
begin
  if Buffer <> '' then
    Write('', [cboEndLine]);
  inherited Destroy;
end;

procedure TmnwWebCallbackObject.Write(S: string; Options: TmnwWriterOptions);
begin
  Buffer := Buffer + S;
  {if (cboEndChunk in Options) and (Callback.Count > 0) then
    Buffer := Buffer + ';';}
  if (cboEndLine in Options) or (cboEndChunk in Options) then
  begin
    if Buffer <> '' then
      Callback.Add(Buffer);
    Buffer := '';
  end;
  if (cboEndChunk in Options) and (Callback.Count > 0) then
  begin
    Callback.Add('^');
//    Callback.Add(' ');
  end;
end;

function TmnwSchema.Render(AStrings: TStrings): Boolean;
var
  CallbackCB: TmnwWebCallbackObject;
begin
  CallbackCB := TmnwWebCallbackObject.Create(AStrings);
  try
    Render(CallbackCB);
  finally
    FreeAndNil(CallbackCB);
  end;
  Result := True;
end;

function TmnwSchema.Render(WriterObject: TmnwWriterObject): Boolean;
var
  AParams: TStringList;
  o: TmnwObject;
  AElement: TmnwElement;
  Renderer: TmnwRenderer;
begin
  Check;
  AParams := TStringList.Create;
  try
    for o in Self do
    begin
      AElement := (o as TmnwElement);
      if AElement.RendererClass <> nil then
      begin
        Renderer := AElement.RendererClass.Create;
        try
          Renderer.Render(AElement, WriterObject, 0);
        finally
          Renderer.Free;
        end;
      end;
    end;
  finally
    FreeAndNil(AParams);
  end;
  Result := True;
end;

end.
