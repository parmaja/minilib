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

  TmnwElement = class;

  TmnwObject = class;
  TmnwObjectClass = class of TmnwObject;

  { TmnwObject }

  TmnwObject = class(TmnObjectList<TmnwObject>)
  private
    FComment: String;
    FName: String;
    FParent: TmnwObject;
    FRoot: TmnwElement;
    FTags: String;
  protected
    procedure Added(Item: TmnwObject); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwObjectClass; AName: string; RaiseException: Boolean = false): TmnwObject;
  public
    constructor Create(AParent: TmnwObject; AName: String);

    function Find(const Name: string): TmnwObject;
    function IndexOfName(vName: string): Integer;

    property Comment: String read FComment write FComment;
    function This: TmnwObject; //I wish i have templates/meta programming in pascal
    property Root: TmnwElement read FRoot;
    property Parent: TmnwObject read FParent;

    property Name: String read FName write FName;
    property Tags: String read FTags write FTags; //etc: 'Key,Data'
  end;

  { TmnwCallbackObject }

  TmnwCallbackObjectOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TmnwCallbackObject = class(TObject)
  private
    FParams: TStringList;
    FCallbackObject: TObject;
  public
    Index: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure Add(S: string; Options: TmnwCallbackObjectOptions = []); overload; virtual; abstract;
    procedure Add(Level: Integer; S: string; Options: TmnwCallbackObjectOptions = []); overload;
    property CallbackObject: TObject read FCallbackObject write FCallbackObject;
    property Params: TStringList read FParams;
  end;

  { TmnwElement }

  TmnwElement = class(TmnwObject)
  protected
  public
    type
      TmnwWebObject = class;

      TContainer = class;

      { TmnwRenderer }

      TmnwRenderer = class(TObject)
      private
      protected
        function VarBoolToStr(Value: Variant): string; virtual;
        procedure DefaultRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer);
        function DoRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer): Boolean; virtual;
      public
        procedure Render(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer);
        constructor Create; virtual; //usfull for creating it by RendererClass.Create
      end;

      TmnwRendererClass = class of TmnwRenderer;

      TmnwContainerRenderer = class(TmnwRenderer)
      protected
      end;

      { TmnwWebObject }

      TmnwWebObject = class(TmnwObject)
      private
        FRendererClass: TmnwRendererClass;
      protected
        procedure SetRendererClass(AValue: TmnwRendererClass); virtual;
        property RendererClass: TmnwRendererClass read FRendererClass write SetRendererClass;
        procedure Created; override;
      public
        procedure Render(Callback: TmnwCallbackObject; vLevel: Integer);
      end;

      { TDocument }

      TDocument = class(TmnwWebObject)
      private
        FVersion: integer;
      public
        constructor Create(Amnw: TmnwElement; AName: String);
        function This: TDocument;
        property Version: integer read FVersion write FVersion;
      end;

      { TmnwSchema }

      TPage = class(TmnwWebObject)
      public
        constructor Create(ADatabase: TDocument; AName: String);
        function This: TPage;
      end;

      { TContainer }

      TContainer = class(TmnwWebObject)
      protected
        procedure Added(Item: TmnwObject); override;
        procedure SetRendererClass(AValue: TmnwRendererClass); override;
      public
        Prefix: string; //used to added to Renderd name, need more tests
        constructor Create(ASchema: TPage; AName: String; APrefix: string = '');
        function This: TContainer;
      end;

      { Break }

      TBreak = class(TmnwWebObject)
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
    constructor Create(AName: String); virtual;
    destructor Destroy; override;
    function This: TmnwElement;

    function AddDocument(AName: String): TDocument;
    function AddHtml(ADatabase: TDocument; AName: String): TPage;
    function AddContainer(ASchema: TPage; AName: String): TContainer;

    function Render(Callback: TmnwCallbackObject): Boolean; overload;
    function Render(vCallback: TStrings): Boolean; overload;

    procedure RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwRendererClass);

    property ObjectClasses: TRegObjects read FObjectClasses;
    property QuoteChar: string read FQuoteChar write FQuoteChar; //Empty, it will be used with Callback
    property Impact: Boolean read FImpact write FImpact; //use inline peroperty of members
  end;

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  TmnwElementClass = class of TmnwElement;

  TmnwStandard = class(TmnwElement)
  protected
  public
    type

      { TDatabaseStd }

      TDatabaseStd = class(TmnwRenderer)
      public
      end;

      { TSchemaStd }

      TSchemaStd = class(TmnwRenderer)
      public
      end;

      TRenderPlace = (
        gnpNone,
        gnpAttribute,
        gnpInternal,
        gnpExternal
      );

      TRenderOptions = record
        RenderPlace: TRenderPlace;
      end;

      { TContainerStd }

      TContainerStd = class(TmnwContainerRenderer)
      public
        RenderOptions: TRenderOptions;
        constructor Create; override;

        function DoRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer): Boolean; override;
      end;

  end;

function LevelStr(vLevel: Integer): String;
function ValueToStr(vValue: Variant): string;

implementation

{ TmnwStandard.TContainerStd }

constructor TmnwStandard.TContainerStd.Create;
begin
  inherited Create;
  RenderOptions.RenderPlace := gnpInternal;
end;

function TmnwStandard.TContainerStd.DoRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer): Boolean;
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

{ TmnwCallbackObject }

constructor TmnwCallbackObject.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TmnwCallbackObject.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TmnwCallbackObject.Add(Level: Integer; S: string; Options: TmnwCallbackObjectOptions);
begin
  Add(LevelStr(Level) + S, Options);
end;

{ TmnwElement.TmnwRenderer }

function TmnwElement.TmnwRenderer.VarBoolToStr(Value: Variant): string;
begin
  if VarType(Value) = varBoolean then
    Result := BoolToStr(Boolean(Value), true)
  else if VarType(Value) = VarString then
    Result := Value
  else
    Result := BoolToStr(Integer(Value) <> 0, true)
end;

procedure TmnwElement.TmnwRenderer.DefaultRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer);
var
  o: TmnwObject;
begin
  for o in AObject do
    (o as TmnwWebObject).Render(Callback, vLevel);
end;

function TmnwElement.TmnwRenderer.DoRender(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer): Boolean;
begin
  Result := False;
end;

procedure TmnwElement.TmnwRenderer.Render(AObject: TmnwWebObject; Callback: TmnwCallbackObject; vLevel: Integer);
begin
  if not DoRender(AObject, Callback, vLevel) then
    DefaultRender(AObject, Callback, vLevel);
end;

constructor TmnwElement.TmnwRenderer.Create;
begin
  inherited Create;
end;

{ TmnwElement.TmnwWebObject }

procedure TmnwElement.TmnwWebObject.SetRendererClass(AValue: TmnwRendererClass);
begin
  if FRendererClass =AValue then Exit;
  FRendererClass :=AValue;
end;

procedure TmnwElement.TmnwWebObject.Created;
begin
  inherited Created;
  if (FRoot <> nil) then
    RendererClass := (Root as TmnwElement).ObjectClasses.FindRenderer(TmnwObjectClass(ClassType));
end;

procedure TmnwElement.TmnwWebObject.Render(Callback: TmnwCallbackObject; vLevel: Integer);
var
  Renderer: TmnwRenderer;
begin
  if RendererClass <> nil then
  begin
    Renderer := RendererClass.Create;
    try
      Renderer.Render(self, Callback, vLevel);
    finally
      Renderer.Free;
    end;
  end;
end;

{ TmnwElement.TRegObjects }

function TmnwElement.TRegObjects.FindDerived(AObjectClass: TmnwObjectClass): TmnwObjectClass;
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

function TmnwElement.TRegObjects.FindRenderer(AObjectClass: TmnwObjectClass): TmnwRendererClass;
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

{ TmnwElement }

constructor TmnwElement.Create(AName: String);
begin
  inherited Create(nil, AName);
  FRoot := Self;
  FObjectClasses := TRegObjects.Create;
end;

destructor TmnwElement.Destroy;
begin
  FreeAndNil(FObjectClasses);
  inherited Destroy;
end;

function TmnwElement.This: TmnwElement;
begin
  Result := Self;
end;

function TmnwElement.AddDocument(AName: String): TDocument;
begin
  Result := TDocument.Create(Self, AName);
end;

function TmnwElement.AddHTML(ADatabase: TDocument; AName: String): TPage;
begin
  Result := TPage.Create(ADatabase, AName);
end;

function TmnwElement.AddContainer(ASchema: TPage; AName: String): TContainer;
begin
  Result := TContainer.Create(ASchema, AName);
end;

procedure TmnwElement.RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwRendererClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.RendererClass := ARendererClass;
  ObjectClasses.Add(aRegObject);
end;

{ TContainer }

function TmnwElement.TContainer.This: TContainer;
begin
  Result := Self;
end;

procedure TmnwElement.TContainer.Added(Item: TmnwObject);
begin
  inherited Added(Item);
end;

procedure TmnwElement.TContainer.SetRendererClass(AValue: TmnwRendererClass);
begin
  if not (AValue.InheritsFrom(TmnwContainerRenderer)) then
    raise Exception.Create('Renderer should be ContainerRenderer');

  inherited SetRendererClass(AValue);
end;

constructor TmnwElement.TContainer.Create(ASchema: TPage; AName: String; APrefix: string);
begin
  inherited Create(ASchema, AName);
  Prefix := APrefix;
end;

{ TPage }

function TmnwElement.TPage.This: TPage;
begin
  Result := Self;
end;

constructor TmnwElement.TPage.Create(ADatabase: TDocument; AName: String);
begin
  inherited Create(ADatabase, AName);
end;

{ TmnwObject }

function TmnwObject.This: TmnwObject;
begin
  Result := Self;
end;

procedure TmnwObject.Added(Item: TmnwObject);
begin
  inherited Added(Item);
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

constructor TmnwObject.Create(AParent: TmnwObject; AName: String);
begin
  inherited Create;
  FName := AName;
  if AParent <> nil then
  begin
    FParent := AParent;
    AParent.Add(Self);
    FRoot := AParent.Root;
  end;
end;

{ TDocument }

function TmnwElement.TDocument.This: TDocument;
begin
  Result := Self;
end;

constructor TmnwElement.TDocument.Create(Amnw: TmnwElement; AName: String);
begin
  inherited Create(Amnw, AName);
end;

type

  { TmnwWebCallbackObject }

  TmnwWebCallbackObject = class(TmnwCallbackObject)
  private
    Buffer: string;
  public
    Callback: TStrings; //Reference to Callback
    constructor Create(ACallback: TStrings);
    destructor Destroy; override;
    procedure Add(S: string; Options: TmnwCallbackObjectOptions = []); override;
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
    Add('', [cboEndLine]);
  inherited Destroy;
end;

procedure TmnwWebCallbackObject.Add(S: string; Options: TmnwCallbackObjectOptions);
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

function TmnwElement.Render(vCallback: TStrings): Boolean;
var
  CallbackCB: TmnwWebCallbackObject;
begin
  CallbackCB := TmnwWebCallbackObject.Create(vCallback);
  try
    Render(CallbackCB);
  finally
    FreeAndNil(CallbackCB);
  end;
  Result := True;
end;

function TmnwElement.Render(Callback: TmnwCallbackObject): Boolean;
var
  AParams: TStringList;
  o: TmnwObject;
  CallbackObject: TmnwWebObject;
  Renderer: TmnwRenderer;
begin
  Check;
  AParams := TStringList.Create;
  try
    for o in Self do
    begin
      CallbackObject := (o as TmnwWebObject);
      if CallbackObject.RendererClass <> nil then
      begin
        Renderer := CallbackObject.RendererClass.Create;
        try
          Renderer.Render(CallbackObject, Callback, 0);
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
