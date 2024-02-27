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
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwObject = class;
  TmnwObjectClass = class of TmnwObject;

  TmnwContext = record
    Renderer: TmnwRenderer;
    Writer: TmnwWriter;
  end;

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
    function Find(const Name: string): TmnwObject;
    function IndexOfName(vName: string): Integer;

    function This: TmnwObject; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwObject read FParent;

    property Name: String read FName write FName;
    property Tags: String read FTags write FTags; //etc: 'Key,Data'
    property Comment: String read FComment write FComment;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TmnwWriter = class(TObject)
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

  { TmnwElement }

  TmnwElement = class(TmnwObject)
  private
  protected
    procedure Created; override;
  public
    procedure Render(Context: TmnwContext; vLevel: Integer);
  end;

  { TmnwSchema }

  TmnwSchema = class(TmnwObject)
  protected
  public
  private

  protected
  public
    constructor Create(AName: String); overload;
    destructor Destroy; override;

    function Render(Context: TmnwContext): Boolean; overload;
    function Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean; overload;

    procedure Compose; virtual; abstract;
  end;

  TmnwRenderer = class(TmnwObject)
  public
    type
      { TmnwElementRenderer }

      TmnwElementRenderer = class(TObject)
      private
      protected
        procedure DefaultRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
        procedure DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer); virtual;
      public
        procedure Render(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
        constructor Create; virtual; //usfull for creating it by RendererClass.Create
      end;

      TmnwElementRendererClass = class of TmnwElementRenderer;

      TmnwContainerRenderer = class(TmnwElementRenderer)
      protected
      end;

    type

      TRegObject = class(TObject)
      public
        ObjectClass: TmnwObjectClass;
        RendererClass: TmnwElementRendererClass;
      end;

      { TRegObjects }

      TRegObjects = class(TmnObjectList<TRegObject>)
      public
        function FindDerived(AObjectClass: TmnwObjectClass): TmnwObjectClass;
        function FindRenderer(AObjectClass: TmnwObjectClass): TmnwElementRendererClass;
      end;
  protected
    FObjectClasses: TRegObjects;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwElementRendererClass);
    property ObjectClasses: TRegObjects read FObjectClasses;

  end;

{-------------------------------------------------------}
{-----------------    STANDARD    ----------------------}
{-------------------------------------------------------}

  TmnwSchemaClass = class of TmnwSchema;

  TmnwHTML =class(TmnwSchema)
  public
    type
      TContainer = class;

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
  end;

  { TmnwRendererHTML }

  TmnwRendererHTML = class(TmnwRenderer)
  protected
  public
    type
      { TDocumentHTML }

      TDocumentHtml = class(TmnwElementRenderer)
      public
        procedure DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer); override;
      end;

      { TContainerHTML }

      TContainerHTML = class abstract(TmnwContainerRenderer)
      protected
        procedure DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer); override;
      public
        constructor Create; override;
      end;

      { TPageHTML }

      TPageHTML = class(TContainerHTML)
      public
        procedure DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer); override;
      end;

    protected
      procedure Created; override;
  end;

function LevelStr(vLevel: Integer): String;

implementation

{ TmnwHTML.TContainerHTML }

constructor TmnwRendererHTML.TContainerHTML.Create;
begin
  inherited Create;
end;

procedure TmnwRendererHTML.TContainerHTML.DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
begin
  inherited;
end;

{ TmnwWriter }

constructor TmnwWriter.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
end;

destructor TmnwWriter.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

procedure TmnwWriter.Write(Level: Integer; S: string; Options: TmnwWriterOptions);
begin
  Write(LevelStr(Level) + S, Options);
end;

{ TmnwRenderer.TmnwElementRenderer }

procedure TmnwRenderer.TmnwElementRenderer.DefaultRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
var
  o: TmnwObject;
begin
  for o in AElement do
    (o as TmnwElement).Render(Context, vLevel);
end;

procedure TmnwRenderer.TmnwElementRenderer.DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
begin
  DefaultRender(AElement, Context, vLevel);
end;

procedure TmnwRenderer.TmnwElementRenderer.Render(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
begin
  DoRender(AElement, Context, vLevel);
end;

constructor TmnwRenderer.TmnwElementRenderer.Create;
begin
  inherited Create;
end;

procedure TmnwElement.Created;
begin
  inherited Created;
end;

procedure TmnwElement.Render(Context: TmnwContext; vLevel: Integer);
var
  Renderer: TmnwRenderer.TmnwElementRenderer;
  RendererClass: TmnwRenderer.TmnwElementRendererClass;
begin
  if (Context.Renderer <> nil) then
  begin
    RendererClass := Context.Renderer.ObjectClasses.FindRenderer(TmnwObjectClass(ClassType));
    if RendererClass <> nil then
    begin
      Renderer := RendererClass.Create;
      try
        Renderer.Render(self, Context, vLevel);
      finally
        Renderer.Free;
      end;
    end;
  end;
end;

{ TmnwSchema.TRegObjects }

function TmnwRenderer.TRegObjects.FindDerived(AObjectClass: TmnwObjectClass): TmnwObjectClass;
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

function TmnwRenderer.TRegObjects.FindRenderer(AObjectClass: TmnwObjectClass): TmnwElementRendererClass;
var
  o: TRegObject;
begin
  Result := TmnwElementRenderer;
  for o in Self do
  begin
    if AObjectClass.InheritsFrom(o.ObjectClass) then
    begin
      Result := o.RendererClass;
      break;
    end;
  end;
end;

procedure TmnwRendererHTML.Created;
begin
  inherited Created;
  RegisterRenderer(TmnwHTML.TDocument ,TDocumentHtml);
  RegisterRenderer(TmnwHTML.TPage, TPageHTML);
end;

{ TmnwRendererHTML.TDocumentHtml }

procedure TmnwRendererHTML.TDocumentHtml.DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
begin
  Context.Writer.Write('<html>', [cboEndLine]);
  Context.Writer.Write('<head>', [cboEndLine]);
  Context.Writer.Write('</head>', [cboEndLine]);
  inherited;
  Context.Writer.Write('</html>', [cboEndLine]);
end;

{ TmnwRendererHTML.TPageHTML }

procedure TmnwRendererHTML.TPageHTML.DoRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
begin
  Context.Writer.Write('<body>', [cboEndLine]);
  inherited;
  Context.Writer.Write('</body>', [cboEndLine]);
end;

{ TmnwSchema }

constructor TmnwSchema.Create(AName: String);
begin
  inherited Create;
  FRoot := Self;
  Compose;
end;

destructor TmnwSchema.Destroy;
begin
  inherited;
end;

procedure TmnwRenderer.RegisterRenderer(AObjectClass: TmnwObjectClass; ARendererClass: TmnwElementRendererClass);
var
  aRegObject: TRegObject;
begin
  aRegObject := TRegObject.Create;
  aRegObject.ObjectClass := AObjectClass;
  aRegObject.RendererClass := ARendererClass;
  ObjectClasses.Add(aRegObject);
end;

{ TContainer }

procedure TmnwHTML.TContainer.Added(Item: TmnwObject);
begin
  inherited Added(Item);
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

function TmnwObject.Add<O>(const AName: String): O;
begin
  Result := O.Create;
  Result.FName := AName;
  if Result.FName = '' then
    Result.FName := ClassName;
  Result.FParent := Self;
  Result.FRoot := FRoot;
  Add(Result);
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

{ TDocument }

type

  { TmnwStringsWriter }

  TmnwStringsWriter = class(TmnwWriter)
  private
    Buffer: string;
  public
    Output: TStrings; //Reference to Output
    constructor Create(AOutput: TStrings);
    destructor Destroy; override;
    procedure Write(S: string; Options: TmnwWriterOptions = []); override;
  end;

{ TmnwStringsWriter }

constructor TmnwStringsWriter.Create(AOutput: TStrings);
begin
  inherited Create;
  Output := AOutput;
end;

destructor TmnwStringsWriter.Destroy;
begin
  if Buffer <> '' then
    Write('', [cboEndLine]);
  inherited Destroy;
end;

procedure TmnwStringsWriter.Write(S: string; Options: TmnwWriterOptions);
begin
  Buffer := Buffer + S;
  {if (cboEndChunk in Options) and (Output.Count > 0) then
    Buffer := Buffer + ';';}
  if (cboEndLine in Options) or (cboEndChunk in Options) then
  begin
    if Buffer <> '' then
      Output.Add(Buffer);
    Buffer := '';
  end;
  if (cboEndChunk in Options) and (Output.Count > 0) then
  begin
//    Output.Add(' ');
  end;
end;

function TmnwSchema.Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean;
var
  Writer: TmnwStringsWriter;
  Context: TmnwContext;
begin
  Context.Writer := TmnwStringsWriter.Create(AStrings);
  Context.Renderer := RendererClass.Create;
  try
    Render(Context);
  finally
    FreeAndNil(Context.Writer);
    FreeAndNil(Context.Renderer);
  end;
  Result := True;
end;

function TmnwSchema.Render(Context: TmnwContext): Boolean;
var
  AParams: TStringList;
  o: TmnwObject;
  AElement: TmnwElement;
  Renderer: TmnwRenderer.TmnwElementRenderer;
  RendererClass: TmnwRenderer.TmnwElementRendererClass;
begin
  Check;
  AParams := TStringList.Create;
  try
    for o in Self do
    begin
      AElement := (o as TmnwElement);

      if (Context.Renderer <> nil) then
      begin
        RendererClass := Context.Renderer.ObjectClasses.FindRenderer(TmnwObjectClass(AElement.ClassType));
        if RendererClass <> nil then
        begin
          Renderer := RendererClass.Create;
          try
            Renderer.Render(AElement, Context, 0);
          finally
            Renderer.Free;
          end;
        end;
      end;
    end;

  finally
    FreeAndNil(AParams);
  end;
  Result := True;
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
