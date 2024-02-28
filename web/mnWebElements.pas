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
  mnClasses, mnStreams;

type

  TmnwSchema = class;
  TmnwRenderer = class;
  TmnwElement = class;
  TmnwWriter = class;
  TmnwRendererClass = class of TmnwRenderer;

  TmnwElementClass = class of TmnwElement;

  TmnwContext = record
    Renderer: TmnwRenderer;
    Writer: TmnwWriter;
  end;

  { TmnwElement }

  TmnwElement = class(TmnObjectList<TmnwElement>)
  private
    FComment: String;
    FName: String;
    FParent: TmnwElement;
    FRoot: TmnwSchema;
    FTags: String;
  protected
    procedure Update; virtual;
    procedure Added(Item: TmnwElement); override;
    procedure Check; virtual;
    function FindObject(ObjectClass: TmnwElementClass; AName: string; RaiseException: Boolean = false): TmnwElement;
    procedure DoCompose; virtual;
  public
    constructor Create; virtual;
    function Add<O: TmnwElement>(const AName: String = ''): O; overload;
    function Find(const Name: string): TmnwElement;
    function IndexOfName(vName: string): Integer;

    function This: TmnwElement; //I wish i have templates/meta programming in pascal
    property Root: TmnwSchema read FRoot;
    property Parent: TmnwElement read FParent;

    procedure Render(Context: TmnwContext; vLevel: Integer);
    procedure Compose;

    property Name: String read FName write FName;
    property Tags: String read FTags write FTags; //etc: 'Key,Data'
    property Comment: String read FComment write FComment;
  end;

  { TmnwWriter }

  TmnwWriterOptions = set of (cboEndLine, cboEndChunk, cboMore);

  TmnwWriter = class(TObject)
  private
    FName: string;
    FStream: TStream;
  public
    constructor Create(AName: string; AStream: TStream);
    procedure Write(const S: string; Options: TmnwWriterOptions = []);
    property Stream: TStream read FStream write FStream;
    property Name: string read FName write FName;
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

    function Render(Context: TmnwContext): Boolean; overload;
    function Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean; overload;
  end;

  TmnwRenderer = class(TmnwElement)
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
        procedure Added(Item: TmnwElement); override;
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

{ TmnwRenderer.TmnwElementRenderer }

procedure TmnwRenderer.TmnwElementRenderer.DefaultRender(AElement: TmnwElement; Context: TmnwContext; vLevel: Integer);
var
  o: TmnwElement;
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

procedure TmnwElement.Render(Context: TmnwContext; vLevel: Integer);
var
  Renderer: TmnwRenderer.TmnwElementRenderer;
  RendererClass: TmnwRenderer.TmnwElementRendererClass;
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

constructor TmnwSchema.Create;
begin
  inherited Create;
  FRoot := Self;
  Compose;
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

procedure TmnwHTML.TContainer.Added(Item: TmnwElement);
begin
  inherited Added(Item);
end;

{ TmnwElement }

function TmnwElement.This: TmnwElement;
begin
  Result := Self;
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
  FName := ClassName();
end;

function TmnwElement.Add<O>(const AName: String): O;
begin
  Result := O.Create;
  Result.FName := AName;
  if Result.FName = '' then
    Result.FName := ClassName;
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

function TmnwSchema.Render(RendererClass: TmnwRendererClass; AStrings: TStrings): Boolean;
var
  Context: TmnwContext;
  AStringStream: TStringStream;
begin
  AStringStream := TStringStream.Create();
  Context.Writer := TmnwWriter.Create('', AStringStream);
  Context.Renderer := RendererClass.Create;
  try
    Render(Context);
    AStrings.Text := AStringStream.DataString;
  finally
    FreeAndNil(Context.Writer);
    FreeAndNil(Context.Renderer);
    FreeAndNil(AStringStream);
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
end;

constructor TmnwWriter.Create(AName: string; AStream: TStream);
begin
  inherited Create;
  FName := AName;
  FStream := AStream;
end;

procedure TmnwWriter.Write(const S: string; Options: TmnwWriterOptions);
begin
  if (cboEndLine in Options) then
    FStream.WriteString(S + sEndOfLine)
  else
    FStream.WriteString(S)
end;

function TmnwSchema.Render(Context: TmnwContext): Boolean;
var
  AParams: TStringList;
  o: TmnwElement;
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
        RendererClass := Context.Renderer.ObjectClasses.FindRenderer(TmnwElementClass(AElement.ClassType));
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
