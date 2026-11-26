unit mnwFileHandlers;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}
{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$endif}

{
  File handlers registry

  Terminology note:
    TElementExtension (TID_Extension, TName_Extension, TRoute_Extension) is an RTTI
    attribute used when an element class is registered, it has nothing to do with the
    file extension of a requested document. THIS unit is about file extensions:
    'md' -> a handler that knows how to present a *.md file to the browser.

  Flow:
    TmnwElement.ServeFile resolves a real file on the disk, then asks
    Web.FileHandlers to handle it. If no handler is registered for the extension, or the
    handler declines, the file is streamed as it was before (100% backwards compatible).

  A handler normally turns the request into a schema that renders the document:
    '.md' -> TmnwMarkdownHandler -> TmnwMarkdownDocumentSchema -> Render

  Registering (before Web.Start, same rule as RegisterRenderer):
    Web.RegisterFileHandler(TmnwMarkdownHandlerClass);
}

interface

uses
  SysUtils, Classes, syncobjs, StrUtils,
  mnUtils, mnClasses, mnModules, mnWebModules,
  mnWebElements;

type
  TmnwFileHandler = class;
  TmnwFileHandlerClass = class of TmnwFileHandler;
  TmnwFileHandlers = class;

  { TmnwFileHandler }

  TmnwFileHandler = class(TmnwObject)
  private
    FExtensions: TStringList;
    FContentType: string;
    FSchemaClass: TmnwSchemaClass;
    FMaxSize: Int64;
    function GetExtensions: string;
  protected
    //* Return False to not handle this request, then the file will be served as it is
    function Accepts(const Ctx: TmnwContext; const AFileName: string): Boolean; virtual;
    //* Override this one, the default implementation renders it by a document schema
    function DoHandle(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean; virtual;
    //* Return false to not cache the schema, it will be freed after the request
    function SharedSchema: Boolean; virtual;
    property ExtensionsList: TStringList read FExtensions;
    property MaxSizeInternal: Int64 read FMaxSize write FMaxSize;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //* Security gate: dot files, missing files, anything your app wants to hide
    function Visible(const AFileName: string): Boolean; virtual;
    //* Add one extension, with or without the dot, case insensitive
    procedure AddExtension(const AExt: string); overload;
    procedure AddExtension(const Extensions: array of string); overload;
    //* Normalized extension without the dot and lower cased
    function CanHandle(const AFileName: string): Boolean;
    //* Return True if the handler took the responsibility of the response
    function Handle(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean;
    //* Content type of the original document, used if you want to serve it as raw
    property ContentType: string read FContentType write FContentType;
    //* Schema class used to render the document, it should be a TmnwDocumentSchema
    property SchemaClass: TmnwSchemaClass read FSchemaClass write FSchemaClass;
    //* Comma separated extensions, for logging
    property Extensions: string read GetExtensions;
    //* Max file size to render, bigger documents are served as files, 0 = no limit
    property MaxSize: Int64 read FMaxSize write FMaxSize;
  end;

  { TmnwFileHandlers }

  TmnwFileHandlers = class(TmnwFileHandlersBase)
  private
    FItems: TList;
    FIndex: TStringList; //* ext -> TmnwFileHandler, sorted, dupIgnore so first added wins
    FLock: TCriticalSection;
    FEnabled: Boolean;
    FMaxSize: Int64;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): TmnwFileHandler;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Add(AHandler: TmnwFileHandler): TmnwFileHandler; overload;
    function Add(AHandlerClass: TmnwFileHandlerClass): TmnwFileHandler; overload;
    //* Find by file name, nil if nothing is registered for its extension
    function Find(const AFileName: string): TmnwFileHandler; overload;
    //* Find by extension, with or without the dot, case insensitive
    function FindByExtension(const AExtension: string): TmnwFileHandler; overload;
    //* The main entry point, called from TmnwElement.ServeFile
    function ServeDocument(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean; override;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TmnwFileHandler read GetItems; default;
    //* Turn it off to keep the old behavior of serving files as they are
    property Enabled: Boolean read FEnabled write FEnabled;
    //* Default max size for handlers that did not set their own
    property MaxSize: Int64 read FMaxSize write FMaxSize;
  end;

  //* Get the registry of a web app, create it if this is the first time
  function WebGetFileHandlers(AWeb: TmnwWeb): TmnwFileHandlers;
  //* Register a handler, must be called before AWeb.Start
  function WebRegisterFileHandler(AWeb: TmnwWeb; AHandler: TmnwFileHandler): TmnwFileHandler; overload;
  function WebRegisterFileHandler(AWeb: TmnwWeb; AHandlerClass: TmnwFileHandlerClass): TmnwFileHandler; overload;

implementation

uses
  mnwDocuments;

const
  cDefaultMaxSize = 2 * 1024 * 1024; //* 2 MB

{ TmnwFileHandler }

constructor TmnwFileHandler.Create;
begin
  inherited Create;
  FExtensions := TStringList.Create;
  FExtensions.CaseSensitive := False;
  FExtensions.Sorted := True;
  FExtensions.Duplicates := dupIgnore;
  FMaxSize := 0; //* use the registry default
end;

destructor TmnwFileHandler.Destroy;
begin
  FreeAndNil(FExtensions);
  inherited;
end;

procedure TmnwFileHandler.AddExtension(const AExt: string);
var
  aValue: string;
begin
  aValue := Trim(LowerCase(AExt));
  while StartsStr('.', aValue) do
    aValue := Copy(aValue, 2, MaxInt);
  if aValue <> '' then
    FExtensions.Add(aValue); //* sorted, dupIgnore
end;

procedure TmnwFileHandler.AddExtension(const Extensions: array of string);
var
  i: Integer;
begin
  for i := Low(Extensions) to High(Extensions) do
    AddExtension(Extensions[i]);
end;

function TmnwFileHandler.GetExtensions: string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FExtensions.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + FExtensions[i];
  end;
end;

function TmnwFileHandler.CanHandle(const AFileName: string): Boolean;
var
  aExt: string;
begin
  aExt := Trim(LowerCase(ExtractFileExt(AFileName)));
  while StartsStr('.', aExt) do
    aExt := Copy(aExt, 2, MaxInt);
  Result := (aExt <> '') and (FExtensions.IndexOf(aExt) >= 0);
end;

function TmnwFileHandler.Visible(const AFileName: string): Boolean;
begin
  //* No file starts with a dot, same rule as TmnwElement.ServeFile
  Result := (AFileName <> '') and not StartsText('.', ExtractFileName(AFileName));
end;

function TmnwFileHandler.Accepts(const Ctx: TmnwContext; const AFileName: string): Boolean;
var
  aInfo: TFileInfo;
  aRaw: string;
begin
  Result := False;
  if not Visible(AFileName) then
    Exit;
  if not GetFileInfo(AFileName, aInfo) then
    Exit;

  //* A raw request from our own viewer, or an API consumer that wants the source
  aRaw := Ctx.Request.Params['raw'];
  if IsStrInArray(aRaw, ['1', 'true', 'yes'], True) then
    Exit;

  //* Do not render huge documents, stream them instead
  if (MaxSize > 0) and (aInfo.Size > MaxSize) then
    Exit;

  Result := True;
end;

function TmnwFileHandler.SharedSchema: Boolean;
begin
  Result := True;
end;

function TmnwFileHandler.DoHandle(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean;
var
  aSchema: TmnwDocumentSchema;
  aIsNew: Boolean;
begin
  Result := False;
  if (FSchemaClass = nil) or (Ctx.Web = nil) then
    Exit;

  //* a shared schema is cached per folder by the web, not created per request,
  //* the requested file is handed to it for this render only
  aSchema := Ctx.Web.AcquireDocumentSchema(FSchemaClass, ExtractFileDir(AFileName),
    SharedSchema, aIsNew) as TmnwDocumentSchema;
  if aSchema = nil then
    Exit;
  try
    aSchema.DocumentHandler := Self;
    aSchema.DocumentFile := AFileName;
    aSchema.RenderDocument(AFileName, Ctx);
    Result := True;
  finally
    //* AIsNew is True for a schema that is not cached (not shared), we own it
    if aIsNew and (not SharedSchema) then
      aSchema.Free;
  end;
end;

function TmnwFileHandler.Handle(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean;
begin
  Result := False;
  if not Accepts(Ctx, AFileName) then
    Exit;
  Result := DoHandle(Ctx, AFileName, Options);
end;

{ TmnwFileHandlers }

constructor TmnwFileHandlers.Create;
begin
  inherited Create;
  FItems := TList.Create;
  FIndex := TStringList.Create;
  FIndex.CaseSensitive := False;
  FIndex.Sorted := True;
  FIndex.Duplicates := dupIgnore;
  FLock := TCriticalSection.Create;
  FEnabled := True;
  FMaxSize := cDefaultMaxSize;
end;

destructor TmnwFileHandlers.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  FreeAndNil(FIndex);
  FreeAndNil(FLock);
  inherited;
end;

procedure TmnwFileHandlers.Clear;
var
  i: Integer;
begin
  if FItems = nil then
    Exit;
  FLock.Enter;
  try
    for i := 0 to FItems.Count - 1 do
      TmnwFileHandler(FItems[i]).Free;
    FItems.Clear;
    FIndex.Clear;
  finally
    FLock.Leave;
  end;
end;

function TmnwFileHandlers.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TmnwFileHandlers.GetItems(AIndex: Integer): TmnwFileHandler;
begin
  Result := TmnwFileHandler(FItems[AIndex]);
end;

function TmnwFileHandlers.Add(AHandler: TmnwFileHandler): TmnwFileHandler;
var
  i: Integer;
  aExt: string;
begin
  Result := AHandler;
  if AHandler.MaxSize = 0 then
    AHandler.MaxSizeInternal := FMaxSize;
  FLock.Enter;
  try
    FItems.Add(AHandler);
    for i := 0 to AHandler.ExtensionsList.Count - 1 do
    begin
      aExt := AHandler.ExtensionsList[i];
      //* FIndex is dupIgnore, first handler registered wins
      if FIndex.IndexOf(aExt) < 0 then
        FIndex.AddObject(aExt, AHandler);
    end;
  finally
    FLock.Leave;
  end;
end;

function TmnwFileHandlers.Add(AHandlerClass: TmnwFileHandlerClass): TmnwFileHandler;
begin
  Result := Add(AHandlerClass.Create);
end;

function TmnwFileHandlers.FindByExtension(const AExtension: string): TmnwFileHandler;
var
  aExt: string;
  i: Integer;
begin
  Result := nil;
  aExt := Trim(LowerCase(AExtension));
  while StartsStr('.', aExt) do
    aExt := Copy(aExt, 2, MaxInt);
  if aExt = '' then
    Exit;
  FLock.Enter;
  try
    i := FIndex.IndexOf(aExt);
    if i >= 0 then
      Result := TmnwFileHandler(FIndex.Objects[i]);
  finally
    FLock.Leave;
  end;
end;

function TmnwFileHandlers.Find(const AFileName: string): TmnwFileHandler;
begin
  Result := FindByExtension(ExtractFileExt(AFileName));
end;

function TmnwFileHandlers.ServeDocument(const Ctx: TmnwContext; const AFileName: string; Options: TmodServeFiles): Boolean;
var
  aHandler: TmnwFileHandler;
begin
  Result := False;
  if not FEnabled then
    Exit;
  aHandler := Find(AFileName);
  if aHandler = nil then
    Exit;
  Result := aHandler.Handle(Ctx, AFileName, Options);
end;

{ registration }

function WebGetFileHandlers(AWeb: TmnwWeb): TmnwFileHandlers;
begin
  Result := nil;
  if AWeb = nil then
    Exit;
  if not (AWeb.FileHandlers is TmnwFileHandlers) then
    AWeb.FileHandlers := TmnwFileHandlers.Create;
  Result := TmnwFileHandlers(AWeb.FileHandlers);
end;

function WebRegisterFileHandler(AWeb: TmnwWeb; AHandler: TmnwFileHandler): TmnwFileHandler;
begin
  if AWeb = nil then
    raise Exception.Create('Web is required to register a file handler');
  if AWeb.Started then
    raise Exception.Create('Once web app started you can''t register file handler');
  Result := WebGetFileHandlers(AWeb).Add(AHandler);
end;

function WebRegisterFileHandler(AWeb: TmnwWeb; AHandlerClass: TmnwFileHandlerClass): TmnwFileHandler;
begin
  Result := WebRegisterFileHandler(AWeb, AHandlerClass.Create);
end;

end.
