unit mnwDocuments;
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
  Document schema: a schema whose page body is a file on the disk.

  It is created by TmnwFileHandler (mnwFileHandlers.pas) when a request for a registrable
  file arrives, eg GET /notes/README.md

  Lifetime:
    * one instance is cached per (schema class, folder) by TmnwWeb.AcquireDocumentSchema
    * the requested file is passed by the context (Ctx.DocumentFile) or by the
      DocumentFile property, so the cached schema itself stays unchanged
    * the composed tree is cached and rebuilt only when the file changes
      (size/time stamp), see RebuildIfChanged

  Rendering:
    * DoRespond -> ServeFile (the app shell html/css/js from schemaGetPublicPath)
    * DoCompose -> Composition (the visible page)
}

interface

uses
  SysUtils, Classes, syncobjs, StrUtils,
  mnUtils, mnClasses, mnDON, mnModules, mnWebModules, mnMIME,
  mnWebElements;

type
  { TmnwDocumentSchema }

  TmnwDocumentSchema = class(TmnwSchema)
  private
    FDocumentFile: string;
    FDocumentChanged: Int64;
    FDocumentLoaded: Boolean;
    FDocumentSize: Int64;
    FDocumentHandler: TObject;
    FDocumentLock: TCriticalSection;
    procedure SetDocumentFile(const Value: string);
    //* Created on first use; the web creates schemas through TmnwSchemaClass
    //* so a descendant constructor can not be relied on here
    function GetDocumentLock: TCriticalSection;
  protected
    //* Hook, call it before Compose to load/rebuild the body from the file
    procedure CheckDocument(const Ctx: TmnwContext); virtual;    //* Read the current content, override if you need an other encoding
    function LoadDocument: string; virtual;
    //* Build the visible page, called from DoCompose
    procedure ComposeDocument(const Ctx: TmnwContext); virtual;
    //* Escape text so it can be written as html content
    function TextToHTML(const S: string): string;
    procedure DoCompose(const Ctx: TmnwContext); override;
    procedure DoInit; override;
    procedure DoRespond(const Ctx: TmnwContext); override;
  public
    constructor Create(AWeb: TmnwWeb; AName: string; ARoute: string = ''); reintroduce;
    destructor Destroy; override;    //* The name of the document, used as the page title
    function GetDocumentTitle: string; virtual;
    //* The url of the raw file, for the "source" link
    function GetDocumentSourceURL(const Ctx: TmnwContext): string; virtual;

    //* Render the page for a document
    procedure RenderDocument(const AFileName: string; const Ctx: TmnwContext); virtual;

    //* True if the file is loaded and still matches the disk
    function DocumentLoaded: Boolean;
    //* Forget the composed tree if the file changed on the disk
    procedure RebuildIfChanged; virtual;
    //* Drop the composed tree, the next request will rebuild it
    procedure ResetDocument; virtual;

    property DocumentFile: string read FDocumentFile write SetDocumentFile;
    property DocumentHandler: TObject read FDocumentHandler write FDocumentHandler;
    property DocumentSize: Int64 read FDocumentSize;
    property DocumentLock: TCriticalSection read GetDocumentLock;
  end;

const
  cDocumentLockTimeout = 10000; //* ms

implementation

uses
  mnStreams;

{ TmnwDocumentSchema }

//* The web creates his schemas through a class reference, so this constructor
//* is not always the one that runs; anything created here is created lazily
//* instead, see GetDocumentLock
constructor TmnwDocumentSchema.Create(AWeb: TmnwWeb; AName: string; ARoute: string);
begin
  inherited Create(AWeb, AName, ARoute);
  //* no listing and no default documents on a document schema
  ServeFiles := [serveEnabled, serveSmart];
end;

function TmnwDocumentSchema.GetDocumentLock: TCriticalSection;
begin
  if FDocumentLock = nil then
    FDocumentLock := TCriticalSection.Create;
  Result := FDocumentLock;
end;

procedure TmnwDocumentSchema.DoInit;
begin
  inherited;
  //* set here, not in Create: the web builds his schemas through the class
  //* reference, so a descendant constructor is bypassed
  ServeFiles := [serveEnabled, serveSmart];
end;

destructor TmnwDocumentSchema.Destroy;
begin
  FreeAndNil(FDocumentLock);
  inherited;
end;

procedure TmnwDocumentSchema.SetDocumentFile(const Value: string);
begin
  if FDocumentFile <> Value then
  begin
    FDocumentFile := Value;
    ResetDocument;
  end;
end;

//* The app shell (mnWebElements.css, libraries, logos) comes from the app,
//* so GetPublicPath is intentionally not overridden.

function TmnwDocumentSchema.DocumentLoaded: Boolean;
begin
  Result := FDocumentLoaded;
end;

procedure TmnwDocumentSchema.ResetDocument;
begin
  FDocumentLoaded := False;
  Clear;
end;

procedure TmnwDocumentSchema.RebuildIfChanged;
var
  aInfo: TFileInfo;
  aChanged: Int64;
begin
  if FDocumentFile = '' then
    Exit;
  aChanged := 0;
  if GetFileInfo(FDocumentFile, aInfo) then
  begin
    aChanged := Trunc(aInfo.TimeStamp) xor aInfo.Size;
    FDocumentSize := aInfo.Size;
  end;
  if (not FDocumentLoaded) or (FDocumentChanged <> aChanged) then
  begin
    FDocumentChanged := aChanged;
    Clear;
  end;
end;

procedure TmnwDocumentSchema.CheckDocument(const Ctx: TmnwContext);
begin
  RebuildIfChanged;
  FDocumentLoaded := True;
end;

function TmnwDocumentSchema.LoadDocument: string;
begin
  Result := '';
  if (FDocumentFile <> '') and FileExists(FDocumentFile) then
    Result := LoadFileString(FDocumentFile);
end;

function TmnwDocumentSchema.TextToHTML(const S: string): string;
begin
  Result := StringReplace(S, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;

function TmnwDocumentSchema.GetDocumentTitle: string;
begin
  if FDocumentFile <> '' then
    Result := ExtractFileName(FDocumentFile)
  else
    Result := Name;
end;

function TmnwDocumentSchema.GetDocumentSourceURL(const Ctx: TmnwContext): string;
begin
  Result := '';
  if FDocumentFile = '' then
    Exit;
  //* relative to the schema url, the file name itself
  Result := Ctx.GetPath(Self) + '/' + ExtractFileName(FDocumentFile) + '?raw=1';
end;

procedure TmnwDocumentSchema.ComposeDocument(const Ctx: TmnwContext);
begin
  //* Descendants draw the body here, the default is the plain source text
  Ctx.Writer.AddTag('pre', 'class="mnw-document-pre"', TextToHTML(LoadDocument));
end;

procedure TmnwDocumentSchema.DoCompose(const Ctx: TmnwContext);
begin
  inherited;
  ComposeDocument(Ctx);
end;

procedure TmnwDocumentSchema.DoRespond(const Ctx: TmnwContext);
begin
  //* A document schema always renders the page, it never streams the file
  Render(Ctx);
end;

procedure TmnwDocumentSchema.RenderDocument(const AFileName: string; const Ctx: TmnwContext);
var
  aLock: TCriticalSection;
begin
  aLock := GetDocumentLock;
  aLock.Enter;
  try
    if AFileName <> '' then
      DocumentFile := AFileName;
    CheckDocument(Ctx);
    //* Clear in CheckDocument drops estComposed, so Compose rebuilds the tree
    if not (estComposed in State) then
    begin
      Init;
      Compose(Ctx);
    end;
    Render(Ctx);
  finally
    aLock.Leave;
  end;
end;

end.
