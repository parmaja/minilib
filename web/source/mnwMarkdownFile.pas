unit mnwMarkdownFile;
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
  Markdown document handler

  GET /notes/README.md  -> renders the file as a styled html page
  GET /notes/README.md?raw=1 -> serves the file as text/markdown (the old behavior)

  Register it in the web app before Start:
    Web.RegisterFileHandler(TmnwMarkdownFileHandler);

  Rendering is done in the browser by markdown-it + DOMPurify (mnwMarkdownIt.pas),
  the source is embedded in a text/template script tag so it is never parsed as html.
}

interface

uses
  SysUtils, Classes, StrUtils,
  mnUtils, mnClasses, mnModules, mnMIME,
  mnWebElements, mnwFileHandlers, mnwDocuments;

type
  { TmnwMarkdownDocumentSchema }

  TmnwMarkdownDocumentSchema = class(TmnwDocumentSchema)
  protected
    procedure ComposeDocument(const Ctx: TmnwContext); override;
  public
    class function GetCapabilities: TmnwSchemaCapabilities; override;
  end;

  { TmnwMarkdownFileHandler }

  TmnwMarkdownFileHandler = class(TmnwFileHandler)
  protected
    function Accepts(const Ctx: TmnwContext; const AFileName: string): Boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses
  mnDON,
  mnwMarkdownIt;

const
  cMarkdownExtensions: array [0 .. 4] of string = ('md', 'markdown', 'mdown', 'mkd', 'mkdn');

{ TmnwMarkdownDocumentSchema }

class function TmnwMarkdownDocumentSchema.GetCapabilities: TmnwSchemaCapabilities;
begin
  //* not schemaStatic: the schema is owned and cached by TmnwWeb.FDocumentSchemas
  Result := [];
end;

procedure TmnwMarkdownDocumentSchema.ComposeDocument(const Ctx: TmnwContext);
var
  aID: string;
  aSource: string;
begin
  aID := 'mnw_markdown_doc';

  Ctx.Writer.OpenTag('div', 'class="mnw-document"');
  Ctx.Writer.OpenTag('div', 'class="mnw-document-header d-flex align-items-center gap-2 mb-3"');
  Ctx.Writer.AddTag('h1', 'class="h4 mb-0"', TextToHTML(ExtractFileName(DocumentFile)));
  Ctx.Writer.OpenInlineTag('div', 'class="ms-auto"');
  Ctx.Writer.AddInlineTag('a', 'class="btn btn-sm btn-outline-secondary" href="?raw=1"',
    Ctx._T('source', 'Source'));
  Ctx.Writer.CloseTag('div');
  Ctx.Writer.CloseTag('div');

  aSource := LoadDocument;
  if Trim(aSource) = '' then
    Ctx.Writer.AddTag('p', 'class="text-muted"', Ctx._T('empty_document', 'This document is empty.'))
  else
  begin
    //* markdown-it needs its libraries in the page head
    MarkdownItRequire(Ctx);
    MarkdownItWrite(Ctx, aID, aSource, [mioLinkify, mioSanitize, mioTaskLists, mioHighlight]);
  end;

  Ctx.Writer.CloseTag('div');
end;

{ TmnwMarkdownFileHandler }

constructor TmnwMarkdownFileHandler.Create;
begin
  inherited Create;
  Name := 'markdown';
  AddExtension(cMarkdownExtensions);
  ContentType := 'text/markdown';
  SchemaClass := TmnwMarkdownDocumentSchema;
end;

function TmnwMarkdownFileHandler.Accepts(const Ctx: TmnwContext; const AFileName: string): Boolean;
begin
  Result := inherited Accepts(Ctx, AFileName);
end;

end.
