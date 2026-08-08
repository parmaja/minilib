unit mnwMarkdownIt;
{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$endif}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{
  Markdown viewer control using markdown-it.

  Renders markdown text as styled HTML in the browser using:
    - markdown-it   https://github.com/markdown-it/markdown-it
    - DOMPurify     https://github.com/cure53/DOMPurify
    - github-markdown-css
    - highlight.js  https://highlightjs.org/  (optional)
    - markdown-it-task-lists (optional)

  Usage:
    with TMarkdownIt.Create(Document.Body.Main) do
    begin
      ID := 'myMD';
      Source := '# Hello World'#13#10'This is **markdown** text.';
    end;
}

interface

uses
  SysUtils, Classes, StrUtils,
  mnTypes, mnUtils, mnClasses, mnFields, mnServers, mnDON, mnLogs, mnConfigs, mnParams,
  mnModules, mnWebElements, mnBootstraps;

type

  { TMarkdownItOption / TMarkdownItOptions }

  TMarkdownItOption = (
    mioHTML,          //* Enable HTML tags in source
    mioXHTMLOut,      //* Use '/' to close single tags (<br />)
    mioBreaks,        //* Convert '\n' in paragraphs into <br>
    mioLinkify,       //* Autoconvert URL-like text to links
    mioTypographer,   //* Enable smartypants and other sweet transforms
    mioSanitize,      //* Sanitize rendered HTML with DOMPurify (XSS protection)
    mioTaskLists,     //* Enable task list checkbox rendering with markdown-it-task-lists plugin
    mioHighlight      //* Enable syntax highlighting for code blocks with highlight.js
  );
  TMarkdownItOptions = set of TMarkdownItOption;

  { TMarkdownIt }

  [TID_Extension]
  TMarkdownIt = class(THTML.THTMLControl)
  private
    FSource: string;
    FOptions: TMarkdownItOptions;
  protected
    procedure Created; override;
    procedure DoRequired(const Context: TmnwContext); override;
  public
    //* The markdown source text to render
    property Source: string read FSource write FSource;
    //* Rendering options
    property Options: TMarkdownItOptions read FOptions write FOptions;
  end;

  { TMarkdownItRenderer }

  TMarkdownItRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; const Context: TmnwContext); override;
  end;

  { TMarkdownIt_Library }

  TMarkdownIt_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

  THighlightJS_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;
  
implementation

{ TMarkdownIt }

procedure TMarkdownIt.Created;
begin
  inherited;
  FOptions := [mioHTML, mioLinkify, mioSanitize];
end;

procedure TMarkdownIt.DoRequired(const Context: TmnwContext);
begin
  inherited;
  Context.Require(TBootstrap_Library);
  Context.Require(TBootstrapIcons_Library);
  Context.Require(TMarkdownIt_Library);
  Context.Require(THighlightJS_Library);
end;

{ TMarkdownItRenderer }

procedure TMarkdownItRenderer.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('markdown-body');
  //Scope.Classes.Add('p-1');
end;

procedure TMarkdownItRenderer.DoInnerRender(Scope: TmnwScope; const Context: TmnwContext);
var
  e: TMarkdownIt;
  srcID: string;
  src: string;
  aOptions: string;
begin
  inherited;
  e := Scope.Element as TMarkdownIt;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  srcID := e.ID + '_src';

  // Output container - markdown-body class is added by DoCollectAttributes
  Context.Writer.OpenTag('div', Scope.ToString(True));
  Context.Writer.CloseTag('div');

  // Store source text in a text/template script to avoid HTML escaping issues.
  src := StringReplace(e.Source, '</script>', '<\/script>', [rfReplaceAll]);
  Context.Writer.OpenTag('script', 'type="text/template" id=' + DQ(srcID));
  Context.Writer.Write(src);
  Context.Writer.CloseTag('script');

  // Build markdown-it options as a JavaScript object
  aOptions := '{';
  aOptions := aOptions + '"html": ' + When(mioHTML in e.Options, 'true', 'false') + ', ';
  aOptions := aOptions + '"xhtmlOut": ' + When(mioXHTMLOut in e.Options, 'true', 'false') + ', ';
  aOptions := aOptions + '"breaks": ' + When(mioBreaks in e.Options, 'true', 'false') + ', ';
  aOptions := aOptions + '"linkify": ' + When(mioLinkify in e.Options, 'true', 'false') + ', ';
  aOptions := aOptions + '"typographer": ' + When(mioTypographer in e.Options, 'true', 'false');
  aOptions := aOptions + '}';

  // Renderer script: on DOMContentLoaded, parse markdown and inject into the container.
  Context.Writer.OpenTag('script');
  Context.Writer.WriteLn('document.addEventListener("DOMContentLoaded", function(){');
  Context.Writer.WriteLn('  var srcEl = document.getElementById(' + DQ(srcID) + ');');
  Context.Writer.WriteLn('  var outEl = document.getElementById(' + DQ(e.ID) + ');');
  Context.Writer.WriteLn('  if (srcEl && outEl) {');
  Context.Writer.WriteLn('    var mdOptions = ' + aOptions + ';');
  if mioHighlight in e.Options then
  begin
    Context.Writer.WriteLn('    mdOptions.highlight = function(str, lang) {');
    Context.Writer.WriteLn('      if (lang && hljs.getLanguage(lang)) {');
    Context.Writer.WriteLn('        try {');
    Context.Writer.WriteLn('          return ''<pre><code class="hljs language-'' + lang + ''">'' + hljs.highlight(str, {language: lang, ignoreIllegals: true}).value + ''</code></pre>'';');
    Context.Writer.WriteLn('        } catch (e) {}');
    Context.Writer.WriteLn('      }');
    Context.Writer.WriteLn('      return ''<pre><code class="hljs">'' + str.replace(/&/g, ''&amp;'').replace(/</g, ''&lt;'').replace(/>/g, ''&gt;'') + ''</code></pre>'';');
    Context.Writer.WriteLn('    };');
  end;
  Context.Writer.WriteLn('    var md = window.markdownit(mdOptions);');
  if mioTaskLists in e.Options then
    Context.Writer.WriteLn('    md.use(window.markdownitTaskLists);');
  Context.Writer.WriteLn('    var html = md.render(srcEl.textContent || srcEl.text);');
  if mioSanitize in e.Options then
    Context.Writer.WriteLn('    html = DOMPurify.sanitize(html);');
  Context.Writer.WriteLn('    outEl.innerHTML = html;');
  Context.Writer.WriteLn('  }');
  Context.Writer.WriteLn('});');
  Context.Writer.CloseTag('script');
end;

{ TMarkdownIt_Library }

procedure TMarkdownIt_Library.Created;
begin
  inherited;
  // GitHub markdown CSS - auto-themed (responds to data-theme attribute and prefers-color-scheme)
  //Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/github-markdown-css@5.9.0/', 'github-markdown.min.css');
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/github-markdown-css@5.9.0/', 'github-markdown-dark-dimmed.css');
  // markdown-it - fast markdown parser
  Sources.Add(stScript, 'https://cdn.jsdelivr.net/npm/markdown-it@14.3.0/dist/', 'markdown-it.min.js');
  // DOMPurify - HTML sanitizer for XSS prevention
  Sources.Add(stScript, 'https://cdnjs.cloudflare.com/ajax/libs/dompurify/3.2.4/', 'purify.min.js');
  // markdown-it-task-lists - checkbox task list support
  Sources.Add(stScript, 'https://cdn.jsdelivr.net/npm/markdown-it-task-lists@2.1.1/dist/', 'markdown-it-task-lists.min.js');
end;

{ THighlightJS_Library }

procedure THighlightJS_Library.Created;
begin
  inherited;
  // highlight.js - syntax highlighting for code blocks
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/highlight.js@11.11.1/styles/', 'github-dark-dimmed.min.css');
  Sources.Add(stScript, 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/', 'highlight.min.js');
end;

initialization
  Libraries.RegisterLibrary(TMarkdownIt_Library);
  Libraries.RegisterLibrary(THighlightJS_Library);
  TBSRenderer.RegisterRenderer(TMarkdownIt, TMarkdownItRenderer);
end.
