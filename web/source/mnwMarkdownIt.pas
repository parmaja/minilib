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
  For more markdowns

  *  https://markdown.org/developers/javascript/

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
    procedure DoRequired(const Ctx: TmnwContext); override;
  public
    //* The markdown source text to render
    property Source: string read FSource write FSource;
    //* Rendering options
    property Options: TMarkdownItOptions read FOptions write FOptions;
  end;

  { TMarkdownItRenderer }

  TMarkdownItRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
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

  //* Require the libraries used by markdown blocks (markdown-it, DOMPurify,
  //* github-markdown-css and highlight.js). Call it while composing the page.
  procedure MarkdownItRequire(const Ctx: TmnwContext);

  //* Write one markdown block into the raw writer output (container, source
  //* template and renderer script). The page must call MarkdownItRequire while
  //* composing so the libraries are added to the page head.
  procedure MarkdownItWrite(const Ctx: TmnwContext; const AID, ASource: string;
    AOptions: TMarkdownItOptions; const AClasses: string = ''); overload;
  procedure MarkdownItWrite(const Ctx: TmnwContext; const AID, ASource: string); overload;

implementation

{ Markdown blocks for raw writer output }

function MarkdownItOptionsJSON(const AOptions: TMarkdownItOptions): string;
begin
  Result := '{';
  Result := Result + '"html": ' + When(mioHTML in AOptions, 'true', 'false') + ', ';
  Result := Result + '"xhtmlOut": ' + When(mioXHTMLOut in AOptions, 'true', 'false') + ', ';
  Result := Result + '"breaks": ' + When(mioBreaks in AOptions, 'true', 'false') + ', ';
  Result := Result + '"linkify": ' + When(mioLinkify in AOptions, 'true', 'false') + ', ';
  Result := Result + '"typographer": ' + When(mioTypographer in AOptions, 'true', 'false');
  Result := Result + '}';
end;

//Store the markdown source in a text/template script so the writer does not
//have to HTML-escape it. `</script` is escaped so the template can not be
//closed by the note text itself.
procedure MarkdownItWriteSource(const Ctx: TmnwContext; const AID, ASource: string);
begin
  //OpenInlineTag keeps the writer from indenting the first source line; an
  //indented first line would be parsed by markdown as a code block.
  Ctx.Writer.OpenInlineTag('script', 'type="text/template" id=' + DQ(AID + '_src'));
  Ctx.Writer.Write(StringReplace(ASource, '</script', '<\/script', [rfReplaceAll, rfIgnoreCase]));
  Ctx.Writer.CloseTag('script');
end;

procedure MarkdownItWriteScript(const Ctx: TmnwContext; const AID: string;
  const AOptions: TMarkdownItOptions);
var
  srcID, OptionsJSON: string;
begin
  srcID := AID + '_src';
  OptionsJSON := MarkdownItOptionsJSON(AOptions);

  Ctx.Writer.OpenTag('script');
  Ctx.Writer.WriteLn('(function(){');
  Ctx.Writer.WriteLn('  var outEl = document.getElementById(' + DQ(AID) + ');');
  Ctx.Writer.WriteLn('  if (!outEl) return;');
  Ctx.Writer.WriteLn('  function render(){');
  Ctx.Writer.WriteLn('    var srcEl = document.getElementById(' + DQ(srcID) + ');');
  Ctx.Writer.WriteLn('    if (!srcEl) return;');
  Ctx.Writer.WriteLn('    if (!window.markdownit) { outEl.textContent = srcEl.textContent || srcEl.text; return; }');
  Ctx.Writer.WriteLn('    var mdOptions = ' + OptionsJSON + ';');
  if mioHighlight in AOptions then
  begin
    Ctx.Writer.WriteLn('    mdOptions.highlight = function(str, lang) {');
    //The fence info is user text; keep only language-name characters before
    //it is placed in the class attribute and passed to highlight.js.
    Ctx.Writer.WriteLn('      lang = String(lang || "").replace(/[^A-Za-z0-9_+#.-]/g, "");');
    Ctx.Writer.WriteLn('      if (window.hljs && lang && hljs.getLanguage(lang)) {');
    Ctx.Writer.WriteLn('        try {');
    Ctx.Writer.WriteLn('          return ''<pre><code class="hljs language-'' + lang + ''">'' + hljs.highlight(str, {language: lang, ignoreIllegals: true}).value + ''</code></pre>'';');
    Ctx.Writer.WriteLn('        } catch (e) {}');
    Ctx.Writer.WriteLn('      }');
    Ctx.Writer.WriteLn('      return ''<pre><code class="hljs">'' + str.replace(/&/g, ''&amp;'').replace(/</g, ''&lt;'').replace(/>/g, ''&gt;'') + ''</code></pre>'';');
    Ctx.Writer.WriteLn('    };');
  end;
  Ctx.Writer.WriteLn('    var md = window.markdownit(mdOptions);');
  if mioTaskLists in AOptions then
    Ctx.Writer.WriteLn('    if (window.markdownitTaskLists) md.use(window.markdownitTaskLists);');
  Ctx.Writer.WriteLn('    var html = md.render(srcEl.textContent || srcEl.text);');
  if mioSanitize in AOptions then
  begin
    Ctx.Writer.WriteLn('    if (window.DOMPurify) html = DOMPurify.sanitize(html);');
    //Fail closed: never inject unsanitized HTML when DOMPurify did not load.
    Ctx.Writer.WriteLn('    else { outEl.textContent = srcEl.textContent || srcEl.text; return; }');
  end;
  Ctx.Writer.WriteLn('    outEl.innerHTML = html;');
  Ctx.Writer.WriteLn('  }');
  Ctx.Writer.WriteLn('  if (document.readyState === "loading")');
  Ctx.Writer.WriteLn('    document.addEventListener("DOMContentLoaded", render);');
  Ctx.Writer.WriteLn('  else');
  Ctx.Writer.WriteLn('    render();');
  Ctx.Writer.WriteLn('})();');
  Ctx.Writer.CloseTag('script');
end;

procedure MarkdownItRequire(const Ctx: TmnwContext);
begin
  Ctx.Require(TBootstrap_Library);
  Ctx.Require(TBootstrapIcons_Library);
  Ctx.Require(TMarkdownIt_Library);
  Ctx.Require(THighlightJS_Library);
end;

procedure MarkdownItWrite(const Ctx: TmnwContext; const AID, ASource: string;
  AOptions: TMarkdownItOptions; const AClasses: string);
var
  Cls: string;
begin
  if AID = '' then
    raise Exception.Create('ID is required for markdown block');
  Cls := 'markdown-body';
  if AClasses <> '' then
    Cls := Cls + ' ' + AClasses;
  Ctx.Writer.OpenTagA('div', Cls, 'id=' + DQ(AID));
  Ctx.Writer.CloseTag('div');
  MarkdownItWriteSource(Ctx, AID, ASource);
  MarkdownItWriteScript(Ctx, AID, AOptions);
end;

procedure MarkdownItWrite(const Ctx: TmnwContext; const AID, ASource: string);
begin
  MarkdownItWrite(Ctx, AID, ASource, [mioHTML, mioLinkify, mioSanitize]);
end;

{ TMarkdownIt }

procedure TMarkdownIt.Created;
begin
  inherited;
  FOptions := [mioHTML, mioLinkify, mioSanitize];
end;

procedure TMarkdownIt.DoRequired(const Ctx: TmnwContext);
begin
  inherited;
  MarkdownItRequire(Ctx);
end;

{ TMarkdownItRenderer }

procedure TMarkdownItRenderer.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('markdown-body');
  //Scope.Classes.Add('p-1');
end;

procedure TMarkdownItRenderer.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: TMarkdownIt;
begin
  inherited;
  e := Scope.Element as TMarkdownIt;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  // Output container - markdown-body class is added by DoCollectAttributes
  Ctx.Writer.OpenTag('div', Scope.ToString(True));
  Ctx.Writer.CloseTag('div');

  MarkdownItWriteSource(Ctx, e.ID, e.Source);
  MarkdownItWriteScript(Ctx, e.ID, e.Options);
end;

{ TMarkdownIt_Library }

//Enable the light or dark stylesheet matching the application theme. The app
//stores the selected theme in `mnw-theme` and mirrors it on body[data-theme].
const
  MarkdownThemeScript =
    '''
    (function(){
      function apply(){
        var theme = 'light';
        try { theme = localStorage.getItem('mnw-theme') || 'light'; } catch (e) {}
        var bodyTheme = document.body && (document.body.getAttribute('data-theme') ||
          document.body.getAttribute('data-bs-theme'));
        if (bodyTheme)
          theme = bodyTheme;
        var links = document.querySelectorAll('link[rel="stylesheet"]');
        for (var i = 0; i < links.length; i++) {
          var href = links[i].getAttribute('href') || '';
          if (href.indexOf('github-markdown-light') >= 0)
            links[i].disabled = (theme !== 'light');
          else if (href.indexOf('github-markdown-dark') >= 0)
            links[i].disabled = (theme !== 'dark');
          else if (href.indexOf('github-dark-dimmed') >= 0)
            links[i].disabled = (theme !== 'dark');
          else if (href.indexOf('/github.min.css') >= 0 || href.indexOf('/github.css') >= 0)
            links[i].disabled = (theme !== 'light');
        }
      }
      apply();
      document.addEventListener('DOMContentLoaded', function(){
        apply();
        if (window.MutationObserver)
          new MutationObserver(apply).observe(document.body, {attributes: true, attributeFilter: ['data-theme', 'data-bs-theme']});
      });
    })();
    ''';

procedure TMarkdownIt_Library.Created;
begin
  inherited;
  // GitHub markdown CSS - light and dark variants, the theme script enables one
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/github-markdown-css@5.9.0/', 'github-markdown-light.css');
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/github-markdown-css@5.9.0/', 'github-markdown-dark-dimmed.css');
  // compact variant used by cards/comments: drop the outer margins
  Sources.AddStyle(
    '.markdown-body.markdown-compact > :first-child { margin-top: 0 !important; }' + sLineBreak +
    '.markdown-body.markdown-compact > :last-child { margin-bottom: 0 !important; }',
    'markdown-compact.css');
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
  // highlight.js - syntax highlighting for code blocks (light + dark variants)
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/highlight.js@11.11.1/styles/', 'github.min.css');
  Sources.Add(stStyle, 'https://cdn.jsdelivr.net/npm/highlight.js@11.11.1/styles/', 'github-dark-dimmed.min.css');
  Sources.Add(stScript, 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/', 'highlight.min.js');
  // Delphi/Pascal is not in the common highlight.js build
  Sources.Add(stScript, 'https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/', 'delphi.min.js');
  // the theme script must run after all stylesheets above
  with Sources.AddEmbed(stScript, 'markdown-theme.js', MarkdownThemeScript) do
    Options := [];
end;

initialization
  Libraries.RegisterLibrary(TMarkdownIt_Library);
  Libraries.RegisterLibrary(THighlightJS_Library);
  TBSRenderer.RegisterRenderer(TMarkdownIt, TMarkdownItRenderer);
end.
