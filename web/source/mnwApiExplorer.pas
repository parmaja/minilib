unit mnwApiExplorer;
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

(*
  OpenAPI Explorer API documentation viewer control.

  Renders OpenAPI/Swagger specifications as interactive API documentation using:
    - OpenAPI Explorer  https://github.com/Authress-Engineering/openapi-explorer

  OpenAPI Explorer is a Web Component custom element (<openapi-explorer>) that
  supports OpenAPI 3.x specs via URL or an inline spec object.

  Usage with URL:
    with TApiExplorer.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecURL := 'https://petstore3.swagger.io/api/v3/openapi.json';
    end;

  Usage with inline spec (JSON string):
    with TApiExplorer.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecContent := '{"openapi":"3.0.0","info":{"title":"My API","version":"1.0"},...}';
    end;

  Note: the inline content must be JSON. The component's loadSpec() accepts a
  spec object or a URL, it does not parse YAML strings.

  Bootstrap integration:
    By default (BootstrapTheme = True) the viewer takes its colors from the
    Bootstrap CSS variables (--bs-body-bg, --bs-body-color, --bs-primary, ...)
    and follows the page light/dark mode set with data-bs-theme="dark" on
    <html> (Bootstrap 5.3+), the common .dark class, or the OS preference.
    Theme can also force 'light' or 'dark' for the viewer alone.

    Colors set explicitly (BackgroundColor, TextColor, PrimaryColor,
    SecondaryColor, NavBackgroundColor) are never overridden.
*)

interface

uses
  SysUtils, Classes, StrUtils,
  mnTypes, mnUtils, mnClasses, mnFields, mnServers, mnDON, mnLogs, mnConfigs, mnParams,
  mnModules, mnWebElements, mnBootstraps;

type

  { TApiExplorerOption / TApiExplorerOptions }

  TApiExplorerOption = (
    aeoDisplayNulls,              //* Force rendering of null types in the models
    aeoHideDefaults,              //* Do not pre-fill request data with default values
    aeoCollapse,                  //* Collapse operations and components by default
    aeoTree,                      //* Render body schemas as JSON trees instead of tables
    aeoHideConsole,               //* Disable executing API requests (hide the console)
    aeoHideAuthentication,        //* Hide the authentication section
    aeoHideInfo,                  //* Hide the info/overview section
    aeoHideSearch,                //* Hide the navigation search box
    aeoHideServerSelection,       //* Hide the server selection section
    aeoHideComponents,            //* Hide the OpenAPI components section
    aeoUsePathInNavBar            //* Show "METHOD - Path" in the navigation bar
  );
  TApiExplorerOptions = set of TApiExplorerOption;

  { TApiExplorerTheme }

  TApiExplorerTheme = (
    aetAuto,                      //* Follow Bootstrap/OS light or dark mode
    aetLight,                     //* Force light mode
    aetDark                       //* Force dark mode
  );

  { TApiExplorer }

  [TID_Extension]
  TApiExplorer = class(THTML.THTMLControl)
  private
    FSpecURL: string;
    FSpecContent: string;
    FOptions: TApiExplorerOptions;
    FTheme: TApiExplorerTheme;
    FBootstrapTheme: Boolean;
    FHeadingText: string;
    FExplorerLocation: string;
    FServerURL: string;
    FSchemaExpandLevel: Integer;
    FDefaultSchemaTab: string;
    FResponseAreaHeight: string;
    FFetchCredentials: string;
    FMatchPaths: string;
    FPrimaryColor: string;
    FSecondaryColor: string;
    FBackgroundColor: string;
    FHeaderBackgroundColor: string;
    FTextColor: string;
    FNavBackgroundColor: string;
    FNavTextColor: string;
    FNavHoverBackgroundColor: string;
    FNavHoverTextColor: string;
  protected
    procedure Created; override;
    procedure DoRequired(const Ctx: TmnwContext); override;
  public
    //* URL to the OpenAPI spec (JSON)
    property SpecURL: string read FSpecURL write FSpecURL;
    //* Inline OpenAPI spec content as JSON string
    property SpecContent: string read FSpecContent write FSpecContent;
    //* Rendering options
    property Options: TApiExplorerOptions read FOptions write FOptions;
    //* Light/dark theme (default aetAuto follows Bootstrap/OS)
    property Theme: TApiExplorerTheme read FTheme write FTheme;
    //* Use Bootstrap CSS variables for colors and follow its light/dark mode
    property BootstrapTheme: Boolean read FBootstrapTheme write FBootstrapTheme;
    //* Custom heading text shown above the API title
    property HeadingText: string read FHeadingText write FHeadingText;
    //* Initial location in the document, e.g. 'get-/v1/users'
    property ExplorerLocation: string read FExplorerLocation write FExplorerLocation;
    //* Override the server URL (defaults to the first server in the spec)
    property ServerURL: string read FServerURL write FServerURL;
    //* Schema expand depth (default 99999, i.e. unlimited)
    property SchemaExpandLevel: Integer read FSchemaExpandLevel write FSchemaExpandLevel;
    //* Default schema tab: 'model' (default) or 'body'
    property DefaultSchemaTab: string read FDefaultSchemaTab write FDefaultSchemaTab;
    //* Response area height (e.g. '300px')
    property ResponseAreaHeight: string read FResponseAreaHeight write FResponseAreaHeight;
    //* Fetch credentials mode: 'omit', 'same-origin' or 'include'
    property FetchCredentials: string read FFetchCredentials write FFetchCredentials;
    //* Filter operations by path (search text)
    property MatchPaths: string read FMatchPaths write FMatchPaths;
    //* Primary color (e.g. '#007bff')
    property PrimaryColor: string read FPrimaryColor write FPrimaryColor;
    //* Secondary color (e.g. '#6c757d')
    property SecondaryColor: string read FSecondaryColor write FSecondaryColor;
    //* Background color (e.g. '#ffffff')
    property BackgroundColor: string read FBackgroundColor write FBackgroundColor;
    //* Header background color (e.g. '#f8f9fa')
    property HeaderBackgroundColor: string read FHeaderBackgroundColor write FHeaderBackgroundColor;
    //* Text color (e.g. '#333333')
    property TextColor: string read FTextColor write FTextColor;
    //* Navigation background color
    property NavBackgroundColor: string read FNavBackgroundColor write FNavBackgroundColor;
    //* Navigation text color
    property NavTextColor: string read FNavTextColor write FNavTextColor;
    //* Navigation hover background color
    property NavHoverBackgroundColor: string read FNavHoverBackgroundColor write FNavHoverBackgroundColor;
    //* Navigation hover text color
    property NavHoverTextColor: string read FNavHoverTextColor write FNavHoverTextColor;
  end;

  { TApiExplorerRenderer }

  TApiExplorerRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
  end;

  { TApiExplorer_Library }

  TApiExplorer_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

implementation

{ TApiExplorer }

procedure TApiExplorer.Created;
begin
  inherited;
  FSchemaExpandLevel := 0; // 0 = use the component default (unlimited)
  FTheme := aetAuto;
  FBootstrapTheme := True;
end;

procedure TApiExplorer.DoRequired(const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Require(TBootstrap_Library);
  Ctx.Require(TBootstrapIcons_Library);
  Ctx.Require(TApiExplorer_Library);
end;

{ TApiExplorerRenderer }

procedure TApiExplorerRenderer.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('openapi-explorer-container');
  Scope.Classes.Add('w-100');
end;

procedure TApiExplorerRenderer.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: TApiExplorer;
  aAttrText: string;

  procedure AddAttr(const AName, AValue: string);
  begin
    if AValue <> '' then
    begin
      if aAttrText <> '' then
        aAttrText := aAttrText + ' ';
      aAttrText := aAttrText + AName + '=' + DQ(AValue);
    end;
  end;

  procedure AddFlag(const AName: string; AOption: TApiExplorerOption);
  begin
    if AOption in e.FOptions then
      AddAttr(AName, 'true');
  end;

  function BoolJS(AValue: Boolean): string;
  begin
    Result := IfThen(AValue, 'true', 'false');
  end;

  // Applies Bootstrap colors and follows its light/dark mode at runtime.
  // The component only accepts hex colors in its color attributes, so the
  // Bootstrap CSS variables are read and converted to hex here.
  procedure WriteThemeScript;
  var
    aTheme: string;
  begin
    case e.FTheme of
      aetLight: aTheme := 'light';
      aetDark: aTheme := 'dark';
    else
      aTheme := 'auto';
    end;

    Ctx.Writer.OpenTag('script');
    Ctx.Writer.WriteLn('(function(){');
    Ctx.Writer.WriteLn('  var el = document.getElementById(' + DQ(e.ID) + ');');
    Ctx.Writer.WriteLn('  if (!el) return;');
    Ctx.Writer.WriteLn('  var theme = ' + DQ(aTheme) + ';');
    Ctx.Writer.WriteLn('  var useBootstrap = ' + BoolJS(e.FBootstrapTheme) + ';');
    Ctx.Writer.WriteLn('  var explicit = { bg: ' + BoolJS(e.FBackgroundColor <> '') +
      ', fg: ' + BoolJS(e.FTextColor <> '') +
      ', primary: ' + BoolJS(e.FPrimaryColor <> '') +
      ', secondary: ' + BoolJS(e.FSecondaryColor <> '') +
      ', navBg: ' + BoolJS(e.FNavBackgroundColor <> '') + ' };');
    Ctx.Writer.WriteLn('  function toHex(v){');
    Ctx.Writer.WriteLn('    if (!v) return "";');
    Ctx.Writer.WriteLn('    v = String(v).trim().toLowerCase();');
    Ctx.Writer.WriteLn('    if (/^#[0-9a-f]{3}$/.test(v)) return "#" + v.charAt(1) + v.charAt(1) + v.charAt(2) + v.charAt(2) + v.charAt(3) + v.charAt(3);');
    Ctx.Writer.WriteLn('    if (/^#[0-9a-f]{6}$/.test(v)) return v;');
    Ctx.Writer.WriteLn('    var m = v.match(/^rgba?\(([^)]+)\)$/);');
    Ctx.Writer.WriteLn('    if (!m) return "";');
    Ctx.Writer.WriteLn('    var p = m[1].split(",");');
    Ctx.Writer.WriteLn('    if (p.length < 3) return "";');
    Ctx.Writer.WriteLn('    var r = "";');
    Ctx.Writer.WriteLn('    for (var i = 0; i < 3; i++) {');
    Ctx.Writer.WriteLn('      var h = Math.max(0, Math.min(255, Math.round(parseFloat(p[i])))).toString(16);');
    Ctx.Writer.WriteLn('      r += h.length === 1 ? "0" + h : h;');
    Ctx.Writer.WriteLn('    }');
    Ctx.Writer.WriteLn('    return "#" + r;');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  function isDark(){');
    Ctx.Writer.WriteLn('    if (theme === "dark") return true;');
    Ctx.Writer.WriteLn('    if (theme === "light") return false;');
    Ctx.Writer.WriteLn('    var host = el.closest("[data-bs-theme]");');
    Ctx.Writer.WriteLn('    var t = (host && host.getAttribute("data-bs-theme")) || document.documentElement.getAttribute("data-bs-theme");');
    Ctx.Writer.WriteLn('    if (t === "dark") return true;');
    Ctx.Writer.WriteLn('    if (t === "light") return false;');
    Ctx.Writer.WriteLn('    if (document.documentElement.classList.contains("dark") || (document.body && document.body.classList.contains("dark"))) return true;');
    Ctx.Writer.WriteLn('    return !!(window.matchMedia && matchMedia("(prefers-color-scheme: dark)").matches);');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  function readColors(dark){');
    Ctx.Writer.WriteLn('    var probe = document.createElement("div");');
    Ctx.Writer.WriteLn('    probe.setAttribute("data-bs-theme", dark ? "dark" : "light");');
    Ctx.Writer.WriteLn('    probe.style.display = "none";');
    Ctx.Writer.WriteLn('    (el.parentNode || document.body).appendChild(probe);');
    Ctx.Writer.WriteLn('    var cs = getComputedStyle(probe);');
    Ctx.Writer.WriteLn('    function read(name, fallback){');
    Ctx.Writer.WriteLn('      return toHex(cs.getPropertyValue(name)) || fallback;');
    Ctx.Writer.WriteLn('    }');
    Ctx.Writer.WriteLn('    var c = {');
    Ctx.Writer.WriteLn('      bg: read("--bs-body-bg", dark ? "#212529" : "#ffffff"),');
    Ctx.Writer.WriteLn('      fg: read("--bs-body-color", dark ? "#dee2e6" : "#212529"),');
    Ctx.Writer.WriteLn('      primary: read("--bs-primary", "#0d6efd"),');
    Ctx.Writer.WriteLn('      secondary: read("--bs-secondary", "#6c757d"),');
    Ctx.Writer.WriteLn('      navBg: read("--bs-dark", dark ? "#1a1d20" : "#212529"),');
    Ctx.Writer.WriteLn('      border: read("--bs-border-color", dark ? "#495057" : "#dee2e6"),');
    Ctx.Writer.WriteLn('      blue: read("--bs-link-color", read("--bs-primary", "#0d6efd")),');
    Ctx.Writer.WriteLn('      green: read("--bs-green", "#198754"),');
    Ctx.Writer.WriteLn('      red: read("--bs-red", "#dc3545"),');
    Ctx.Writer.WriteLn('      orange: read("--bs-orange", "#fd7e14"),');
    Ctx.Writer.WriteLn('      yellow: read("--bs-yellow", "#ffc107"),');
    Ctx.Writer.WriteLn('      purple: read("--bs-purple", "#6f42c1"),');
    Ctx.Writer.WriteLn('      pink: read("--bs-pink", "#d63384"),');
    Ctx.Writer.WriteLn('      gray: read("--bs-gray", "#6c757d"),');
    Ctx.Writer.WriteLn('      white: read("--bs-white", "#ffffff")');
    Ctx.Writer.WriteLn('    };');
    Ctx.Writer.WriteLn('    (el.parentNode || document.body).removeChild(probe);');
    Ctx.Writer.WriteLn('    if (dark && c.navBg.toLowerCase() === c.bg.toLowerCase()) c.navBg = "#1a1d20";');
    Ctx.Writer.WriteLn('    return c;');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  function builtinColors(dark){');
    Ctx.Writer.WriteLn('    return dark');
    Ctx.Writer.WriteLn('      ? { bg: "#212529", fg: "#dee2e6", primary: "#0d6efd", secondary: "#6c757d", navBg: "#1a1d20", border: "#495057" }');
    Ctx.Writer.WriteLn('      : { bg: "#ffffff", fg: "#212529", primary: "#0d6efd", secondary: "#6c757d", navBg: "#212529", border: "#dee2e6" };');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  function apply(){');
    Ctx.Writer.WriteLn('    var dark = isDark();');
    Ctx.Writer.WriteLn('    var c = useBootstrap ? readColors(dark) : builtinColors(dark);');
    Ctx.Writer.WriteLn('    var changed = false;');
    Ctx.Writer.WriteLn('    function attr(name, value, skip){');
    Ctx.Writer.WriteLn('      if (!skip && value && el.getAttribute(name) !== value) { el.setAttribute(name, value); changed = true; }');
    Ctx.Writer.WriteLn('    }');
    Ctx.Writer.WriteLn('    attr("bg-color", c.bg, explicit.bg);');
    Ctx.Writer.WriteLn('    attr("text-color", c.fg, explicit.fg);');
    Ctx.Writer.WriteLn('    attr("primary-color", c.primary, explicit.primary);');
    Ctx.Writer.WriteLn('    attr("secondary-color", c.secondary, explicit.secondary);');
    Ctx.Writer.WriteLn('    attr("nav-bg-color", c.navBg, explicit.navBg);');
    Ctx.Writer.WriteLn('    if (useBootstrap) {');
    Ctx.Writer.WriteLn('      var vars = { "--blue": c.blue, "--green": c.green, "--red": c.red, "--orange": c.orange, "--yellow": c.yellow, "--purple": c.purple, "--pink": c.pink, "--gray": c.gray, "--white": c.white, "--border-color": c.border };');
    Ctx.Writer.WriteLn('      for (var k in vars) {');
    Ctx.Writer.WriteLn('        if (el.style.getPropertyValue(k) !== vars[k]) { el.style.setProperty(k, vars[k]); changed = true; }');
    Ctx.Writer.WriteLn('      }');
    Ctx.Writer.WriteLn('    }');
    Ctx.Writer.WriteLn('    if (changed && typeof el.requestUpdate === "function") el.requestUpdate();');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  apply();');
    Ctx.Writer.WriteLn('  if (theme === "auto" && window.MutationObserver) {');
    Ctx.Writer.WriteLn('    var timer = null;');
    Ctx.Writer.WriteLn('    function schedule(){ if (timer) clearTimeout(timer); timer = setTimeout(apply, 50); }');
    Ctx.Writer.WriteLn('    var mo = new MutationObserver(schedule);');
    Ctx.Writer.WriteLn('    mo.observe(document.documentElement, { attributes: true, attributeFilter: ["data-bs-theme", "class"] });');
    Ctx.Writer.WriteLn('    if (document.body) mo.observe(document.body, { attributes: true, attributeFilter: ["data-bs-theme", "class"] });');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  if (theme === "auto" && window.matchMedia) {');
    Ctx.Writer.WriteLn('    var mq = matchMedia("(prefers-color-scheme: dark)");');
    Ctx.Writer.WriteLn('    if (mq.addEventListener) mq.addEventListener("change", apply);');
    Ctx.Writer.WriteLn('    else if (mq.addListener) mq.addListener(apply);');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('})();');
    Ctx.Writer.CloseTag('script');
  end;

begin
  inherited;
  e := Scope.Element as TApiExplorer;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  aAttrText := '';

  // Spec source, the component loads it by itself when the attribute changes
  if e.FSpecURL <> '' then
    AddAttr('spec-url', e.FSpecURL);

  // Setup
  AddAttr('heading-text', e.FHeadingText);
  AddAttr('explorer-location', e.FExplorerLocation);
  AddAttr('server-url', e.FServerURL);
  AddAttr('default-schema-tab', e.FDefaultSchemaTab);
  AddAttr('response-area-height', e.FResponseAreaHeight);
  AddAttr('fetch-credentials', e.FFetchCredentials);
  AddAttr('match-paths', e.FMatchPaths);

  if e.FSchemaExpandLevel > 0 then
    AddAttr('schema-expand-level', e.FSchemaExpandLevel.ToString);

  // Boolean flags
  AddFlag('display-nulls', aeoDisplayNulls);
  AddFlag('hide-defaults', aeoHideDefaults);
  AddFlag('collapse', aeoCollapse);
  AddFlag('tree', aeoTree);
  AddFlag('hide-console', aeoHideConsole);
  AddFlag('hide-authentication', aeoHideAuthentication);
  AddFlag('hide-info', aeoHideInfo);
  AddFlag('hide-search', aeoHideSearch);
  AddFlag('hide-server-selection', aeoHideServerSelection);
  AddFlag('hide-components', aeoHideComponents);
  AddFlag('use-path-in-nav-bar', aeoUsePathInNavBar);

  // Colors
  AddAttr('primary-color', e.FPrimaryColor);
  AddAttr('secondary-color', e.FSecondaryColor);
  AddAttr('bg-color', e.FBackgroundColor);
  AddAttr('header-bg-color', e.FHeaderBackgroundColor);
  AddAttr('text-color', e.FTextColor);
  AddAttr('nav-bg-color', e.FNavBackgroundColor);
  AddAttr('nav-text-color', e.FNavTextColor);
  AddAttr('nav-hover-bg-color', e.FNavHoverBackgroundColor);
  AddAttr('nav-hover-text-color', e.FNavHoverTextColor);

  // Render the <openapi-explorer> custom element
  Ctx.Writer.OpenTag('openapi-explorer', 'id=' + DQ(e.ID) + IfThen(aAttrText <> '', ' ' + aAttrText, ''));
  Ctx.Writer.CloseTag('openapi-explorer');

  // Bootstrap colors and light/dark theme
  if e.FBootstrapTheme or (e.FTheme <> aetAuto) then
    WriteThemeScript;

  // For inline spec content (no URL), load it with loadSpec() after the
  // custom element is defined
  if (e.FSpecURL = '') and (e.FSpecContent <> '') then
  begin
    // Store inline spec in a text/template script
    Ctx.Writer.OpenTag('script', 'type="text/template" id=' + DQ(e.ID + '_spec'));
    Ctx.Writer.Write(StringReplace(e.FSpecContent, '</script>', '<\/script>', [rfReplaceAll]));
    Ctx.Writer.CloseTag('script');

    Ctx.Writer.OpenTag('script');
    Ctx.Writer.WriteLn('(function(){');
    Ctx.Writer.WriteLn('  function load(){');
    Ctx.Writer.WriteLn('    var el = document.getElementById(' + DQ(e.ID) + ');');
    Ctx.Writer.WriteLn('    var specEl = document.getElementById(' + DQ(e.ID + '_spec') + ');');
    Ctx.Writer.WriteLn('    if (!el || !specEl || typeof el.loadSpec !== "function") return;');
    Ctx.Writer.WriteLn('    try {');
    Ctx.Writer.WriteLn('      el.loadSpec(JSON.parse(specEl.textContent.trim()));');
    Ctx.Writer.WriteLn('    } catch (err) {');
    Ctx.Writer.WriteLn('      console.error("OpenAPI Explorer: invalid spec content", err);');
    Ctx.Writer.WriteLn('    }');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('  if (window.customElements && customElements.whenDefined) {');
    Ctx.Writer.WriteLn('    customElements.whenDefined("openapi-explorer").then(load);');
    Ctx.Writer.WriteLn('  } else if (document.readyState === "loading") {');
    Ctx.Writer.WriteLn('    document.addEventListener("DOMContentLoaded", load);');
    Ctx.Writer.WriteLn('  } else {');
    Ctx.Writer.WriteLn('    load();');
    Ctx.Writer.WriteLn('  }');
    Ctx.Writer.WriteLn('})();');
    Ctx.Writer.CloseTag('script');
  end;
end;

{ TApiExplorer_Library }

procedure TApiExplorer_Library.Created;
const
  cBaseURL = 'https://cdn.jsdelivr.net/npm/openapi-explorer@2.4.838/dist/browser/';
begin
  inherited;
  // OpenAPI Explorer browser bundle (IIFE, defines the web component, styles included)
  Sources.Add(stScript, cBaseURL, 'openapi-explorer.min.js');
end;

initialization
  Libraries.RegisterLibrary(TApiExplorer_Library);
  TBSRenderer.RegisterRenderer(TApiExplorer, TApiExplorerRenderer);
end.
