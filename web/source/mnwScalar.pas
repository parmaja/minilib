unit mnwScalar;
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
  Scalar API Reference viewer control.

  Renders OpenAPI/Swagger specifications as interactive API documentation using:
    - Scalar API Reference  https://github.com/scalar/scalar

  The control supports URL-based, inline, and multiple-document rendering via
  Scalar.createApiReference().

  Usage:
    with TScalar.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecURL := 'https://petstore.swagger.io/v2/swagger.json';
    end;

  Or with inline spec content (JSON/YAML string):
    with TScalar.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecContent := '{"openapi":"3.0.0","info":{"title":"My API","version":"1.0"},...}';
    end;
*)

interface

uses
  SysUtils, Classes, StrUtils,
  mnTypes, mnUtils, mnClasses, mnFields, mnServers, mnDON, mnLogs, mnConfigs, mnParams,
  mnModules, mnWebElements, mnBootstraps;

type

  { TScalarOption / TScalarOptions }

  TScalarOption = (
    scoDarkMode,                  //* Enable dark mode by default
    scoHideDownloadButton,        //* Hide the "Download spec" button
    scoHideSearch,                //* Hide the sidebar search bar
    scoHideModels,                //* Hide models/schemas section
    scoHideTestRequestButton,     //* Hide the "Test Request" button
    scoHideClientButton,          //* Hide the client button from sidebar/modal
    scoHideDarkModeToggle,        //* Hide the dark mode toggle
    scoExpandAllResponses,        //* Expand all response sections by default
    scoExpandAllModels,           //* Expand all model sections by default
    scoClassicLayout,             //* Use classic layout instead of modern
    scoDefaultOpenAllTags,        //* Open all tags by default
    scoWithDefaultFonts           //* Load default fonts (Inter, JetBrains Mono)
  );
  TScalarOptions = set of TScalarOption;

  { TScalar }

  [TID_Extension]
  TScalar = class(THTML.THTMLControl)
  private
    FSpecURL: string;
    FSpecContent: string;
    FOptions: TScalarOptions;
    FProxyURL: string;
    FTheme: string;
    FCustomCSS: string;
    FShowSidebar: Boolean;
  protected
    procedure Created; override;
    procedure DoRequired(const Ctx: TmnwContext); override;
  public
    //* URL to the OpenAPI/Swagger spec (JSON or YAML)
    property SpecURL: string read FSpecURL write FSpecURL;
    //* Inline OpenAPI/Swagger spec content as JSON or YAML string
    property SpecContent: string read FSpecContent write FSpecContent;
    //* Rendering options
    property Options: TScalarOptions read FOptions write FOptions;
    //* Proxy URL for CORS, e.g. 'https://proxy.scalar.com'
    property ProxyURL: string read FProxyURL write FProxyURL;
    //* Theme name: default, alternate, moon, purple, solarized, bluePlanet,
    //* saturn, kepler, mars, deepSpace, laserwave, none
    property Theme: string read FTheme write FTheme;
    //* Custom CSS to inject into the Scalar component
    property CustomCSS: string read FCustomCSS write FCustomCSS;
    //* Whether to show the sidebar (default True)
    property ShowSidebar: Boolean read FShowSidebar write FShowSidebar;
  end;

  { TScalarRenderer }

  TScalarRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
  end;

  { TScalar_Library }

  TScalar_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

implementation

{ TScalar }

procedure TScalar.Created;
begin
  inherited;
  FOptions := [scoWithDefaultFonts];
  FShowSidebar := True;
end;

procedure TScalar.DoRequired(const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Require(TBootstrap_Library);
  Ctx.Require(TBootstrapIcons_Library);
  Ctx.Require(TScalar_Library);
end;

{ TScalarRenderer }

procedure TScalarRenderer.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('scalar-container');
  // Minimal style: full-width, no extra margin/padding interference
  Scope.Classes.Add('w-100');
end;

procedure TScalarRenderer.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: TScalar;
  aOptions: string;

  //a JSON string literal (quoted and escaped) of AValue
  function JsonStr(const AValue: string): string;
  begin
    Result := '"' + EscapeJSONString(AValue) + '"';
  end;

  procedure AddOption(const AJSName: string; const AJSValue: string);
  begin
    if aOptions <> '' then
      aOptions := aOptions + ', ';
    aOptions := aOptions + JsonStr(AJSName) + ': ' + AJSValue;
  end;

  procedure AddBoolOption(const AJSName: string; AOption: TScalarOption);
  begin
    if AOption in e.FOptions then
      AddOption(AJSName, 'true');
  end;

  procedure AddStringOption(const AJSName: string; const AValue: string);
  begin
    if AValue <> '' then
      AddOption(AJSName, JsonStr(AValue));
  end;

begin
  inherited;
  e := Scope.Element as TScalar;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  // Build options JS object
  aOptions := '';

  // Spec source: URL takes priority over inline content
  if e.FSpecURL <> '' then
    AddOption('url', JsonStr(e.FSpecURL))
  else if e.FSpecContent <> '' then
    AddOption('content', 'document.getElementById(' + DQ(e.ID + '_spec') + ').textContent');

  // Options mapping
  AddBoolOption('darkMode', scoDarkMode);
  AddBoolOption('hideSearch', scoHideSearch);
  AddBoolOption('hideModels', scoHideModels);
  AddBoolOption('hideTestRequestButton', scoHideTestRequestButton);
  AddBoolOption('hideClientButton', scoHideClientButton);
  AddBoolOption('hideDarkModeToggle', scoHideDarkModeToggle);
  AddBoolOption('expandAllResponses', scoExpandAllResponses);
  AddBoolOption('expandAllModelSections', scoExpandAllModels);
  AddBoolOption('defaultOpenAllTags', scoDefaultOpenAllTags);

  // Layout option (modern is default)
  if scoClassicLayout in e.FOptions then
    AddOption('layout', '"classic"');

  // Fonts option (default is true)
  if not (scoWithDefaultFonts in e.FOptions) then
    AddOption('withDefaultFonts', 'false');

  // Hide download button
  if scoHideDownloadButton in e.FOptions then
    AddOption('documentDownloadType', '"none"');

  // Show sidebar (default true)
  if not e.FShowSidebar then
    AddOption('showSidebar', 'false');

  // Proxy URL
  AddStringOption('proxyUrl', e.FProxyURL);

  // Theme
  AddStringOption('theme', e.FTheme);

  // Custom CSS
  AddStringOption('customCss', e.FCustomCSS);

  aOptions := '{' + aOptions + '}';

  // Output container div
  Ctx.Writer.OpenTag('div', Scope.ToString(True));
  Ctx.Writer.CloseTag('div');

  // Store inline spec content if provided (but no URL)
  if (e.FSpecURL = '') and (e.FSpecContent <> '') then
  begin
    Ctx.Writer.OpenTag('script', 'type="text/template" id=' + DQ(e.ID + '_spec'));
    Ctx.Writer.Write(StringReplace(e.FSpecContent, '</script>', '<\/script>', [rfReplaceAll]));
    Ctx.Writer.CloseTag('script');
  end;

  // Initialisation script, it runs now when the DOM is already parsed (the
  // control can be rendered later, e.g. inside an updated panel) or on
  // DOMContentLoaded when the page is still loading
  Ctx.Writer.OpenTag('script');
  Ctx.Writer.WriteLn('(function(){');
  Ctx.Writer.WriteLn('  function init(){');
  Ctx.Writer.WriteLn('    var el = document.getElementById(' + DQ(e.ID) + ');');
  Ctx.Writer.WriteLn('    if (el && typeof Scalar !== "undefined" && Scalar.createApiReference) {');
  Ctx.Writer.WriteLn('      var opts = ' + aOptions + ';');
  Ctx.Writer.WriteLn('      Scalar.createApiReference(' + DQ('#' + e.ID) + ', opts);');
  Ctx.Writer.WriteLn('    }');
  Ctx.Writer.WriteLn('  }');
  Ctx.Writer.WriteLn('  if (document.readyState === "loading")');
  Ctx.Writer.WriteLn('    document.addEventListener("DOMContentLoaded", init);');
  Ctx.Writer.WriteLn('  else');
  Ctx.Writer.WriteLn('    init();');
  Ctx.Writer.WriteLn('})();');
  Ctx.Writer.CloseTag('script');
end;

{ TScalar_Library }

procedure TScalar_Library.Created;
const
  cBaseURL = 'https://cdn.jsdelivr.net/npm/@scalar/api-reference@1.63.0/dist/browser/';
begin
  inherited;
  //The standalone bundle injects its own stylesheet (style#scalar-style) and
  //exposes window.Scalar, no separate CSS file is needed
  Sources.Add(stScript, cBaseURL, 'standalone.js');
end;

initialization
  Libraries.RegisterLibrary(TScalar_Library);
  TBSRenderer.RegisterRenderer(TScalar, TScalarRenderer);
end.

