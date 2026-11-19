unit mnwAPIDoc;
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
  API documentation viewer control, built from scratch with WebElements.

  Renders OpenAPI 3.x and Swagger 2.0 JSON specifications as interactive API
  documentation using the bundled vanilla JavaScript viewer (mnwAPIDoc.js).
  The script is compiled into this unit resources (mnwAPIDoc.rc) and served
  from the assets schema, no external libraries or CDN are required.

  Features: sidebar navigation with search, grouped operations by tags,
  parameters/request body/responses with collapsible schema trees, models
  section, light/dark themes (optionally following the Bootstrap theme),
  and a "Try it out" console with cURL support.

  UI texts are translated through the mnw language files (Ctx._T with the
  keys 'apidoc.*' in en.mnw.json/ar.mnw.json), English is the fallback.

  Usage with URL:
    with TAPIDoc.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecURL := 'https://petstore.swagger.io/v2/swagger.json';
    end;

  Usage with inline spec content (JSON string):
    with TAPIDoc.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecContent := '{"openapi":"3.0.0","info":{"title":"My API","version":"1.0"},...}';
    end;

  ProxyURL is used by the TryIt console for CORS, when it contains '{url}'
  the request URL is placed there (URL encoded), otherwise it is appended
  after a slash, e.g. 'https://cors-anywhere.example.com/'.
*)

interface

uses
  SysUtils, Classes, StrUtils,
  mnTypes, mnUtils, mnClasses, mnFields, mnServers, mnDON, mnLogs, mnConfigs, mnParams,
  mnModules, mnWebElements, mnBootstraps;

type

  { TAPIDocOption / TAPIDocOptions }

  TAPIDocOption = (
    adoShowSidebar,           //* Show the side navigation (default on)
    adoShowSearch,            //* Show the search box in the sidebar (default on)
    adoShowTryIt,             //* Enable the "Try it out" console (default on)
    adoShowDownload,          //* Show the "Download spec" button (default on)
    adoFillBodyExample,       //* Pre-fill the request body with an example (default on)
    adoShowCurl,              //* Show cURL buttons in the TryIt console
    adoExpandOperations,      //* Expand all operations by default
    adoExpandSchemas,         //* Expand schema trees by default
    adoHideModels,            //* Hide the models/schemas section
    adoSortTags,              //* Sort tag groups alphabetically
    adoSortOperations,        //* Sort operations by path inside a group
    adoExpandSections         //* Expand the tag/models sections by default (default is collapsed)
  );
  TAPIDocOptions = set of TAPIDocOption;

  { TAPIDocTheme }

  TAPIDocTheme = (
    adtAuto,                  //* Follow Bootstrap theme / OS preference
    adtLight,                 //* Force light mode
    adtDark                   //* Force dark mode
  );

  { TAPIDoc }

  [TID_Extension]
  TAPIDoc = class(THTML.THTMLControl)
  private
    FSpecURL: string;
    FSpecContent: string;
    FOptions: TAPIDocOptions;
    FTheme: TAPIDocTheme;
    FBootstrapTheme: Boolean;
    FHeadingText: string;
    FServerURL: string;
    FProxyURL: string;
    FHeight: string;
    FFetchCredentials: string;
    FCustomCSS: string;
  protected
    procedure Created; override;
    procedure DoRequired(const Ctx: TmnwContext); override;
  public
    //* URL to the OpenAPI/Swagger spec (JSON)
    property SpecURL: string read FSpecURL write FSpecURL;
    //* Inline OpenAPI/Swagger spec content as JSON string
    property SpecContent: string read FSpecContent write FSpecContent;
    //* Viewer options
    property Options: TAPIDocOptions read FOptions write FOptions;
    //* Light/dark theme (default adtAuto follows Bootstrap/OS)
    property Theme: TAPIDocTheme read FTheme write FTheme;
    //* Take the colors from the Bootstrap CSS variables (default True)
    property BootstrapTheme: Boolean read FBootstrapTheme write FBootstrapTheme;
    //* Custom heading text shown above the API title
    property HeadingText: string read FHeadingText write FHeadingText;
    //* Override the server URL used by the docs and TryIt
    property ServerURL: string read FServerURL write FServerURL;
    //* CORS proxy for TryIt requests, use '{url}' as the target placeholder
    property ProxyURL: string read FProxyURL write FProxyURL;
    //* CSS height of the viewer (default fills the viewport)
    property Height: string read FHeight write FHeight;
    //* Fetch credentials mode for TryIt: 'omit', 'same-origin' or 'include'
    property FetchCredentials: string read FFetchCredentials write FFetchCredentials;
    //* Custom CSS injected into the viewer
    property CustomCSS: string read FCustomCSS write FCustomCSS;
  end;

  { TAPIDocRenderer }

  TAPIDocRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext); override;
  end;

  { TAPIDoc_Library }

  TAPIDoc_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

implementation

{$ifdef FPC}
{$R 'mnwAPIDoc.rc'}
{$else}
//* You need to compile it by brcc32 mnwAPIDoc.rc or wait another 100 years till Delphi auto compile it
{$R 'mnwAPIDoc.res' 'mnwAPIDoc.rc'}
{$endif}

{ TAPIDoc }

procedure TAPIDoc.Created;
begin
  inherited;
  FOptions := [adoShowSidebar, adoShowSearch, adoShowTryIt, adoShowDownload, adoFillBodyExample];
  FTheme := adtAuto;
  FBootstrapTheme := True;
end;

procedure TAPIDoc.DoRequired(const Ctx: TmnwContext);
begin
  inherited;
  Ctx.Require(TBootstrap_Library);
  Ctx.Require(TBootstrapIcons_Library);
  Ctx.Require(TAPIDoc_Library);
end;

{ TAPIDocRenderer }

procedure TAPIDocRenderer.DoCollectAttributes(Scope: TmnwScope; Ctx: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('mnw-apidoc-container');
  Scope.Classes.Add('w-100');
end;

procedure TAPIDocRenderer.DoInnerRender(Scope: TmnwScope; const Ctx: TmnwContext);
var
  e: TAPIDoc;
  aOptions: string;

  //A JSON string literal (quoted and escaped) of AValue
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

  procedure AddBoolOption(const AJSName: string; AValue: Boolean);
  begin
    AddOption(AJSName, When(AValue, 'true', 'false'));
  end;

  procedure AddStringOption(const AJSName: string; const AValue: string);
  begin
    if AValue <> '' then
      AddOption(AJSName, JsonStr(AValue));
  end;

  //The viewer UI texts, translated via the page language files
  function LabelsJSON: string;
  var
    aLabels: string;

    procedure AddLabel(const AKey: string; const ADefault: string);
    begin
      if aLabels <> '' then
        aLabels := aLabels + ', ';
      aLabels := aLabels + JsonStr(AKey) + ': ' + JsonStr(Ctx._T('apidoc.' + AKey, ADefault));
    end;

  begin
    aLabels := '';
    AddLabel('overview', 'Overview');
    AddLabel('servers', 'Servers');
    AddLabel('authentication', 'Authentication');
    AddLabel('download', 'Download spec');
    AddLabel('models', 'Models');
    AddLabel('search', 'Search...');
    AddLabel('parameters', 'Parameters');
    AddLabel('requestBody', 'Request body');
    AddLabel('responses', 'Responses');
    AddLabel('tryit', 'Try it out');
    AddLabel('send', 'Send');
    AddLabel('clear', 'Clear');
    AddLabel('status', 'Status');
    AddLabel('time', 'Time');
    AddLabel('headers', 'Headers');
    AddLabel('body', 'Body');
    AddLabel('example', 'Example');
    AddLabel('required', 'required');
    AddLabel('deprecated', 'deprecated');
    AddLabel('noresult', 'No matching operations');
    AddLabel('loaderror', 'Failed to load the specification');
    AddLabel('name', 'Name');
    AddLabel('type', 'Type');
    AddLabel('in', 'In');
    AddLabel('description', 'Description');
    AddLabel('nocontent', 'No content');
    Result := '{' + aLabels + '}';
  end;

  function ThemeJS: string;
  begin
    case e.FTheme of
      adtLight: Result := 'light';
      adtDark: Result := 'dark';
    else
      Result := 'auto';
    end;
  end;

begin
  inherited;
  e := Scope.Element as TAPIDoc;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  //Build the options JS object
  aOptions := '';

  //Spec source: URL takes priority over inline content
  if e.FSpecURL <> '' then
    AddOption('url', JsonStr(e.FSpecURL))
  else if e.FSpecContent <> '' then
    AddOption('contentElementId', JsonStr(e.ID + '_spec'));

  //Options mapping
  AddBoolOption('sidebar', adoShowSidebar in e.FOptions);
  AddBoolOption('search', adoShowSearch in e.FOptions);
  AddBoolOption('tryIt', adoShowTryIt in e.FOptions);
  AddBoolOption('download', adoShowDownload in e.FOptions);
  AddBoolOption('fillBody', adoFillBodyExample in e.FOptions);
  AddBoolOption('showCurl', adoShowCurl in e.FOptions);
  AddBoolOption('expandSections', adoExpandSections in e.FOptions);
  AddBoolOption('expandOperations', adoExpandOperations in e.FOptions);
  AddBoolOption('expandSchemas', adoExpandSchemas in e.FOptions);
  AddBoolOption('hideModels', adoHideModels in e.FOptions);
  AddBoolOption('sortTags', adoSortTags in e.FOptions);
  AddBoolOption('sortOperations', adoSortOperations in e.FOptions);

  //Theme
  AddStringOption('theme', ThemeJS);
  AddBoolOption('bootstrapTheme', e.FBootstrapTheme);

  //Strings
  AddStringOption('headingText', e.FHeadingText);
  AddStringOption('serverUrl', e.FServerURL);
  AddStringOption('proxyUrl', e.FProxyURL);
  AddStringOption('height', e.FHeight);
  AddStringOption('credentials', e.FFetchCredentials);
  AddStringOption('customCss', e.FCustomCSS);

  //UI texts
  AddOption('labels', LabelsJSON);

  aOptions := '{' + aOptions + '}';

  //Output container div
  Ctx.Writer.OpenTag('div', Scope.ToString(True));
  Ctx.Writer.CloseTag('div');

  //Store inline spec content if provided (but no URL)
  if (e.FSpecURL = '') and (e.FSpecContent <> '') then
  begin
    Ctx.Writer.OpenTag('script', 'type="text/template" id=' + DQ(e.ID + '_spec'));
    Ctx.Writer.Write(StringReplace(e.FSpecContent, '</script>', '<\/script>', [rfReplaceAll]));
    Ctx.Writer.CloseTag('script');
  end;

  //Initialisation script, it runs now when the DOM is already parsed (the
  //control can be rendered later, e.g. inside an updated panel) or on
  //DOMContentLoaded when the page is still loading
  Ctx.Writer.OpenTag('script');
  Ctx.Writer.WriteLn('(function(){');
  Ctx.Writer.WriteLn('  function init(){');
  Ctx.Writer.WriteLn('    var el = document.getElementById(' + DQ(e.ID) + ');');
  Ctx.Writer.WriteLn('    if (el && typeof mnwAPIDoc !== "undefined" && mnwAPIDoc.render) {');
  Ctx.Writer.WriteLn('      mnwAPIDoc.render(el, ' + aOptions + ');');
  Ctx.Writer.WriteLn('    }');
  Ctx.Writer.WriteLn('  }');
  Ctx.Writer.WriteLn('  if (document.readyState === "loading")');
  Ctx.Writer.WriteLn('    document.addEventListener("DOMContentLoaded", init);');
  Ctx.Writer.WriteLn('  else');
  Ctx.Writer.WriteLn('    init();');
  Ctx.Writer.WriteLn('})();');
  Ctx.Writer.CloseTag('script');
end;

{ TAPIDoc_Library }

procedure TAPIDoc_Library.Created;
begin
  inherited;
  //The viewer script is compiled into the binary resources (mnwAPIDoc.rc),
  //no CDN is needed, all UI style and logic lives in the JavaScript file
  Sources.Add(stScript, stResource, 'apidoc.js', '?minilib\web\source\mnwAPIDoc.js');
end;

initialization
  Libraries.RegisterLibrary(TAPIDoc_Library);
  TBSRenderer.RegisterRenderer(TAPIDoc, TAPIDocRenderer);
end.
