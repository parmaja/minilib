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

  { TApiExplorer }

  [TID_Extension]
  TApiExplorer = class(THTML.THTMLControl)
  private
    FSpecURL: string;
    FSpecContent: string;
    FOptions: TApiExplorerOptions;
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
