unit mnwRapiDoc;
{$H+}{$M+}
{$ifdef fpc}
{$mode delphi}
{$modeswitch functionreferences}{$modeswitch anonymousfunctions}
{$endif}
{**
 *  This file is part of the "Mini Library"
 *
 * @license   MIT
 *
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

(*
  RapiDoc API documentation viewer control.

  Renders OpenAPI/Swagger specifications as interactive API documentation using:
    - RapiDoc  https://github.com/rapi-doc/RapiDoc

  RapiDoc is a Web Component custom element (<rapi-doc>) that supports
  OpenAPI 3.0 and Swagger 2.0 specs via URL or inline content.

  Usage with URL:
    with TRapiDoc.Create(Document.Body.Main) do
    begin
      ID := 'myAPI';
      SpecURL := 'https://petstore.swagger.io/v2/swagger.json';
    end;

  Usage with inline spec (JSON string):
    with TRapiDoc.Create(Document.Body.Main) do
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

  { TRapiDocOption / TRapiDocOptions }

  TRapiDocOption = (
    rdoColumnLayout,             //* Column layout (default is row)
    rdoReadStyle,                //* Read render style (default is focused)
    rdoViewStyle,                //* View render style
    rdoTableSchema,              //* Table schema style (default is tree)
    rdoHeader,                   //* Add header section
    rdoNoSideNav,                //* Hide side navigation
    rdoNoInfo,                   //* Hide info section
    rdoAllowSpecUrlLoad,         //* Allow loading spec from URL
    rdoAllowSpecFileLoad,        //* Allow loading spec from file
    rdoAllowSpecFileDownload,    //* Allow downloading spec file
    rdoAllowSearch,              //* Show search box
    rdoNoTry,                    //* Disable API try-it-out
    rdoNoFonts,                  //* Do not load Google fonts
    rdoSchemaDescriptionExpanded,//* Schema descriptions expanded by default
    rdoSchemaHideReadOnly,       //* Hide read-only properties in schema
    rdoSchemaHideWriteOnly,      //* Hide write-only properties in schema
    rdoFillRequestWithExample,   //* Fill request fields with example values
    rdoShowCurlBeforeTry,        //* Show curl command before trying
    rdoSortTags,                 //* Sort tags alphabetically
    rdoSortEndpointsByPath,      //* Sort endpoints by path
    rdoSortEndpointsByMethod,    //* Sort endpoints by method
    rdoSortEndpointsBySummary    //* Sort endpoints by summary
  );
  TRapiDocOptions = set of TRapiDocOption;

  { TRapiDocControl }

  [TID_Extension]
  TRapiDocControl = class(THTML.THTMLControl)
  private
    FSpecURL: string;
    FSpecContent: string;
    FOptions: TRapiDocOptions;
    FSchemaExpandLevel: Integer;
    FServerURL: string;
    FHeadingText: string;
    FPrimaryColor: string;
    FBackgroundColor: string;
    FTextColor: string;
    FFontSize: string;
  protected
    procedure Created; override;
    procedure DoRequired(const Context: TmnwContext); override;
  public
    //* URL to the OpenAPI/Swagger spec (JSON)
    property SpecURL: string read FSpecURL write FSpecURL;
    //* Inline OpenAPI/Swagger spec content as JSON string
    property SpecContent: string read FSpecContent write FSpecContent;
    //* Rendering options
    property Options: TRapiDocOptions read FOptions write FOptions;
    //* Schema expand level (default unlimited)
    property SchemaExpandLevel: Integer read FSchemaExpandLevel write FSchemaExpandLevel;
    //* Override server URL
    property ServerURL: string read FServerURL write FServerURL;
    //* Custom heading text
    property HeadingText: string read FHeadingText write FHeadingText;
    //* Primary color (e.g. '#007bff')
    property PrimaryColor: string read FPrimaryColor write FPrimaryColor;
    //* Background color (e.g. '#ffffff')
    property BackgroundColor: string read FBackgroundColor write FBackgroundColor;
    //* Text color (e.g. '#333333')
    property TextColor: string read FTextColor write FTextColor;
    //* Font size (e.g. '14px')
    property FontSize: string read FFontSize write FFontSize;
  end;

  { TRapiDocRenderer }

  TRapiDocRenderer = class(TBSRenderer.THTMLControl)
  protected
    procedure DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext); override;
    procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext); override;
  end;

  { TRapiDoc_Library }

  TRapiDoc_Library = class(TmnwLibrary)
  protected
    procedure Created; override;
  public
  end;

implementation

{ TRapiDocControl }

procedure TRapiDocControl.Created;
begin
  inherited;
//  FOptions := [rdoDark];
  FSchemaExpandLevel := -1; // unlimited
end;

procedure TRapiDocControl.DoRequired(const Context: TmnwContext);
begin
  inherited;
  Context.Require(TBootstrap_Library);
  Context.Require(TBootstrapIcons_Library);
  Context.Require(TRapiDoc_Library);
end;

{ TRapiDocRenderer }

procedure TRapiDocRenderer.DoCollectAttributes(var Scope: TmnwScope; Context: TmnwContext);
begin
  inherited;
  Scope.Classes.Add('rapidoc-container');
  Scope.Classes.Add('w-100');
  Scope.Classes.Add('p-0');
  Scope.Classes.Add('m-0');
end;

procedure TRapiDocRenderer.DoInnerRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: TRapiDocControl;
  aSpecURL: string;
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

begin
  inherited;
  e := Scope.Element as TRapiDocControl;
  if e.ID = '' then
    raise Exception.Create('ID is required for: ' + e.ClassName);

  aAttrText := '';
  // Determine spec source URL
  if e.FSpecURL <> '' then
    aSpecURL := e.FSpecURL
  else
    aSpecURL := '';

  // Set attributes on the <rapi-doc> custom element
  if aSpecURL <> '' then
    AddAttr('spec-url', aSpecURL);

  // Theme
  {if rdoDark in e.FOptions then
    AddAttr('theme', 'dark')
  else
    AddAttr('theme', 'light');}

  // Layout
  if rdoColumnLayout in e.FOptions then
    AddAttr('layout', 'column')
  else
    AddAttr('layout', 'row');

  // Render style
  if rdoReadStyle in e.FOptions then
    AddAttr('render-style', 'read')
  else if rdoViewStyle in e.FOptions then
    AddAttr('render-style', 'view')
  else
    AddAttr('render-style', 'focused');

  // Schema style
  if rdoTableSchema in e.FOptions then
    AddAttr('schema-style', 'table')
  else
    AddAttr('schema-style', 'tree');

  // Boolean flags
  if rdoHeader in e.FOptions then
    AddAttr('show-header', 'true')
  else
    AddAttr('show-header', 'false');
  if rdoNoSideNav in e.FOptions then
    AddAttr('show-side-nav', 'false');
  if rdoNoInfo in e.FOptions then
    AddAttr('show-info', 'false');
  if rdoAllowSpecUrlLoad in e.FOptions then
    AddAttr('allow-spec-url-load', 'true')
  else
    AddAttr('allow-spec-url-load', 'false');
  if rdoAllowSpecFileLoad in e.FOptions then
    AddAttr('allow-spec-file-load', 'true')
  else
    AddAttr('allow-spec-file-load', 'false');
  if rdoAllowSpecFileDownload in e.FOptions then
    AddAttr('allow-spec-file-download', 'true');
  if rdoAllowSearch in e.FOptions then
    AddAttr('allow-search', 'true');
  if rdoNoTry in e.FOptions then
    AddAttr('allow-try', 'false');
  if rdoNoFonts in e.FOptions then
    AddAttr('load-fonts', 'false')
  else
    AddAttr('load-fonts', 'true');
  if rdoSchemaDescriptionExpanded in e.FOptions then
    AddAttr('schema-description-expanded', 'true');
  if rdoSchemaHideReadOnly in e.FOptions then
    AddAttr('schema-hide-read-only', 'true');
  if rdoSchemaHideWriteOnly in e.FOptions then
    AddAttr('schema-hide-write-only', 'true');
  if rdoFillRequestWithExample in e.FOptions then
    AddAttr('fill-request-fields-with-example', 'true');
  if rdoShowCurlBeforeTry in e.FOptions then
    AddAttr('show-curl-before-try', 'true');
  if rdoSortTags in e.FOptions then
    AddAttr('sort-tags', 'true');
  if rdoSortEndpointsByPath in e.FOptions then
    AddAttr('sort-endpoints-by', 'path')
  else if rdoSortEndpointsByMethod in e.FOptions then
    AddAttr('sort-endpoints-by', 'method')
  else if rdoSortEndpointsBySummary in e.FOptions then
    AddAttr('sort-endpoints-by', 'summary');

  // Schema expand level
  if e.FSchemaExpandLevel > 0 then
    AddAttr('schema-expand-level', e.FSchemaExpandLevel.ToString);

  // Server URL override
  if e.FServerURL <> '' then
    AddAttr('server-url', e.FServerURL);

  // Heading text
  if e.FHeadingText <> '' then
    AddAttr('heading-text', e.FHeadingText);

  // Color overrides
  if e.FPrimaryColor <> '' then
    AddAttr('primary-color', e.FPrimaryColor);
  if e.FBackgroundColor <> '' then
    AddAttr('bg-color', e.FBackgroundColor);
  if e.FTextColor <> '' then
    AddAttr('text-color', e.FTextColor);
  if e.FFontSize <> '' then
    AddAttr('font-size', e.FFontSize);

  // Render the <rapi-doc> custom element
  Context.Writer.OpenTag('rapi-doc', 'id=' + DQ(e.ID) + IfThen(aAttrText <> '', ' ' + aAttrText, ''));
  Context.Writer.CloseTag('rapi-doc');

  // For inline spec content (no URL), use a JavaScript blob approach
  if (e.FSpecURL = '') and (e.FSpecContent <> '') then
  begin
    // Store inline spec in a text/template script
    Context.Writer.OpenTag('script', 'type="text/template" id=' + DQ(e.ID + '_spec'));
    Context.Writer.Write(StringReplace(e.FSpecContent, '</script>', '<\/script>', [rfReplaceAll]));
    Context.Writer.CloseTag('script');

    // Initialisation script that sets spec-content attribute from inline JSON
    Context.Writer.OpenTag('script');
    Context.Writer.WriteLn('document.addEventListener("DOMContentLoaded", () => {');
    Context.Writer.WriteLn('  var el = document.getElementById(' + DQ(e.ID) + ');');
    Context.Writer.WriteLn('  if (el) {');
    Context.Writer.WriteLn('    var specEl = document.getElementById(' + DQ(e.ID + '_spec') + ');');
    Context.Writer.WriteLn('    if (specEl) {');
    Context.Writer.WriteLn('      var spec = specEl.textContent.trim();');
    Context.Writer.WriteLn('      el.loadSpec(JSON.parse(spec));');
    Context.Writer.WriteLn('    }');
    Context.Writer.WriteLn('  }');
    Context.Writer.WriteLn(' });');
    Context.Writer.CloseTag('script');
  end;
end;

{ TRapiDoc_Library }

procedure TRapiDoc_Library.Created;
const
  cBaseURL = 'https://cdn.jsdelivr.net/npm/rapidoc@9.3.8/dist/';
begin
  inherited;
  // RapiDoc main bundle (ES module web component, CSS included in JS)
  Sources.Add(stScript, cBaseURL, 'rapidoc-min.js');
end;

initialization
  Libraries.RegisterLibrary(TRapiDoc_Library);
  TBSRenderer.RegisterRenderer(TRapiDocControl, TRapiDocRenderer);
end.
