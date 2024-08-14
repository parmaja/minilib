unit mnBootstraps;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>

  https://fastbootstrap.com/components/accordion/
  https://fastbootstrap.com/components/layout/
  https://freefrontend.com/bootstrap-sidebars/

  https://dev.to/codeply/bootstrap-5-sidebar-examples-38pb

  https://bootswatch.com/darkly/
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$MODE delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, syncobjs, StrUtils, //NetEncoding, Hash,
  DateUtils,
  mnUtils, mnSockets, mnServers, mnStreams, mnStreamUtils,
  mnFields, mnParams, mnMultipartData, mnModules, mnWebModules, mnWebElements;

type
  TBootstrap_Library = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
  end;

  { TBootstrap_LocalLibrary }

  TBootstrap_LocalLibrary = class(TmnwLibrary)
  public
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
  end;

  { TmnwBootstrap }

  { TmnwBootstrapRenderer }

  TmnwBootstrapRenderer = class(TmnwHTMLRenderer)
  public
    type

    { TBSDocumentHTML }

    TDocument = class(TmnwHTMLRenderer.TDocument)
    public
      procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
    end;

    TMain = class abstract(THTMLElement)
    protected
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
    end;

    TRow = class(TmnwHTMLRenderer.THTMLElement)
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
    end;

    TColumn = class(TmnwHTMLRenderer.THTMLElement)
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
    end;

    TCard = class abstract(THTMLElement)
    protected
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn); override;
    end;

  protected
    procedure Created; override;
    procedure AddHead(AElement: TmnwElement; const Context: TmnwContext); override;
  public

    class constructor RegisterObjects;
  end;

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSContentAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;

implementation

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  if Align = alignStart then
    Result := 'align-self-start'

  else if Align = alignCenter then
    Result := 'align-self-center'
  else if Align = alignStreach then
    Result := 'align-self-Streach'
  else if Align = alignEnd then
    Result := 'align-self-end'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSContentAlignToStr(Align: TmnwAlign; WithSpace: Boolean): string;
begin
  if Align = alignStart then
    Result := 'justify-content-start'
  else if Align = alignCenter then
    Result := 'justify-content-center'
  else if Align = alignStreach then
    Result := 'justify-content-streach'
  else if Align = alignEnd then
    Result := 'justify-content-end'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;
begin
  if Fixed = fixedTop then
    Result := 'fixed-top'
  else if Fixed = fixedBottom then
    Result := 'fixed-bottom'
  else
    Result := '';
  if (Result <> '') and WithSpace then
    Result := ' ' + Result;
end;

function BSSizeToStr(Size: TSize; WithSpace: Boolean = True): string;
begin
  case Size of
	  szVerySmall: Result := 'xm';
		szSmall: Result := 'sm';
		szNormal: Result := 'md';
		szLarge: Result := 'lg';
		szVeryLarge: Result := 'xl';
  end;
end;

{ TmnwBootstrapRenderer }

procedure TmnwBootstrapRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary('Bootstrap', False, TBootstrap_Library);
  Libraries.RegisterLibrary('Bootstrap', True, TBootstrap_LocalLibrary);
  Libraries.Use('Bootstrap');
end;

procedure TmnwBootstrapRenderer.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  inherited;
  Context.Writer.WriteLn('<style type="text/css">', [woOpenIndent]);
  Context.Writer.WriteLn('.small-card {');
  Context.Writer.WriteLn('    max-width: 22rem;');
  Context.Writer.WriteLn('}');
  Context.Writer.WriteLn('</style>', [woCloseIndent]);
end;

class constructor TmnwBootstrapRenderer.RegisterObjects;
begin
  RegisterRenderer(THTML.TDocument, TDocument, Replace);
  RegisterRenderer(THTML.TMain, TMain, Replace);
  RegisterRenderer(THTML.TRow, TRow, Replace);
  RegisterRenderer(THTML.TColumn, TColumn, Replace);
  RegisterRenderer(THTML.TCard, TCard, Replace);
end;

{ TmnwBootstrapRenderer.TBSInputHTML }

procedure TmnwBootstrapRenderer.TDocument.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<meta charset="UTF-8">');
  Context.Writer.WriteLn('<meta name="viewport" content="width=device-width, initial-scale=1">');
  inherited;
end;

{ TBootstrap_Library }

procedure TBootstrap_Library.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  if AElement.Schema.Direction = dirRightToLeft then
    Context.Writer.WriteLn('<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.rtl.min.css" integrity="sha384-dpuaG1suU0eT09tx5plTaGMLBsfDLzUCCUXOY2j/LSvXYuG6Bqs43ALlhIqAJVRb" crossorigin="anonymous">')
  else
    Context.Writer.WriteLn('<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>');
end;

{ TBootstrap_LocalLibrary }

procedure TBootstrap_LocalLibrary.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  if AElement.Schema.Direction = dirRightToLeft then
    Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.rtl.min.css" crossorigin="anonymous">')
  else
    Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.min.css" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.bundle.min.js" crossorigin="anonymous"></script>');
end;

{ TmnwBootstrapRenderer.TColumn }

procedure TmnwBootstrapRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Writer.WriteLn('<div class="col-md-'+e.Size.ToString + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align) + '"' + Scope.Attributes.GetText + '>', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ TmnwBootstrapRenderer.TMain }

procedure TmnwBootstrapRenderer.TMain.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TMain;
begin
  e := Scope.Element as THTML.TMain;
  Context.Writer.OpenTag('div class="row flex-nowrap"');
  //Scope.Classes.Add('main');
  if e.Margin > 0 then
    Scope.Classes.Add('m-'+e.Margin.ToString);
  if (e.Parent.Parent as THTML.TBody).SideBar.CanRender then
    Scope.Classes.Add('col-9');
  //Scope.Classes.Add('d-flex');
  Scope.Classes.Add('flex-nowrap');
  Scope.Classes.Add('justify-content-center');
//container-fluid for full width, container not full width
  Context.Writer.WriteLn('<main'+Scope.GetText+'>', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</main>', [woCloseIndent]);
  Context.Writer.CloseTag('div');
end;

{ TmnwBootstrapRenderer.TCard }

//https://disjfa.github.io/bootstrap-tricks/card-collapse-tricks/
//https://bootstrapbrain.com/tutorial/bootstrap-accordion-with-plus-minus-icon/

procedure TmnwBootstrapRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');
  Scope.Classes.Add(BSFixedToStr(e.Fixed));
  Scope.Classes.Add(BSAlignToStr(e.Align));
  //Scope.Classes.Add('col-12');
  //Scope.Classes.Add('col-lg-6');
  if e.Shadow then
    Scope.Classes.Add('shadow-sm');
  if SameText(e.Style, 'center') then
  begin
    Scope.Classes.Add('ms-auto');
    Scope.Classes.Add('me-auto');
  end;

  Context.Writer.WriteLn('<div' + Scope.GetText + '>', [woOpenIndent]);
  if e.Caption <> '' then
  begin
//    Context.Writer.WriteLn('<h5 class="card-header" id="'+e.id+'-header">', [woOpenIndent]);
    Context.Writer.Write('<h5 class="card-header" id="'+e.id+'-header"');
    if e.Collapse then
      Context.Writer.Write(' role="button" data-bs-toggle="collapse" data-bs-target="#'+e.id+'-body" aria-expanded="true" aria-controls="'+e.id+'-body"');
    Context.Writer.Write('>', [woOpenIndent]);
    Context.Writer.Write(e.Caption);
    if e.Collapse then
    begin
      Context.Writer.Write('<span class="icons float-right fa fa-arrow-alt-circle-up"></span>', [woOpenIndent, woCloseIndent]);
    end;
    Context.Writer.WriteLn('</h5>', [woCloseIndent]);
  end;

  Context.Writer.WriteLn('<div class="card-body collapse show" aria-labelledby="'+e.id+'-header" id="'+e.id+'-body">', [woOpenIndent]);
//  collapse
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

{ TmnwBootstrapRenderer.TRow }

procedure TmnwBootstrapRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var AReturn: TmnwReturn);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Scope.Classes.Add(BSContentAlignToStr(e.ContentAlign));
  Context.Writer.WriteLn('<div class="row' + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align) + '">', [woOpenIndent]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseIndent]);
end;

end.

