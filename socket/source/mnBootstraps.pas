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

    TContainer = class abstract(TElementHTML)
    protected
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult); override;
    end;

    TRow = class(TmnwHTMLRenderer.TElementHTML)
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult); override;
    end;

    TColumn = class(TmnwHTMLRenderer.TElementHTML)
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult); override;
    end;

    TCard = class abstract(TElementHTML)
    protected
    public
      procedure DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult); override;
    end;

  protected
    procedure Created; override;
  public

    class constructor RegisterObjects;
  end;

function BSAlignToStr(Align: TmnwAlign; WithSpace: Boolean = True): string;
function BSFixedToStr(Fixed: TmnwFixed; WithSpace: Boolean = True): string;

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

{ TmnwBootstrapRenderer }

procedure TmnwBootstrapRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary('Bootstrap', False, TBootstrap_Library);
  Libraries.RegisterLibrary('Bootstrap', True, TBootstrap_LocalLibrary);
  Libraries.Use('Bootstrap');
end;

class constructor TmnwBootstrapRenderer.RegisterObjects;
begin
  RegisterRenderer(THTML.TDocument, TDocument, Replace);
  RegisterRenderer(THTML.TContainer, TContainer, Replace);
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
  Context.Writer.WriteLn('<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>');
end;

{ TBootstrap_LocalLibrary }

procedure TBootstrap_LocalLibrary.AddHead(AElement: TmnwElement; const Context: TmnwContext);
begin
  Context.Writer.WriteLn('<link href="' + IncludeURLDelimiter(Context.Renderer.GetAssetsURL) + 'bootstrap.min.css" rel="stylesheet" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="' + IncludeURLDelimiter(Context.Renderer.GetAssetsURL) + 'bootstrap.bundle.min.js" crossorigin="anonymous"></script>');
end;

{ TmnwBootstrapRenderer.TColumn }

procedure TmnwBootstrapRenderer.TColumn.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Writer.WriteLn('<div class="col-md-'+e.Size.ToString + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align) + '"' + Scope.Attributes.GetText + '>', [woOpenTag]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseTag]);
end;

{ TmnwBootstrapRenderer.TContainer }

procedure TmnwBootstrapRenderer.TContainer.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult);
var
  e: THTML.TContainer;
begin
  e := Scope.Element as THTML.TContainer;
//container-fluid for full width, container not full width
  Context.Writer.WriteLn('<div class="container-fluid mt-'+e.Margin.ToString+'"'+Scope.Attributes.GetText+'>', [woOpenTag]);
  Context.Writer.WriteLn('<main>', [woOpenTag]);
  inherited;
  Context.Writer.WriteLn('</main>', [woCloseTag]);
  //Context.Writer.WriteLn('</div>', [woCloseTag]);
  Context.Writer.WriteLn('</div>', [woCloseTag]);
end;

{ TmnwBootstrapRenderer.TCard }


//https://disjfa.github.io/bootstrap-tricks/card-collapse-tricks/
//https://bootstrapbrain.com/tutorial/bootstrap-accordion-with-plus-minus-icon/

procedure TmnwBootstrapRenderer.TCard.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult);
var
  e: THTML.TCard;
begin
  e := Scope.Element as THTML.TCard;
  Scope.Classes.Add('card');
  Scope.Classes.Add(BSFixedToStr(e.Fixed));
  Scope.Classes.Add(BSAlignToStr(e.Align));

  Context.Writer.WriteLn('<div' + Scope.GetText + '>', [woOpenTag]);
  if e.Caption <> '' then
  begin
//    Context.Writer.WriteLn('<h5 class="card-header" id="'+e.id+'-header">', [woOpenTag]);
    Context.Writer.Write('<h5 class="card-header" id="'+e.id+'-header"');
    if e.Collapse then
      Context.Writer.Write('role="button" data-bs-toggle="collapse" data-bs-target="#'+e.id+'-body" aria-expanded="true" aria-controls="'+e.id+'-body"');
    Context.Writer.Write('>', [woOpenTag]);
    Context.Writer.Write(e.Caption);
    if e.Collapse then
    begin
      Context.Writer.Write('<span class="icons float-right fa fa-arrow-alt-circle-up"></span>', [woOpenTag, woCloseTag]);
    end;
    Context.Writer.WriteLn('</h5>', [woCloseTag]);
  end;

  Context.Writer.WriteLn('<div class="card-body collapse show" aria-labelledby="'+e.id+'-header" id="'+e.id+'-body">', [woOpenTag]);
//  collapse
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseTag]);
  Context.Writer.WriteLn('</div>', [woCloseTag]);
end;

{ TmnwBootstrapRenderer.TRow }

procedure TmnwBootstrapRenderer.TRow.DoInnerRender(Scope: TmnwScope; Context: TmnwContext; var ARespondResult: TmnwRespondResult);
var
  e: THTML.TRow;
begin
  e := Scope.Element as THTML.TRow;
  Context.Writer.WriteLn('<div class="row' + BSFixedToStr(e.Fixed) + BSAlignToStr(e.Align) + '">', [woOpenTag]);
  inherited;
  Context.Writer.WriteLn('</div>', [woCloseTag]);
end;

end.

