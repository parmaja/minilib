unit mnBootstraps;
{**
 *  This file is part of the "Mini Library"
 *
 * @license   modifiedLGPL (modified of mod://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
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
    procedure AddHead(AElement: TmnwElement; Context: TmnwContext); override;
  end;

  { TmnwBootstrap }

  TmnwBootstrapHTML = class(THTML)
  public
  end;

  { TmnwBootstrapRenderer }

  TmnwBootstrapRenderer = class(TmnwHTMLRenderer)
  public
    type
    { TBSDocumentHTML }

    TDocument = class(TmnwHTMLRenderer.TDocument)
    public
      procedure AddHead(AElement: TmnwElement; Context: TmnwContext); override;
    end;

    TContainer = class abstract(TElementHTML)
    protected
    public
      procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
    end;

    TColumn = class(TmnwHTMLRenderer.TElementHTML)
    public
      procedure DoRender(Scope: TmnwScope; Context: TmnwContext); override;
    end;

  public
    procedure Created; override;
  end;

implementation

{ TmnwBootstrapRenderer }

procedure TmnwBootstrapRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary('Bootstrap', TBootstrap_Library);
  RegisterRenderer(THTML.TDocument, TDocument);
  RegisterRenderer(THTML.TContainer, TContainer, True);
  RegisterRenderer(THTML.TColumn, TColumn, True);
  Libraries.Use('Bootstrap');
end;

{ TmnwBootstrapRenderer.TBSInputHTML }

procedure TmnwBootstrapRenderer.TDocument.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<meta charset="UTF-8">');
  Context.Output.WriteLn('html', '<meta name="viewport" content="width=device-width, initial-scale=1">');
  inherited;
end;

{ TBootstrap_Library }

procedure TBootstrap_Library.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
  inherited;
  Context.Output.WriteLn('html', '<link href="' +GetSource('https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/') + 'bootstrap.min.css" rel="stylesheet" integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">');
  Context.Output.WriteLn('html', '<script src="' + GetSource('https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/')+'bootstrap.bundle.min.js" integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>');
end;

{ TmnwBootstrapRenderer.TColumn }

procedure TmnwBootstrapRenderer.TColumn.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TColumn;
begin
  e := Scope.Element as THTML.TColumn;
  Context.Output.WriteLn('html', '<div class="col-md-'+e.Size.ToString+'">', [woOpenTag]);
  inherited;
  Context.Output.Writeln('html', '</div>', [woCloseTag]);
end;

{ TmnwBootstrapRenderer.TContainer }

procedure TmnwBootstrapRenderer.TContainer.DoRender(Scope: TmnwScope; Context: TmnwContext);
var
  e: THTML.TContainer;
begin
  e := Scope.Element as THTML.TContainer;
  Context.Output.WriteLn('html', '<div class="container mt-'+e.Size.ToString+'">', [woOpenTag]);
  //Context.Output.WriteLn('html', '<div class="col-md-9">', [woOpenTag]);
  Context.Output.WriteLn('html', '<main>', [woOpenTag]);
  inherited;
  Context.Output.WriteLn('html', '</main>', [woCloseTag]);
  //Context.Output.WriteLn('html', '</div>', [woCloseTag]);
  Context.Output.WriteLn('html', '</div>', [woCloseTag]);
end;

end.

