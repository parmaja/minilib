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

  { TmnwBootstrap }

  TmnwBootstrap = class(THTML)
  public
    procedure Created; override;
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

  public
    procedure Created; override;
  end;

implementation

{ TmnwBootstrap }

procedure TmnwBootstrap.Created;
begin
  inherited;
end;

{ TmnwBootstrapRenderer }

procedure TmnwBootstrapRenderer.Created;
begin
  inherited;
  RegisterRenderer(THTML.TDocument, TDocument);
end;

{ TmnwBootstrapRenderer.TBSInputHTML }

procedure TmnwBootstrapRenderer.TDocument.AddHead(AElement: TmnwElement; Context: TmnwContext);
begin
  Context.Output.WriteLn('html', '<meta charset="UTF-8">');
  Context.Output.WriteLn('html', '<meta name="viewport" content="width=device-width, initial-scale=1">');
  Context.Output.WriteLn('html', '<link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet" integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">');
  Context.Output.WriteLn('html', '<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>');
  inherited;
end;

end.

