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
  TmodBootstrapModule = class(TmodWebModule)
  public
    CachePath: string;
    HomeURL: string;
    HostURL: string;
  end;

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

    TBSDocumentHTML = class(TDocumentHTML)
    public
      procedure AddHeader(AElement: TmnwElement; Context: TmnwContext); override;
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
  RegisterRenderer(THTML.TDocument, TBSDocumentHTML, True);
end;

{ TmnwBootstrapRenderer.TBSInputHTML }

procedure TmnwBootstrapRenderer.TBSDocumentHTML.AddHeader(AElement: TmnwElement; Context: TmnwContext);
begin
  Context.Output.Write('html', '<link href="https://stackpath.bootstrapcdn.com/bootstrap/4.5.2/css/bootstrap.min.css" rel="stylesheet">', [woEndLine]);
  Context.Output.Write('html', '<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/js/bootstrap.bundle.min.js"></script>', [woEndLine]);
  inherited;
end;

end.

