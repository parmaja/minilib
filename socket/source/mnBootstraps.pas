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
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TBootstrap_LocalLibrary = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TBootstrapIcons_Library = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  TBootstrapIcons_LocalLibrary = class(TmnwLibrary)
  public
    procedure AddHead(const Context: TmnwContext); override;
  end;

  { TmnwBootstrapRenderer }

  TmnwBootstrapRenderer = class(TmnwHTMLRenderer)
  public
  protected
    procedure Created; override;
  public
    procedure AddHead(const Context: TmnwContext); override;
    class constructor RegisterObjects;
  end;

implementation

{ TmnwBootstrapRenderer }

procedure TmnwBootstrapRenderer.Created;
begin
  inherited;
  Libraries.RegisterLibrary('Bootstrap', False, TBootstrap_Library);
  Libraries.RegisterLibrary('BootstrapIcons', False, TBootstrapIcons_Library);
  Libraries.RegisterLibrary('Bootstrap', True, TBootstrap_LocalLibrary);
  Libraries.RegisterLibrary('BootstrapIcons', True, TBootstrapIcons_LocalLibrary);
  Libraries.Use('Bootstrap');
end;

procedure TmnwBootstrapRenderer.AddHead(const Context: TmnwContext);
begin
  inherited;
(*  Context.Writer.WriteLn('<style type="text/css">', [woOpenIndent]);
  Context.Writer.WriteLn('.small-card {');
  Context.Writer.WriteLn('    max-width: 22rem;');
  Context.Writer.WriteLn('}');
  Context.Writer.WriteLn('</style>', [woCloseIndent]); *)
end;

class constructor TmnwBootstrapRenderer.RegisterObjects;
begin
end;

{ TBootstrap_Library }

procedure TBootstrap_Library.AddHead(const Context: TmnwContext);
begin
  if Context.Schema.Direction = dirRightToLeft then
    Context.Writer.WriteLn('<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.rtl.min.css" integrity="sha384-dpuaG1suU0eT09tx5plTaGMLBsfDLzUCCUXOY2j/LSvXYuG6Bqs43ALlhIqAJVRb" crossorigin="anonymous">')
  else
    Context.Writer.WriteLn('<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" integrity="sha384-QWTKZyjpPEjISv5WaRU9OFeRpok6YctnYmDr5pNlyT2bRjXh0JMhjY6hW+ALEwIH" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js" integrity="sha384-YvpcrYf0tY3lHB60NNkmXc5s9fDVZLESaAA55NDzOxhy9GkcIdslK1eN7N6jIeHz" crossorigin="anonymous"></script>');
end;

{ TBootstrap_LocalLibrary }

procedure TBootstrap_LocalLibrary.AddHead(const Context: TmnwContext);
begin
  if Context.Schema.Direction = dirRightToLeft then
    Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.rtl.min.css?v='+IntToStr(Context.Schema.TimeStamp)+'" crossorigin="anonymous">')
  else
    Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.min.css?v='+IntToStr(Context.Schema.TimeStamp)+'" crossorigin="anonymous">');
  Context.Writer.WriteLn('<script src="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap.bundle.min.js?v='+IntToStr(Context.Schema.TimeStamp)+'" crossorigin="anonymous"></script>');
end;

{ TBootstrapIcons_Library }

procedure TBootstrapIcons_Library.AddHead(const Context: TmnwContext);
begin
  inherited;
  Context.Writer.WriteLn('<link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bootstrap-icons@1.11.3/font/bootstrap-icons.min.css">');
end;

{ TBootstrapIcons_LocalLibrary }

procedure TBootstrapIcons_LocalLibrary.AddHead(const Context: TmnwContext);
begin
  inherited;
  Context.Writer.WriteLn('<link rel="stylesheet" href="' + IncludeURLDelimiter(Context.Schema.App.GetAssetsURL) + 'bootstrap-icons.min.css?v='+IntToStr(Context.Schema.TimeStamp)+'" crossorigin="anonymous">');
end;

end.

