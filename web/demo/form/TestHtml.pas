unit TestHtml;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, RTTI, TypInfo, StrUtils, Classes,
  mnWebElements;

type

  { TmyHtml }

  TmyHome = class(THTML)
  private
  public
    type
      TMyTag = class(TmnwElement)
      private
      public
      end;

  protected
    procedure Created; override;
    procedure DoCompose; override;
  public
  end;

  TmyHomeRenderer = class(TmnwHTMLRenderer)
  public
    type
      TMyTagHTML = class(TmnwElementRenderer)
      public
        procedure DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer); override;
      end;

  protected
    procedure Created; override;
  end;

procedure Run;

implementation

{ TmyHome }

procedure TmyHomeRenderer.Created;
begin
  inherited;
  RegisterRenderer(TmyHome.TMyTag, TMyTagHTML);
end;

{ TmyHome.TMyTagHTML }

procedure TmyHomeRenderer.TMyTagHTML.DoRender(AElement: TmnwElement; Attributes: TmnwAttributes; Context: TmnwContext; vLevel: Integer);
begin
  Context.Output.Write('html', '<'+AElement.Name+'>', [cboEndLine]);
  inherited;
  Context.Output.Write('html', '</'+AElement.Name+'>', [cboEndLine]);
end;

{ TmyHome }

procedure TmyHome.DoCompose;
begin
  Name := 'HelloWorld';
  with This.Add<TDocument>('html') do
  begin
    Title := 'MyHome';
    Direction := dirLTR;
    with This.Add<TPage>('page') do
    begin
      with This.Add<TMyTag>('MyTag1') do
      begin
      end;
    end;
  end;
end;

procedure TmyHome.Created;
begin
  inherited;
end;

procedure Run;
var
  HTML: TmnwSchema;
  Strings: TStringList;
  s: string;
begin
  Strings := TStringList.Create;
  try
    HTML:= TmyHome.Create;
    HTML.Compose;
    HTML.Render(TmyHomeRenderer, Strings);
    for s in Strings do
      WriteLn(s);
  finally
    FreeAndNil(Strings);
  end;
  WriteLn( 'Press Enter to exit');
  ReadLn;
end;

end.
