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
  TmyHtml = class(TmnwSchema)
  private
  public
    type
      TMyTag = class(TmnwElement)
      private
      public
      end;

  protected
    procedure Created;
    procedure Compose; override;
  end;

  TmyRendererHtml = class(TmnwRendererHTML)
  public
    type
      TMyTagHTML = class(TmnwElementRenderer)
      public
        procedure DoRender(AElement: TmnwElement; AContext: TmnwContext; vLevel: Integer); override;
      end;

  protected
    procedure Created; override;
  end;

procedure Run;

implementation

{ TmyHtml }

procedure TmyRendererHtml.Created;
begin
  inherited;
  RegisterRenderer(TmyHTML.TMyTag, TMyTagHTML);
end;

{ TmyHtml.TMyTagHTML }

procedure TmyRendererHtml.TMyTagHTML.DoRender(AElement: TmnwElement; AContext: TmnwContext; vLevel: Integer);
begin
  AContext.Writer.Write('<'+AElement.Name+'>', [cboEndLine]);
  inherited;
  AContext.Writer.Write('</'+AElement.Name+'>', [cboEndLine]);
end;

{ TmyHtml }

procedure TmyHtml.Compose;
begin
  with This.Add<TmnwHTML.TDocument> do
  begin
    with This.Add<TmnwHTML.TPage> do
    begin
      with This.Add<TmyHtml.TMyTag>('MyTag1') do
      begin
      end;
    end;
  end;
end;

procedure TmyHtml.Created;
begin
  inherited;
end;

procedure Run;
var
  Schema: TmnwSchema;
  Strings: TStringList;
  s: string;
begin
  Strings := TStringList.Create;
  try
    Schema := TmyHtml.Create('HelloWorld');
    Schema.Render(TmyRendererHtml, Strings);
    for s in Strings do
      WriteLn(s);
  finally
    FreeAndNil(Strings);
  end;
  WriteLn( 'Press Enter to exit');
  ReadLn;
end;

end.
