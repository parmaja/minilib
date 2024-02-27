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
  TmyHtml = class(TmnwHTML)
  public
    type
      TMyTag = class(TmnwElement)
      private
      public
      end;

      TMyTagHTML = class(TmnwRenderer)
      public
        function DoRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer): Boolean; override;
      end;

    procedure Created; override;
  end;

procedure Run;

implementation

function CreateDocument(SchemaClass: TmnwSchemaClass): TmnwSchema;
begin
  if SchemaClass = nil then
    Result := nil
  else
  begin
    Result := SchemaClass.Create('HelloWorld');
    with Result do
    begin
      with This.Add<TmnwSchema.TDocument> do
      begin
        with This.Add<TmnwSchema.TPage> do
        begin
        end;
        with This.Add<TmyHtml.TMyTag>('MyTag1') do
        begin
        end;
      end;
    end;
  end;
end;

procedure Run;
var
  Schema: TmnwSchema;
  Strings: TStringList;
  s: string;
begin
  Strings := TStringList.Create;
  try
    Schema := CreateDocument(TmyHtml);
    Schema.Render(Strings);
    for s in Strings do
      WriteLn(s);
  finally
    FreeAndNil(Strings);
  end;
  WriteLn( 'Press Enter to exit');
  ReadLn;
end;

{ TmyHtml }

procedure TmyHtml.Created;
begin
  inherited;
  RegisterRenderer(TMyTag, TMyTagHTML);
end;

{ TmyHtml.TMyTagHTML }

function TmyHtml.TMyTagHTML.DoRender(AElement: TmnwElement; WriterObject: TmnwWriterObject; vLevel: Integer): Boolean;
begin
  WriterObject.Write('<'+AElement.Name+'>', [cboEndLine]);
  inherited;
  WriterObject.Write('</'+AElement.Name+'>', [cboEndLine]);
end;

end.
