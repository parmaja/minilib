program project1;
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

uses
  SysUtils, StrUtils, Classes,
  mnWebElements;

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
        with This.Add<TmnwSchema.TParagraph> do
        begin
        end;
      end;
    end;
  end;
end;

var
  Schema: TmnwSchema;
  Strings: TStringList;
  s: string;
begin
  Strings:=TStringList.Create;
  try
    Schema := CreateDocument(TmnwHTML);
    Schema.Render(Strings);
    for s in Strings do
      WriteLn(s);
  finally
    FreeAndNil(Strings);
  end;
  WriteLn('Press Enter to exit');
  ReadLn;
end.

