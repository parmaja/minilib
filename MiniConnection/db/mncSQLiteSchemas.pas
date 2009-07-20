unit mncSQLiteSchemas;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, contnrs,
  mncSQL, mncSchemas, mncConnections, mncSQLite;

type
  { TmncSQLiteSchema }

  TmncSQLiteSchema = class(TmncSchema)
  private
  protected
    function CreateCMD(SQL: string): TmncSQLiteCommand;
    procedure EnumCMD(Schema: TmncSchemaItems; SQL: string);//use field 'name' and 'comment'
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
  public
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumTypes(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumConstraints(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndexes(Schema: TmncSchemaItems; RelationName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; RelationName: string; Options: TschmEnumOptions = []); override;
  end;

implementation

{ TmncSchemaItems }

function TmncSQLiteSchema.CreateCMD(SQL: string): TmncSQLiteCommand;
begin
  Result := TmncSQLiteCommand.Create(Session);
  Result.SQL.Text := SQL;
end;

procedure TmncSQLiteSchema.EnumCMD(Schema: TmncSchemaItems; SQL: string);
var
  aCMD: TmncSQLiteCommand;
  aComment: string;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      if aCMD.FieldIsExist('comment') then
        aComment := aCMD.Field['comment'].AsString;
      Schema.Add(aCMD.Field['name'].AsString, aComment);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteSchema.FetchCMD(Strings: TStringList; SQL: string);
var
  aCMD: TmncSQLiteCommand;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      Strings.Add(aCMD.Field['name'].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteSchema.EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
  EnumCMD(Schema, 'select name from sqlite_master where type = ''table''');
end;

procedure TmncSQLiteSchema.EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions
  );
begin

end;

procedure TmncSQLiteSchema.EnumProcedures(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumSequences(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumFunctions(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumExceptions(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumTypes(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumConstraints(Schema: TmncSchemaItems;
  RelationName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumTriggers(Schema: TmncSchemaItems;
  RelationName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''trigger''';
  if RelationName <> '' then
    s := s + ' and tbl_name = ''' +RelationName+ '''';
  EnumCMD(Schema, s);
end;

procedure TmncSQLiteSchema.EnumIndexes(Schema: TmncSchemaItems; RelationName: string;
  Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''index''';
  if RelationName <> '' then
    s := s + ' and tbl_name = ''' +RelationName+ '''';
  EnumCMD(Schema, s);
end;

procedure TmncSQLiteSchema.GetTriggerSource(Strings: TStringList; RelationName: string; Options: TschmEnumOptions = []);
var
  s: string;
begin
  s := 'select "sql" as name from sqlite_master where type = ''trigger''';
  s := s + ' and name = ''' +RelationName+ '''';
  FetchCMD(Strings, s);
end;

procedure TmncSQLiteSchema.EnumFields(Schema: TmncSchemaItems; RelationName: string;
  Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  i: Integer;
  aItem: TmncSchemaItem;
begin
  aCMD := CreateCMD('pragma table_info(''' + (RelationName) + ''')');
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := TmncSchemaItem.Create;
      aItem.Name := aCMD.Field['name'].AsString;
      aItem.Attributes.Add(aCMD.Field['type'].AsString);
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add(aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add(aCMD.Field['cid'].AsString);
      Schema.Add(aItem);
      aCMD.Next;
    end;
  finally
  end;
end;

end.

