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
  SysUtils, Classes,
  mncSchemas, mncConnections, mncSQLite;

type
  { TmncSQLiteSchema }

  TmncSQLiteSchema = class(TmncSchema)
  private
  protected
    function CreateCMD(SQL: string): TmncSQLiteCommand;
    procedure EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TschmEnumOptions):string;
  public
    procedure EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Schema: TmncSchemaItems; MemberName: string; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumDomains(Schema: TmncSchemaItems; Options: TschmEnumOptions = []); override;
    procedure EnumConstraints(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndices(Schema: TmncSchemaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; MemberName: string; Options: TschmEnumOptions = []); override;
    procedure GetIndexInfo(Schema: TmncSchemaItems; MemberName: string; Options: TschmEnumOptions = []);
  end;

implementation

{ TmncSchemaItems }

function TmncSQLiteSchema.CreateCMD(SQL: string): TmncSQLiteCommand;
begin
  Result := TmncSQLiteCommand.CreateBy(Session);
  Result.SQL.Text := SQL;
end;

procedure TmncSQLiteSchema.EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string; Fields: array of string);
var
  aCMD: TmncSQLiteCommand;
  aItem: TmncSchemaItem;
  i: Integer;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := Schema.Add(aCMD.Field['name'].AsString);
      aItem.Kind := vKind;
      for i := Low(Fields) to High(Fields) do
        aItem.Attributes.Add(Fields[i], aCMD.Field[Fields[i]].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteSchema.EnumCMD(Schema: TmncSchemaItems; vKind: TschmKind; SQL: string);
begin
  EnumCMD(Schema, vKind, SQL, []);
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

function TmncSQLiteSchema.GetSortSQL(Options: TschmEnumOptions): string;
begin
  if ekSort in Options then
    Result := ' order by name'
  else
    Result := '';
end;

procedure TmncSQLiteSchema.EnumTables(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
  EnumCMD(Schema, sokTable, 'select name from sqlite_master where type = ''table''' + GetSortSQL(Options));
end;

procedure TmncSQLiteSchema.EnumViews(Schema: TmncSchemaItems; Options: TschmEnumOptions);
begin
  EnumCMD(Schema, sokView, 'select name from sqlite_master where type = ''view'''+ GetSortSQL(Options));
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

procedure TmncSQLiteSchema.EnumDomains(Schema: TmncSchemaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumConstraints(Schema: TmncSchemaItems;
  MemberName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteSchema.EnumTriggers(Schema: TmncSchemaItems;
  MemberName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''trigger''';
  if MemberName <> '' then
    s := s + ' and tbl_name = ''' +MemberName+ '''';
  s := s +  GetSortSQL(Options);
  EnumCMD(Schema, sokTrigger, s);
end;

procedure TmncSQLiteSchema.EnumIndices(Schema: TmncSchemaItems; MemberName: string;
  Options: TschmEnumOptions);
var
  s: string;
begin
  s := '';
  if MemberName <> '' then
  begin
    s := s + 'PRAGMA index_list('''+ MemberName +''')' + GetSortSQL(Options);
    EnumCMD(Schema, sokIndex, s, ['unique']);
  end
  else
  begin
    s := 'select name from sqlite_master where type = ''index''' + GetSortSQL(Options);
    EnumCMD(Schema, sokIndex, s);
  end;
end;

procedure TmncSQLiteSchema.GetTriggerSource(Strings: TStringList; MemberName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select "sql" as name from sqlite_master where type = ''trigger''';
  s := s + ' and name = ''' +MemberName+ '''';
  FetchCMD(Strings, s);
end;

procedure TmncSQLiteSchema.GetIndexInfo(Schema: TmncSchemaItems; MemberName: string; Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  aItem: TmncSchemaItem;
begin
  aCMD := CreateCMD('PRAGMA index_info('''+ MemberName +''')');
  try
    if aCMD.Execute then
    begin
      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Name';
      aItem.Attributes.Add('name', MemberName);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Field';
      aItem.Attributes.Add('field', aCMD.Field['name'].AsString);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'CID';
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsString);
      Schema.Add(aItem);

      aItem := TmncSchemaItem.Create;
      aItem.Name := 'Sequence NO';
      aItem.Attributes.Add('seqno',  aCMD.Field['seqno'].AsString);
      Schema.Add(aItem);
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncSQLiteSchema.EnumFields(Schema: TmncSchemaItems; MemberName: string; Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  aItem: TmncSchemaItem;
begin
  aCMD := CreateCMD('pragma table_info(''' + (MemberName) + ''')' + GetSortSQL(Options));
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := TmncSchemaItem.Create;
      aItem.Name := aCMD.Field['name'].AsString;
      aItem.Attributes.Add('type', aCMD.Field['type'].AsString);
      aItem.Attributes.Add('pk', IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add('notnull', IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add('dflt_value', aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsString);
      Schema.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

end.

