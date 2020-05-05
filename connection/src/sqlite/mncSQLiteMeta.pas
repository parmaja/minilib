unit mncSQLiteMeta;
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
  SysUtils, Classes, mnUtils,
  mncMeta, mncConnections, mncSQLite;

type
  { TmncSQLiteMeta }

  TmncSQLiteMeta = class(TmncSQLMeta)
  private
    function GetSession: TmncSession;
    procedure SetSession(AValue: TmncSession);
  protected
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TmetaEnumOptions): string;
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions =[]); override;
    procedure EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); override;
    procedure EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumDomains(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumConstraints(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); override;
    procedure EnumTriggers(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); override;
    procedure EnumIndices(Meta: TmncMetaItems; SQLName: string = ''; Options: TmetaEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; SQLName: string; Options: TmetaEnumOptions = []); override;
    procedure GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); override;
  end;

implementation

uses
  mncDB, mncSQL;

function TmncSQLiteMeta.GetSession: TmncSession;
begin
  Result := Link as TmncSession;
end;

procedure TmncSQLiteMeta.SetSession(AValue: TmncSession);
begin
  inherited Link := AValue;
end;

procedure TmncSQLiteMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string; Fields: array of string);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
  i: Integer;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := Meta.Add(aCMD.Field['name'].AsString);
      aItem.Kind := vKind;
      for i := Low(Fields) to High(Fields) do
        aItem.Attributes.Add(Fields[i], aCMD.Field[Fields[i]].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string);
begin
  EnumCMD(Meta, vKind, SQL, []);
end;

procedure TmncSQLiteMeta.FetchCMD(Strings: TStringList; SQL: string);
var
  aCMD: TmncSQLCommand;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      Strings.Add(aCMD.Field['name'].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

function TmncSQLiteMeta.GetSortSQL(Options: TmetaEnumOptions): string;
begin
  if ekSort in Options then
    Result := ' order by name'
  else
    Result := '';
end;

procedure TmncSQLiteMeta.EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions);
var
  i: Integer;
  files: TStringList;
begin
  files := TStringList.Create;
  try
    EnumFiles(files, ServerInfo.Host, '*.sqlite');
    for i := 0 to files.Count -1 do
      Meta.Add(files[i]);
  finally
    files.Free;
  end;
end;

procedure TmncSQLiteMeta.EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
  EnumCMD(Meta, sokTable, 'select name from sqlite_master where type = ''table''' + GetSortSQL(Options));
end;

procedure TmncSQLiteMeta.EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
  EnumCMD(Meta, sokView, 'select name from sqlite_master where type = ''view'''+ GetSortSQL(Options));
end;

procedure TmncSQLiteMeta.EnumProcedures(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumSequences(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumFunctions(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumExceptions(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumDomains(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumConstraints(Meta: TmncMetaItems;
  SQLName: string; Options: TmetaEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumTriggers(Meta: TmncMetaItems;
  SQLName: string; Options: TmetaEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''trigger''';
  if SQLName <> '' then
    s := s + ' and tbl_name = ''' +SQLName+ '''';
  s := s +  GetSortSQL(Options);
  EnumCMD(Meta, sokTrigger, s);
end;

procedure TmncSQLiteMeta.EnumIndices(Meta: TmncMetaItems; SQLName: string;
  Options: TmetaEnumOptions);
var
  s: string;
begin
  s := '';
  if SQLName <> '' then
  begin
    s := s + 'PRAGMA index_list('''+ SQLName +''')' + GetSortSQL(Options);
    EnumCMD(Meta, sokIndex, s, ['unique']);
  end
  else
  begin
    s := 'select name from sqlite_master where type = ''index''' + GetSortSQL(Options);
    EnumCMD(Meta, sokIndex, s);
  end;
end;

procedure TmncSQLiteMeta.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TmetaEnumOptions);
var
  s: string;
begin
  s := 'select "sql" as name from sqlite_master where type = ''trigger''';
  s := s + ' and name = ''' +SQLName+ '''';
  FetchCMD(Strings, s);
end;

procedure TmncSQLiteMeta.GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
begin
  aCMD := CreateCMD('PRAGMA index_info('''+ SQLName +''')');
  try
    if aCMD.Execute then
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := 'Name';
      aItem.Attributes.Add('name', SQLName);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'Field';
      aItem.Attributes.Add('field', aCMD.Field['name'].AsString);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'CID';
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsString);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'Sequence NO';
      aItem.Attributes.Add('seqno',  aCMD.Field['seqno'].AsString);
      Meta.Add(aItem);
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncSQLiteMeta.EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
begin
  aCMD := CreateCMD('pragma table_info(''' + (SQLName) + ''')' + GetSortSQL(Options));
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := aCMD.Field['name'].AsString;
      aItem.Attributes.Add('type', aCMD.Field['type'].AsString);
      aItem.Attributes.Add('pk', IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add('notnull', IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add('dflt_value', aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add('cid', aCMD.Field['cid'].AsString);
      Meta.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

initialization
  Engines.RegisterMeta(TmncSQLiteConnection.EngineName, TmncSQLiteMeta);
end.
