unit mncSQLiteMeta;
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
  SysUtils, Classes, mnUtils,
  mncMeta, mncConnections, mncSQLite;

type
  { TmncSQLiteMeta }

  TmncSQLiteMeta = class(TmncSQLMeta)
  private
    function GetSession: TmncSession;
    procedure SetSession(AValue: TmncSession);
  protected
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions =[]); override;
    procedure EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); override;
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

procedure TmncSQLiteMeta.EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
  EnumCMD(Session, Meta, sokTable, 'name', 'Table', 'select name from sqlite_master where type = ''table''' + GetSortSQL(Options), []);
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
    EnumCMD(Session, Meta, sokIndex, 'name', 'Index', s, ['unique']);
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
  FetchCMD(Strings, 'name', s);
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
      aItem.Kind := sokField;
      aItem.SQLName := aItem.Name;
      aItem.SQLType := 'Field';

      aItem.Definitions['Type'] := 'Field';
      aItem.Definitions['Field'] := aItem.Name;

      aItem.Attributes.Add('type', aCMD.Field['type'].AsString);
      aItem.Attributes.Add('nullable', IntToStr(ord(aCMD.Field['notnull'].AsInteger = 0)));
      aItem.Attributes.Add('primary', IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
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
