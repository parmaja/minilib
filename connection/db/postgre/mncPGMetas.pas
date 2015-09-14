unit mncPGMetas;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 * @author    Belal Hamed <belalhamed at gmail dot com>  
 * 
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}

interface

uses
  SysUtils, Classes, contnrs,
  mncSQL, mncMetas, mncConnections, mncPostgre;

type
  { TmncSQLiteMeta }

  TmncSQLiteMeta = class(TmncMeta)
  private
  protected
    function CreateCMD(SQL: string): TmncPGCommand;
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TschmEnumOptions):string;
  public
    procedure EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFields(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    //procedure EnumTypes(Meta: TmncMetaItems; Options: TschmEnumOptions = []); override;
    procedure EnumConstraints(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumTriggers(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    procedure EnumIndices(Meta: TmncMetaItems; MemberName: string = ''; Options: TschmEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; MemberName: string; Options: TschmEnumOptions = []); override;
    procedure GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions = []);
  end;

implementation

{ TmncMetaItems }

function TmncSQLiteMeta.CreateCMD(SQL: string): TmncPGCommand;
begin
  Result := TmncPGCommand.Create(Session);
  Result.SQL.Text := SQL;
end;

procedure TmncSQLiteMeta.EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string);
var
  aCMD: TmncSQLiteCommand;
  aItem: TmncMetaItem;
  aComment: string;
  i: Integer;
begin
  aCMD := CreateCMD(SQL);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := Meta.Add(aCMD.Field['name'].AsString);
      for i := Low(Fields) to High(Fields) do
        aItem.Attributes.Add(aCMD.Field[Fields[i]].AsString);
      aCMD.Next;
    end;
  finally
  end;
end;

procedure TmncSQLiteMeta.EnumCMD(Meta: TmncMetaItems; SQL: string);
begin
  EnumCMD(Meta, SQL, []);
end;

procedure TmncSQLiteMeta.FetchCMD(Strings: TStringList; SQL: string);
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

function TmncSQLiteMeta.GetSortSQL(Options: TschmEnumOptions): string;
begin
  if ekSort in Options then
    Result := ' order by name'
  else
    Result := '';
end;

procedure TmncSQLiteMeta.EnumTables(Meta: TmncMetaItems; Options: TschmEnumOptions);
begin
  EnumCMD(Meta, 'select name from sqlite_master where type = ''table'''+ GetSortSQL(Options));
end;

procedure TmncSQLiteMeta.EnumViews(Meta: TmncMetaItems; Options: TschmEnumOptions
  );
begin
  EnumCMD(Meta, 'select name from sqlite_master where type = ''view'''+ GetSortSQL(Options));
end;

procedure TmncSQLiteMeta.EnumProcedures(Meta: TmncMetaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumSequences(Meta: TmncMetaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumFunctions(Meta: TmncMetaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumExceptions(Meta: TmncMetaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumTypes(Meta: TmncMetaItems;
  Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumConstraints(Meta: TmncMetaItems;
  MemberName: string; Options: TschmEnumOptions);
begin

end;

procedure TmncSQLiteMeta.EnumTriggers(Meta: TmncMetaItems;
  MemberName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select name from sqlite_master where type = ''trigger''';
  if MemberName <> '' then
    s := s + ' and tbl_name = ''' +MemberName+ '''';
  s := s +  GetSortSQL(Options);
  EnumCMD(Meta, s);
end;

procedure TmncSQLiteMeta.EnumIndices(Meta: TmncMetaItems; MemberName: string;
  Options: TschmEnumOptions);
var
  s: string;
begin
  if MemberName <> '' then
  begin
    s := s + 'PRAGMA index_list('''+ MemberName +''')' + GetSortSQL(Options);
    EnumCMD(Meta, s, ['unique']);
  end
  else
  begin
    s := 'select name from sqlite_master where type = ''index''' + GetSortSQL(Options);
    EnumCMD(Meta, s);
  end;
end;

procedure TmncSQLiteMeta.GetTriggerSource(Strings: TStringList; MemberName: string; Options: TschmEnumOptions);
var
  s: string;
begin
  s := 'select "sql" as name from sqlite_master where type = ''trigger''';
  s := s + ' and name = ''' +MemberName+ '''';
  FetchCMD(Strings, s);
end;

procedure TmncSQLiteMeta.GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  i: Integer;
  aItem: TmncMetaItem;
begin
  aCMD := CreateCMD('PRAGMA index_info('''+ MemberName +''')');
  try
    if aCMD.Execute then
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := 'Name';
      aItem.Attributes.Add(MemberName);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'Field';
      aItem.Attributes.Add(aCMD.Field['name'].AsString);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'CID';
      aItem.Attributes.Add(aCMD.Field['cid'].AsString);
      Meta.Add(aItem);

      aItem := TmncMetaItem.Create;
      aItem.Name := 'Sequence NO';
      aItem.Attributes.Add(aCMD.Field['seqno'].AsString);
      Meta.Add(aItem);
    end;
  finally
    aCMD.Free;
  end;
end;

procedure TmncSQLiteMeta.EnumFields(Meta: TmncMetaItems; MemberName: string;
  Options: TschmEnumOptions);
var
  aCMD: TmncSQLiteCommand;
  i: Integer;
  aItem: TmncMetaItem;
begin
  aCMD := CreateCMD('pragma table_info(''' + (MemberName) + ''')' + GetSortSQL(Options));
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.EOF do
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := aCMD.Field['name'].AsString;
      aItem.Attributes.Add(aCMD.Field['type'].AsString);
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['pk'].AsInteger <> 0)));
      aItem.Attributes.Add(IntToStr(ord(aCMD.Field['notnull'].AsInteger <> 0)));
      aItem.Attributes.Add(aCMD.Field['dflt_value'].AsString);
      aItem.Attributes.Add(aCMD.Field['cid'].AsString);
      Meta.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

end.

