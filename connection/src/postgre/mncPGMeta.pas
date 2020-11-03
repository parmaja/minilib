unit mncPGMeta;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher, zaherdirkey>
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
  mncMeta, mncConnections, mncPostgre;

type
  { TmncPGMeta }

  TmncPGMeta = class(TmncSQLMeta)
  private
  protected
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TmetaEnumOptions):string;
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions =[]); override;
    procedure EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); override;
    procedure EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;

    procedure EnumConstraints(Meta: TmncMetaItems; MemberName: string = ''; Options: TmetaEnumOptions = []); override;
    procedure EnumTriggers(Meta: TmncMetaItems; MemberName: string = ''; Options: TmetaEnumOptions = []); override;
    procedure EnumIndices(Meta: TmncMetaItems; MemberName: string = ''; Options: TmetaEnumOptions = []); override;
    //source
    procedure GetTriggerSource(Strings:TStringList; MemberName: string; Options: TmetaEnumOptions = []); override;
    procedure GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions = []); override;
  end;

implementation

uses
  mncDB, mncSQL;

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string; Fields: array of string);
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

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; vKind: TmetaKind; SQL: string);
begin
  EnumCMD(Meta, vKind, SQL, []);
end;

procedure TmncPGMeta.FetchCMD(Strings: TStringList; SQL: string);
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

function TmncPGMeta.GetSortSQL(Options: TmetaEnumOptions): string;
begin
  if ekSort in Options then
    Result := ' order by name'
  else
    Result := '';
end;

procedure TmncPGMeta.EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions);
var
  conn: TmncPGConnection;
  session: TmncPGSession;
  cmd: TmncPGCommand;
begin
  conn := TmncPGConnection.Create;
  try
    conn.Resource := 'postgres';
    conn.ServerInfo := ServerInfo;

    conn.ClientEncoding := 'UNICODE';
    conn.ByteaOutput := 'escape';
    conn.DateStyle := 'iso, mdy';

    conn.Connect;
    //PGConn.AutoStart : = true;
    session := conn.CreateSession as TmncPGSession;
    try
      session.Start;

      Meta.Clear;
      cmd := session.CreateCommand as TmncPGCommand;
      try
        cmd.SQL.Text := 'SELECT datname as name FROM pg_database';
        cmd.SQL.Add('WHERE datistemplate = false and datname <> ''postgres''');
        //cmd.SQL.Add('or datname like ''%_temp%'')');
        cmd.SQL.Add('order by datname');

        if cmd.Execute then
        begin
          while not cmd.Done do
          begin
            Meta.Add(cmd.Field['name'].AsString);
            //Log(cmd.Field['name'].AsString);
            cmd.Next;
          end;
        end;
      finally
        cmd.Free;
      end;
    finally
      FreeAndNil(session);
    end;
  finally
    FreeAndNil(conn);
  end;
end;

procedure TmncPGMeta.EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
  EnumCMD(Meta, sokTable, 'select tablename as name FROM pg_catalog.pg_tables where schemaname != ''pg_catalog'' and schemaname != ''information_schema'' ' + GetSortSQL(Options));
end;

procedure TmncPGMeta.EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncPGMeta.EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncPGMeta.EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncPGMeta.EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncPGMeta.EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin

end;

procedure TmncPGMeta.EnumConstraints(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncPGMeta.EnumTriggers(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncPGMeta.EnumIndices(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncPGMeta.GetTriggerSource(Strings: TStringList; MemberName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncPGMeta.GetIndexInfo(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncPGMeta.EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
begin
  Meta.Header.Add('type', 'Type');
  Meta.Header.Add('size', 'Size');
  Meta.Header.Add('nullable', 'Nullable');
  aCMD := CreateCMD('SELECT * FROM information_schema.columns WHERE table_name = ''' + SQLName + '''' + GetSortSQL(Options));
  //aCMD := CreateCMD('pragma table_info(''' + (SQLName) + ''')' + GetSortSQL(Options));
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := aCMD.Field['column_name'].AsString;
      aItem.Attributes.Add('type', aCMD.Field['data_type'].AsString);
      aItem.Attributes.Add('size', IntToStr(ord(aCMD.Field['character_maximum_length'].AsInteger)));
      aItem.Attributes.Add('nullable', aCMD.Field['is_nullable'].AsString);
      Meta.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

initialization
  Engines.RegisterMeta(TmncPGConnection.EngineName, TmncPGMeta);
end.
