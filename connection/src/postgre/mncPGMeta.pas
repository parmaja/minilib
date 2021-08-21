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
  SysUtils, Classes, contnrs, mnUtils,
  mncMeta, mncConnections, mncPostgre, mncSQL;

type
  { TmncPGMeta }

  TmncPGMeta = class(TmncSQLMeta)
  private
  protected
    function QuoteIt(S: string): string; override;
    function CreateConnection: TmncSQLConnection;
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions =[]); override;
    procedure EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions = []); override;
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
  mncDB;

function TmncPGMeta.QuoteIt(S: string): string;
begin
  if S <> '' then
  begin
    if IsAllLowerCase(S) then
      Result := S
    else
      Result := '"' + S + '"';
  end
  else
    Result := '';
end;

function TmncPGMeta.CreateConnection: TmncSQLConnection;
begin
  Result := TmncPGConnection.Create;
end;

procedure TmncPGMeta.EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions);
var
  conn: TmncPGConnection;
  session: TmncPGSession;
  cmd: TmncPGCommand;
  aMetaItem: TmncMetaItem;
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
            aMetaItem := Meta.Add(cmd.Field['name'].AsString);
            aMetaItem.SQLName := QuoteIt(aMetaItem.Name);
            aMetaItem.SQLType := 'Database';
            aMetaItem.Master := 'Databases';

            aMetaItem.Definitions['Type'] := 'Database';
            aMetaItem.Definitions['Database'] := aMetaItem.Name;

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

procedure TmncPGMeta.EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
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
  aCMD := CreateCMD('SELECT * FROM information_schema.columns WHERE table_name = ''' + SQLName + '''' + GetSortSQL(Options));
  //aCMD := CreateCMD('pragma table_info(''' + (SQLName) + ''')' + GetSortSQL(Options));
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := aCMD.Field['column_name'].AsString;
      aItem.SQLName := QuoteIt(aItem.Name);
      aItem.SQLType := 'Field';
      aItem.Master := 'Table';

      aItem.Definitions['Type'] := 'Field';
      aItem.Definitions['Table'] := SQLName;
      aItem.Definitions['Field'] := aItem.Name;

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
