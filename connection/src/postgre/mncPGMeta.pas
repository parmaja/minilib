unit mncPGMeta;
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
  mncMeta, mncConnections, mncPostgre;

type
  { TmncPGMeta }

  TmncPGMeta = class(TmncSQLMeta)
  private
  protected
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string); overload;//use field 'name'
    procedure EnumCMD(Meta: TmncMetaItems; SQL: string); overload;
    procedure FetchCMD(Strings:TStringList; SQL: string);//use field 'name'
    function GetSortSQL(Options: TmetaEnumOptions):string;
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions =[]); override;
    procedure EnumTables(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
    procedure EnumFields(Meta: TmncMetaItems; MemberName: string = ''; Options: TmetaEnumOptions = []); override;
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

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; SQL: string; Fields: array of string);
begin
end;

procedure TmncPGMeta.EnumCMD(Meta: TmncMetaItems; SQL: string);
begin
end;

procedure TmncPGMeta.FetchCMD(Strings: TStringList; SQL: string);
begin
end;

function TmncPGMeta.GetSortSQL(Options: TmetaEnumOptions): string;
begin
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

procedure TmncPGMeta.EnumFields(Meta: TmncMetaItems; MemberName: string; Options: TmetaEnumOptions);
begin
end;

initialization
  Engines.RegisterMeta(TmncPGConnection.EngineName, TmncPGMeta);
end.
