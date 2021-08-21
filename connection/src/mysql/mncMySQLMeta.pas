unit mncMySQLMeta;
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
  SysUtils, Classes,
  mncMeta, mncConnections, mncSQL, mncMySQL;

type
  { TmncMySQLMeta }

  TmncMySQLMeta = class(TmncSQLMeta)
  private
  protected
    function DoCreateConnection: TmncSQLConnection; override;
  public
    procedure EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions = []); override;
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
  mncDB;

function TmncMySQLMeta.DoCreateConnection: TmncSQLConnection;
begin
  Result := TmncMySQLConnection.Create;
end;

procedure TmncMySQLMeta.EnumDatabases(Meta: TmncMetaItems; Options: TmetaEnumOptions);
var
  aConnection: TmncSQLConnection;
  aSession: TmncSQLSession;
begin
  aConnection := CreateConnection;
  try
    aConnection.Resource := 'mysql';
    aConnection.Connect;
    aSession := aConnection.CreateSession;
    try
      EnumCMD(aSession, Meta, sokDatabase, 'name', 'Database', '', 'select schema_name as name from information_schema.schemata ' + GetSortSQL(Options), []);
    finally
      aSession.Free;
    end;
  finally
    aConnection.Free;
  end;
end;

procedure TmncMySQLMeta.EnumTables(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
  {EnumCMD(Meta, sokTable, 'name',
    'SELECT TABLE_NAME as name FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_SCHEMA = '''+SQLName + ''' ' + GetSortSQL(Options), []);}
  EnumCMD(Session, Meta, sokTable, 'Tables_in_'+ SQLName, 'Database', SQLName, 'SHOW TABLES', []);
end;

procedure TmncMySQLMeta.EnumViews(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumProcedures(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumSequences(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumFunctions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumExceptions(Meta: TmncMetaItems; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumDomains(Meta: TmncMetaItems;
  Options: TmetaEnumOptions);
begin

end;

procedure TmncMySQLMeta.EnumConstraints(Meta: TmncMetaItems;
  SQLName: string; Options: TmetaEnumOptions);
begin

end;

procedure TmncMySQLMeta.EnumTriggers(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumIndices(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.GetTriggerSource(Strings: TStringList; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.GetIndexInfo(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
begin
end;

procedure TmncMySQLMeta.EnumFields(Meta: TmncMetaItems; SQLName: string; Options: TmetaEnumOptions);
var
  aCMD: TmncSQLCommand;
  aItem: TmncMetaItem;
begin
  aCMD := CreateCMD('show columns from ' + SQLName);
  try
    aCMD.Prepare;
    aCMD.Execute;
    while not aCMD.Done do
    begin
      aItem := TmncMetaItem.Create;
      aItem.Name := aCMD.Field['field'].AsString;
      aItem.Kind := sokField;
      aItem.SQLName := aItem.Name;
      aItem.SQLType := 'Field';
      aItem.Master := 'Table';

      aItem.Definitions['Type'] := 'Field';
      aItem.Definitions['Table'] := SQLName;
      aItem.Definitions['Field'] := aItem.Name;

      aItem.Attributes.Add('type', aCMD.Field['type'].AsString);
      aItem.Attributes.Add('key', aCMD.Field['key'].AsString);
      aItem.Attributes.Add('null', aCMD.Field['null'].AsString);
      aItem.Attributes.Add('default', aCMD.Field['default'].AsString);
      aItem.Attributes.Add('extra', aCMD.Field['extra'].AsString);
      Meta.Add(aItem);
      aCMD.Next;
    end;
  finally
    aCMD.Free;
  end;
end;

initialization
  Engines.RegisterMeta(TmncMySQLConnection.EngineName, TmncMySQLMeta);
end.
