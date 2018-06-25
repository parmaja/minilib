unit mncSQLiteORM;
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
  mncMeta, mncORM, mncSQLite;

type

  { TmncORMSQLite }

  TmncORMSQLite = class(TmncORM)
  protected
    type

      { TormSchemaSQLite }

      TormSchemaSQLite = class(TormSchema)
      public
        function GenerateSQL(vSQL: TStringList): Boolean; override;
      end;

      { TormDatabaseSQLite }

      TormDatabaseSQLite = class(TormDatabase)
      public
        function GenerateSQL(vSQL: TStringList): Boolean; override;
      end;

      { TormTableSQLite }

      TormTableSQLite = class(TormTable)
      public
        function GenerateSQL(vSQL: TStringList): Boolean; override;
      end;

      { TormFieldSQLite }

      TormFieldSQLite = class(TormField)
      public
        function GenerateSQL(vSQL: TStringList): Boolean; override;
      end;
  public
    function CreateDatabase(AName: string): TormDatabase; override;
    function CreateSchema(ADatabase: TormDatabase; AName: string): TormSchema; override;
    function CreateTable(ASchema: TormSchema; AName: string): TormTable; override;
    function CreateField(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []): TormField; override;
  end;

implementation

{ TmncORMSQLite.TormFieldSQLite }

function TmncORMSQLite.TormFieldSQLite.GenerateSQL(vSQL: TStringList): Boolean;
begin
  Result := True;
end;

{ TmncORMSQLite.TormTableSQLite }

function TmncORMSQLite.TormTableSQLite.GenerateSQL(vSQL: TStringList): Boolean;
begin
  Result := True;
end;

{ TmncORMSQLite.TormDatabaseSQLite }

function TmncORMSQLite.TormDatabaseSQLite.GenerateSQL(vSQL: TStringList): Boolean;
begin
  Result := True; //in sqlite no need to create, hmmm ok , let me find it on internet
end;

{ TmncORMSQLite.TormSchemaSQLite }

function TmncORMSQLite.TormSchemaSQLite.GenerateSQL(vSQL: TStringList): Boolean;
begin
  Result := True; //nothing to do
end;

{ TmncORMSQLite }

function TmncORMSQLite.CreateSchema(ADatabase: TormDatabase; AName: string): TormSchema;
begin
  Result := TormSchemaSQLite.Create(ADatabase, AName);
end;

function TmncORMSQLite.CreateDatabase(AName: string): TormDatabase;
begin
  Result := TormDatabaseSQLite.Create(AName);
end;

function TmncORMSQLite.CreateTable(ASchema: TormSchema; AName: string): TormTable;
begin
  Result := TormTableSQLite.Create(ASchema, AName);
end;

function TmncORMSQLite.CreateField(ATable: TormTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions): TormField;
begin
  Result := TormFieldSQLite.Create(ATable, AName, AFieldType, AOptions);
end;

end.
