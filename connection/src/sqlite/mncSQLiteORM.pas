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

      { TormDatabaseSQLite }

      TormDatabaseSQLite = class(TDatabase)
      public
        function DoSQL(vSQL: TStrings; Params: TStringList): Boolean; override;
      end;

      { TormSchemaSQLite }

      TormSchemaSQLite = class(TSchema)
      public
        function DoSQL(vSQL: TStrings; Params: TStringList): Boolean; override;
      end;

      { TormTableSQLite }

      TormTableSQLite = class(TTable)
      public
        function DoSQL(vSQL: TStrings; Params: TStringList): Boolean; override;
      end;

      { TormFieldSQLite }

      TormFieldSQLite = class(TField)
      public
        function DoSQL(vSQL: TStrings; Params: TStringList): Boolean; override;
      end;
  protected
    procedure Created; override;
  public
    function CreateDatabase(AName: string): TDatabase; override;
    function CreateSchema(ADatabase: TDatabase; AName: string): TSchema; override;
    function CreateTable(ASchema: TSchema; AName: string): TTable; override;
    function CreateField(ATable: TTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions = []): TField; override;
  end;

implementation

{ TmncORMSQLite.TormFieldSQLite }

function TmncORMSQLite.TormFieldSQLite.DoSQL(vSQL: TStrings; Params: TStringList): Boolean;
begin
  Result := True;
end;

{ TmncORMSQLite.TormTableSQLite }

function TmncORMSQLite.TormTableSQLite.DoSQL(vSQL: TStrings; Params: TStringList): Boolean;
begin
  Result := True;
end;

{ TmncORMSQLite.TormDatabaseSQLite }

function TmncORMSQLite.TormDatabaseSQLite.DoSQL(vSQL: TStrings; Params: TStringList): Boolean;
begin
  Result := True; //in sqlite no need to create, hmmm ok , let me find it on internet
end;

{ TmncORMSQLite.TormSchemaSQLite }

function TmncORMSQLite.TormSchemaSQLite.DoSQL(vSQL: TStrings; Params: TStringList): Boolean;
begin
  Result := True; //nothing to do
end;

{ TmncORMSQLite }

function TmncORMSQLite.CreateSchema(ADatabase: TDatabase; AName: string): TSchema;
begin
  Result := TormSchemaSQLite.Create(ADatabase, AName);
end;

procedure TmncORMSQLite.Created;
begin
  inherited Created;
  //Register(TormFieldSQLite);
end;

function TmncORMSQLite.CreateDatabase(AName: string): TDatabase;
begin
  Result := TormDatabaseSQLite.Create(Self, AName);
end;

function TmncORMSQLite.CreateTable(ASchema: TSchema; AName: string): TTable;
begin
  Result := TormTableSQLite.Create(ASchema, AName);
end;

function TmncORMSQLite.CreateField(ATable: TTable; AName: string; AFieldType: TormFieldType; AOptions: TormFieldOptions): TField;
begin
  Result := TormFieldSQLite.Create(ATable.Fields, AName, AFieldType, AOptions);
end;

end.
