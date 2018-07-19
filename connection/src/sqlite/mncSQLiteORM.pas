unit mncSQLiteORM;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL (modified of http://www.gnu.org/licenses/lgpl.html)
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  SysUtils, Classes, mnUtils,
  mncMeta, mncORM, mncSQLite;

type

  { TmncORMSQLite }

  TmncORMSQLite = class(TmncORM)
  protected
    type

      { TDatabaseSQLite }

      TDatabaseSQLite = class(TormHelper)
      public
        function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TSchemaSQLite }

      TSchemaSQLite = class(TormHelper)
      public
        function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TTableSQLite }

      TTableSQLite = class(TormHelper)
      public
        function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldsSQLite }

      TFieldsSQLite = class(TormHelper)
      public
        function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TFieldSQLite }

      TFieldSQLite = class(TormHelper)
      public
        function ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;
  protected
    class function FieldTypeToString(FieldType:TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
  end;

implementation

{ TmncORMSQLite.TFieldsSQLite }

function TmncORMSQLite.TFieldsSQLite.ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  GenerateObjects(AObject, SQL, vLevel);
  Result := True;
end;

{ TmncORMSQLite.TFieldSQLite }

function TmncORMSQLite.TFieldSQLite.ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
//  vSQL.Send(LevelStr(vLevel) + Name + ' as Integer'); bug in fpc needs to reproduce but i can
  with AObject as TField do
  begin
    SQL.Add(LevelStr(vLevel) + Name + ' as '+ FieldTypeToString(FieldType, FieldSize));
  end;
  Result := True;
end;

{ TmncORMSQLite.TTableSQLite }

function TmncORMSQLite.TTableSQLite.ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  Field: TField;
begin
  Result := True;
  with AObject as TTable do
  begin
    SQL.Add('create table ' + Name);
    SQL.Add('(');
    for Field in Fields do
    begin
      SQL.Params.Values['Table'] := Name;
      Field.GenerateSQL(SQL, vLevel + 1);
    end;
    SQL.Add(')');
  end;
end;

{ TmncORMSQLite.TDatabaseSQLite }

function TmncORMSQLite.TDatabaseSQLite.ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  GenerateObjects(AObject, SQL, vLevel);
  Result := True;
end;

{ TmncORMSQLite.SchemaSQLite }

function TmncORMSQLite.TSchemaSQLite.ProduceSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  GenerateObjects(AObject, SQL, vLevel);
  Result := True;
end;

{ TmncORMSQLite }

class function TmncORMSQLite.FieldTypeToString(FieldType: TormFieldType; FieldSize: Integer): String;
begin
  case FieldType of
    ftString: Result := 'varchar('+IntToStr(FieldSize)+')';
    ftBoolean: Result := 'boolean';
    ftInteger: Result := 'integer';
    ftCurrency: Result := 'decimal(12, 4)';
    ftFloat: Result := 'float';
    ftDate: Result := 'date';
    ftTime: Result := 'time';
    ftDateTime: Result := 'datetime';
    ftText: Result := 'text';
    ftBlob: Result := 'text';
  end;
end;

procedure TmncORMSQLite.Created;
begin
  inherited Created;
  Register(TDatabase ,TDatabaseSQLite);
  Register(TSchema, TSchemaSQLite);
  Register(TTable, TTableSQLite);
  Register(TFields, TFieldsSQLite);
  Register(TField, TFieldSQLite);
end;

end.
