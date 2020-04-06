unit mncSQLiteORM;
{**
 *  This file is part of the "Mini Connections"
 *
 * @license   modifiedLGPL modified of http://www.gnu.org/licenses/lgpl.html
 *            See the file COPYING.MLGPL, included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}
{
  TODO:
    use foUnique
}
{$IFDEF FPC}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  SysUtils, Classes, mnUtils, Variants,
  mncORM, mncConnections, mncSQLite;

type

  { TmncSQLiteORM }

  TmncSQLiteORM = class(TmncStdORM)
  protected
    type

      { TDatabaseSQLite }

      TDatabaseSQLite = class(TormGenerator)
      public
      end;

      { TSchemaSQLite }

      TSchemaSQLite = class(TormGenerator)
      public
      end;

      { TTableSQLite }

      TTableSQLite = class(TTableStd)
      public
        constructor Create; override;
        function GenForignKey(Field: TField; AExternal: Boolean): string; virtual;
      end;

      { TFieldsSQLite }

      TFieldsSQLite = class(TFieldsStd)
      public
      end;

      { TFieldSQLite }

      TFieldSQLite = class(TFieldStd)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

      { TInsertDataSQLite }

      TInsertDataSQLite = class(TormGenerator)
      public
        function DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean; override;
      end;

  protected
    class function FieldTypeToString(FieldType:TormFieldType; FieldSize: Integer): String;
    procedure Created; override;
  public
    class function GetConnectionClass: TmncConnectionClass; override;
  end;

implementation

{ TmncSQLiteORM.TInsertDataSQLite }

function TmncSQLiteORM.TInsertDataSQLite.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
var
  o: TormObject;
  i: Integer;
begin
  i := 0;
  with AObject as TInsertData do
  begin
    SQL.Add('insert into ' + Table.QuotedSQLName + '(' );
    for o in this do
    begin
      if i > 0 then
        SQL.Add(',', []);
      SQL.Add(Table.Prefix + (o as TFieldValue).Name); //todo need to use genname
      Inc(i);
    end;
    SQL.Add(') values (');

    i := 0;
    for o in this do
    begin
      if i > 0 then
        SQL.Add(',', []);
      SQL.Add(ValueToStr((o as TFieldValue).Value));
      Inc(i);
    end;
    SQL.Add(')', [cboEndChunk, cboEndLine]);
  end;
  Result := True;
end;

{ TmncSQLiteORM.TFieldSQLite }

function TmncSQLiteORM.TFieldSQLite.DoGenerateSQL(AObject: TormSQLObject; SQL: TCallbackObject; vLevel: Integer): Boolean;
begin
  with AObject as TField do
  begin
    SQL.Add(vLevel, QuotedSQLName + ' '+ FieldTypeToString(FieldType, FieldSize));
    if (foNotNull in Options) or (foPrimary in Options) then
      SQL.Add(' not null');
    if (foPrimary in Options) then
      SQL.Add(' primary key');
    if (foSequenced in Options) then
      SQL.Add(' autoincrement');
    if not VarIsEmpty(DefaultValue) then
    begin
      if VarType(DefaultValue) = varString then
        SQL.Add(' default ''' + DefaultValue + '''')
      else
        SQL.Add(' default ' + VarToStr(DefaultValue));
    end;
  end;
  Result := True;
end;

constructor TmncSQLiteORM.TTableSQLite.Create;
begin
  inherited Create;
  GenerateOptions := [tgnExternalIndexes];
end;

function TmncSQLiteORM.TTableSQLite.GenForignKey(Field: TField; AExternal: Boolean): string;
begin
  Result := 'constraint REF_' + Field.Table.Name + '_' +  Field.ReferenceInfo.Table.Name + Field.ReferenceInfo.Field.Name + ' foreign key (' + Field.QuotedSQLName + ')'
          +' references ' + Field.ReferenceInfo.Table.QuotedSQLName + '(' + Field.ReferenceInfo.Field.QuotedSQLName + ')';
end;

{ TmncSQLiteORM }

class function TmncSQLiteORM.FieldTypeToString(FieldType: TormFieldType; FieldSize: Integer): String;
begin
  case FieldType of
    ftString: Result := 'varchar('+IntToStr(FieldSize)+')';
    ftBoolean: Result := 'boolean';
    ftSmallInteger: Result := 'smllint';
    ftInteger: Result := 'integer';
    ftBigInteger: Result := 'bigint';
    ftCurrency: Result := 'decimal(12, 4)';
    ftFloat: Result := 'float';
    ftDate: Result := 'date';
    ftTime: Result := 'time';
    ftDateTime: Result := 'datetime';
    ftText: Result := 'text';
    ftBlob: Result := 'text';
  end;
end;

procedure TmncSQLiteORM.Created;
begin
  inherited Created;
  RegisterGenerator(TDatabase ,TDatabaseSQLite);
  RegisterGenerator(TSchema, TSchemaSQLite);
  RegisterGenerator(TTable, TTableSQLite);
  RegisterGenerator(TFields, TFieldsSQLite);
  RegisterGenerator(TField, TFieldSQLite);
  RegisterGenerator(TInsertData, TInsertDataSQLite);
end;

class function TmncSQLiteORM.GetConnectionClass: TmncConnectionClass;
begin
  Result := TmncSQLiteConnection;
end;

end.

